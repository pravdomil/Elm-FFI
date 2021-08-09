module ElmFfi.Patch exposing (apply)

import Parser as P exposing ((|.), (|=), Parser)


apply : String -> Result (List P.DeadEnd) String
apply a =
    a
        |> stringToFunction
        |> Result.map implementRunTask



--


implementRunTask : String -> String
implementRunTask a =
    a
        |> String.replace
            "var task = function (arg_) {\n\t\t\treturn $elm$core$Task$fail($author$project$Interop$JavaScript$FileNotPatched);\n\t\t};"
            fn


fn : String
fn =
    """
var task = function(arg_) {
  return _Scheduler_binding(function(callback) {
    function ok(a) {
      callback(_Scheduler_succeed(_Json_wrap(a)));
    }

    function err(a) {
      callback(_Scheduler_fail(toException(_Json_wrap(a))));
    }

    var a;
    try       { a = { $: 0, a: code(_Json_unwrap(arg_)) } }
    catch (e) { a = { $: 1, a: e } }

    if (a.$ === 0) {
      if (a.a instanceof Promise) {
        a.a.then(ok).catch(err)
      }
      else {
        ok(a.a)
      }
    }
    else {
      err(a.a)
    }
  })
};
    """



--


type Fragment
    = Code String
    | Run String


runFnName : String
runFnName =
    "$author$project$Interop$JavaScript" ++ "$run"


stringToFunction : String -> Result (List P.DeadEnd) String
stringToFunction a =
    a
        |> P.run parser
        |> Result.map
            (\v ->
                v
                    |> List.map
                        (\vv ->
                            case vv of
                                Code b ->
                                    b

                                Run b ->
                                    runFnName ++ ", function(a) { return " ++ String.trim b ++ " },"
                        )
                    |> String.join ""
            )


parser : Parser (List Fragment)
parser =
    let
        spaces : Parser ()
        spaces =
            P.chompWhile (\v -> v == ' ' || v == '\t' || v == '\n' || v == '\u{000D}')

        loop : List Fragment -> Parser (P.Step (List Fragment) (List Fragment))
        loop acc =
            P.oneOf
                [ P.succeed (\_ -> P.Done (List.reverse acc))
                    |= P.end
                , P.succeed (\_ -> P.Loop (Code (runFnName ++ " = ") :: acc))
                    |= P.symbol (runFnName ++ " = ")
                , P.succeed (\v -> P.Loop (Run v :: acc))
                    |. P.symbol runFnName
                    |. spaces
                    |. P.symbol ","
                    |. spaces
                    |= quotedString
                    |. spaces
                    |. P.symbol ","
                , P.succeed (\v -> P.Loop (Code v :: acc))
                    |= P.getChompedString (P.chompUntilEndOr runFnName)
                ]
    in
    P.loop [] loop


quotedString : Parser String
quotedString =
    let
        loop : List String -> Parser (P.Step (List String) String)
        loop acc =
            P.oneOf
                [ P.succeed (\v -> P.Loop (v :: acc))
                    |. P.symbol "\\"
                    |= P.oneOf
                        [ P.succeed (\_ -> "\n")
                            |= P.symbol "n"
                        , P.succeed (\_ -> "\t")
                            |= P.symbol "t"
                        , P.succeed (\_ -> "\u{000D}")
                            |= P.symbol "r"
                        , P.succeed (\_ -> "'")
                            |= P.symbol "'"
                        ]
                , P.succeed (\_ -> P.Done (String.join "" (List.reverse acc)))
                    |= P.symbol "'"
                , P.succeed (\v -> P.Loop (v :: acc))
                    |= P.getChompedString (P.chompWhile (\v -> v /= '\\' && v /= '\''))
                ]
    in
    P.symbol "'"
        |> P.andThen (\_ -> P.loop [] loop)
