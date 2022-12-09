module ElmFfi.Patch exposing (apply)

import Parser as P exposing ((|.), (|=))


apply : String -> Result (List P.DeadEnd) String
apply a =
    a
        |> stringToFunction
        |> Result.map implementRunTask
        |> Result.map
            (String.replace
                "\tfunction sendToApp(msg, viewMetadata)\n"
                ("\tscope.ports = ports\n"
                    ++ "\tfunction sendToApp(msg, viewMetadata)\n"
                )
            )
        |> Result.map
            (String.replace
                "var $author$project$JavaScript$Decoder$bytes = $elm$json$Json$Decode$fail('Compiled file needs to be processed via elm-ffi command.');"
                "var $author$project$JavaScript$Decoder$bytes = __Json_decodePrim(function(a) { return a instanceof ArrayBuffer ? __Result_Ok(a) : __Json_expecting('a Buffer', a) });"
            )



--


implementRunTask : String -> String
implementRunTask a =
    a
        |> String.replace
            "var task = function (arg_) {\n\t\t\treturn $elm$core$Task$fail($author$project$JavaScript$FileNotPatched);\n\t\t};"
            fn


fn : String
fn =
    """
var task = function(arg_) {
  return _Scheduler_binding(function(callback) {
    var canceled = false
    var cancelFn = undefined

    function onCancel(a) {
      cancelFn = a
    }

    function try_(fn) {
      try       { return { $: 0, a: fn() } }
      catch (e) { return { $: 1, a: e    } }
    }

    function ok(a) {
      if (canceled === false) callback(_Scheduler_succeed(_Json_wrap(a)))
    }

    function err(a) {
      if (canceled === false) callback(_Scheduler_fail(toError(_Json_wrap(a))))
    }

    var a = try_(function() { return code(_Json_unwrap(arg_), onCancel) })

    if (a.$ === 0) {
      if (typeof Promise !== "undefined" && a.a instanceof Promise) {
        a.a.then(ok).catch(err)
      }
      else {
        ok(a.a)
      }
    }
    else {
      err(a.a)
    }

    return function() {
      if (cancelFn) {
        var a = try_(cancelFn)
        if (a.$ === 1) err(a.a)
      }
      canceled = true
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
    "$author$project$JavaScript" ++ "$run"


stringToFunction : String -> Result (List P.DeadEnd) String
stringToFunction a =
    a
        |> P.run parser
        |> Result.map
            (\x ->
                x
                    |> List.map
                        (\x2 ->
                            case x2 of
                                Code b ->
                                    b

                                Run b ->
                                    runFnName ++ ", function(a, onCancel) { return " ++ String.trim b ++ " },"
                        )
                    |> String.join ""
            )


parser : P.Parser (List Fragment)
parser =
    let
        spaces : P.Parser ()
        spaces =
            P.chompWhile (\x -> x == ' ' || x == '\t' || x == '\n' || x == '\u{000D}')

        loop : List Fragment -> P.Parser (P.Step (List Fragment) (List Fragment))
        loop acc =
            P.oneOf
                [ P.succeed (\_ -> P.Done (List.reverse acc))
                    |= P.end
                , P.succeed (\_ -> P.Loop (Code (runFnName ++ " = ") :: acc))
                    |= P.symbol (runFnName ++ " = ")
                , P.succeed (\x -> P.Loop (Run x :: acc))
                    |. P.symbol runFnName
                    |. spaces
                    |. P.symbol ","
                    |. spaces
                    |= quotedString
                    |. spaces
                    |. P.symbol ","
                , P.succeed (\x -> P.Loop (Code x :: acc))
                    |= P.getChompedString (P.chompUntilEndOr runFnName)
                ]
    in
    P.loop [] loop


quotedString : P.Parser String
quotedString =
    let
        loop : List String -> P.Parser (P.Step (List String) String)
        loop acc =
            P.oneOf
                [ P.succeed (\x -> P.Loop (x :: acc))
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
                , P.succeed (\x -> P.Loop (x :: acc))
                    |= P.getChompedString (P.chompWhile (\x -> x /= '\\' && x /= '\''))
                ]
    in
    P.symbol "'"
        |> P.andThen (\_ -> P.loop [] loop)
