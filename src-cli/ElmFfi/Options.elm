module ElmFfi.Options exposing (..)

import Parser as P exposing ((|.), (|=))


type alias Options =
    { shebang : Bool
    , run : Bool
    , legacy : Bool
    , files : List String
    }


toString : Options -> String
toString a =
    let
        boolToString : Bool -> String
        boolToString b =
            if b then
                "true"

            else
                "false"
    in
    [ "shebang: " ++ boolToString a.shebang
    , "run:     " ++ boolToString a.run
    , "legacy:  " ++ boolToString a.legacy
    , "files:   " ++ String.join "," a.files
    ]
        |> String.join "\n"



--


parse : List String -> Result (List P.DeadEnd) Options
parse a =
    a |> String.join "\u{0000}" |> P.run parser


parser : P.Parser Options
parser =
    let
        loop : Options -> P.Parser (P.Step Options Options)
        loop acc =
            P.oneOf
                [ P.succeed (\x -> P.Loop { acc | shebang = x })
                    |= boolArg "shebang"
                , P.succeed (\x -> P.Loop { acc | run = x })
                    |= boolArg "run"
                , P.succeed (\x -> P.Loop { acc | legacy = x })
                    |= boolArg "legacy"

                --
                , P.succeed (\x -> P.Loop { acc | files = x :: acc.files })
                    |= argument

                --
                , P.succeed (\_ -> P.Done { acc | files = List.reverse acc.files })
                    |= P.end
                ]

        boolArg : String -> P.Parser Bool
        boolArg name =
            P.succeed True
                |. P.symbol ("--" ++ name)
                |. argEnd

        argument : P.Parser String
        argument =
            P.getChompedString
                (P.succeed ()
                    |. P.chompIf (\x -> x /= '-' && x /= '\u{0000}')
                    |. P.chompUntilEndOr "\u{0000}"
                )
                |. argEnd

        argEnd : P.Parser ()
        argEnd =
            P.oneOf
                [ P.symbol "\u{0000}"
                , P.end
                ]
    in
    P.loop
        { shebang = False
        , run = False
        , legacy = False
        , files = []
        }
        loop
