module Cli.Options exposing (..)

import Parser as P exposing ((|.), (|=), Parser)


type alias Options =
    { shebang : Bool
    , autorun : Bool
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
    , "autorun: " ++ boolToString a.autorun
    , "legacy:  " ++ boolToString a.legacy
    , "files:   " ++ String.join "," a.files
    ]
        |> String.join "\n"



--


parse : List String -> Result (List P.DeadEnd) Options
parse a =
    a |> String.join "\u{0000}" |> P.run parser


parser : Parser Options
parser =
    let
        loop : Options -> Parser (P.Step Options Options)
        loop acc =
            P.oneOf
                [ P.succeed (\v -> P.Loop { acc | shebang = v })
                    |= boolArg "shebang"
                , P.succeed (\v -> P.Loop { acc | autorun = v })
                    |= boolArg "autorun"
                , P.succeed (\v -> P.Loop { acc | legacy = v })
                    |= boolArg "legacy"

                --
                , P.succeed (\v -> P.Loop { acc | files = v :: acc.files })
                    |= P.getChompedString
                        (P.succeed identity
                            |= P.chompIf ((/=) '\u{0000}')
                            |. P.chompUntilEndOr "\u{0000}"
                        )
                    |. P.oneOf
                        [ P.symbol "\u{0000}"
                        , P.end
                        ]

                --
                , P.succeed (\_ -> P.Done { acc | files = List.reverse acc.files })
                    |= P.end
                ]

        boolArg : String -> Parser Bool
        boolArg name =
            P.succeed True
                |. P.symbol ("--" ++ name)
                |. P.oneOf
                    [ P.symbol "\u{0000}"
                    , P.end
                    ]
    in
    P.loop
        { shebang = False
        , autorun = False
        , legacy = False
        , files = []
        }
        loop