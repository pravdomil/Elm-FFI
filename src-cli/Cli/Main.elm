module Cli.Main exposing (..)

import Cli.Options as Options exposing (Options)
import Interop.JavaScript as JavaScript
import Parser
import Task exposing (Task)


main : Program () () ()
main =
    JavaScript.cli mainTask


mainTask : { args : List String } -> Task String String
mainTask { args } =
    Options.parse (List.drop 2 args)
        |> taskFromResult
        |> Task.mapError CannotParseArgs
        |> Task.map (\v -> "Welcome to elm-ffi.\n\nI got following options:\n" ++ Options.toString v ++ "\n")
        |> Task.mapError (errorToString >> (\v -> v ++ "\n"))



--


type Error
    = CannotParseArgs (List Parser.DeadEnd)


errorToString : Error -> String
errorToString a =
    let
        usage : String
        usage =
            "Usage: elm-ffi [--shebang] [--run] [--legacy] <file.js>..."
    in
    case a of
        CannotParseArgs _ ->
            usage



--


taskFromResult : Result x a -> Task x a
taskFromResult a =
    case a of
        Ok b ->
            Task.succeed b

        Err b ->
            Task.fail b
