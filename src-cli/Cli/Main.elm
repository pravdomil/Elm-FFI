module Cli.Main exposing (..)

import Cli.Options as Options exposing (Options)
import Cli.Patch as Patch
import Interop.JavaScript as JavaScript
import Json.Decode as Decode
import Parser
import Task exposing (Task)


main : Program () () ()
main =
    JavaScript.cli mainTask



--


mainTask : { args : List String } -> Task String String
mainTask { args } =
    Options.parse (List.drop 2 args)
        |> taskFromResult
        |> Task.mapError CannotParseArgs
        |> Task.andThen checkFiles
        |> Task.andThen (\v -> v.files |> List.map (patchFile v) |> Task.sequence)
        |> Task.map
            (\v ->
                let
                    count : String
                    count =
                        case List.length v of
                            1 ->
                                "1 file"

                            vv ->
                                String.fromInt vv ++ " files"
                in
                "Elm FFI patched " ++ count ++ "."
            )
        |> Task.mapError (errorToString >> (\v -> v ++ "\n"))


checkFiles : Options -> Task Error Options
checkFiles a =
    case a.files of
        [] ->
            Task.fail NoInputFiles

        _ ->
            Task.succeed a


patchFile : Options -> String -> Task Error ()
patchFile opt a =
    let
        applyPatch : String -> Task Error String
        applyPatch b =
            Task.succeed (Patch.apply b)
    in
    read a
        |> Task.andThen applyPatch
        |> Task.andThen applyShebang
        |> Task.andThen applyRun
        |> Task.andThen (write a)



--


type Error
    = CannotParseArgs (List Parser.DeadEnd)
    | NoInputFiles
    | JavaScriptError JavaScript.Error


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

        NoInputFiles ->
            usage

        JavaScriptError b ->
            JavaScript.errorToString b



--


taskFromResult : Result x a -> Task x a
taskFromResult a =
    case a of
        Ok b ->
            Task.succeed b

        Err b ->
            Task.fail b
