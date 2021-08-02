module Cli.Main exposing (..)

import Cli.Options as Options exposing (Options)
import Cli.Patch as Patch
import Interop.JavaScript as JavaScript
import Json.Decode as Decode
import Json.Encode as Encode
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

        applyShebang : String -> Task Error String
        applyShebang b =
            if opt.shebang then
                --      0o755
                chmod a 0x01ED
                    |> Task.map
                        (\_ ->
                            "#!/usr/bin/env node\n" ++ b
                        )

            else
                Task.succeed b

        applyRun : String -> Task Error String
        applyRun b =
            if opt.run then
                Task.succeed (String.dropRight 16 b ++ "(0)()}});}(this));")

            else
                Task.succeed b

        applyLegacy : String -> Task Error String
        applyLegacy b =
            if opt.legacy then
                Task.fail LegacyNotImplemented

            else
                Task.succeed b
    in
    read a
        |> Task.andThen applyPatch
        |> Task.andThen applyShebang
        |> Task.andThen applyRun
        |> Task.andThen applyLegacy
        |> Task.andThen (write a)



--


type Error
    = CannotParseArgs (List Parser.DeadEnd)
    | NoInputFiles
    | JavaScriptError JavaScript.Error
    | LegacyNotImplemented


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

        LegacyNotImplemented ->
            "Legacy option is not implemented."



--


taskFromResult : Result x a -> Task x a
taskFromResult a =
    case a of
        Ok b ->
            Task.succeed b

        Err b ->
            Task.fail b



--


read : String -> Task Error String
read =
    JavaScript.run
        "await require('fs/promises').readFile(a, 'utf-8')"
        Encode.string
        Decode.string
        >> Task.mapError JavaScriptError


write : String -> String -> Task Error ()
write =
    JavaScript.run2 "await require('fs/promises').writeFile(a, b)"
        Encode.string
        Encode.string
        (Decode.succeed ())
        |> (\fn v1 v2 ->
                fn v1 v2
                    |> Task.mapError JavaScriptError
           )


chmod : String -> Int -> Task Error ()
chmod =
    JavaScript.run2 "await require('fs/promises').chmod(a, b)"
        Encode.string
        Encode.int
        (Decode.succeed ())
        |> (\fn v1 v2 ->
                fn v1 v2
                    |> Task.mapError JavaScriptError
           )
