module ElmFfi.Main exposing (..)

import ElmFfi.Options
import ElmFfi.Patch
import JavaScript
import Json.Decode
import Json.Encode
import Parser
import Parser.DeadEnd
import Task


main : Program () () ()
main =
    JavaScript.commandLineProgram mainTask



--


mainTask : List String -> Task.Task String String
mainTask args =
    ElmFfi.Options.parse (List.drop 2 args)
        |> resultToTask
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
        |> Task.map (\v -> v ++ "\n")
        |> Task.mapError (errorToString >> (\v -> v ++ "\n"))


checkFiles : ElmFfi.Options.Options -> Task.Task Error ElmFfi.Options.Options
checkFiles a =
    case a.files of
        [] ->
            Task.fail NoInputFiles

        _ ->
            Task.succeed a


patchFile : ElmFfi.Options.Options -> String -> Task.Task Error ()
patchFile opt a =
    let
        applyPatch : String -> Task.Task Error String
        applyPatch b =
            ElmFfi.Patch.apply b
                |> resultToTask
                |> Task.mapError PatchError

        applyShebang : String -> Task.Task Error String
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

        applyRun : String -> Task.Task Error String
        applyRun b =
            if opt.run then
                Task.succeed (String.dropRight 16 b ++ "(0)({ flags: { global: global } })}});}(this));")

            else
                Task.succeed b

        applyLegacy : String -> Task.Task Error String
        applyLegacy b =
            if opt.legacy then
                Task.succeed
                    (b
                        -- fix nested ternary operator
                        |> String.replace
                            "return x === y ? /*EQ*/ 0 : x < y ? /*LT*/ -1 : /*GT*/ 1;"
                            "return x === y ? /*EQ*/ 0 : (x < y ? /*LT*/ -1 : /*GT*/ 1);"
                        |> String.replace
                            "return (\n\t\tstring.length <= offset\n\t\t\t? -1\n\t\t\t:\n\t\t(string.charCodeAt(offset) & 0xF800) === 0xD800\n\t\t\t? (predicate(_Utils_chr(string.substr(offset, 2))) ? offset + 2 : -1)\n\t\t\t:\n\t\t(predicate(_Utils_chr(string[offset]))\n\t\t\t? ((string[offset] === '\\n') ? -2 : (offset + 1))\n\t\t\t: -1\n\t\t)\n\t);"
                            "if (string.length <= offset) {\n\t\treturn -1\n\t} else if ((string.charCodeAt(offset) & 0xF800) === 0xD800) {\n\t\treturn predicate(__Utils_chr(string.substr(offset, 2))) ? offset + 2 : -1\n\t} else if (predicate(_Utils_chr(string[offset]))) {\n\t\treturn (string[offset] === '\\n') ? -2 : (offset + 1)\n\t} else {\n\t\treturn -1\n\t}"
                        |> String.replace
                            "? (name == 'init')"
                            "? ((name == 'init')"
                        |> String.replace
                            ": (obj[name] = exports[name]);"
                            "): (obj[name] = exports[name]);"
                        -- replace reserved word "char"
                        |> String.replace " char " " char_ "
                        |> String.replace " char;" " char_;"
                        |> String.replace " char." " char_."
                        |> String.replace "\tchar " "\tchar_ "
                        |> String.replace "(char)" "(char_)"
                        |> String.replace "(char." "(char_."
                        -- implement Array.isArray
                        |> String.replace
                            "Array.isArray("
                            "(function(a) { return \"length\" in a })("
                        -- temporary pseudo-implement JSON
                        |> String.replace
                            "JSON.stringify("
                            "(function(a) { return String(a) })("
                        -- fix stack overflow
                        |> String.replace
                            "var res = (ctr > 500) ? A3("
                            "var res = (ctr > 100) ? A3("
                    )

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
    | PatchError (List Parser.DeadEnd)


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

        PatchError b ->
            "Patch error:\n" ++ Parser.DeadEnd.listToString b



--


resultToTask : Result x a -> Task.Task x a
resultToTask a =
    case a of
        Ok b ->
            Task.succeed b

        Err b ->
            Task.fail b



--


read : String -> Task.Task Error String
read path =
    JavaScript.run "require('fs/promises').readFile(a, 'utf-8')"
        (Json.Encode.string path)
        Json.Decode.string
        |> Task.mapError JavaScriptError


write : String -> String -> Task.Task Error ()
write path data =
    JavaScript.run "require('fs/promises').writeFile(a.path, a.data)"
        (Json.Encode.object
            [ ( "path", Json.Encode.string path )
            , ( "data", Json.Encode.string data )
            ]
        )
        (Json.Decode.succeed ())
        |> Task.mapError JavaScriptError


chmod : String -> Int -> Task.Task Error ()
chmod path mode =
    JavaScript.run "require('fs/promises').chmod(a.path, a.mode)"
        (Json.Encode.object
            [ ( "path", Json.Encode.string path )
            , ( "mode", Json.Encode.int mode )
            ]
        )
        (Json.Decode.succeed ())
        |> Task.mapError JavaScriptError
