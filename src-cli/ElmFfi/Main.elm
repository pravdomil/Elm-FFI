module ElmFfi.Main exposing (..)

import ElmFfi.Options
import ElmFfi.Patch
import FileSystem
import JavaScript
import Parser
import Parser.DeadEnd
import Task
import Task.Extra


main : Program () () ()
main =
    JavaScript.commandLineProgram mainTask



--


mainTask : List String -> Task.Task String String
mainTask args =
    ElmFfi.Options.parse (List.drop 2 args)
        |> Task.Extra.fromResult
        |> Task.mapError CannotParseArgs
        |> Task.andThen checkFiles
        |> Task.andThen (\x -> x.files |> List.map (FileSystem.stringToPath >> patchFile x) |> Task.sequence)
        |> Task.map
            (\x ->
                let
                    count : String
                    count =
                        case List.length x of
                            1 ->
                                "1 file"

                            x2 ->
                                String.fromInt x2 ++ " files"
                in
                "Elm FFI patched " ++ count ++ ".\n"
            )
        |> Task.mapError (errorToString >> (\x -> x ++ "\n"))


checkFiles : ElmFfi.Options.Options -> Task.Task Error ElmFfi.Options.Options
checkFiles a =
    case a.files of
        [] ->
            Task.fail NoInputFiles

        _ ->
            Task.succeed a


patchFile : ElmFfi.Options.Options -> FileSystem.Path -> Task.Task Error ()
patchFile opt path =
    FileSystem.read path
        |> Task.mapError ReadError
        |> Task.andThen
            (\x ->
                ElmFfi.Patch.apply x
                    |> Task.Extra.fromResult
                    |> Task.mapError PatchError
            )
        |> Task.andThen
            (\x ->
                if opt.shebang then
                    --      0o755
                    FileSystem.chmod path 0x01ED
                        |> Task.mapError ChmodError
                        |> Task.map
                            (\_ ->
                                "#!/usr/bin/env node\n" ++ x
                            )

                else
                    Task.succeed x
            )
        |> Task.map
            (\x ->
                if opt.run then
                    String.dropRight 16 x ++ "(0)({ flags: { global: global } })}});}(this));"

                else
                    x
            )
        |> Task.map
            (\x ->
                if opt.legacy then
                    applyLegacy x

                else
                    x
            )
        |> Task.andThen
            (\x ->
                FileSystem.write path x
                    |> Task.mapError WriteError
            )


applyLegacy : String -> String
applyLegacy a =
    a
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



--


type Error
    = CannotParseArgs (List Parser.DeadEnd)
    | NoInputFiles
    | ReadError JavaScript.Error
    | ChmodError JavaScript.Error
    | WriteError JavaScript.Error
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

        ReadError b ->
            "Read error:\n" ++ JavaScript.errorToString b

        ChmodError b ->
            "Chmod error:\n" ++ JavaScript.errorToString b

        WriteError b ->
            "Write error:\n" ++ JavaScript.errorToString b

        PatchError b ->
            "Patch error:\n" ++ Parser.DeadEnd.listToString b
