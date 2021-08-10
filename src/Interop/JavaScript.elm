module Interop.JavaScript exposing (Error(..), cli, cliWithStdin, errorToString, run)

{-| Part of <https://github.com/pravdomil/Elm-FFI>.
-}

import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import Task exposing (Task)


run : String -> Decode.Value -> Decoder b -> Task Error b
run code arg decoder =
    let
        toException : Decode.Value -> Error
        toException b =
            Exception
                (b
                    |> Decode.decodeValue (Decode.field "code" Decode.string)
                    |> Result.withDefault ""
                )
                (b
                    |> Decode.decodeValue
                        (Decode.oneOf
                            [ Decode.string
                            , Decode.field "message" Decode.string
                            ]
                        )
                    |> Result.withDefault ""
                )

        task : Decode.Value -> Task Error Decode.Value
        task arg_ =
            Task.fail FileNotPatched
    in
    task arg
        |> Task.andThen
            (\v ->
                case v |> Decode.decodeValue decoder of
                    Ok b ->
                        Task.succeed b

                    Err b ->
                        Task.fail (DecodeError b)
            )



--


type Error
    = FileNotPatched
    | Exception Code Message
    | DecodeError Decode.Error


type alias Code =
    String


type alias Message =
    String


errorToString : Error -> String
errorToString a =
    let
        indent : String -> String
        indent b =
            b
                |> String.split "\n"
                |> List.map ((++) "  ")
                |> String.join "\n"
    in
    case a of
        FileNotPatched ->
            "Compiled file needs to be processed via elm-ffi command."

        Exception code msg ->
            let
                firstLine : String
                firstLine =
                    msg
                        |> String.split "\n"
                        |> List.head
                        |> Maybe.withDefault ""
                        |> String.trim
                        |> (\v ->
                                if String.endsWith "." v then
                                    v

                                else
                                    v ++ "."
                           )
            in
            (if String.isEmpty firstLine then
                "There was an error, but we got no details."

             else
                firstLine
            )
                ++ (if String.isEmpty code then
                        ""

                    else
                        " (" ++ code ++ ")"
                   )

        DecodeError b ->
            "Cannot decode value because:\n" ++ indent (Decode.errorToString b)



--


cli : ({ args : List String } -> Task String String) -> Program () () ()
cli fn =
    cliHelper
        (readArgs
            |> Task.map (\v -> { args = v })
            |> Task.mapError errorToString
            |> Task.andThen fn
        )


cliWithStdin : ({ args : List String, stdin : String } -> Task String String) -> Program () () ()
cliWithStdin fn =
    cliHelper
        (Task.map2 (\v1 v2 -> { args = v1, stdin = v2 })
            readArgs
            readStdin
            |> Task.mapError errorToString
            |> Task.andThen fn
        )



--


cliHelper : Task String String -> Program () () ()
cliHelper a =
    let
        cmd : Cmd ()
        cmd =
            a
                |> Task.andThen
                    (\v ->
                        writeStdout v
                            |> Task.andThen (\_ -> exit 0)
                            |> Task.mapError errorToString
                    )
                |> Task.onError
                    (\v ->
                        writeStderr v
                            |> Task.andThen (\_ -> exit 1)
                            |> Task.mapError errorToString
                    )
                |> Task.attempt (\_ -> ())
    in
    Platform.worker
        { init = \_ -> ( (), cmd )
        , update = \_ _ -> ( (), Cmd.none )
        , subscriptions = \_ -> Sub.none
        }


readArgs : Task Error (List String)
readArgs =
    run "process.argv"
        Encode.null
        (Decode.list Decode.string)


readStdin : Task Error String
readStdin =
    run "require('fs').readFileSync(0, 'utf8')"
        Encode.null
        Decode.string


writeStdout : String -> Task Error ()
writeStdout data =
    run "process.stdout.write(a)"
        (Encode.string data)
        (Decode.succeed ())


writeStderr : String -> Task Error ()
writeStderr data =
    run "process.stderr.write(a)"
        (Encode.string data)
        (Decode.succeed ())


exit : Int -> Task Error ()
exit code =
    run "process.exit(a)"
        (Encode.int code)
        (Decode.succeed ())
