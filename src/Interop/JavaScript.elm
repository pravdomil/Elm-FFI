module Interop.JavaScript exposing (Error(..), cli, cliWithStdin, errorCode, errorMessage, errorToString, run)

{-| Part of <https://github.com/pravdomil/Elm-FFI>.
-}

import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import Task exposing (Task)


run : String -> Decode.Value -> Decoder b -> Task Error b
run code arg decoder =
    let
        _ =
            Exception

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
    | Exception Decode.Value
    | DecodeError Decode.Error


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

        Exception _ ->
            [ Just "Got JavaScript exception."
            , errorCode a |> Maybe.map (\v -> "Code: " ++ v |> indent)
            , errorMessage a |> Maybe.map (\v -> "Message: " ++ v |> indent)
            ]
                |> List.filterMap identity
                |> String.join "\n"

        DecodeError b ->
            "Cannot decode JavaScript value because:\n" ++ Decode.errorToString b


errorCode : Error -> Maybe String
errorCode a =
    case a of
        Exception b ->
            b
                |> Decode.decodeValue (Decode.field "code" Decode.string)
                |> Result.toMaybe

        _ ->
            Nothing


errorMessage : Error -> Maybe String
errorMessage a =
    case a of
        Exception b ->
            b
                |> Decode.decodeValue (Decode.field "message" Decode.string)
                |> Result.toMaybe

        _ ->
            Nothing



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
