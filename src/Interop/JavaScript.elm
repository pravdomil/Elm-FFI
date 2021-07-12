module Interop.JavaScript exposing (Error(..), anyDecoder, cli, cliWithStdin, decode, errorToString, run)

{-| Part of <https://github.com/pravdomil/Elm-FFI>.
-}

import Json.Decode as Decode exposing (Decoder)
import Task exposing (Task)


run : String -> Task Error Decode.Value
run _ =
    let
        _ =
            anyDecoder

        _ =
            Exception
    in
    Task.fail FileNotPatched


decode : Decoder a -> Task Error Decode.Value -> Task Error a
decode decoder a =
    a
        |> Task.andThen
            (\v ->
                case v |> Decode.decodeValue decoder of
                    Ok b ->
                        Task.succeed b

                    Err b ->
                        Task.fail (DecodeError b)
            )


anyDecoder : Decoder a
anyDecoder =
    Decode.fail (errorToString FileNotPatched)



--


type Error
    = FileNotPatched
    | Exception Decode.Value
    | DecodeError Decode.Error


errorToString : Error -> String
errorToString a =
    case a of
        FileNotPatched ->
            "Compiled file needs to be processed via elm-ffi command."

        Exception b ->
            "Got JavaScript exception:\n"
                ++ (b
                        |> Decode.decodeValue (Decode.field "message" Decode.string)
                        |> Result.withDefault "No message provided."
                   )

        DecodeError b ->
            "Cannot decode JavaScript value because:\n" ++ Decode.errorToString b



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
        |> decode (Decode.list Decode.string)


readStdin : Task Error String
readStdin =
    run "require('fs').readFileSync(0, 'utf8')"
        |> decode Decode.string


writeStdout : String -> Task Error Decode.Value
writeStdout _ =
    run "process.stdout.write(_v0)"


writeStderr : String -> Task Error Decode.Value
writeStderr _ =
    run "process.stderr.write(_v0)"


exit : Int -> Task Error Decode.Value
exit _ =
    run "process.exit(_v0)"
