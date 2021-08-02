module Interop.JavaScript exposing (Error(..), cli, cliWithStdin, errorToString, run, run2)

{-| Part of <https://github.com/pravdomil/Elm-FFI>.
-}

import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import Task exposing (Task)


run : String -> (a -> Decode.Value) -> Decoder b -> a -> Task Error b
run code encoder decoder =
    let
        task : Decode.Value -> Task Error Decode.Value
        task arg =
            Task.fail FileNotPatched
    in
    \arg ->
        task (encoder arg)
            |> Task.andThen
                (\v ->
                    case v |> Decode.decodeValue decoder of
                        Ok b ->
                            Task.succeed b

                        Err b ->
                            Task.fail (DecodeError b)
                )


run2 : String -> (a -> Decode.Value) -> (b -> Decode.Value) -> Decoder c -> a -> b -> Task Error c
run2 code encoder encoder2 decoder =
    let
        task : Decode.Value -> Decode.Value -> Task Error Decode.Value
        task arg arg2 =
            Task.fail FileNotPatched
    in
    \arg arg2 ->
        task (encoder arg) (encoder2 arg2)
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
    case a of
        FileNotPatched ->
            "Compiled file needs to be processed via elm-ffi command."

        Exception b ->
            b
                |> Decode.decodeValue (Decode.field "message" Decode.string)
                |> Result.map (\v -> "Got JavaScript exception:\n" ++ v)
                |> Result.withDefault "Got JavaScript exception."

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
        (\() -> Encode.null)
        (Decode.list Decode.string)
        ()


readStdin : Task Error String
readStdin =
    run "require('fs').readFileSync(0, 'utf8')"
        (\() -> Encode.null)
        Decode.string
        ()


writeStdout : String -> Task Error ()
writeStdout =
    run "process.stdout.write(a)"
        Encode.string
        (Decode.succeed ())


writeStderr : String -> Task Error ()
writeStderr =
    run "process.stderr.write(a)"
        Encode.string
        (Decode.succeed ())


exit : Int -> Task Error ()
exit =
    run "process.exit(a)"
        Encode.int
        (Decode.succeed ())
