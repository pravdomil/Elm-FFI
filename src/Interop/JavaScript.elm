module Interop.JavaScript exposing (..)

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


cli : Task String String -> Program () () ()
cli a =
    let
        cmd : Cmd ()
        cmd =
            a
                |> Task.andThen
                    (\v ->
                        log v
                            |> Task.andThen (\_ -> exit 0)
                            |> Task.mapError errorToString
                    )
                |> Task.onError
                    (\v ->
                        logError v
                            |> Task.andThen (\_ -> exit 1)
                            |> Task.mapError errorToString
                    )
                |> Task.attempt (\_ -> ())

        log : String -> Task Error Decode.Value
        log _ =
            run "console.log(_v8)"

        logError : String -> Task Error Decode.Value
        logError _ =
            run "console.error(_v9)"

        exit : Int -> Task Error Decode.Value
        exit _ =
            run "process.exit(_v7)"
    in
    Platform.worker
        { init = \_ -> ( (), cmd )
        , update = \_ _ -> ( (), Cmd.none )
        , subscriptions = \_ -> Sub.none
        }
