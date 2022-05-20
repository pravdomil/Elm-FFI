module JavaScript exposing (Error(..), ErrorCode(..), ErrorMessage(..), ErrorName(..), commandLineProgram, commandLineProgramWithStdin, errorToString, run)

import Json.Decode
import Json.Encode
import Task


{-| Run JavaScript code.
-}
run : String -> Json.Decode.Value -> Json.Decode.Decoder b -> Task.Task Error b
run code arg decoder =
    let
        toException : Json.Decode.Value -> Error
        toException b =
            Exception
                (b
                    |> Json.Decode.decodeValue (Json.Decode.field "name" Json.Decode.string)
                    |> Result.withDefault ""
                    |> ErrorName
                )
                (b
                    |> Json.Decode.decodeValue
                        (Json.Decode.field "code"
                            (Json.Decode.oneOf
                                [ Json.Decode.string
                                , Json.Decode.int |> Json.Decode.map String.fromInt
                                ]
                            )
                        )
                    |> Result.withDefault ""
                    |> ErrorCode
                )
                (b
                    |> Json.Decode.decodeValue
                        (Json.Decode.oneOf
                            [ Json.Decode.string
                            , Json.Decode.field "message" Json.Decode.string
                            ]
                        )
                    |> Result.withDefault ""
                    |> ErrorMessage
                )

        task : Json.Decode.Value -> Task.Task Error Json.Decode.Value
        task arg_ =
            Task.fail FileNotPatched
    in
    task arg
        |> Task.andThen
            (\v ->
                case v |> Json.Decode.decodeValue decoder of
                    Ok b ->
                        Task.succeed b

                    Err b ->
                        Task.fail (DecodeError b)
            )



--


type Error
    = FileNotPatched
    | Exception ErrorName ErrorCode ErrorMessage
    | DecodeError Json.Decode.Error


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

        Exception name code msg ->
            "There was a runtime error. More details:\n"
                ++ indent
                    (("name: " ++ (\(ErrorName v) -> v) name)
                        ++ "\n"
                        ++ ("code: " ++ (\(ErrorCode v) -> v) code)
                        ++ "\n"
                        ++ ("message: " ++ (\(ErrorMessage v) -> v) msg)
                    )

        DecodeError b ->
            "There was a decode error. More details:\n" ++ indent (Json.Decode.errorToString b)



--


type ErrorName
    = ErrorName String



--


type ErrorCode
    = ErrorCode String



--


type ErrorMessage
    = ErrorMessage String



--


commandLineProgram : (List String -> Task.Task String String) -> Program () () ()
commandLineProgram fn =
    cliHelper
        (readArgs
            |> Task.mapError errorToString
            |> Task.andThen fn
        )


commandLineProgramWithStdin : ({ arguments : List String, stdin : String } -> Task.Task String String) -> Program () () ()
commandLineProgramWithStdin fn =
    cliHelper
        (Task.map2 (\v1 v2 -> { arguments = v1, stdin = v2 })
            readArgs
            readStdin
            |> Task.mapError errorToString
            |> Task.andThen fn
        )



--


cliHelper : Task.Task String String -> Program () () ()
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


readArgs : Task.Task Error (List String)
readArgs =
    run "process.argv"
        Json.Encode.null
        (Json.Decode.list Json.Decode.string)


readStdin : Task.Task Error String
readStdin =
    run "require('fs').readFileSync(0, 'utf8')"
        Json.Encode.null
        Json.Decode.string


writeStdout : String -> Task.Task Error ()
writeStdout data =
    run "process.stdout.write(a)"
        (Json.Encode.string data)
        (Json.Decode.succeed ())


writeStderr : String -> Task.Task Error ()
writeStderr data =
    run "process.stderr.write(a)"
        (Json.Encode.string data)
        (Json.Decode.succeed ())


exit : Int -> Task.Task Error ()
exit code =
    run "process.exit(a)"
        (Json.Encode.int code)
        (Json.Decode.succeed ())
