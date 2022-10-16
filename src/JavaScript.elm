module JavaScript exposing
    ( run
    , commandLineProgram, commandLineProgramWithStdin
    , Error(..), ErrorCode(..), ErrorMessage(..), ErrorName(..), errorToString, decodeError
    )

{-|

@docs run
@docs commandLineProgram, commandLineProgramWithStdin
@docs Error, ErrorCode, ErrorMessage, ErrorName, errorToString, decodeError

-}

import Json.Decode
import Json.Encode
import Task


{-| Run JavaScript code.
-}
run : String -> Json.Decode.Value -> Json.Decode.Decoder b -> Task.Task Error b
run code arg decoder =
    let
        toError : Json.Decode.Value -> Error
        toError =
            decodeError

        task : Json.Decode.Value -> Task.Task Error Json.Decode.Value
        task arg_ =
            Task.fail FileNotPatched
    in
    task arg
        |> Task.andThen
            (\x ->
                case x |> Json.Decode.decodeValue decoder of
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
                    (("name: " ++ (\(ErrorName x) -> x) name)
                        ++ "\n"
                        ++ ("code: " ++ (\(ErrorCode x) -> x) code)
                        ++ "\n"
                        ++ ("message: " ++ (\(ErrorMessage x) -> x) msg)
                    )

        DecodeError b ->
            "There was a decode error. More details:\n" ++ indent (Json.Decode.errorToString b)


decodeError : Json.Decode.Value -> Error
decodeError a =
    Exception
        (a
            |> Json.Decode.decodeValue (Json.Decode.field "name" Json.Decode.string)
            |> Result.withDefault ""
            |> ErrorName
        )
        (a
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
        (a
            |> Json.Decode.decodeValue
                (Json.Decode.oneOf
                    [ Json.Decode.string
                    , Json.Decode.field "message" Json.Decode.string
                    ]
                )
            |> Result.withDefault ""
            |> ErrorMessage
        )



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
        (Task.map2 (\x x2 -> { arguments = x, stdin = x2 })
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
                    (\x ->
                        writeStdout x
                            |> Task.andThen (\_ -> exit 0)
                            |> Task.mapError errorToString
                    )
                |> Task.onError
                    (\x ->
                        writeStderr x
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
