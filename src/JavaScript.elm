module JavaScript exposing
    ( run
    , Error(..), ErrorCode(..), ErrorMessage(..), ErrorName(..), errorToString, decodeError
    )

{-|

@docs run

@docs Error, ErrorCode, ErrorMessage, ErrorName, errorToString, decodeError

-}

import Json.Decode
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
        (ErrorName
            (Result.withDefault ""
                (Json.Decode.decodeValue
                    (Json.Decode.field "name" Json.Decode.string)
                    a
                )
            )
        )
        (ErrorCode
            (Result.withDefault ""
                (Json.Decode.decodeValue
                    (Json.Decode.field "code"
                        (Json.Decode.oneOf
                            [ Json.Decode.string
                            , Json.Decode.map String.fromInt Json.Decode.int
                            ]
                        )
                    )
                    a
                )
            )
        )
        (ErrorMessage
            (Result.withDefault ""
                (Json.Decode.decodeValue
                    (Json.Decode.oneOf
                        [ Json.Decode.string
                        , Json.Decode.field "message" Json.Decode.string
                        ]
                    )
                    a
                )
            )
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
