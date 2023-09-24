module JavaScript.Codec exposing (..)

import Codec
import JavaScript
import Json.Decode


errorCodec : Codec.Codec JavaScript.Error
errorCodec =
    Codec.custom
        (\fn1 fn2 fn3 x ->
            case x of
                JavaScript.FileNotPatched ->
                    fn1

                JavaScript.Exception x1 x2 x3 ->
                    fn2 x1 x2 x3

                JavaScript.DecodeError x1 ->
                    fn3 x1
        )
        |> Codec.variant0 JavaScript.FileNotPatched
        |> Codec.variant3 JavaScript.Exception errorNameCodec errorCodeCodec errorMessageCodec
        |> Codec.variant1 JavaScript.DecodeError jsonDecodeErrorCodec
        |> Codec.buildCustom


errorNameCodec : Codec.Codec JavaScript.ErrorName
errorNameCodec =
    Codec.map (\(JavaScript.ErrorName x) -> x) JavaScript.ErrorName Codec.string


errorCodeCodec : Codec.Codec JavaScript.ErrorCode
errorCodeCodec =
    Codec.map (\(JavaScript.ErrorCode x) -> x) JavaScript.ErrorCode Codec.string


errorMessageCodec : Codec.Codec JavaScript.ErrorMessage
errorMessageCodec =
    Codec.map (\(JavaScript.ErrorMessage x) -> x) JavaScript.ErrorMessage Codec.string


jsonDecodeErrorCodec : Codec.Codec Json.Decode.Error
jsonDecodeErrorCodec =
    Codec.lazy
        (\() ->
            Codec.custom
                (\fn1 fn2 fn3 fn4 x ->
                    case x of
                        Json.Decode.Field x1 x2 ->
                            fn1 x1 x2

                        Json.Decode.Index x1 x2 ->
                            fn2 x1 x2

                        Json.Decode.OneOf x1 ->
                            fn3 x1

                        Json.Decode.Failure x1 x2 ->
                            fn4 x1 x2
                )
                |> Codec.variant2 Json.Decode.Field Codec.string jsonDecodeErrorCodec
                |> Codec.variant2 Json.Decode.Index Codec.int jsonDecodeErrorCodec
                |> Codec.variant1 Json.Decode.OneOf (Codec.list jsonDecodeErrorCodec)
                |> Codec.variant2 Json.Decode.Failure Codec.string Codec.value
                |> Codec.buildCustom
        )
