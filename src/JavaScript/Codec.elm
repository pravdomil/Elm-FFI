module JavaScript.Codec exposing (..)

import Codec
import JavaScript
import Json.Decode


errorCodec : Codec.Codec JavaScript.Error
errorCodec =
    Codec.custom
        (\fn1 fn2 fn3 v ->
            case v of
                JavaScript.FileNotPatched ->
                    fn1

                JavaScript.Exception v1 v2 v3 ->
                    fn2 v1 v2 v3

                JavaScript.DecodeError v1 ->
                    fn3 v1
        )
        |> Codec.variant0 "FileNotPatched" JavaScript.FileNotPatched
        |> Codec.variant3 "Exception" JavaScript.Exception errorNameCodec errorCodeCodec errorMessageCodec
        |> Codec.variant1 "DecodeError" JavaScript.DecodeError jsonDecodeErrorCodec
        |> Codec.buildCustom


errorNameCodec : Codec.Codec JavaScript.ErrorName
errorNameCodec =
    Codec.string |> Codec.map (\(JavaScript.ErrorName v) -> v) JavaScript.ErrorName


errorCodeCodec : Codec.Codec JavaScript.ErrorCode
errorCodeCodec =
    Codec.string |> Codec.map (\(JavaScript.ErrorCode v) -> v) JavaScript.ErrorCode


errorMessageCodec : Codec.Codec JavaScript.ErrorMessage
errorMessageCodec =
    Codec.string |> Codec.map (\(JavaScript.ErrorMessage v) -> v) JavaScript.ErrorMessage


jsonDecodeErrorCodec : Codec.Codec Json.Decode.Error
jsonDecodeErrorCodec =
    Codec.custom
        (\fn1 fn2 fn3 fn4 v ->
            case v of
                Json.Decode.Field v1 v2 ->
                    fn1 v1 v2

                Json.Decode.Index v1 v2 ->
                    fn2 v1 v2

                Json.Decode.OneOf v1 ->
                    fn3 v1

                Json.Decode.Failure v1 v2 ->
                    fn4 v1 v2
        )
        |> Codec.variant2 "Field" Json.Decode.Field Codec.string (Codec.lazy (\_ -> jsonDecodeErrorCodec))
        |> Codec.variant2 "Index" Json.Decode.Index Codec.int (Codec.lazy (\_ -> jsonDecodeErrorCodec))
        |> Codec.variant1 "OneOf" Json.Decode.OneOf (Codec.list (Codec.lazy (\_ -> jsonDecodeErrorCodec)))
        |> Codec.variant2 "Failure" Json.Decode.Failure Codec.string Codec.value
        |> Codec.buildCustom
