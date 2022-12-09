module JavaScript.Decoder exposing (..)

import Bytes
import Json.Decode


bytes : Json.Decode.Decoder Bytes.Bytes
bytes =
    Json.Decode.fail "Compiled file needs to be processed via elm-ffi command."
