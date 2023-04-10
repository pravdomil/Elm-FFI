module JavaScript.Encoder exposing (..)

import Bytes
import Json.Encode


bytes : Bytes.Bytes -> Json.Encode.Value
bytes _ =
    Json.Encode.null
