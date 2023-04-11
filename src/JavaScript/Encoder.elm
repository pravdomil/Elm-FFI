module JavaScript.Encoder exposing (..)

import Bytes
import Json.Encode
import Time


bytes : Bytes.Bytes -> Json.Encode.Value
bytes _ =
    Json.Encode.null


timePosix : Time.Posix -> Json.Encode.Value
timePosix _ =
    let
        _ =
            Time.posixToMillis
    in
    Json.Encode.null
