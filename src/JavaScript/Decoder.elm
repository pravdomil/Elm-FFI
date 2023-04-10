module JavaScript.Decoder exposing (..)

import Bytes
import Json.Decode
import Time


bytes : Json.Decode.Decoder Bytes.Bytes
bytes =
    Json.Decode.fail "Compiled file needs to be processed via elm-ffi command."


timePosix : Json.Decode.Decoder Time.Posix
timePosix =
    let
        _ =
            Time.millisToPosix
    in
    Json.Decode.fail "Compiled file needs to be processed via elm-ffi command."
