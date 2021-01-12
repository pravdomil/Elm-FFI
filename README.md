# Elm FFI

Read [The Limits of Elm/JS Interop](https://guide.elm-lang.org/interop/limits.html) first.

## Install

`npm i pravdomil/elm-ffi -g`

## Usage

1. Create following module:

```elm
module Interop.JsCode exposing (..)

{-| Part of <https://github.com/pravdomil/Elm-FFI>.
-}

import Json.Decode as Decode
import Task exposing (Task)


{-| To define exception.
-}
type alias Exception =
    String


{-| To run JavaScript code.
-}
eval : String -> Task Exception Decode.Value
eval _ =
    Task.fail "Function is not implemented."
```

2. Run `elm-ffi elm.js` on JavaScript file produced by Elm compiler.
