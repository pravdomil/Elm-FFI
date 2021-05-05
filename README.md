# Elm FFI

Read [The Limits of Elm/JS Interop](https://guide.elm-lang.org/interop/limits.html) first.

## Install

`npm i pravdomil/elm-ffi -g`

## Usage

1. Create following module:

```elm
module Interop.JavaScript exposing (..)

{-| Part of <https://github.com/pravdomil/Elm-FFI>.
-}

import Json.Decode as Decode exposing (Decoder)
import Task exposing (Task)


run : String -> Task Error Decode.Value
run _ =
    Task.fail (Exception "Compiled file needs to be processed via elm-ffi command.")


decode : Decoder a -> Task Error Decode.Value -> Task Error a
decode decoder a =
    a
        |> Task.andThen
            (\v ->
                case v |> Decode.decodeValue decoder of
                    Ok b ->
                        Task.succeed b

                    Err b ->
                        Task.fail (DecodeError b)
            )



--


type Error
    = Exception Decode.Value
    | DecodeError Decode.Error


errorToString : Error -> String
errorToString a =
    case a of
        Exception b ->
            "Got JavaScript exception:\n"
                ++ (b
                        |> Decode.decodeValue (Decode.field "message" Decode.string)
                        |> Result.withDefault "No message provided."
                   )

        DecodeError b ->
            "Cannot decode JavaScript value because:\n" ++ Decode.errorToString b
```

2. Run `elm-ffi elm.js` on JavaScript file produced by Elm compiler.

## Options

To show commandline options run `elm-ffi`.
