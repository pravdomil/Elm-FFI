# Elm FFI

Read [The Limits of Elm/JS Interop](https://guide.elm-lang.org/interop/limits.html) first.

## Install

1. `npm i pravdomil/elm-ffi --save-dev`
1. Add `node_modules/elm-ffi/src` into `elm.json`/`source-directories`.
1. Use `Interop.JavaScript` module.

## Usage

1. `elm make src/Main.elm --output elm.js`
1. `elm-ffi elm.js`

To show commandline options run `elm-ffi`.
