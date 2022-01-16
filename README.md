# Elm FFI

Foreign function interface for Elm.

Read [The Limits of Elm/JS Interop](https://guide.elm-lang.org/interop/limits.html) first.

## Install

1. `npm i pravdomil/elm-ffi --save-dev`
1. Add `node_modules/elm-ffi/src` into `elm.json`/`source-directories`.

## Usage

1. Use [`JavaScript`](src/JavaScript.elm) module.
1. `elm make src/Main.elm --output elm.js`
1. `elm-ffi elm.js`

To show commandline options run `elm-ffi`.
