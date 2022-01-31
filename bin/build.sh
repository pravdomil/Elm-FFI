#!/usr/bin/env bash

# Stop if any command fails.
set -e

# Stop on unset variables.
set -u

# Be in project root.
cd "${0%/*}/.."

# Compile application.
elm make src-cli/Main.elm --output bin/elm-ffi.js --optimize
elm-ffi bin/elm-ffi.js --run --shebang
mv bin/elm-ffi.js bin/elm-ffi
