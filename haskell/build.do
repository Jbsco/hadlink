#!/bin/bash
# Build Haskell components

exec >&2

cd "$(dirname "$0")"

# Declare dependency on SPARK library (must build first)
redo-ifchange ../spark-core/build

# Declare dependency on cabal file and source files
redo-ifchange hadlink.cabal $(find src src-shorten app -name '*.hs' 2>/dev/null)

echo "Building Haskell components..."
stack build --test --no-run-tests

# Mark as successfully built
touch "$3"
