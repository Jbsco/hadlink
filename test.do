#!/bin/bash
# Run property-based tests

exec >&2

redo-ifchange haskell/build

echo "Running property-based tests..."
cd haskell
cabal test

touch "$3"
