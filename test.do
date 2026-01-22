#!/bin/bash
# Run property-based tests

exec >&2

redo-ifchange haskell/build

echo "Running property-based tests..."
cd haskell
stack test

touch "$3"
