#!/bin/bash
# Run property-based tests

exec >&2

redo-ifchange haskell/build

echo "Running property-based tests..."
cd haskell

# Set library path for SPARK FFI
export LD_LIBRARY_PATH="$PWD/../spark-core/lib:$LD_LIBRARY_PATH"

stack test

touch "$3"
