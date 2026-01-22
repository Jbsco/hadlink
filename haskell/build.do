#!/bin/bash
# Build Haskell components

exec >&2

cd "$(dirname "$0")"

echo "Building Haskell components..."
stack build

# Mark as successfully built
touch "$3"
