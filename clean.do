#!/bin/bash
# Clean build artifacts

exec >&2

echo "Cleaning Haskell build artifacts..."
cd haskell && cabal clean

echo "Cleaning SPARK build artifacts..."
rm -rf spark-core/proof/obj spark-core/proof/gnatprove spark-core/proof/bin

echo "Cleaning redo artifacts..."
rm -f .redo all test prove clean haskell/build

echo "Clean complete"
