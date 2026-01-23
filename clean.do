#!/bin/bash
# Clean build artifacts

exec >&2

echo "Cleaning Haskell build artifacts..."
cd haskell && stack clean

echo "Cleaning SPARK build artifacts..."
cd ../spark-core && alr clean >/dev/null 2>&1 || true
rm -rf proof/obj proof/gnatprove proof/bin

echo "Cleaning redo artifacts..."
cd ..
rm -rf .redo
rm -f all prove style haskell/build spark-core/build
rm -f test 2>/dev/null || rm -rf test 2>/dev/null || true

echo "Clean complete"
