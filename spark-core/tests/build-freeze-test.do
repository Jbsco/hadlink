#!/bin/bash
# Build and run the SPARK FFI freeze test

exec >&2

SPARK_DIR="$(cd "$(dirname "$0")/.." && pwd)"

redo-ifchange "$SPARK_DIR/build"

echo "Building FFI freeze test..."
gcc -o "$SPARK_DIR/tests/test_ffi_freeze" \
    "$SPARK_DIR/tests/test_ffi_freeze.c" \
    -L"$SPARK_DIR/lib" -lHadlink_Core \
    -Wl,-rpath,"$SPARK_DIR/lib"

echo "Running FFI freeze test..."
LD_LIBRARY_PATH="$SPARK_DIR/lib:$LD_LIBRARY_PATH" \
    "$SPARK_DIR/tests/test_ffi_freeze"

touch "$3"
