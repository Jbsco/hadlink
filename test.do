#!/bin/bash
# Run property-based tests

exec >&2

redo-ifchange haskell/build

echo "Running property-based tests..."
cd haskell

# Build the test suite
stack build --test --no-run-tests

# Set library path for SPARK FFI
export LD_LIBRARY_PATH="$PWD/../spark-core/lib:$LD_LIBRARY_PATH"

# Find and run the test executable directly
# This ensures LD_LIBRARY_PATH is properly inherited
TEST_EXE=$(find .stack-work -name "hadlink-test" -type f -executable 2>/dev/null | head -1)

if [ -z "$TEST_EXE" ]; then
    echo "Error: Could not find hadlink-test executable"
    exit 1
fi

echo "Running: $TEST_EXE"
# Run single-threaded: Ada FFI is not thread-safe for concurrent calls
"$TEST_EXE" +RTS -N1 -RTS

touch "$3"
