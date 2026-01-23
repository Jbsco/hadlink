#!/bin/bash
# Build SPARK core library

exec >&2

cd "$(dirname "$0")"

# Declare dependency on source files
redo-ifchange src/core.ads src/core.adb src/core_ffi.ads src/core_ffi.adb hadlink_core.gpr

echo "Building SPARK core library..."
alr build

# Mark as successfully built
touch "$3"
