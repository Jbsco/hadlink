#!/bin/bash
# Run the shorten daemon locally for testing

exec >&2

redo-ifchange all

export LD_LIBRARY_PATH="$PWD/spark-core/lib:$LD_LIBRARY_PATH"
export HADLINK_PORT="${HADLINK_PORT:-8080}"
export HADLINK_SECRET="${HADLINK_SECRET:-your-32-byte-secret-key-here....}"

echo "Starting shorten daemon on port $HADLINK_PORT"
echo "Press Ctrl+C to stop"
echo ""

exec stack --stack-yaml haskell/stack.yaml exec hadlink-shorten
