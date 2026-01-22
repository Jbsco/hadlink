#!/bin/bash
# Run code style checks

exec >&2

echo "Checking SPARK/Ada style..."

# Check if alire is available
if ! command -v alr &> /dev/null; then
    echo "Warning: alr (Alire) not found - skipping SPARK style checks"
    echo "Install Alire: https://alire.ada.dev"
else
    echo "Running SPARK style checks..."
    cd spark-core
    # Compile with style checks (creates .o files in current dir, cleaned up after)
    STYLE_OUTPUT=$(alr exec -- gcc -c -gnatwa -gnaty src/*.ads src/*.adb 2>&1 | grep -E "(warning:|(style))")
    # Clean up object files
    rm -f *.o *.ali 2>/dev/null
    if [ -z "$STYLE_OUTPUT" ]; then
        echo "✓ No style issues found in SPARK code"
    else
        echo "$STYLE_OUTPUT" | head -50
    fi
fi

echo ""
echo "Checking Haskell style..."
cd "$(dirname "$0")/haskell"

# Try to use hlint via stack, fallback to system hlint
if stack exec -- which hlint &> /dev/null; then
    stack exec -- hlint src/ app/ --color=never || true
elif command -v hlint &> /dev/null; then
    hlint src/ app/ --color=never || true
else
    echo "Warning: hlint not found - install with 'stack install hlint'"
fi

touch "$3"
