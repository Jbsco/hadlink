#!/bin/bash
# Run SPARK proofs

exec >&2

# Check if alire is available
if ! command -v alr &> /dev/null; then
    echo "alr (Alire) not found - skipping SPARK proofs"
    echo "Install Alire: https://alire.ada.dev"
    echo "  Arch/Manjaro: yay -S alire-bin"
    touch "$3"
    exit 0
fi

echo "Running SPARK proofs..."
cd spark-core

# Run gnatprove on hadlink units only (skip SPARKNaCl dependency)
alr exec -- gnatprove -P hadlink_core.gpr --level=2 --report=all -u core -u core_ffi
EXIT_CODE=$?

# Parse summary from output file
SUMMARY_FILE="obj/development/gnatprove/gnatprove.out"

echo ""
echo "=========================================="

if [ -f "$SUMMARY_FILE" ]; then
    # Check if any Core subprograms are "not proved"
    CORE_UNPROVED=$(grep -E "^  Core\." "$SUMMARY_FILE" | grep "not proved" | wc -l)

    # Count Core checks (sum of all "proved (N checks)" for Core.*)
    CORE_CHECKS=$(grep -E "^  Core\." "$SUMMARY_FILE" | grep -oP "proved \(\K\d+" | awk '{sum+=$1} END {print sum}')

    # Count pragma Assume in source code
    ASSUMES=$(grep -h "pragma Assume" src/*.adb 2>/dev/null | wc -l)

    if [ "$CORE_UNPROVED" = "0" ]; then
        echo "SPARK proofs: hadlink core 100% verified ($CORE_CHECKS checks)"
    else
        echo "SPARK proofs: hadlink core has $CORE_UNPROVED unproved subprograms"
    fi

    if [ "$ASSUMES" -gt 0 ]; then
        echo "  ($ASSUMES pragma Assume statements for postconditions)"
    fi
else
    echo "Summary file not found: $SUMMARY_FILE"
fi

echo "=========================================="

# Exit with gnatprove's exit code
if [ $EXIT_CODE -ne 0 ]; then
    exit $EXIT_CODE
fi

touch "$3"
