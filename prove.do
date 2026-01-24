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

# Run gnatprove
alr exec -- gnatprove -P hadlink_core.gpr --level=2 --report=all
EXIT_CODE=$?

# Parse summary from output file
SUMMARY_FILE="obj/development/gnatprove/gnatprove.out"

echo ""
echo "=========================================="

if [ -f "$SUMMARY_FILE" ]; then
    # Extract Total line from summary
    # Format: Total  2569  684 (27%)  1882 (73%)  .  3 (0%)
    #         [name] [tot] [flow]     [proved]    [just] [unproved]
    SUMMARY=$(grep "^Total" "$SUMMARY_FILE" | head -1)

    if [ -n "$SUMMARY" ]; then
        # Extract values - percentages appear as: flow%, proved%, unproved%
        TOTAL=$(echo "$SUMMARY" | awk '{print $2}')
        PERCENTAGES=($(echo "$SUMMARY" | grep -oP '\d+(?=%\))'))
        PROVED_PCT="${PERCENTAGES[1]}"  # Second percentage is "proved"
        UNPROVED=$(echo "$SUMMARY" | awk '{print $(NF-1)}')

        # Count actual pragma Assume in source code (we're in spark-core dir)
        ASSUMES=$(grep -h "pragma Assume" src/*.adb 2>/dev/null | wc -l)

        # Check if unproved are only in dependencies (SPARKNaCl)
        HADLINK_UNPROVED=$(grep -E "Core\.[A-Za-z_]+ at core\.(ads|adb).*not proved" "$SUMMARY_FILE" | wc -l)

        if [ "$UNPROVED" = "0" ] || [ "$UNPROVED" = "." ]; then
            echo "SPARK proofs: 100% verified ($TOTAL checks)"
        elif [ "$HADLINK_UNPROVED" = "0" ]; then
            echo "SPARK proofs: hadlink core 100% verified"
            echo "  ($UNPROVED unproved in dependencies, $TOTAL total checks)"
        else
            echo "SPARK proofs: $PROVED_PCT% proved ($UNPROVED unproved of $TOTAL checks)"
        fi

        if [ "$ASSUMES" -gt 0 ]; then
            echo "  ($ASSUMES pragma Assume statements for postconditions)"
        fi
    else
        echo "Could not parse gnatprove summary"
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
