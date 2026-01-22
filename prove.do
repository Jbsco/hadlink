#!/bin/bash
# Run SPARK proofs

exec >&2

# Check if gnatprove is available
if ! command -v gnatprove &> /dev/null; then
    echo "gnatprove not found - skipping SPARK proofs"
    echo "Install GNAT Community Edition with SPARK support"
    touch "$3"
    exit 0
fi

echo "Running SPARK proofs..."
cd spark-core/proof
gnatprove -P core.gpr --level=2

touch "$3"
