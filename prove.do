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
alr exec -- gnatprove -P hadlink_core.gpr --level=2 --report=all

touch "$3"
