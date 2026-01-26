#!/bin/bash
# Default target - show help

exec >&2

cat <<EOF
hadlink - High-assurance URL Shortener
Build system: redo

Available targets:
  redo all            - Build all components
  redo test           - Run property-based tests
  redo prove          - Run SPARK proofs
  redo style          - Run code style checks
  redo clean          - Clean build artifacts
  redo deploy         - Build deployment artifacts

Development targets:
  redo run-shorten    - Run shorten daemon locally
  redo run-redirect   - Run redirect daemon locally
  redo generate-secret - Generate secret key

Example workflow:
  redo all            # Build everything
  redo test           # Run tests
  redo prove          # Run SPARK proofs

For more information, see docs/README.md
EOF
