# Contributing

Contributions are welcome. This document outlines expectations and workflow.

---

## Core Principles

Before contributing, understand the project values:

1. **Boring is better than clever**
2. **Proofs over tests** (for core logic)
3. **Minimal surface area**
4. **Infrastructure, not features**
5. **Explicit non-goals matter**

---

## What to Contribute

### Welcome Contributions

- Bug fixes (with test cases)
- Documentation improvements
- Property-based tests
- SPARK proofs
- Deployment examples
- Performance optimizations (with benchmarks)

### Unlikely to Be Accepted

- Marketing features (tracking, analytics)
- User-facing UI
- Custom aliases
- Feature requests that violate non-goals

When in doubt, open an issue first.

---

## Development Setup

### Prerequisites

For Haskell development:
```bash
# Install Stack
curl -sSL https://get.haskellstack.org/ | sh

# Build project
cd haskell
stack build
stack test
```

For SPARK development:
```bash
# Install GNAT Community Edition with SPARK
# https://www.adacore.com/community

cd spark-core/proof
gnatprove -P core.gpr
```

---

## Testing

### Property-Based Tests

All new functionality should include property-based tests:

```haskell
prop_new_feature :: Input -> Bool
prop_new_feature input = ...
```

### SPARK Proofs

SPARK code must prove stated contracts:

```ada
function Foo (X : T) return T
with
  Pre  => Valid_Input (X),
  Post => Valid_Output (Foo'Result);
```

Run proofs with:
```bash
gnatprove -P core.gpr --level=2
```

---

## Commit Guidelines

Commits should follow established patterns:
- Single purpose per commit
- Linear history
- No broken intermediate states
- Capital letter start, no end punctuation

Format:
```
Add proof for URL canonicalization bounds
```

Not:
```
fixed stuff
```

---

## Pull Request Process

1. Open an issue describing the change
2. Fork and create a feature branch
3. Make changes with tests/proofs
4. Ensure all tests and proofs pass
5. Update documentation
6. Submit PR referencing the issue

### PR Checklist

- [ ] Tests added or updated
- [ ] Proofs pass (if SPARK changes)
- [ ] Documentation updated
- [ ] No new dependencies without justification
- [ ] Follows existing code style
- [ ] Linear commit history

---

## Code Style

### Haskell

Follow standard Haskell style:
- Wall and Werror enabled
- HLint clean
- No partial functions
- Explicit export lists

### Ada/SPARK

Follow Ada Quality and Style Guide:
- SPARK_Mode enabled
- All contracts proven
- No dynamic allocation in core
- Clear separation of concerns

---

## Documentation

Documentation changes should:
- Be concise and technical
- Avoid buzzwords and marketing language
- Include concrete examples
- Explain "why" not just "what"

---

## Security

Security issues should be reported privately. See `SECURITY.md` for details.

Do not open public issues for security vulnerabilities.

---

## Non-Goals Enforcement

Requests that conflict with documented non-goals will be closed with explanation. This is not personal; it's scope protection.

Examples of closable requests:
- "Add click tracking"
- "Create web dashboard"
- "Support custom aliases"

---

## License

By contributing, you agree that your contributions will be licensed under the project's MIT license.

---

## Questions

Open an issue for questions about:
- Implementation approach
- SPARK proof strategies
- Architecture decisions
- Testing approach

---

## Recognition

Contributors will be recognized in release notes and documentation.
