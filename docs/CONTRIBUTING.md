# Contributing

Contributions are welcome. This document outlines expectations and workflow.

## Contents

- [Core Principles](#core-principles)
- [What to Contribute](#what-to-contribute)
- [Development Setup](#development-setup)
- [Testing](#testing)
- [Commit Guidelines](#commit-guidelines)
- [Pull Request Process](#pull-request-process)
- [Code Style](#code-style)
- [Documentation](#documentation)
- [Security](#security)
- [Non-Goals Enforcement](#non-goals-enforcement)
- [License](#license)
- [Questions](#questions)
- [Recognition](#recognition)

---

## Core Principles

Before contributing, understand the project values:

1. **Proofs over tests** (for core logic)
2. **Minimal surface area**
3. **Infrastructure, not features**
4. **Explicit non-goals matter**

For background on the project's design philosophy, see the [whitepapers](whitepapers/):
- [hadlink](whitepapers/hadlink.pdf) - System design and assurance model
- [SPARK as a Design Lens](whitepapers/formal-verification-lens.pdf) - How formal verification shaped the architecture
- [From Coursework to Craft](whitepapers/sprint-projects.pdf) - Project methodology and engineering practice

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

1. **redo** - Build system ([dinkelk/redo](https://github.com/dinkelk/redo))
2. **Stack** - Haskell build tool
3. **Alire** - Ada/SPARK package manager ([alire.ada.dev](https://alire.ada.dev))

### Building

```bash
redo all      # Build everything (SPARK + Haskell)
redo test     # Run tests
redo prove    # Run SPARK proofs
redo style    # Check code style
```

For SPARK-only work:
```bash
cd spark-core
alr build
alr exec -- gnatprove -P hadlink_core.gpr
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
redo prove
# Or directly: alr exec -- gnatprove -P hadlink_core.gpr
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

Security issues should be reported privately:
- Do not open public issues for security vulnerabilities
- Contact the maintainer directly
- Provide clear reproduction steps

---

## Non-Goals Enforcement

Requests that conflict with documented non-goals will be closed with explanation. This is not personal; it's scope protection.

Examples of closable requests:
- "Add click tracking"
- "Create web dashboard"
- "Support custom aliases"

---

## License

By contributing, you agree that your contributions will be licensed under the project's AGPL-3.0-or-later license.

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
