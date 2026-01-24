# Build System

hadlink uses `redo` as its build system, which aligns with the project's embedded/minimalist philosophy.

## Why redo?

- Simple, declarative build scripts
- Correct dependency tracking
- No complex syntax or DSL
- Scripts are just shell scripts
- Minimal, auditable
- Written in Haskell (like hadlink's integration layer)

## Prerequisites

Install redo (dinkelk's Haskell implementation):

```bash
# Clone and build
git clone https://github.com/dinkelk/redo.git
cd redo && ./do
export PATH=$PATH:$(pwd)/bin
```

Or if available via your package manager, install from there.

**Stack** - Haskell build tool
```bash
curl -sSL https://get.haskellstack.org/ | sh
# Or: sudo pacman -S stack (Arch)
```

## Building

### Build everything

```bash
redo all
```

This builds the SPARK core library, then all Haskell components.

### Run tests

```bash
redo test
```

Runs the property-based test suite.

### Run SPARK proofs (Phase 2+)

```bash
redo prove
```

Requires Alire with gnatprove. The `spark-core/` directory is an Alire project:

```bash
cd spark-core
alr build                                    # Build SPARK library
alr exec -- gnatprove -P hadlink_core.gpr   # Run formal proofs
```

### Clean build artifacts

```bash
redo clean
```

## Development Workflow

### Incremental builds

redo tracks dependencies automatically:

```bash
# Edit source files
vim haskell/src/Types.hs

# Rebuild only what changed
redo all
```

### Generate secrets

```bash
redo generate-secret
```

Creates a deployment secret in `deploy/docker/secret.key`.

## Build Scripts

All build logic lives in `.do` files:

- `all.do` - Build all components
- `test.do` - Run tests
- `prove.do` - Run SPARK proofs
- `clean.do` - Clean artifacts
- `default.do` - Show help
- `generate-secret.do` - Generate deployment secret
- `run-shorten.do` - Run shorten daemon locally

### Writing new build scripts

Follow redo conventions:

```bash
#!/bin/bash
# Brief description

exec >&2  # Redirect script output to stderr

# Add dependencies
redo-ifchange dependency1 dependency2

# Do the work
echo "Building..."
actual_build_command

# Mark as done
touch "$3"
```

## Continuous Integration

Example CI configuration using redo:

```yaml
# .gitlab-ci.yml
image: haskell:9.2

before_script:
  - git clone https://github.com/dinkelk/redo.git
  - cd redo && ./do && cd ..
  - export PATH=$PATH:$(pwd)/redo/bin

test:
  script:
    - redo all
    - redo test
    - redo prove  # if SPARK available
```

## Troubleshooting

### redo command not found

Install redo from https://github.com/dinkelk/redo

Requirements:
- Haskell Stack
- Run `./do` in the redo repository

### Stale builds

Clean and rebuild:

```bash
redo clean
redo all
```

### Permission issues

Ensure `.do` files are executable:

```bash
chmod +x *.do haskell/*.do
```

## Advanced Usage

### Parallel builds

redo supports parallel execution:

```bash
redo -j4 all
```

### Verbose output

```bash
redo -v all
```

### Debugging

```bash
redo -x all  # Show commands as they execute
```

### Direct Stack usage

You can also use Stack directly:

```bash
cd haskell
stack build          # Build
stack test           # Test
stack exec hadlink   # Run
stack clean          # Clean
```

## Why dinkelk/redo?

This implementation:
- Written in Haskell
- Actively maintained
- Bug fixes over other implementations
- Fast and reliable
- Clean, readable codebase

## References

- [dinkelk/redo](https://github.com/dinkelk/redo) - This implementation
- [DJB's redo design](https://cr.yp.to/redo.html) - Original design notes
- [apenwarr/redo](https://github.com/apenwarr/redo) - Python implementation
