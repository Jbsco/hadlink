# Design Document

## Contents

- [Overview](#overview)
- [Core Principle](#core-principle)
- [Architecture](#architecture)
- [SPARK Core Responsibilities](#spark-core-responsibilities)
- [Haskell Layer Responsibilities](#haskell-layer-responsibilities)
- [FFI Boundary](#ffi-boundary)
- [Testing Strategy](#testing-strategy)
- [Storage Model](#storage-model)
- [Deployment Model](#deployment-model)
- [Security Properties](#security-properties)
- [Migration Plan](#migration-plan)
- [Why This Approach?](#why-this-approach)
- [Invariants](#invariants)
- [Future Considerations](#future-considerations)
- [Cryptography](#cryptography)
- [Toolchain](#toolchain)
- [Assurance Model](#assurance-model)
- [References](#references)

---

## Overview

hadlink is a URL shortener designed with embedded systems principles: deterministic behavior, bounded resources, and explicit security properties.

## Core Principle

**SPARK proves invariants. Haskell composes systems. The boundary is small and frozen.**

---

## Architecture

### Two-Layer Design

```
┌──────────────────────────┐
│   Haskell Frontend       │  ← API, orchestration, testing
│  (API, orchestration)    │
└──────────┬───────────────┘
           │ FFI (pure, narrow)
┌──────────▼───────────────┐
│   SPARK Core Library     │  ← Proved invariants
│  (proved invariants)     │
└──────────────────────────┘
```

### Separation of Concerns

| Concern | Language | Rationale |
|---------|----------|-----------|
| URL validation & canonicalization | SPARK | Total functions, provable invariants |
| Short-code generation | SPARK | Determinism, no collisions beyond math bounds |
| API composition | Haskell | Expressive, concise |
| Storage | Haskell | Flexibility, multiple backends |
| Redirect path | Haskell | Simple lookup, read-only |
| Concurrency & orchestration | Haskell | Lightweight threads, STM |
| Property testing | Haskell | Hedgehog via Tasty (24 tests) |

---

## SPARK Core Responsibilities

The SPARK core is intentionally minimal and contains only:

- URL canonicalization & validation
- Scheme / host / port rules
- Private address rejection
- Maximum length guarantees
- Short-code generation
- Base62 encoding
- Collision bounds (mathematical, not empirical)

- No IO
- No networking
- No storage
- No concurrency
- No configuration parsing

### What SPARK Proves

For canonicalization:
- Output scheme ∈ {http, https}
- No credentials present
- Host is not private / loopback
- Output length ≤ input length + constant
- Function terminates for all valid inputs
- No exceptions possible

For short-code generation:
- Output is exactly N Base62 characters
- Characters are limited to allowed alphabet
- No heap allocation
- Runtime is bounded
- Deterministic for same input + secret

---

## Haskell Layer Responsibilities

Haskell provides the integration and operational layer:

- HTTP server (WAI / Warp)
- API routing
- Rate limiting
- Proof-of-Work
- Storage backends
- Configuration
- Logging
- Concurrency
- Property-based tests
- Fuzzing

---

## FFI Boundary

### Design Rules

1. Only pass plain bytes
2. No mutable buffers
3. No callbacks
4. No business logic on the Haskell side
5. SPARK is the source of truth

### Interface Shape

SPARK exports simple C-compatible functions:

```ada
function Canonicalize_C
  (Input  : System.Address;
   Length : Natural;
   Output : System.Address)
  return Result_Code
with Export, Convention => C;
```

Haskell imports them:

```haskell
foreign import ccall "canonicalize"
  c_canonicalize :: Ptr Word8 -> CInt -> Ptr Word8 -> IO CInt
```

Haskell never "understands" the URL — it just passes bytes.

---

## Testing Strategy

### Property-Based Testing Pipeline

1. Generate arbitrary URLs (including garbage)
2. Feed into SPARK canonicalizer
3. Assert: Either rejected OR invariant holds
4. Compare Haskell vs SPARK behavior during migration

### Example Properties

```haskell
prop_no_private_hosts :: RawURL -> Property
prop_no_private_hosts url =
  case canonicalize url of
    Left _  -> property True
    Right v -> not (isPrivateHost v)

prop_roundTrip :: ValidURL -> Bool
prop_roundTrip url =
  resolve (shorten url) == Just url
```

Breadth from Haskell, depth from SPARK.

---

## Storage Model

### Append-Only Design

- Short codes map to canonical URLs
- No deletes on hot path
- Redirect daemon opens DB read-only
- Periodic compaction offline

### Supported Backends

- SQLite (WAL mode)
- LMDB (planned)

---

## Deployment Model

### Two-Service Architecture

```
Internet / VPN
      |
  redirect-daemon  (read-only, fast)
      |
  LMDB / SQLite (RO)
      |
  shorten-daemon   (private, guarded)
```

- `redirect-daemon` is exposed
- `shorten-daemon` is LAN/VPN-only
- Same database file, different permissions

---

## Security Properties

By design:
- No URL enumeration
- No SSRF (IPv4 and IPv6 private ranges blocked)
- No open redirect abuse
- No unbounded memory growth
- Stateless anti-spam
- Fast redirect path survives DoS
- Secret key required at startup (no insecure defaults)
- X-Forwarded-For trust disabled by default

---

## Migration Plan

### Phase 1 — Haskell-only (Complete)
- Implement everything in Haskell
- Lock down invariants in docs
- Write property tests

### Phase 2 — SPARK extraction (Complete)
- Move canonicalization and short-code generation into SPARK
- FFI integration (Haskell calls SPARK via C ABI)
- SPARK proofs at 100% for hadlink core

### Phase 3 — Hardening (current)
- Freeze SPARK API
- Full proof coverage
- Production hardening
- Tag v1.0

---

## Why This Approach?

This project is using **two verification regimes, each where it is strongest**:
- SPARK for exhaustive proof
- Haskell for compositional testing

The goal is building a high-assurance system.

---

## Invariants

### Core Invariants (Must Never Violate)

- All stored URLs are canonicalized
- Redirect path performs no allocation beyond lookup
- Short codes cannot be user-chosen
- Storage is append-only
- SPARK functions are total

Violating these is a bug.

---

## Future Considerations

Potential extensions:
- Read-only backup mirror
- Configurable TTLs (default: infinite)
- Multiple namespaces (/b/, /a/, /h/)
- VPN-only create endpoint
- Hardware-constrained deployment (Raspberry Pi)

---

## Cryptography

Cryptographic primitives are provided by [SPARKNaCl](https://github.com/rod-chapman/SPARKNaCl) (BSD 3-Clause), chosen for compatibility with SPARK proofs, minimal attack surface, and permissive licensing compatible with AGPL-3.0.

See [LICENSES/BSD-3-Clause-SPARKNaCl.txt](/LICENSES/BSD-3-Clause-SPARKNaCl.txt) for the full license.

---

## Toolchain

### Ada/SPARK (AdaCore)

- **GNAT** - Ada compiler (GPL-3.0 with Runtime Library Exception)
- **GNATprove** - SPARK formal verification (GPL-3.0)
- **SPARK Runtime** - Runtime support for SPARK (GPL-3.0 with RLE)
- **GNAT Standalone Library** - Encapsulated Ada runtime for FFI
- **Alire** - Ada package manager (GPL-3.0)

The SPARK core (`libHadlink_Core.so`) is built as a standalone library with the Ada runtime encapsulated, enabling clean FFI integration with Haskell.

### Haskell

- **GHC** - Glasgow Haskell Compiler (BSD-3-Clause)
- **Stack** - Build tool and dependency manager (BSD-3-Clause)

See [LICENSES/](/LICENSES/) for full license texts.

---

## Assurance Model

This project references DO-278A (Software Integrity Level 3) objectives for:
- Integrity allocation between high-integrity core and supporting service layer
- Explicit assumption documentation at proof boundaries
- Verification evidence via formal proofs and property tests

See [ROADMAP.md](ROADMAP.md#do-278a-sil-3-mapping) for detailed objective mapping.

**This project is not certified.** It is a single-developer project without certification authority or independent verification resources.

---

## References

This design draws from:
- Embedded systems design principles
- High-assurance software engineering
- Infrastructure-first thinking
- DO-278A integrity objectives (where applicable)
