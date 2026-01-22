
---

# `DESIGN.md` (Draft for `hadlink`)

```markdown
# hadlink Design Overview

**hadlink** uses a dual-language architecture:

- **SPARK / Ada**: Proves core invariants
- **Haskell**: Composes system, handles IO, orchestrates services

---

## SPARK Core Responsibilities

- URL validation and canonicalization
- Short-code generation
- Base62 encoding
- Collision bounds (mathematical guarantees)
- Termination and length guarantees

**Properties to prove:**

- Only `http` or `https` leave canonicalization
- No credentials or private IPs
- Output length ≤ input length + constant
- Deterministic short-code generation
- Function termination for all inputs ≤ 2048 bytes

SPARK code is **side-effect-free** and **total**.

---

## Haskell Responsibilities

- HTTP server (API routing)
- Rate limiting and optional PoW
- Storage backend interface
- Logging and metrics
- Property-based testing (QuickCheck/Hedgehog)
- Concurrency and orchestration

Haskell treats SPARK as a **black-box library**.

---

## Haskell ↔ SPARK Boundary

- Only pass **bytes** across FFI
- No mutable buffers or callbacks
- No bypass of SPARK invariants
- SPARK is the **source of truth** for validation

Example:

```haskell
foreign import ccall "canonicalize"
  c_canonicalize :: Ptr Word8 -> CInt -> Ptr Word8 -> IO CInt
