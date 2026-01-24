# Roadmap

This document outlines the planned development phases for hadlink.

## Core Principle

Follow the migration plan: Haskell-only → SPARK extraction → frozen API.

---

## Phase 1: Haskell Implementation (v0.1.0)

**Goal**: Working URL shortener in pure Haskell with well-defined invariants.

### Milestone 1.1: Core Functionality
- [x] URL validation and canonicalization in Haskell
- [x] Deterministic short code generation (HMAC-SHA256 + Base62)
- [x] Basic storage backend (SQLite)
- [x] Simple HTTP API (create + resolve)

### Milestone 1.2: Property Tests
- [x] Property-based test suite framework (Hedgehog via Tasty)
- [x] URL canonicalization properties (valid URLs pass, preserves content)
- [x] Short code generation properties (determinism, length, alphabet)
- [x] Round-trip properties (idempotent canonicalization)
- [x] Negative testing for invalid inputs (private IPs, credentials, bad schemes)

### Milestone 1.3: Abuse Mitigation
- [x] Rate limiting (token bucket per IP, configurable via cfgRateLimitPerIP/Window)
- [ ] Optional Proof-of-Work verification (implemented, not integrated)
- [x] Bounded memory structures

### Deliverables
- [x] Working Haskell implementation
- [x] Comprehensive test suite (14 property tests, 100 iterations each)
- [x] Documentation of invariants
- [x] Initial deployment (Docker + systemd)

**Status**: Complete (abuse mitigation pending)

---

## Phase 2: SPARK Core Extraction (v0.5.0)

**Goal**: Move security-critical logic to SPARK with formal proofs.

### Milestone 2.1: SPARK Canonicalization
- [x] Implement URL parsing in SPARK
- [x] Implement scheme validation (http/https only)
- [x] Implement private address rejection
- [x] Implement credential rejection
- [x] Implement length bounds
- [x] Prove all properties (postconditions via pragma Assume)

### Milestone 2.2: SPARK Short Code Generation
- [x] Implement HMAC-SHA256 in SPARK (via SPARKNaCl)
- [x] Implement Base62 encoding
- [x] Prove determinism
- [x] Prove output length
- [x] Prove character set constraints

### Milestone 2.3: FFI Integration
- [x] Design C-compatible boundary
- [x] Export SPARK functions (hadlink_canonicalize, hadlink_make_short_code)
- [x] Build SPARK as standalone relocatable library
- [x] Configure GNAT standalone library with encapsulated Ada runtime
- [x] Create Haskell FFI module (SparkFFI.hs)
- [x] Update Haskell modules to use FFI (IO-based signatures)
- [x] Configure build system (SPARK builds before Haskell)
- [x] Fix Ada runtime initialization (GNAT standalone library)
- [x] Fix FFI buffer management (Update with Check => False)
- [x] Full integration test (HTTP daemon with SPARK core)
- [x] Maintain identical external behavior
- [x] Property tests pass with SPARK backend (14 tests, single-threaded FFI)

### Deliverables
- [x] Proved SPARK core library (100% with pragma Assume for postconditions)
- [x] FFI boundary (working)
- [x] Haskell using SPARK via FFI
- [x] Test suite validates equivalence

**Status**: Complete

---

## Phase 3: Hardening & v1.0 (v1.0.0)

**Goal**: Production-ready, frozen API.

### Milestone 3.1: Freeze SPARK API
- [ ] Lock SPARK interface
- [ ] Full proof coverage
- [ ] Comprehensive documentation
- [ ] Security audit of boundary

### Milestone 3.2: Deployment Hardening
- [ ] Separate redirect/shorten binaries
- [ ] Read-only redirect daemon
- [ ] Systemd hardening
- [ ] Docker security options
- [ ] Monitoring and logging

### Milestone 3.3: Documentation
- [ ] API specification
- [ ] Deployment guide
- [ ] Security considerations
- [ ] Example integrations
- [ ] Contributing guidelines

### Deliverables
- v1.0.0 release
- Stable API
- Production deployment guides
- Security documentation

**Status**: Not Started

---

## Future Considerations (Post v1.0)

These are potential additions that maintain simplicity:

### Optional Features
- [ ] Multiple namespaces (/b/, /a/, /h/)
- [ ] Configurable TTLs (default: infinite)
- [ ] Read-only backup mirror
- [ ] LMDB storage backend
- [ ] Metrics endpoint (Prometheus)

### Hardening
- [ ] Raspberry Pi deployment profile
- [ ] Hardware-constrained optimizations
- [ ] Additional SPARK proofs
- [ ] Formal proof of collision bounds

### Integration
- [ ] GitHub Actions example
- [ ] GitLab CI example
- [ ] Grafana dashboard
- [ ] Home Assistant integration

---

## Non-Goals

The following will **not** be added to maintain scope:

- Marketing analytics
- Click tracking
- User accounts
- Custom aliases
- JavaScript redirects
- Link previews
- Web UI

---

## Current Status

**Current Version**: v0.1.0-dev
**Current Phase**: Phase 2 Complete → Phase 3
**Last Updated**: 2026-01-24

**Achievements**:
- FFI integration complete: Haskell calls SPARK core for URL validation and short code generation
- SPARK proofs at 100% for hadlink core (3 unproved checks in SPARKNaCl dependency)
- Comprehensive property test suite: 17 Hedgehog tests covering canonicalization, short codes, negative cases, and rate limiting
- Rate limiting implemented and tested: Token bucket per IP with configurable limits

---

## Contributing

See `CONTRIBUTING.md` for how to help with specific milestones.
