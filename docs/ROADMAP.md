# Roadmap

This document outlines the planned development phases for hadlink.

## Contents

- [Core Principle](#core-principle)
- [Phase 1: Haskell Implementation](#phase-1-haskell-implementation-v010)
- [Phase 2: SPARK Core Extraction](#phase-2-spark-core-extraction-v050)
- [Phase 3: Hardening & v1.0](#phase-3-hardening--v10-v100)
  - [Lock SPARK Interface](#lock-spark-interface)
  - [Separate Redirect/Shorten Binaries](#separate-redirectshorten-binaries)
  - [Monitoring and Logging](#monitoring-and-logging)
- [Future Considerations](#future-considerations-post-v10)
- [Non-Goals](#non-goals)
- [Current Status](#current-status)
- [DO-278A SIL-3 Mapping](#do-278a-sil-3-mapping)
- [Contributing](#contributing)

---

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
- [x] Optional Proof-of-Work verification
- [x] Bounded memory structures

### Deliverables
- [x] Working Haskell implementation
- [x] Comprehensive test suite (24 property tests, 100 iterations each)
- [x] Documentation of invariants
- [x] Initial deployment (Docker + systemd)

**Status**: Complete

---

## Phase 2: SPARK Core Extraction (v0.5.0)

**Goal**: Move security-critical logic to SPARK with formal proofs.

### Milestone 2.1: SPARK Canonicalization
- [x] Implement URL parsing in SPARK
- [x] Implement scheme validation (http/https only)
- [x] Implement private address rejection
- [x] Implement credential rejection
- [x] Implement length bounds
- [x] Prove all properties (near Gold-level via ghost lemma)

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
- [x] Proved SPARK core library (near Gold-level, assumes confined to ghost lemma)
- [x] FFI boundary (working)
- [x] Haskell using SPARK via FFI
- [x] Test suite validates equivalence

**Status**: Complete

---

## Phase 3: Hardening & v1.0 (v1.0.0)

**Goal**: Production-ready, frozen API.

### Milestone 3.1: Freeze SPARK API
- [x] Lock SPARK interface
- [x] Full proof coverage (137 checks, 100% verified)
- [x] Comprehensive documentation
- [x] Security audit of boundary (P0/P1 items addressed)

### Milestone 3.2: Deployment Hardening
- [x] Separate redirect/shorten binaries
- [x] Read-only redirect daemon
- [x] Systemd hardening
- [x] Docker security options
- [x] Secure secret handling (required at startup)
- [x] X-Forwarded-For protection (configurable, disabled by default)
- [x] Comprehensive SSRF protection (IPv4 + IPv6)
- [x] Safe deploy script (.env injection prevention)
- [x] Monitoring and logging

### Milestone 3.3: Documentation
- [x] API specification
- [x] Deployment guide
- [x] Security considerations
- [x] Example integrations
- [x] Contributing guidelines

### Lock SPARK Interface

The SPARK core API surface (`Canonicalize`, `Make_Short_Code`, and their FFI wrappers) should be declared frozen for v1.0. This means:

- **Version the API**: Add a version constant to `core.ads` (e.g., `API_Version : constant := 1`) so callers can assert compatibility.
- **Document the contract**: Record the current function signatures, preconditions, postconditions, and return codes as the stable interface. Changes after v1.0 require a major version bump.
- **Restrict exports**: Audit `core_ffi.ads` to confirm only intended symbols are exported. Remove or mark internal helpers as private.
- **Add a freeze test**: A dedicated test that compiles against the documented API and fails if signatures change. This can be a minimal C caller that links against `libHadlink_Core.so` and exercises both entry points.

### Separate Redirect/Shorten Binaries

Two separate binaries (`hadlink-shorten`, `hadlink-redirect`) via Cabal internal libraries:

- **`hadlink-common`**: Types, Store, API.Resolve, Logging, Health (no SPARK dependency)
- **`hadlink-shorten-lib`**: Canonicalize, ShortCode, SparkFFI, RateLimit, ProofOfWork, API.Shorten, API
- **`hadlink-redirect`**: Links only `hadlink-common` — no SPARK, no FFI, no rate limiting
- **`hadlink-shorten`**: Links both libraries, includes full creation pipeline

The redirect binary does not link `libHadlink_Core.so`, reducing attack surface and deployment complexity for the public-facing service.

### Monitoring and Logging

Implemented:

- **Structured logging**: JSON log lines via `fast-logger` to stdout (timestamps, level, method, path, status). Compatible with Docker and systemd journal.
- **Health check**: `GET /health` endpoint on both daemons returns `{"status":"ok"}` (200) or `{"status":"error","detail":"..."}` (503) based on database connectivity.

Deferred to post-v1.0.0:

- Metrics endpoint (Prometheus)
- Audit trail (append-only creation log)

### Deliverables
- v1.0.0 release
- Stable API
- Production deployment guides
- Security documentation

**Status**: In Progress

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

**Current Version**: v1.0.0
**Current Phase**: Phase 3 (Hardening) in progress
**Last Updated**: 2026-01-27

**Achievements**:
- FFI integration complete: Haskell calls SPARK core for URL validation and short code generation
- Near Gold-level SPARK proofs: Business logic assume-free, 2 pragma Assume confined to ghost lemma (see [GOLD_LEVEL_PROOFS.md](GOLD_LEVEL_PROOFS.md))
- Comprehensive property test suite: 24 Hedgehog tests covering canonicalization, short codes, negative cases, rate limiting, proof-of-work, and difficulty selection
- Rate limiting implemented and tested: Token bucket per IP with configurable limits
- Docker deployment: Multi-stage Dockerfile, security-hardened docker-compose with health checks
- Systemd deployment: Hardened service units with comprehensive security directives
- CI workflows: Docker build/test and systemd validation workflows
- Security self-audit complete: All P0/P1 vulnerabilities addressed
  - Secret key now required at startup (no insecure defaults)
  - X-Forwarded-For header trust configurable (disabled by default)
  - SSRF protection extended to IPv6 (loopback, link-local, ULA, mapped IPv4)
  - Deploy script hardened against .env injection

---

## DO-278A SIL-3 Mapping

This project is designed and developed following the principles of DO-278A Software Integrity Level 3, where applicable. This is a single-developer project without certification authority or independent verification resources.

### Objective Status

| DO-278A Objective | hadlink Status | Notes |
|-------------------|----------------|-------|
| **Integrity Allocation** | Complete | SPARK core (high-integrity) / Haskell service (supporting) |
| **Deterministic Core Behavior** | Complete | SPARK proves termination, bounded execution, no exceptions |
| **Input Validation** | Complete | URL validation with proven postconditions |
| **Assumption Documentation** | Complete | 2 `pragma Assume` confined to ghost lemma with rationale; see [GOLD_LEVEL_PROOFS.md](GOLD_LEVEL_PROOFS.md) |
| **Verification Evidence** | Complete | GNATprove output (137 checks), Hedgehog tests (24 properties) |
| **High-Level Requirements** | Complete | Invariants, non-goals, security constraints, abuse mitigation |
| **Traceability** | Partial | Requirements documented; formal traceability matrix out of scope |
| **Defined Baselines** | Out of Scope | Git tags serve as informal baselines |
| **Change Classification** | Out of Scope | Single-developer workflow |
| **Independent QA** | Out of Scope | No independent verification resources |
| **Tool Qualification** | Out of Scope | See [GNATprove qualification](https://docs.adacore.com/live/wave/spark2014/html/spark2014_ug/en/usage_scenarios.html#tool-qualification) |

### Component Integrity Levels

| Component | Level | Verification Method |
|-----------|-------|---------------------|
| `Core.Canonicalize` | High | Formal proof (GNATprove) |
| `Core.Make_Short_Code` | High | Formal proof (GNATprove) |
| `Core_FFI` | High | Thin wrapper, no logic |
| Haskell API | Supporting | Property tests (Hedgehog) |
| Rate Limiter | Supporting | Property tests (Hedgehog) |
| Proof of Work | Supporting | Property tests (Hedgehog) |
| Storage Layer | Supporting | Integration tests |

### Explicitly Out of Scope

- Formal certification process
- DO-278A documentation package
- Independent verification and validation (IV&V)
- Certification authority engagement
- Tool qualification evidence package
- Requirements management system integration

---

## Contributing

See `CONTRIBUTING.md` for how to help with specific milestones.
