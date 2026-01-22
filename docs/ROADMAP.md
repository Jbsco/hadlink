# Roadmap

This document outlines the planned development phases for hadlink.

## Core Principle

Follow the migration plan: Haskell-only → SPARK extraction → frozen API.

---

## Phase 1: Haskell Implementation (v0.1.0)

**Goal**: Working URL shortener in pure Haskell with well-defined invariants.

### Milestone 1.1: Core Functionality
- [ ] URL validation and canonicalization in Haskell
- [ ] Deterministic short code generation (HMAC-SHA256 + Base62)
- [ ] Basic storage backend (SQLite)
- [ ] Simple HTTP API (create + resolve)

### Milestone 1.2: Property Tests
- [ ] Property-based test suite
- [ ] URL canonicalization properties
- [ ] Short code generation properties
- [ ] Round-trip properties
- [ ] Negative testing for invalid inputs

### Milestone 1.3: Abuse Mitigation
- [ ] Rate limiting (token bucket per IP/subnet)
- [ ] Optional Proof-of-Work verification
- [ ] Bounded memory structures

### Deliverables
- Working Haskell implementation
- Comprehensive test suite
- Documentation of invariants
- Initial deployment (Docker + systemd)

**Status**: In Progress

---

## Phase 2: SPARK Core Extraction (v0.5.0)

**Goal**: Move security-critical logic to SPARK with formal proofs.

### Milestone 2.1: SPARK Canonicalization
- [ ] Implement URL parsing in SPARK
- [ ] Prove scheme validation (http/https only)
- [ ] Prove private address rejection
- [ ] Prove credential rejection
- [ ] Prove length bounds

### Milestone 2.2: SPARK Short Code Generation
- [ ] Implement HMAC-SHA256 in SPARK
- [ ] Implement Base62 encoding
- [ ] Prove determinism
- [ ] Prove output length
- [ ] Prove character set constraints

### Milestone 2.3: FFI Integration
- [ ] Design C-compatible boundary
- [ ] Export SPARK functions
- [ ] Import into Haskell
- [ ] Maintain identical external behavior
- [ ] Property tests pass with SPARK backend

### Deliverables
- Proved SPARK core library
- FFI boundary
- Haskell using SPARK via FFI
- Test suite validates equivalence

**Status**: Not Started

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
**Current Phase**: Phase 1
**Last Updated**: 2026-01-22

---

## Contributing

See `CONTRIBUTING.md` for how to help with specific milestones.
