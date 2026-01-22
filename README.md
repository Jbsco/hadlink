# hadlink

**hadlink** is a self-hosted, high-assurance URL shortener designed for:

- Automated systems (CI/CD, monitoring)
- Constrained channels (SMS, QR codes, logs)
- Long-lived, auditable links

It is **not** a marketing or analytics platform. The system prioritizes:

- Deterministic behavior
- Security and abuse resistance
- Provable correctness for core logic
- Minimal and auditable architecture

---

## Motivation

Most URL shorteners are designed for user-facing growth or marketing.  
**hadlink** treats URL shortening as **infrastructure**, not a toy:

- URLs are validated and canonicalized
- Short codes are deterministic and non-enumerable
- Redirects are fast, read-only, and predictable

---

## Intended Use

- CI/CD build links
- Monitoring alerts (Grafana, Uptime Kuma, etc.)
- Artifact and log references
- QR codes for physical infrastructure
- Home labs and small deployments

---

## Non-Goals

- Marketing analytics / tracking
- User accounts or dashboards
- Custom aliases
- JavaScript redirects or previews
- Link previews or metadata scraping

---

## Architecture

                    ┌──────────────────────┐
                    │     Haskell API      │
                    │  (Warp HTTP Server)  │
                    └─────────┬────────────┘
                              │
                              │  API Requests / Responses
                              │
        ┌─────────────────────▼──────────────────────┐
        │             FFI Boundary                   │
        │   (SPARK core: canonicalize & shortcode)   │
        └─────────────────────┬──────────────────────┘
                              │
           ┌──────────────────▼───────────────────┐
           │              SPARK Core              │
           │ ┌──────────────┐  ┌────────────────┐ │
           │ │ Canonicalize │  │ Make_ShortCode │ │
           │ │  Valid_URL   │  │ Short_Code     │ │
           │ └──────────────┘  └────────────────┘ │
           └──────────────────┬───────────────────┘
                              │
                              │ Deterministic, provable results
                              │
           ┌──────────────────▼───────────────────┐
           │          Storage Backend             │
           │  SQLite / LMDB / Append-Only Table   │
           │ Short_Code -> Canonical_URL Mapping  │
           └──────────────────┬───────────────────┘
                              │
                              │
           ┌──────────────────▼───────────────────┐
           │           Redirect Service           │
           │   GET /{shortcode} -> 302 redirect   │
           └──────────────────────────────────────┘


**Key Principles:**

- SPARK proves all **security-critical logic**
- Haskell composes the system, handles IO and concurrency
- Boundary is minimal and frozen

---

## Security Model

- All URLs are validated in SPARK
- Short codes are deterministic and non-sequential
- Redirects are read-only
- Rate limiting and optional PoW mitigate abuse
- SPARK proofs guarantee invariants such as:
  - Only `http`/`https` URLs leave canonicalization
  - No private IPs
  - Maximum URL lengths

See [THREAT_MODEL.md](THREAT_MODEL.md) for full details.

```

---

## Development Status

**Current Phase**: Phase 1 Complete → Phase 2

[x] Project structure and documentation
[x] Build system (redo + Stack)
[x] Phase 1: Complete Haskell implementation
  - Core modules: Types, Canonicalize, ShortCode, Store, API
  - Security: ProofOfWork, RateLimit
  - Executables: shorten/redirect daemon modes
  - Clean build
[x] Phase 2: SPARK core implementation
  - SPARK canonicalization (URL validation, scheme checks, private IP blocking)
  - SPARK short code generation (HMAC + Base62)
  - C-compatible FFI boundary
  - Clean build (proofs require gnatprove)
[ ] Phase 2.5: FFI integration into Haskell (next)

See [docs/ROADMAP.md](docs/ROADMAP.md) for detailed milestones.

---

## Quick Start

### Prerequisites

1. **redo** - Build system
   ```bash
   git clone https://github.com/dinkelk/redo.git
   cd redo && ./do
   export PATH=$PATH:$(pwd)/bin
   ```

2. **Stack** - Haskell build tool
   ```bash
   # On most systems
   curl -sSL https://get.haskellstack.org/ | sh

   # Or via package manager
   # Arch: sudo pacman -S stack
   # Ubuntu: sudo apt install haskell-stack
   ```

3. **GNAT with SPARK** (Optional, for Phase 2)
   - Download from https://www.adacore.com/community

### Build

```bash
cd hadlink
redo all      # Build everything
redo test     # Run tests
redo prove    # SPARK proofs (Phase 2+)
```

### Common Commands

```bash
redo           # Show help
redo clean     # Clean artifacts
redo generate-secret  # Generate deployment secret
```

---

## Quick Example

```bash
# Create a short link (API)
curl -X POST https://hadlink.home/api/create \
  -H "X-API-Key: ci" \
  -d "url=https://example.com/very/long/path"

# Response
{"short": "https://hadlink.home/8F3kP2Q"}

# Resolve (automatic redirect)
curl -I https://hadlink.home/8F3kP2Q
# HTTP/1.1 302 Found
# Location: https://example.com/very/long/path
```

---

## Security

### Design Philosophy

Security properties are achieved through architecture and invariants rather than complex logic:

- Predictable behavior
- Minimal attack surface
- Explicit trust boundaries
- Bounded resource usage

### Security Guarantees

- Stored URLs are validated and canonicalized
- Redirects do not execute scripts or modify headers
- Short codes are not enumerable
- Redirect path is read-only
- Resource usage is bounded under hostile input

### What This Project Does NOT Guarantee

- Safety of destination content
- Protection against malicious but authorized users
- Anonymity of users
- Compliance with regulatory regimes
- Defense against host-level compromise

### Cryptography

- SHA-256
- HMAC
- No custom cryptographic algorithms

### Operational Guidance

For best security:
- Expose only the redirect daemon publicly
- Restrict the shorten daemon to LAN or VPN
- Run services with least privilege
- Use read-only filesystems where possible
- Monitor logs for abnormal creation rates

### Reporting a Vulnerability

If you believe you have found a security issue:
- **Do not** open a public issue
- Contact the maintainer privately
- Provide clear reproduction steps, affected component, and expected vs actual behavior

---

## Threat Model

### Assets

- Integrity of stored URL mappings
- Availability of redirect service
- Predictable resource usage
- Privacy of internal URLs

### Trust Boundaries

- All input to shorten-daemon is untrusted
- redirect-daemon trusts only validated short codes
- Storage is trusted but may be read concurrently
- Configuration is trusted

### Attacker Profiles

**1. Spammers** - Attempt to generate large numbers of short links for abuse

Mitigations: Rate limiting, optional Proof-of-Work, deterministic codes

**2. Enumerators** - Attempt to discover valid short codes via brute force

Mitigations: HMAC-derived short codes, no sequential identifiers, fixed-length Base62

**3. SSRF Attackers** - Attempt to use the service to access internal resources

Mitigations: Strict URL validation, private IP ranges rejected, non-HTTP(S) schemes rejected

**4. DoS Attackers** - Attempt to exhaust CPU, memory, or storage

Mitigations: Bounded in-memory structures, separate fast/slow paths, read-only redirect service

### Explicit Non-Defenses

Out of scope:
- Protecting against compromise of the host OS
- Protecting against authorized users submitting malicious URLs
- Preventing access to publicly reachable URLs
- Content scanning or malware detection

### Design Invariants

- All stored URLs are canonicalized
- Redirect path performs no allocation beyond lookup
- Short codes cannot be user-chosen
- Storage is append-only

Violating these invariants is considered a bug.

---

## Documentation

- **[Architecture](docs/)** - Design philosophy and dual-language approach
- **[Build System](docs/build/)** - Using redo to build the project
- **[Examples](docs/examples/)** - CI integration, monitoring, deployment
- **[Roadmap](docs/ROADMAP.md)** - Development phases and milestones
- **[Contributing](docs/CONTRIBUTING.md)** - How to contribute

---

## License

MIT - See [LICENSE.md](LICENSE.md)
