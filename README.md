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

## Example Use Cases

### CI Build Notifications

Jenkins (or any CI) posts build results to Slack/SMS. Long artifact URLs don't fit:

```bash
# In your Jenkinsfile or post-build script
SHORT=$(curl -s -X POST http://hadlink.internal/api/create \
  -H "X-API-Key: $HADLINK_KEY" \
  -d "url=$BUILD_URL/artifact/report.html" | jq -r .short)

# Send notification with short link
curl -X POST "$SLACK_WEBHOOK" \
  -d "{\"text\": \"Build #${BUILD_NUMBER} complete: $SHORT\"}"
```

The same URL always produces the same short code. Re-running the build doesn't create duplicates.

### QR Codes for Infrastructure

Label physical equipment with stable, scannable links to dashboards or documentation:

```bash
# Generate short link for a rack's monitoring dashboard
curl -X POST http://hadlink.internal/api/create \
  -d "url=https://grafana.internal/d/xyz/rack-14-temps"

# Response: {"short": "https://s.example.com/3Kf8mQp"}
# Print QR code for this URL and affix to rack
```

The short code is deterministic—regenerating it for the same URL returns the same code, so labels remain valid even if the database is rebuilt from the URL list.

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
- Boundary is minimal and frozen (see [FFI_INTEGRATION.md](docs/FFI_INTEGRATION.md))

---

## Status

**Version**: 0.1.0-dev
**Phase**: FFI integration complete, moving to hardening

Haskell handles HTTP, storage, and concurrency. SPARK provides formally-verifiable URL validation and short code generation via FFI. See [ROADMAP.md](docs/ROADMAP.md) for details.

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

3. **Alire** - Ada/SPARK package manager (for Phase 2 proofs)
   ```bash
   # Arch/Manjaro
   yay -S alire-bin

   # Or download from https://alire.ada.dev
   ```

   Then in `spark-core/` directory:
   ```bash
   cd spark-core
   alr build        # Build SPARK core
   alr exec -- gnatprove -P hadlink_core.gpr  # Run proofs
   ```

4. **HLint** - Haskell style checker (optional)
   ```bash
   # Install via Stack (recommended - matches project GHC version)
   cd haskell
   stack install hlint

   # Or system package (may have version conflicts)
   # Arch: sudo pacman -S hlint
   ```

### Build

```bash
cd hadlink
redo all      # Build everything
redo test     # Run tests
redo prove    # SPARK proofs (Phase 2+)
redo style    # Check code style
```

### Common Commands

```bash
redo           # Show help
redo clean     # Clean artifacts
redo style     # Check SPARK and Haskell style
redo generate-secret  # Generate deployment secret
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

- HMAC-SHA256 via [SPARKNaCl](https://github.com/rod-chapman/SPARKNaCl)
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

AGPL-3.0 - See [LICENSE.md](LICENSE.md)

This program is free software: you can redistribute it and/or modify it under
the terms of the GNU Affero General Public License as published by the Free
Software Foundation, either version 3 of the License, or (at your option) any
later version.
