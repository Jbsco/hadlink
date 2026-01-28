# hadlink
[![SPARK Proofs](https://github.com/Jbsco/hadlink/actions/workflows/prove.yml/badge.svg)](https://github.com/Jbsco/hadlink/actions/workflows/prove.yml)[![Integration Tests](https://github.com/Jbsco/hadlink/actions/workflows/integration.yml/badge.svg)](https://github.com/Jbsco/hadlink/actions/workflows/integration.yml)[![Tests](https://github.com/Jbsco/hadlink/actions/workflows/test.yml/badge.svg)](https://github.com/Jbsco/hadlink/actions/workflows/test.yml)[![Style Check](https://github.com/Jbsco/hadlink/actions/workflows/style.yml/badge.svg)](https://github.com/Jbsco/hadlink/actions/workflows/style.yml)

[![Build](https://github.com/Jbsco/hadlink/actions/workflows/build.yml/badge.svg)](https://github.com/Jbsco/hadlink/actions/workflows/build.yml)[![Docker](https://github.com/Jbsco/hadlink/actions/workflows/deploy-docker.yml/badge.svg)](https://github.com/Jbsco/hadlink/actions/workflows/deploy-docker.yml)[![Systemd](https://github.com/Jbsco/hadlink/actions/workflows/deploy-systemd.yml/badge.svg)](https://github.com/Jbsco/hadlink/actions/workflows/deploy-systemd.yml)

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

Examples use [curl](https://curl.se/) ([curl license](https://curl.se/docs/copyright.html)) for HTTP requests.

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

## Why a URL Shortener?

A URL shortener is a small but realistic service where input validation, canonicalization, abuse resistance, and deterministic behavior matter. It is a lightweight field to build embedded-style rigor and formal methods without requiring hardware. Working on this provides an opportunity to work with thin language interop and formal verification tools.

Beyond the technical implementation, this project organization is guided by:

- **Scope management** - Maintaining explicit non-goals, resisting feature creep
- **Invariant-based planning** - Defining what must always be true, then building toward a known ceiling
- **Mature project organization** - Proper licensing, toolchain attribution, phased roadmaps, and documentation that respects users', reviewers', and contributors' time

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

**Performance:**

The redirect path is optimized for speed (Warp + SQLite lookup). Create operations include cryptographic validation and FFI overhead, which is acceptable for infrastructure use cases but not designed for high-volume marketing workloads.

---

## Status

**Version**: 0.1.0-dev
**Phase**: Phase 3 (Hardening) in progress

- SPARK core 100% verified (135 proof checks)
- 24 property tests via Hedgehog (canonicalization, short codes, negative cases, rate limiting, proof-of-work)
- Security self-audit complete (all P0/P1 items addressed)
- Rate limiting integrated and tested (token bucket per IP)
- FFI boundary stable

See [ROADMAP.md](docs/ROADMAP.md) for details.

---

## Quick Start

### Prerequisites

**Docker** (recommended for deployment)

- **[Docker](https://docs.docker.com/get-docker/)** with Compose plugin (Apache-2.0)
  ```bash
  ./deploy/deploy.sh docker start --generate-secret
  ```
  All build dependencies (redo, Stack, Alire) are containerized. Skip to [Deployment](#deployment).

**Native build** (for development)

1. **[dinkelk/redo](https://github.com/dinkelk/redo)** - Build system (MIT), chosen for correct dependency tracking, minimal complexity, and a Haskell implementation that aligns with the project's toolchain.
   ```bash
   git clone https://github.com/dinkelk/redo.git
   cd redo && ./do
   export PATH=$PATH:$(pwd)/bin
   ```

2. **[Stack](https://github.com/commercialhaskell/stack)** - Haskell build tool (BSD-3-Clause)
   ```bash
   # On most systems
   curl -sSL https://get.haskellstack.org/ | sh

   # Or via package manager
   # Arch: sudo pacman -S stack
   # Ubuntu: sudo apt install haskell-stack
   ```

3. **[Alire](https://alire.ada.dev/)** - Ada/SPARK package manager (GPL-3.0)
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

4. **[HLint](https://github.com/ndmitchell/hlint)** - Haskell style checker (BSD-3-Clause, optional)
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
redo prove    # SPARK proofs
redo style    # Check code style
```

### Common Commands

```bash
redo           # Show help
redo clean     # Clean artifacts
redo style     # Check SPARK and Haskell style
redo generate-secret  # Generate deployment secret
```

### Try It Locally

```bash
# Start the server (Ctrl+C to stop)
redo run-shorten

# In another terminal, create a short link
curl -s -X POST http://localhost:8080/api/create \
  -H "X-API-Key: test" \
  -d "url=https://example.com/long/path"
# {"code":"Bmx9c8bI","short":"http://localhost:8080/Bmx9c8bI"}

# Follow the redirect
curl -I http://localhost:8080/Bmx9c8bI
# HTTP/1.1 302 Found
# Location: https://example.com/long/path
```

---

## Deployment

Use the deployment script for quick setup:

```bash
# Show all options
./deploy/deploy.sh --help

# Docker deployment with default settings
./deploy/deploy.sh docker start --generate-secret

# Docker with proof-of-work enabled
./deploy/deploy.sh docker start --generate-secret --pow-difficulty 8 --pow-difficulty-auth 2

# Stop/remove Docker deployment
./deploy/deploy.sh docker stop
./deploy/deploy.sh docker remove              # Keeps data volume
./deploy/deploy.sh docker remove --remove-data  # Removes everything

# Systemd deployment (requires root)
sudo ./deploy/deploy.sh systemd start --generate-secret

# Manage systemd deployment
sudo ./deploy/deploy.sh systemd stop
sudo ./deploy/deploy.sh systemd update        # Reload config and restart
sudo ./deploy/deploy.sh systemd uninstall     # Removes services, keeps database
sudo ./deploy/deploy.sh systemd uninstall --remove-data  # Removes everything
```

### Manual Docker Setup

```bash
cd deploy/docker

# Generate secret key (must be exactly 32 characters)
openssl rand -hex 16 | tr -d '\n' > secret.key
chmod 600 secret.key

# Build and start services
docker build -t hadlink:latest -f Dockerfile ../..
docker compose up -d

# Test the services
curl -X POST http://127.0.0.1:8443/api/create \
  -H "X-API-Key: test" \
  -d "url=https://example.com"
```

### Manual systemd Setup

```bash
# Install binary and library
sudo cp hadlink /usr/local/bin/
sudo cp libHadlink_Core.so /usr/local/lib/
sudo ldconfig

# Create user and directories
sudo useradd -r -s /sbin/nologin hadlink
sudo mkdir -p /var/lib/hadlink /etc/hadlink
sudo chown hadlink:hadlink /var/lib/hadlink

# Install service files
sudo cp deploy/systemd/*.service /etc/systemd/system/
sudo cp deploy/systemd/hadlink.conf /etc/hadlink/

# Generate secret and create secret.conf (secret must be exactly 32 characters)
SECRET=$(openssl rand -hex 16)
echo "HADLINK_SECRET=${SECRET}" | sudo tee /etc/hadlink/secret.conf > /dev/null
sudo chmod 600 /etc/hadlink/secret.conf

# Start services
sudo systemctl daemon-reload
sudo systemctl enable --now hadlink-shorten hadlink-redirect
```

See [docs/examples/README.md](docs/examples/README.md) for detailed deployment guides.

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

Cryptographic primitives are provided by [SPARKNaCl](https://github.com/rod-chapman/SPARKNaCl) (BSD 3-Clause), chosen for compatibility with SPARK proofs, minimal attack surface, and permissive licensing compatible with AGPL-3.0.

- HMAC-SHA256 for short code generation
- No custom cryptographic algorithms

### Operational Guidance

For best security:
- Expose only the redirect daemon publicly
- Restrict the shorten daemon to LAN or VPN
- Run services with least privilege
- Use read-only filesystems where possible
- Monitor logs for abnormal creation rates
- Set `HADLINK_TRUST_PROXY=true` only when behind a trusted reverse proxy

### Proof-of-Work

Optional proof-of-work mitigates spam by requiring clients to compute a valid nonce before creating short links. PoW is disabled by default.

**Security model:**
- Anonymous requests require proof-of-work (when enabled)
- Authenticated API clients may have reduced or bypassed PoW, but are subject to rate limits, revocation, and behavioral constraints

**Configuration:**
```bash
# Secret key for HMAC-based short code generation (REQUIRED, must be set)
# Generate with: openssl rand -hex 16
export HADLINK_SECRET=<your-secret-key>

# Difficulty = number of leading zero bits required in SHA256(url || nonce)
# Higher values = more computation required (each +1 doubles average work)

# Anonymous request difficulty (0 = disabled)
export HADLINK_POW_DIFFICULTY=8

# Authenticated request difficulty (0 = bypass, or set lower for defense-in-depth)
export HADLINK_POW_DIFFICULTY_AUTH=2

# API keys (comma-separated)
export HADLINK_API_KEYS=ci-system-key,monitoring-key

# Trust X-Forwarded-For header (only enable behind trusted reverse proxy)
# Default: false (uses direct socket address for rate limiting)
export HADLINK_TRUST_PROXY=false
```

**Example configurations:**
| Scenario | `DIFFICULTY` | `DIFFICULTY_AUTH` | Effect |
|----------|--------------|-------------------|--------|
| PoW disabled | 0 | 0 | No PoW for anyone |
| Anonymous only | 8 | 0 | Anonymous clients work, API keys bypass |
| Defense-in-depth | 8 | 2 | Everyone works, API keys work less |
| High security | 12 | 4 | Strong protection for both |

**Client usage:**
```bash
# Anonymous: must find nonce where SHA256(canonical_url || nonce) has N leading zero bits
curl -X POST http://hadlink.internal/api/create \
  -d "url=https://example.com/path&nonce=<valid-nonce>"

# Authenticated: uses reduced difficulty (or bypasses if DIFFICULTY_AUTH=0)
curl -X POST http://hadlink.internal/api/create \
  -H "X-API-Key: ci-system-key" \
  -d "url=https://example.com/path&nonce=<valid-nonce>"
```

PoW is verified against the canonicalized URL. Since hadlink's canonicalization is minimal (scheme/host preserved exactly), clients can typically use the URL as-is.

### Reporting a Vulnerability

If you believe you have found a security issue:
- Do not open a public issue
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

Mitigations: Strict URL validation, private IP ranges rejected (IPv4 and IPv6), link-local addresses rejected, non-HTTP(S) schemes rejected

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

### Non-Goals

- Marketing analytics / tracking
- User accounts or dashboards
- Custom aliases
- JavaScript redirects or previews
- Link previews or metadata scraping

Non-goals may be revisited if the threat model or invariants change. For example, stripping tracking parameters (`utm_*`, `fbclid`, etc.) conflicts with canonicalization invariants and proof boundaries; but could be reconsidered if a formally specified opt-in transformation layer were added outside the SPARK core.

---

## Assurance Model

**This project is not certified.**

It is architected and developed with reference to the integrity objectives of DO-278A (Software Integrity Level 3), including integrity allocation, explicit assumptions, formal verification of integrity-critical logic, and traceability of requirements to verification artifacts. The SPARK verification approach follows the [Implementation Guidance for the Adoption of SPARK](https://www.adacore.com/uploads/books/Spark-Guidance-1.2-web.pdf) (AdaCore & Thales).

Formal certification is out of scope due to lack of operational deployment, certification authority, and independent verification resources. This is a single-developer project.

### Integrity Allocation

| Component | Integrity Level | Rationale |
|-----------|-----------------|-----------|
| SPARK Core | High | Integrity-critical: URL validation, short code generation. Formally verified. |
| Haskell Service | Supporting | Network I/O, storage, rate limiting. Property-tested. |

### What This Means

The project is **not intended for safety-critical deployment**, but is structured similarly to DO-278A systems:

- Formally proven core with explicit postconditions
- Documented assumptions at proof boundaries (`pragma Assume`)
- Lower-integrity service layer for I/O and orchestration
- High-level requirements defined as invariants and non-goals
- Verification evidence via GNATprove and Hedgehog property tests

See [ROADMAP.md](docs/ROADMAP.md#do-278a-sil-3-mapping) for detailed objective mapping.

---

## Documentation

- **[Architecture](docs/)** - Design philosophy and dual-language approach
- **[Build System](docs/build/)** - Using redo to build the project
- **[Examples](docs/examples/)** - CI integration, monitoring, deployment
- **[Roadmap](docs/ROADMAP.md)** - Development phases and milestones
- **[Contributing](docs/CONTRIBUTING.md)** - How to contribute

---

## License

AGPL-3.0-or-later - See [LICENSE.md](LICENSE.md)

Third-party licenses: [LICENSES/](LICENSES/)

This program is free software: you can redistribute it and/or modify it under
the terms of the GNU Affero General Public License as published by the Free
Software Foundation, either version 3 of the License, or (at your option) any
later version.
