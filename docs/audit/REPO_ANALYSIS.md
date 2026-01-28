> **Status Update (2026-01-27)**: This is the original analysis report. For current security status after fixes, see [README.md](README.md).

---

## Contents

- [1. Project Overview](#1-project-overview)
- [2. Overall Architecture & Directory Structure](#2-overall-architecture--directory-structure)
- [3. Technologies & Frameworks](#3-technologies--frameworks)
- [4. Entry Points](#4-entry-points)
- [5. Data Flow Analysis](#5-data-flow-analysis)
- [6. Files Handling Security-Sensitive Operations](#6-files-handling-security-sensitive-operations)
- [7. Identified Entry Points for User Input](#7-identified-entry-points-for-user-input)
- [8. Security Analysis By Component](#8-security-analysis-by-component)
- [9. Threat Model & Mitigation](#9-threat-model--mitigation)
- [10. Critical Files Summary for Security Review](#10-critical-files-summary-for-security-review)
- [11. Build & Deployment Infrastructure](#11-build--deployment-infrastructure)
- [12. Summary Table: All Source Files](#12-summary-table-all-source-files)
- [13. Known Limitations & Notes](#13-known-limitations--notes)
- [14. Conclusion](#14-conclusion)

---

THOROUGH CODEBASE EXPLORATION REPORT: hadlink

1. PROJECT OVERVIEW

hadlink is a self-hosted, high-assurance URL shortener designed for automated systems, constrained channels, and long-lived auditable links. It emphasizes:
- Deterministic behavior
- Security and abuse resistance
- Provable correctness (formal verification)
- Minimal and auditable architecture

Version: 0.1.0-dev
License: AGPL-3.0-or-later
Repository: https://github.com/Jbsco/hadlink

---
2. OVERALL ARCHITECTURE & DIRECTORY STRUCTURE

```
/home/jbsco/cs/hadlink/
├── haskell/                    # HTTP Server & Orchestration (Warp)
│   ├── app/Main.hs            # Entry point: "shorten" or "redirect" daemons
│   ├── src/
│   │   ├── API.hs             # WAI application, HTTP handlers
│   │   ├── Types.hs           # Core type definitions
│   │   ├── Canonicalize.hs    # URL validation (calls SPARK FFI)
│   │   ├── ShortCode.hs       # Short code generation (calls SPARK FFI)
│   │   ├── Store.hs           # SQLite persistence
│   │   ├── RateLimit.hs       # Token bucket rate limiting
│   │   ├── ProofOfWork.hs     # Proof-of-work verification (SHA256)
│   │   └── SparkFFI.hs        # Foreign function interface to SPARK core
│   ├── test/Properties.hs     # Property-based tests (Hedgehog)
│   └── hadlink.cabal          # Cabal build manifest
│
├── spark-core/                 # Security-Critical Core (SPARK/Ada)
│   ├── src/
│   │   ├── core.ads           # Core spec with formal proofs (SPARK_Mode On)
│   │   ├── core.adb           # Implementation (450 lines, verified)
│   │   ├── core_ffi.ads       # FFI boundary specification
│   │   └── core_ffi.adb       # FFI implementation
│   ├── lib/libHadlink_Core.so # Compiled shared library
│   └── hadlink_core.gpr       # GNAT project file
│
├── deploy/
│   ├── deploy.sh              # Universal deployment script
│   ├── docker/
│   │   ├── Dockerfile         # Multi-stage build
│   │   └── docker-compose.yml # Compose with network isolation
│   └── systemd/
│       ├── hadlink-shorten.service    # Shorten daemon (write access)
│       ├── hadlink-redirect.service   # Redirect daemon (read-only)
│       └── hadlink.conf               # Configuration template
│
├── docs/
│   ├── FFI_INTEGRATION.md     # FFI implementation details
│   ├── GOLD_LEVEL_PROOFS.md   # Formal proof strategy
│   ├── ROADMAP.md             # Development phases
│   └── examples/              # Deployment examples
│
├── README.md                  # Main documentation
└── [build system files: *.do]  # redo-based build system
```

---
3. TECHNOLOGIES & FRAMEWORKS
```
┌─────────────────────┬─────────────────────────────────┬──────────────────────────────────────────────────┬──────────────────┐
│        Layer        │           Technology            │                     Purpose                      │ Version/License  │
├─────────────────────┼─────────────────────────────────┼──────────────────────────────────────────────────┼──────────────────┤
│ HTTP Server         │ Warp                            │ Non-blocking HTTP server                         │ BSD-3-Clause     │
├─────────────────────┼─────────────────────────────────┼──────────────────────────────────────────────────┼──────────────────┤
│ Web Framework       │ WAI (Web Application Interface) │ Request/response abstraction                     │ BSD-3-Clause     │
├─────────────────────┼─────────────────────────────────┼──────────────────────────────────────────────────┼──────────────────┤
│ Database            │ SQLite                          │ URL storage (short_code → url mapping)           │ Public Domain    │
├─────────────────────┼─────────────────────────────────┼──────────────────────────────────────────────────┼──────────────────┤
│ Cryptography        │ SPARKNaCl                       │ HMAC-SHA256 (formally verified)                  │ BSD-3-Clause     │
├─────────────────────┼─────────────────────────────────┼──────────────────────────────────────────────────┼──────────────────┤
│ Hashing             │ cryptonite library              │ SHA256 for PoW verification                      │ BSD-3-Clause     │
├─────────────────────┼─────────────────────────────────┼──────────────────────────────────────────────────┼──────────────────┤
│ Formal Verification │ SPARK 2014                      │ Formal proofs of core logic                      │ GPL-3.0          │
├─────────────────────┼─────────────────────────────────┼──────────────────────────────────────────────────┼──────────────────┤
│ Ada/SPARK Runtime   │ Ada/SPARK 2014                  │ Language for verified core                       │ GPL-3.0          │
├─────────────────────┼─────────────────────────────────┼──────────────────────────────────────────────────┼──────────────────┤
│ Build System        │ redo                            │ Reproducible builds, correct dependency tracking │ MIT              │
├─────────────────────┼─────────────────────────────────┼──────────────────────────────────────────────────┼──────────────────┤
│ Haskell Tooling     │ Stack                           │ Haskell project manager                          │ BSD-2-Clause     │
├─────────────────────┼─────────────────────────────────┼──────────────────────────────────────────────────┼──────────────────┤
│ Ada Tooling         │ Alire                           │ Ada package manager                              │ GPL-3.0          │
├─────────────────────┼─────────────────────────────────┼──────────────────────────────────────────────────┼──────────────────┤
│ Testing             │ Hedgehog                        │ Property-based testing                           │ BSD-3-Clause     │
├─────────────────────┼─────────────────────────────────┼──────────────────────────────────────────────────┼──────────────────┤
│ Deployment          │ Docker + Systemd                │ Container and system service options             │ Apache-2.0 / GPL │
└─────────────────────┴─────────────────────────────────┴──────────────────────────────────────────────────┴──────────────────┘
```
---
4. ENTRY POINTS

A. Command-Line Interface (CLI)

Located in: /home/jbsco/cs/hadlink/haskell/app/Main.hs

Entry point: hadlink {shorten|redirect|version}

Two operational modes:

4.1. Shorten Daemon - URL creation service
hadlink shorten
- Configuration from environment variables
- Listens on configurable port (default: 8443, localhost-only in production)
- Accepts POST requests to /api/create
- Requires authentication (API key) or proof-of-work
4.2. Redirect Daemon - Fast resolution service
hadlink redirect
- Listens on port 8080 (publicly exposed)
- Read-only database access
- GET/HEAD requests on /{shortcode} return 302 redirects
- Minimal permissions and resource constraints

B. HTTP API

Located in: /home/jbsco/cs/hadlink/haskell/src/API.hs

Endpoints:
- POST /api/create - Create short link (shorten daemon only)
- Required parameter: url (raw URL to shorten)
- Optional header: X-API-Key (for authentication)
- Optional parameter: nonce (for proof-of-work)
- Response: JSON with short (full URL) and code (8-char code)
- GET /{shortcode} or HEAD /{shortcode} - Resolve short link
- Validates 8-character code format
- Returns HTTP 302 Found with Location header
- Or HTTP 404 if not found

C. Configuration Sources

All from environment variables (no file-based config parsing):
HADLINK_SECRET              # 32-byte HMAC secret (CRITICAL)
HADLINK_PORT                # Service port (default: 8443 for shorten, 8080 for redirect)
HADLINK_STORAGE             # SQLite database path (default: ./hadlink.db)
HADLINK_POW_DIFFICULTY      # PoW difficulty for anonymous (0=disabled)
HADLINK_POW_DIFFICULTY_AUTH # PoW difficulty for authenticated (0=bypass)
HADLINK_API_KEYS            # Comma-separated list of API keys
HADLINK_RATE_LIMIT          # Requests per IP per window (default: 10)
HADLINK_RATE_LIMIT_WINDOW   # Window size in seconds (default: 60)

---
5. DATA FLOW ANALYSIS

Request Path: URL Shortening
```
User Request (curl/CI system)
   ↓
POST /api/create with {url, nonce?, X-API-Key?}
   ↓
[API.hs::createHandler]
   ├─→ Extract client IP from request
   ├─→ Check rate limit (RateLimit.hs::checkLimit)
   │   └─→ Token bucket per ClientIP (STM-based concurrent state)
   └─→ If allowed:
      ├─→ Parse body (raw URL extraction)
      ├─→ [Canonicalize.hs::canonicalize]
      │   ├─→ Call [SparkFFI.hs::sparkCanonicalize]
      │   │   └─→ FFI boundary → C function hadlink_canonicalize()
      │   │       └─→ SPARK core validates scheme, host, credentials
      │   └─→ Return ValidURL or ValidationError
      │
      ├─→ If validation fails: Return HTTP 400 + error message
      │
      ├─→ Get effective PoW difficulty (authenticated vs anonymous)
      │
      ├─→ If PoW required (difficulty > 0):
      │   ├─→ Extract nonce parameter
      │   ├─→ [ProofOfWork.hs::verifyPoW]
      │   │   └─→ Compute SHA256(canonical_url || nonce)
      │   │       └─→ Count leading zero bits
      │   └─→ If insufficient: Return HTTP 400
      │
      ├─→ [ShortCode.hs::generateShortCode]
      │   └─→ Call [SparkFFI.hs::sparkMakeShortCode]
      │       └─→ FFI boundary → C function hadlink_make_short_code()
      │           └─→ SPARK core: HMAC-SHA256(secret, url) → 8-char Base62 code
      │
      └─→ [Store.hs::put]
            └─→ SQLite: INSERT OR IGNORE links(short_code, canonical_url)
               └─→ Return HTTP 200 + {short, code}
```
Request Path: URL Resolution
```
User/Bot (browser/curl)
   ↓
GET /8CharCode (or HEAD)
   ↓
[API.hs::resolveHandler]
   ├─→ Validate code format (exactly 8 Base62 characters)
   ├─→ [Store.hs::get]
   │   └─→ SQLite: SELECT canonical_url FROM links WHERE short_code = ?
   └─→ If found:
      └─→ HTTP 302 Found + Location: {canonical_url}
      └─→ Cache-Control: public, max-age=3600
   └─→ If not found:
      └─→ HTTP 404 Not Found
```
Data Storage

SQLite database schema:
CREATE TABLE links (
short_code TEXT PRIMARY KEY NOT NULL,
canonical_url TEXT NOT NULL,
created_at INTEGER NOT NULL DEFAULT (strftime('%s', 'now'))
);
CREATE INDEX idx_created_at ON links(created_at);

Idempotency: INSERT OR IGNORE ensures same URL always produces same short code (deterministic)

---
6. FILES HANDLING SECURITY-SENSITIVE OPERATIONS

A. USER INPUT HANDLING
```
┌──────────┬───────────────┬────────────────────────────────────────┬───────────────────────────────────────┐
│   File   │   Function    │               Input Type               │           Security Measure            │
├──────────┼───────────────┼────────────────────────────────────────┼───────────────────────────────────────┤
│ API.hs   │ createHandler │ Raw HTTP body (URL)                    │ SPARK validation, length bounds       │
├──────────┼───────────────┼────────────────────────────────────────┼───────────────────────────────────────┤
│ API.hs   │ getClientIP   │ HTTP headers (X-Forwarded-For)         │ Extracts first IP, handles fallback   │
├──────────┼───────────────┼────────────────────────────────────────┼───────────────────────────────────────┤
│ Main.hs  │ parseAPIKeys  │ Environment variable (comma-separated) │ String splitting, text packing        │
├──────────┼───────────────┼────────────────────────────────────────┼───────────────────────────────────────┤
│ Types.hs │ All types     │ Type definitions                       │ Newtype wrappers (RawURL vs ValidURL) │
└──────────┴───────────────┴────────────────────────────────────────┴───────────────────────────────────────┘
```
B. AUTHENTICATION & AUTHORIZATION
```
┌──────────┬──────────────────┬──────────────────────┬───────────────────────────────────────────────────────┐
│   File   │    Component     │      Mechanism       │                      Key Points                       │
├──────────┼──────────────────┼──────────────────────┼───────────────────────────────────────────────────────┤
│ API.hs   │ selectDifficulty │ API key validation   │ Header X-API-Key checked against cfgAPIKeys list      │
├──────────┼──────────────────┼──────────────────────┼───────────────────────────────────────────────────────┤
│ API.hs   │ createHandler    │ Rate limiting        │ Per-IP token bucket, STM-based concurrent state       │
├──────────┼──────────────────┼──────────────────────┼───────────────────────────────────────────────────────┤
│ Main.hs  │ runShortenDaemon │ Secret configuration │ 32-byte HMAC secret from environment                  │
├──────────┼──────────────────┼──────────────────────┼───────────────────────────────────────────────────────┤
│ Store.hs │ put, get         │ SQLite               │ No direct SQL construction; uses parametrized queries │
└──────────┴──────────────────┴──────────────────────┴───────────────────────────────────────────────────────┘
```
C. CRYPTOGRAPHIC OPERATIONS
```
┌────────────────┬────────────────────────────┬─────────────┬───────────────────────────────┐
│      File      │         Operation          │  Algorithm  │            Library            │
├────────────────┼────────────────────────────┼─────────────┼───────────────────────────────┤
│ ShortCode.hs   │ Short code generation      │ HMAC-SHA256 │ SPARKNaCl (formally verified) │
├────────────────┼────────────────────────────┼─────────────┼───────────────────────────────┤
│ ProofOfWork.hs │ Proof-of-work verification │ SHA256      │ cryptonite library            │
├────────────────┼────────────────────────────┼─────────────┼───────────────────────────────┤
│ SparkFFI.hs    │ FFI marshalling            │ N/A         │ Foreign function interface    │
└────────────────┴────────────────────────────┴─────────────┴───────────────────────────────┘
```
D. FILE OPERATIONS
```
┌──────────────────┬───────────────────┬─────────────────────────┬───────────────────────────────────────────┐
│       File       │     Operation     │       Path Source       │                  Safety                   │
├──────────────────┼───────────────────┼─────────────────────────┼───────────────────────────────────────────┤
│ Store.hs         │ openStore         │ HADLINK_STORAGE env var │ Environment-configured, no path traversal │
├──────────────────┼───────────────────┼─────────────────────────┼───────────────────────────────────────────┤
│ Store.hs         │ SQLite PRAGMA     │ Hardcoded               │ WAL mode + PRAGMA synchronous=NORMAL      │
├──────────────────┼───────────────────┼─────────────────────────┼───────────────────────────────────────────┤
│ deploy/deploy.sh │ Secret generation │ Hardcoded path          │ --generate-secret flag, openssl rand      │
├──────────────────┼───────────────────┼─────────────────────────┼───────────────────────────────────────────┤
│ deploy/deploy.sh │ File copy         │ User-specified          │ Validates file existence before copy      │
└──────────────────┴───────────────────┴─────────────────────────┴───────────────────────────────────────────┘
```
E. DATABASE OPERATIONS
```
┌──────────┬──────────┬────────────────────────────────────┬────────────────────────────────────────────────┐
│   File   │ Function │             Query Type             │                      Risk                      │
├──────────┼──────────┼────────────────────────────────────┼────────────────────────────────────────────────┤
│ Store.hs │ put      │ INSERT OR IGNORE with placeholders │ SAFE - sqlite-simple uses parametrized queries │
├──────────┼──────────┼────────────────────────────────────┼────────────────────────────────────────────────┤
│ Store.hs │ get      │ SELECT with placeholders           │ SAFE - sqlite-simple uses parametrized queries │
├──────────┼──────────┼────────────────────────────────────┼────────────────────────────────────────────────┤
│ Store.hs │ exists   │ SELECT with placeholders           │ SAFE - sqlite-simple uses parametrized queries │
└──────────┴──────────┴────────────────────────────────────┴────────────────────────────────────────────────┘
```
Critical: Uses sqlite-simple library which uses parametrized query bindings. No string interpolation.

F. SYSTEM COMMAND EXECUTION
```
┌──────────────────┬───────────────────────────┬──────────────────┬───────────────────────────────────────────────────┐
│       File       │         Operation         │   Command Type   │                      Context                      │
├──────────────────┼───────────────────────────┼──────────────────┼───────────────────────────────────────────────────┤
│ deploy/deploy.sh │ openssl rand -hex 16      │ External process │ Secret generation only, output redirected to file │
├──────────────────┼───────────────────────────┼──────────────────┼───────────────────────────────────────────────────┤
│ deploy/deploy.sh │ docker build/compose/exec │ External process │ Container orchestration, user-controlled paths    │
├──────────────────┼───────────────────────────┼──────────────────┼───────────────────────────────────────────────────┤
│ deploy/deploy.sh │ mkdir/cp/chmod            │ Shell commands   │ File/directory operations for deployment          │
└──────────────────┴───────────────────────────┴──────────────────┴───────────────────────────────────────────────────┘
```
Risk in deploy.sh:
- Uses source <(alr printenv) in Dockerfile - process substitution with external command output
- Commands executed with $COMPOSE_CMD - variable expansion in shell context
- File paths from user arguments (--data-dir, --install-dir) not validated

G. FORMAL VERIFICATION (SPARK Core)
```
┌──────────────┬──────────┬─────────────────────────────────┬─────────────────────────────────┐
│     File     │  Module  │       Verification Status       │              Scope              │
├──────────────┼──────────┼─────────────────────────────────┼─────────────────────────────────┤
│ core.ads     │ Core     │ SPARK_Mode (On) - 100% verified │ Canonicalize, Make_Short_Code   │
├──────────────┼──────────┼─────────────────────────────────┼─────────────────────────────────┤
│ core.adb     │ Core     │ Formally proven                 │ URL validation, HMAC derivation │
├──────────────┼──────────┼─────────────────────────────────┼─────────────────────────────────┤
│ core_ffi.ads │ Core_FFI │ SPARK_Mode (Off)                │ FFI boundary - not verified     │
├──────────────┼──────────┼─────────────────────────────────┼─────────────────────────────────┤
│ core_ffi.adb │ Core_FFI │ SPARK_Mode (Off)                │ C marshalling - not verified    │
└──────────────┴──────────┴─────────────────────────────────┴─────────────────────────────────┘
```
---
7. IDENTIFIED ENTRY POINTS FOR USER INPUT
```
┌───────────────────────────────┬─────────────────────┬─────────────────────────────────────────┬───────────────────────────────────────────────┐
│         Input Source          │       Vector        │                 Handler                 │                  Validation                   │
├───────────────────────────────┼─────────────────────┼─────────────────────────────────────────┼───────────────────────────────────────────────┤
│ HTTP POST body                │ Raw URL             │ API.hs::createHandler → Canonicalize.hs │ SPARK FFI validation                          │
├───────────────────────────────┼─────────────────────┼─────────────────────────────────────────┼───────────────────────────────────────────────┤
│ HTTP header (X-API-Key)       │ API key             │ API.hs::selectDifficulty                │ String equality check against config list     │
├───────────────────────────────┼─────────────────────┼─────────────────────────────────────────┼───────────────────────────────────────────────┤
│ HTTP header (X-Forwarded-For) │ Client IP           │ API.hs::getClientIP                     │ Takes first comma-delimited value             │
├───────────────────────────────┼─────────────────────┼─────────────────────────────────────────┼───────────────────────────────────────────────┤
│ HTTP parameter (nonce)        │ Proof-of-work nonce │ API.hs::handleCreate → ProofOfWork.hs   │ SHA256 validation                             │
├───────────────────────────────┼─────────────────────┼─────────────────────────────────────────┼───────────────────────────────────────────────┤
│ URL path (/{shortcode})       │ Short code          │ API.hs::resolveHandler                  │ Length check (exactly 8 chars)                │
├───────────────────────────────┼─────────────────────┼─────────────────────────────────────────┼───────────────────────────────────────────────┤
│ Environment variables         │ All configuration   │ Main.hs                                 │ Parsed via read/lookupEnv, minimal validation │
├───────────────────────────────┼─────────────────────┼─────────────────────────────────────────┼───────────────────────────────────────────────┤
│ .env file (Docker)            │ Configuration       │ deploy/deploy.sh                        │ File sourcing without validation              │
├───────────────────────────────┼─────────────────────┼─────────────────────────────────────────┼───────────────────────────────────────────────┤
│ Command-line args             │ Deploy options      │ deploy/deploy.sh                        │ Minimal validation, used in shell commands    │
└───────────────────────────────┴─────────────────────┴─────────────────────────────────────────┴───────────────────────────────────────────────┘
```
---
8. SECURITY ANALYSIS BY COMPONENT

SPARK Core (core.ads/adb) - HIGHEST ASSURANCE

- [x] Formally proven with GNATprove
- [x] 100% SPARK_Mode (On) for verification
- [x] Predicates: Has_Valid_Scheme, Has_Credentials, Has_Private_Host
- [x] No external dependencies, pure logic
- [x] Explicit postconditions on Canonicalize, Make_Short_Code
- Limited to 2048-byte URL length (enforced at boundary)

Haskell HTTP Layer (API.hs) - MEDIUM ASSURANCE

- [x] Uses Warp (battle-tested HTTP server)
- [x] Uses WAI (standard web framework abstraction)
- [x] sqlite-simple uses parametrized queries (SQL injection safe)
- [x] Rate limiting with STM-based concurrency control
- [x] Proof-of-work validation before database writes
- FFI calls to Ada - requires correct marshalling (see SparkFFI.hs)
- Client IP extraction from headers - vulnerable to header spoofing without reverse proxy validation
- No input validation on nonce parameter (assumed arbitrary bytes)

Rate Limiting (RateLimit.hs) - MEDIUM ASSURANCE

- [x] Token bucket algorithm, per-IP tracking
- [x] STM for thread-safe concurrent access
- [x] Automatic cleanup of old buckets (prevents unbounded memory growth)
- Vulnerable to IP spoofing if X-Forwarded-For header is not validated
- No distributed rate limiting (single-process only)

Proof-of-Work (ProofOfWork.hs) - MEDIUM ASSURANCE

- [x] SHA256 from cryptonite (well-vetted library)
- [x] Leading zero bit counting is deterministic
- [x] Difficulty 0 disables PoW (explicit bypass)
- No re-work verification (client can't prove nonce was computed honestly)
- No replay protection (same URL + nonce always valid)

Deployment Scripts (deploy.sh) - LOW ASSURANCE

- Bash script with complex logic
- source <(alr printenv) - process substitution in Dockerfile
- Variable expansion in shell context
- User-provided paths not validated (--data-dir, --install-dir)
- Secret file handling: generated with openssl, file permissions set with chmod 600
- Docker compose configuration written dynamically

Docker Compose (docker-compose.yml) - MEDIUM ASSURANCE

- [x] Network isolation: redirect on public network, shorten on internal network
- [x] Read-only filesystem for redirect daemon
- [x] Security options: no-new-privileges, cap_drop ALL
- [x] Memory and CPU limits enforced
- [x] Secret managed via Docker Secrets mechanism
- Shorten service still accessible on port 8443 (expects firewall/LAN-only)
- No authentication between services (shared SQLite assumed secure)

Systemd Service Files - MEDIUM ASSURANCE

- [x] ProtectSystem=strict, ProtectHome=true
- [x] MemoryDenyWriteExecute=true, LockPersonality=true
- [x] Separate users (hadlink:hadlink) with least privilege
- [x] Redirect daemon has read-only storage (ReadOnlyPaths)
- [x] Shorten daemon has read-write storage (ReadWritePaths)
- [x] Secret loaded from /etc/hadlink/secret.conf (file permissions managed by root)
- LD_LIBRARY_PATH set globally - could be hijacked if not protected

---
9. THREAT MODEL & MITIGATION

Identified Assets:
9.1. Integrity of URL mappings
9.2. Availability of redirect service
9.3. Predictable resource usage
9.4. Privacy of internal URLs

Identified Threats & Mitigations:
```
┌─────────────────────────────────┬────────────────────────────────────────┬──────────────────────────────────────────────────────────────────────────┐
│             Threat              │             Attack Vector              │                                Mitigation                                │
├─────────────────────────────────┼────────────────────────────────────────┼──────────────────────────────────────────────────────────────────────────┤
│ SQL Injection                   │ Crafted short codes or URLs            │ sqlite-simple parametrized queries, no string interpolation              │
├─────────────────────────────────┼────────────────────────────────────────┼──────────────────────────────────────────────────────────────────────────┤
│ SSRF                            │ Craft URLs to access internal services │ SPARK validates no private IPs (10.x, 192.168.x, 172.16-31.x, localhost) │
├─────────────────────────────────┼────────────────────────────────────────┼──────────────────────────────────────────────────────────────────────────┤
│ URL Canonicalization Bypass     │ Non-canonical form of same URL         │ SPARK Canonicalize normalizes scheme/host, rejects credentials           │
├─────────────────────────────────┼────────────────────────────────────────┼──────────────────────────────────────────────────────────────────────────┤
│ Spam/DoS via Create             │ Flood /api/create endpoint             │ Rate limiting per IP + optional Proof-of-Work                            │
├─────────────────────────────────┼────────────────────────────────────────┼──────────────────────────────────────────────────────────────────────────┤
│ Short Code Enumeration          │ Brute force 8-char codes               │ HMAC-derived (non-sequential), 62^8 ≈ 218 trillion combinations          │
├─────────────────────────────────┼────────────────────────────────────────┼──────────────────────────────────────────────────────────────────────────┤
│ X-Forwarded-For Header Spoofing │ Fake client IP to bypass rate limit    │ Documented as trust boundary (requires reverse proxy validation)         │
├─────────────────────────────────┼────────────────────────────────────────┼──────────────────────────────────────────────────────────────────────────┤
│ Secret Exposure                 │ Read HADLINK_SECRET from environment   │ Environment variables + /etc/hadlink/secret.conf (file perms 600)        │
├─────────────────────────────────┼────────────────────────────────────────┼──────────────────────────────────────────────────────────────────────────┤
│ Malicious Destination URLs      │ Phishing/malware links                 │ No content validation (explicitly out of scope)                          │
├─────────────────────────────────┼────────────────────────────────────────┼──────────────────────────────────────────────────────────────────────────┤
│ Host Compromise                 │ Root-level attacker                    │ Out of scope (assumes hardware security)                                 │
└─────────────────────────────────┴────────────────────────────────────────┴──────────────────────────────────────────────────────────────────────────┘
```
---
10. CRITICAL FILES SUMMARY FOR SECURITY REVIEW

High Priority (Security Critical):
10.1. /home/jbsco/cs/hadlink/haskell/src/API.hs - HTTP request handling, authentication, rate limiting
10.2. /home/jbsco/cs/hadlink/haskell/src/SparkFFI.hs - FFI boundary, memory safety
10.3. /home/jbsco/cs/hadlink/haskell/src/Store.hs - Database access patterns
10.4. /home/jbsco/cs/hadlink/spark-core/src/core.adb - Formal verification implementation
10.5. /home/jbsco/cs/hadlink/deploy/docker/docker-compose.yml - Security hardening
10.6. /home/jbsco/cs/hadlink/deploy/systemd/hadlink-shorten.service - Service isolation

Medium Priority:
10.7. /home/jbsco/cs/hadlink/haskell/src/RateLimit.hs - Concurrency control
10.8. /home/jbsco/cs/hadlink/haskell/src/ProofOfWork.hs - PoW validation
10.9. /home/jbsco/cs/hadlink/haskell/app/Main.hs - Configuration loading, daemon startup
10.10. /home/jbsco/cs/hadlink/deploy/deploy.sh - Deployment automation

Lower Priority (Supporting):
10.11. /home/jbsco/cs/hadlink/haskell/src/Canonicalize.hs - URL preprocessing (delegates to SPARK)
10.12. /home/jbsco/cs/hadlink/haskell/src/ShortCode.hs - Short code generation (delegates to SPARK)
10.13. /home/jbsco/cs/hadlink/haskell/src/Types.hs - Type definitions
10.14. /home/jbsco/cs/hadlink/haskell/test/Properties.hs - Test cases

---
11. BUILD & DEPLOYMENT INFRASTRUCTURE

Build System: redo (dependency-based, correct tracking)
- all.do - Builds everything
- spark-core/build.do - Compiles SPARK core to libHadlink_Core.so
- haskell/build.do - Compiles Haskell with Stack, links SPARK library
- test.do - Runs Hedgehog property tests
- prove.do - Runs GNATprove verification
- style.do - HLint style check

Docker Build: Multi-stage Dockerfile
- Stage 1 (builder): Compiles SPARK (Alire/GNAT), Haskell (Stack), outputs binaries
- Stage 2 (runtime): Minimal Debian image, copies only binary + shared library
- Non-root user (hadlink:1000)
- LD_LIBRARY_PATH configured for SPARK library

Deployment Options:
11.1. Docker Compose (recommended) - network isolation, secrets management
11.2. Systemd services - with comprehensive security hardening options

---
12. SUMMARY TABLE: ALL SOURCE FILES
```
┌──────────────────────────┬───────┬───────────┬───────────────────────────────┬───────────────────────┬────────────────┐
│           File           │ Lines │ Language  │            Purpose            │      Input Type       │ Security Level │
├──────────────────────────┼───────┼───────────┼───────────────────────────────┼───────────────────────┼────────────────┤
│ Main.hs                  │ 136   │ Haskell   │ Entry point, daemon selection │ CLI args, environment │ MEDIUM         │
├──────────────────────────┼───────┼───────────┼───────────────────────────────┼───────────────────────┼────────────────┤
│ API.hs                   │ 209   │ Haskell   │ HTTP handlers, routing        │ HTTP requests         │ HIGH           │
├──────────────────────────┼───────┼───────────┼───────────────────────────────┼───────────────────────┼────────────────┤
│ Types.hs                 │ 95    │ Haskell   │ Type definitions              │ N/A                   │ MEDIUM         │
├──────────────────────────┼───────┼───────────┼───────────────────────────────┼───────────────────────┼────────────────┤
│ Canonicalize.hs          │ 77    │ Haskell   │ URL preprocessing             │ Raw URLs              │ MEDIUM         │
├──────────────────────────┼───────┼───────────┼───────────────────────────────┼───────────────────────┼────────────────┤
│ ShortCode.hs             │ 58    │ Haskell   │ Code generation               │ Validated URLs        │ MEDIUM         │
├──────────────────────────┼───────┼───────────┼───────────────────────────────┼───────────────────────┼────────────────┤
│ Store.hs                 │ 101   │ Haskell   │ SQLite operations             │ Short codes, URLs     │ MEDIUM         │
├──────────────────────────┼───────┼───────────┼───────────────────────────────┼───────────────────────┼────────────────┤
│ RateLimit.hs             │ 101   │ Haskell   │ Token bucket limiter          │ IP addresses          │ MEDIUM         │
├──────────────────────────┼───────┼───────────┼───────────────────────────────┼───────────────────────┼────────────────┤
│ ProofOfWork.hs           │ 54    │ Haskell   │ SHA256 validation             │ Nonces                │ MEDIUM         │
├──────────────────────────┼───────┼───────────┼───────────────────────────────┼───────────────────────┼────────────────┤
│ SparkFFI.hs              │ 141   │ Haskell   │ FFI bindings                  │ Haskell values        │ HIGH           │
├──────────────────────────┼───────┼───────────┼───────────────────────────────┼───────────────────────┼────────────────┤
│ Properties.hs            │ 150+  │ Haskell   │ Property tests                │ Generated data        │ LOW            │
├──────────────────────────┼───────┼───────────┼───────────────────────────────┼───────────────────────┼────────────────┤
│ core.ads                 │ 226   │ SPARK/Ada │ Spec with proofs              │ N/A                   │ HIGHEST        │
├──────────────────────────┼───────┼───────────┼───────────────────────────────┼───────────────────────┼────────────────┤
│ core.adb                 │ 450   │ SPARK/Ada │ Implementation                │ Internal only         │ HIGHEST        │
├──────────────────────────┼───────┼───────────┼───────────────────────────────┼───────────────────────┼────────────────┤
│ core_ffi.ads             │ 81    │ Ada       │ FFI spec                      │ C pointers            │ MEDIUM         │
├──────────────────────────┼───────┼───────────┼───────────────────────────────┼───────────────────────┼────────────────┤
│ core_ffi.adb             │ 138   │ Ada       │ FFI implementation            │ C strings             │ MEDIUM         │
├──────────────────────────┼───────┼───────────┼───────────────────────────────┼───────────────────────┼────────────────┤
│ docker-compose.yml       │ 88    │ YAML      │ Service orchestration         │ Environment vars      │ MEDIUM         │
├──────────────────────────┼───────┼───────────┼───────────────────────────────┼───────────────────────┼────────────────┤
│ Dockerfile               │ 122   │ Docker    │ Container build               │ N/A                   │ MEDIUM         │
├──────────────────────────┼───────┼───────────┼───────────────────────────────┼───────────────────────┼────────────────┤
│ hadlink-shorten.service  │ 46    │ Systemd   │ Shorten daemon unit           │ Environment           │ MEDIUM         │
├──────────────────────────┼───────┼───────────┼───────────────────────────────┼───────────────────────┼────────────────┤
│ hadlink-redirect.service │ 46    │ Systemd   │ Redirect daemon unit          │ Environment           │ MEDIUM         │
├──────────────────────────┼───────┼───────────┼───────────────────────────────┼───────────────────────┼────────────────┤
│ deploy.sh                │ 600+  │ Bash      │ Deployment automation         │ CLI args, files       │ LOW            │
└──────────────────────────┴───────┴───────────┴───────────────────────────────┴───────────────────────┴────────────────┘
```
---
13. KNOWN LIMITATIONS & NOTES

13.1. Single-threaded FFI: Ada runtime is not thread-safe for concurrent calls. Tests run with NumThreads 1.
13.2. No distributed rate limiting: Rate limiter only works within single process. Multiple instances would need coordination.
13.3. Client IP trust: X-Forwarded-For header is trusted. Requires proper reverse proxy validation at deployment boundary.
13.4. No API key rotation: Keys are static configuration; no versioning or revocation mechanism.
13.5. Database shared between daemons: Both shorten and redirect access same SQLite database concurrently.
13.6. No audit logging: Creation of short links is not logged (explicitly out of scope per non-goals).
13.7. No cleanup mechanism: Expired or unused mappings are not automatically removed.
13.8. Proof-of-work is optional: Disabled by default (HADLINK_POW_DIFFICULTY=0).

---
14. CONCLUSION

hadlink is a minimalist, security-conscious URL shortener with:
- Formally verified core logic (SPARK)
- Well-isolated deployment models (Docker + Systemd)
- Explicit threat model and non-goals (no marketing, no personalization)
- Defense-in-depth (rate limiting, PoW, SSRF checks, secret validation)

Security maturity: The project is not production-certified, but is architected with reference to DO-278A integrity levels. The SPARK core is formally proven; the
Haskell orchestration layer is property-tested and uses battle-tested libraries (Warp, sqlite-simple).

Recommended security focus areas for review:
14.1. FFI boundary correctness (SparkFFI.hs memory marshalling)
14.2. Rate limiting under adversarial IP spoofing
14.3. Docker/Systemd deployment isolation verification
14.4. Deployment script parameter validation (deploy.sh)

All files examined. Analysis complete.
