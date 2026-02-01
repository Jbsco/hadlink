# API Specification

This document describes the hadlink HTTP API for creating and resolving short links.

## Contents

- [Overview](#overview)
- [Endpoints](#endpoints)
  - [POST /api/create](#post-apicreate)
  - [GET /{code}](#get-code)
  - [HEAD /{code}](#head-code)
- [Error Format](#error-format)
- [URL Validation Rules](#url-validation-rules)
- [Proof-of-Work](#proof-of-work)
- [Short Code Format](#short-code-format)
- [Service Architecture](#service-architecture)
- [Configuration Reference](#configuration-reference)

---

## Overview

### Base URL

Configure based on your deployment:
- Development: `http://localhost:8080` (redirect), `http://localhost:8443` (shorten)
- Production: Your configured domain (e.g., `https://s.example.com`)

### Authentication

API keys are optional and configured via the `HADLINK_API_KEYS` environment variable (comma-separated list).

When provided, include the key in requests:
```
X-API-Key: your-api-key
```

Authenticated requests may have reduced or bypassed proof-of-work requirements, depending on configuration.

### Rate Limiting

All requests are rate-limited per client IP using a token bucket algorithm:
- Default: 10 requests per 60-second window
- Configurable via `HADLINK_RATE_LIMIT` and `HADLINK_RATE_LIMIT_WINDOW`

When rate limited, requests return HTTP 429.

### Content Types

- **Request**: `application/x-www-form-urlencoded`
- **Response**: `application/json`

---

## Endpoints

### POST /api/create

Create a short link from a URL.

**Request:**

| Parameter | Type | Required | Description |
|-----------|------|----------|-------------|
| `url` | string | Yes | The URL to shorten (max 2048 bytes) |
| `nonce` | string | Conditional | Required when proof-of-work is enabled |

**Headers:**

| Header | Required | Description |
|--------|----------|-------------|
| `X-API-Key` | No | API key for authenticated requests |
| `Content-Type` | Yes | Must be `application/x-www-form-urlencoded` |

**Response (200 OK):**

```json
{
  "code": "Bmx9c8bI",
  "short": "http://localhost:8080/Bmx9c8bI"
}
```

| Field | Type | Description |
|-------|------|-------------|
| `code` | string | 8-character Base62 short code |
| `short` | string | Full short URL (uses `HADLINK_BASE_URL`, default: `http://localhost:$HADLINK_PORT`) |

**Errors:**

| Status | Condition | Response |
|--------|-----------|----------|
| 400 Bad Request | Missing `url` parameter | `{"error": "Missing url parameter"}` |
| 400 Bad Request | Invalid URL (validation failed) | `{"error": "<validation message>"}` |
| 400 Bad Request | Invalid proof-of-work | `{"error": "Invalid proof of work"}` |
| 429 Too Many Requests | Rate limit exceeded | `{"error": "Rate limit exceeded"}` |

**Example:**

```bash
# Without proof-of-work (if disabled)
curl -X POST http://localhost:8443/api/create \
  -d "url=https://example.com/long/path"

# With API key
curl -X POST http://localhost:8443/api/create \
  -H "X-API-Key: my-api-key" \
  -d "url=https://example.com/long/path"

# With proof-of-work nonce
curl -X POST http://localhost:8443/api/create \
  -d "url=https://example.com/long/path&nonce=<valid-nonce>"
```

**Idempotency:**

The same URL always produces the same short code. Re-submitting a URL does not create duplicates.

---

### GET /{code}

Resolve a short code to its destination URL.

**Path Parameters:**

| Parameter | Type | Description |
|-----------|------|-------------|
| `code` | string | 8-character Base62 short code |

**Response (302 Found):**

| Header | Value |
|--------|-------|
| `Location` | Destination URL |
| `Cache-Control` | `public, max-age=3600` |

The response body is empty.

**Errors:**

| Status | Condition | Response |
|--------|-----------|----------|
| 404 Not Found | Invalid or unknown short code | `{"error": "Not found"}` |

**Example:**

```bash
# Follow redirect
curl -L http://localhost:8080/Bmx9c8bI

# Inspect headers only
curl -I http://localhost:8080/Bmx9c8bI
# HTTP/1.1 302 Found
# Location: https://example.com/long/path
# Cache-Control: public, max-age=3600
```

---

### HEAD /{code}

Same as GET but returns headers only (no body). Useful for checking if a short code exists without following the redirect.

**Response:** Same headers as GET, empty body.

---

## Error Format

All errors return JSON with a single `error` field:

```json
{"error": "Description of the error"}
```

HTTP status codes indicate the error category:
- `400` - Client error (bad request, validation failure)
- `404` - Resource not found
- `429` - Rate limit exceeded
- `500` - Server error (rare)

---

## URL Validation Rules

URLs are validated and canonicalized by the SPARK core before storage. The following rules apply:

### Allowed

- Schemes: `http://`, `https://` only
- Standard ports (80, 443) or explicit ports
- Path, query string, and fragment components
- International domain names (as-is, no punycode conversion)

### Rejected

- Non-HTTP schemes (`ftp://`, `file://`, `javascript:`, etc.)
- URLs with credentials (`user:pass@host`)
- Private/local addresses:
  - IPv4: `10.0.0.0/8`, `172.16.0.0/12`, `192.168.0.0/16`, `127.0.0.0/8`, `169.254.0.0/16`, `0.0.0.0`
  - IPv6: `::1`, `::`, `fe80::/10` (link-local), `fc00::/7` (ULA), `::ffff:x.x.x.x` (mapped IPv4 with private address)
- URLs exceeding 2048 bytes

### Canonicalization

URLs are stored in canonical form:
- Scheme and host are preserved exactly as provided
- No normalization of case, trailing slashes, or query parameters
- Path components preserved as-is

This minimal canonicalization ensures deterministic short codes while preserving URL semantics.

---

## Proof-of-Work

When enabled, proof-of-work requires clients to compute a valid nonce before creating short links. This mitigates spam and abuse.

### Algorithm

```
hash = SHA256(canonical_url || nonce)
valid = leading_zero_bits(hash) >= difficulty
```

The client must find a `nonce` such that `SHA256(url + nonce)` has at least `difficulty` leading zero bits.

### Configuration

| Variable | Default | Description |
|----------|---------|-------------|
| `HADLINK_POW_DIFFICULTY` | 0 | Difficulty for anonymous requests (0 = disabled) |
| `HADLINK_POW_DIFFICULTY_AUTH` | 0 | Difficulty for authenticated requests (0 = bypass) |

Each additional bit of difficulty doubles the average computation required.

### Example Configurations

| Scenario | `DIFFICULTY` | `DIFFICULTY_AUTH` | Effect |
|----------|--------------|-------------------|--------|
| Disabled | 0 | 0 | No proof-of-work for anyone |
| Anonymous only | 8 | 0 | Anonymous clients compute, API keys bypass |
| Defense-in-depth | 8 | 2 | Everyone computes, API keys compute less |
| High security | 12 | 4 | Strong protection for all clients |

### Client Implementation

Compute nonces by iterating until a valid hash is found:

```python
import hashlib

def find_nonce(url: str, difficulty: int) -> str:
    nonce = 0
    while True:
        candidate = f"{url}{nonce}".encode()
        hash_bytes = hashlib.sha256(candidate).digest()
        if leading_zero_bits(hash_bytes) >= difficulty:
            return str(nonce)
        nonce += 1
```

---

## Short Code Format

Short codes are 8 characters from the Base62 alphabet:
- Characters: `0-9`, `A-Z`, `a-z` (62 total)
- Total combinations: 62^8 = 218,340,105,584,896

Codes are derived deterministically via HMAC-SHA256 using a server secret, ensuring:
- Same URL always produces the same code
- Codes are not enumerable or predictable without the secret
- No sequential patterns

---

## Service Architecture

hadlink runs as two separate daemons:

| Daemon | Port | Access | Operations |
|--------|------|--------|------------|
| `hadlink-shorten` | 8443 | Private (LAN/VPN) | POST /api/create, GET /health |
| `hadlink-redirect` | 8080 | Public | GET/HEAD /{code}, GET /health |

The redirect daemon has read-only database access and minimal privileges. Only the shorten daemon can create new links.

---

## Configuration Reference

| Variable | Default | Description |
|----------|---------|-------------|
| `HADLINK_SECRET` | (required) | 32-character HMAC secret for code generation |
| `HADLINK_PORT` | 8443/8080 | Service port |
| `HADLINK_STORAGE` | `./hadlink.db` | SQLite database path |
| `HADLINK_API_KEYS` | (empty) | Comma-separated API keys |
| `HADLINK_POW_DIFFICULTY` | 0 | PoW difficulty for anonymous requests |
| `HADLINK_POW_DIFFICULTY_AUTH` | 0 | PoW difficulty for authenticated requests |
| `HADLINK_RATE_LIMIT` | 10 | Requests per IP per window |
| `HADLINK_RATE_LIMIT_WINDOW` | 60 | Rate limit window in seconds |
| `HADLINK_TRUST_PROXY` | false | Trust X-Forwarded-For header |
