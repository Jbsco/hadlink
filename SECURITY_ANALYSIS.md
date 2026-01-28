Hadlink Security Vulnerability Analysis Report

Executive Summary

hadlink is a high-assurance URL shortener designed with security as a primary goal. The codebase demonstrates strong security architecture with formally verified core logic
(SPARK/Ada), defense-in-depth measures, and comprehensive deployment hardening. However, several vulnerabilities and areas of concern were identified.

Overall Assessment: The project demonstrates above-average security maturity for a URL shortener. The use of formal verification for core logic is exceptional. Most findings
are low-to-medium severity with clear mitigation paths.

---
Findings by Severity

HIGH SEVERITY

1. X-Forwarded-For Header Spoofing → Rate Limit Bypass

File: haskell/src/API.hs:172-181
CWE: CWE-290 (Authentication Bypass by Spoofing)

```
getClientIP :: Request -> ClientIP
getClientIP req =
   case lookup "X-Forwarded-For" (requestHeaders req) of
   Just xff ->
      let firstIP = BS.takeWhile (/= 44) xff  -- Takes first IP
      in ClientIP firstIP
   Nothing ->
      ClientIP $ sockAddrToBS (remoteHost req)
```
      
Vulnerability: The application trusts the X-Forwarded-For header without validation. An attacker can spoof this header to bypass rate limiting by using a different fake IP
for each request.

Attack Vector:

```
# Bypass rate limit by spoofing different IPs
for i in {1..1000}; do
   curl -X POST http://target:8443/api/create \
   -H "X-Forwarded-For: 10.0.0.$i" \
   -d "url=https://malicious.com/$i"
done
```

Impact: Complete bypass of rate limiting protection, enabling:
- Spam/DoS of the creation endpoint
- Rapid database pollution
- Enumeration of short codes (by creating many URLs)

Mitigation:
- Only trust X-Forwarded-For behind a properly configured reverse proxy
- Add configuration option to disable X-Forwarded-For trust
- Validate IP format before accepting
- Consider PROXY protocol instead

---
2. Insecure Default Secret

File: haskell/app/Main.hs:53
CWE: CWE-798 (Use of Hard-coded Credentials)

```
secretStr <- getEnvWithDefault "HADLINK_SECRET" "CHANGE_ME_INSECURE_DEFAULT"
```

Vulnerability: The application uses a hardcoded default secret if HADLINK_SECRET is not set. This secret is used for HMAC-SHA256 to generate short codes.

Impact:
- Predictable short codes if default is used
- An attacker knowing the secret can pre-compute short codes for any URL
- Potential for short code collision attacks

Mitigation:
- Fail startup if HADLINK_SECRET is not set or equals the default
- Add explicit check and error message
- Never ship with any default secret value

---
MEDIUM SEVERITY

3. SSRF Bypass via IPv6 and DNS Rebinding

File: spark-core/src/core.adb:96-138
CWE: CWE-918 (Server-Side Request Forgery)

```
function Is_Private_IP (Host : String) return Boolean is
begin
   if Host = "localhost" then return True; end if;
   -- Only checks string prefixes for IPv4 ranges
   if Prefix3 = "10." or else Prefix3 = "127" then return True; end if;
   -- 172.16-31.x and 192.168.x checked...
end Is_Private_IP;
```

Vulnerability: The private IP detection has gaps:
1. IPv6 not blocked: ::1, ::ffff:127.0.0.1, fe80::, fc00::/7 (ULA) all bypass
2. DNS rebinding: URL with hostname evil.com that resolves to 127.0.0.1 bypasses string-based checks
3. Link-local not blocked: 169.254.x.x addresses are not checked
4. 0.0.0.0 not blocked: Can represent localhost on some systems
5. Decimal IP encoding: 2130706433 (decimal for 127.0.0.1) may bypass

Attack Vector:
# IPv6 bypass
curl -X POST http://target:8443/api/create \
   -d "url=http://[::1]:8080/admin"

# DNS rebinding (attacker controls DNS)
curl -X POST http://target:8443/api/create \
   -d "url=http://rebind-to-localhost.attacker.com/secret"

Impact: If the redirect service is used to probe internal networks, attackers can create short links to internal services.

Mitigation:
- Add IPv6 private address checks (::1, fe80::/10, fc00::/7, ::ffff:x.x.x.x)
- Resolve DNS and validate the resolved IP at creation time
- Block 169.254.0.0/16 link-local range
- Block 0.0.0.0
- Consider blocking all non-FQDN hosts

---
4. Proof-of-Work Replay Attack

File: haskell/src/ProofOfWork.hs:32-40
CWE: CWE-294 (Authentication Bypass by Capture-replay)

```
verifyPoW :: Difficulty -> ValidURL -> Nonce -> Bool
verifyPoW (Difficulty diff) (ValidURL url) (Nonce nonce)
   | diff == 0 = True
   | otherwise =
      let urlBytes = encodeUtf8 url
         combined = BS.append urlBytes nonce
         digest = hash combined :: Digest SHA256
```
         
Vulnerability: The PoW verification has no nonce uniqueness check or timestamp. A valid nonce for a URL can be reused indefinitely.

Attack Scenario:
1. Compute valid nonce once for a malicious URL
2. Reuse the same URL+nonce pair to recreate the link after deletion
3. Pre-compute nonces offline for batch submission

Impact: PoW protection is weaker than expected against determined attackers.

Mitigation:
- Include server-provided challenge in hash (SHA256(url || server_challenge || nonce))
- Add timestamp to prevent long-term nonce reuse
- Store used nonces (with TTL) to prevent replay

---
5. Deployment Script Shell Injection Risk

File: deploy/deploy.sh:24-26
CWE: CWE-78 (OS Command Injection)

```
if [ -f "$SCRIPT_DIR/docker/.env" ]; then
   source "$SCRIPT_DIR/docker/.env" 2>/dev/null || true
fi
```

Vulnerability: The script sources a .env file without validation. If an attacker can modify the .env file, they can inject arbitrary shell commands.

Attack Vector:

```
# Malicious .env content:
REDIRECT_PORT=8080; rm -rf /important/*; #
```

Impact: Code execution with the privileges of the user running the deployment script (often root for systemd deployment).

Mitigation:
- Validate .env file contents before sourcing
- Use grep/sed to extract only expected variable patterns
- Set restrictive permissions on .env file (chmod 600)
- Never source untrusted files

---
6. FFI Memory Safety - Unverified Boundary

File: spark-core/src/core_ffi.adb:77-83
CWE: CWE-119 (Improper Restriction of Operations within Bounds of Memory Buffer)

```
pragma SPARK_Mode (Off);  -- NOT formally verified
...
Update (Output, 0, C_Array, Check => False);  -- No bounds check
```

Vulnerability: The FFI boundary code is explicitly excluded from SPARK verification (SPARK_Mode (Off)). The Update call with Check => False disables bounds checking.

On the Haskell side (SparkFFI.hs:100):

```
allocaBytes 2049 $ \outputPtr -> do  -- Fixed buffer size
```

If SPARK core ever returns more than 2048 bytes (or FFI marshalling is incorrect), buffer overflow occurs.

Impact: Potential memory corruption, though likelihood is low given strict bounds in SPARK core.

Mitigation:
- Add explicit bounds assertions in FFI code
- Enable Check => True or manually verify bounds
- Consider memory-safe FFI wrappers
- Property test FFI boundary with edge cases

---
LOW SEVERITY

7. API Key Comparison Timing Attack

File: haskell/src/API.hs:110-113
CWE: CWE-208 (Observable Timing Discrepancy)

```
let hasValidKey = case maybeKey of
      Nothing -> False
      Just key -> key `elem` cfgAPIKeys config  -- String comparison
```
      
Vulnerability: The elem function uses short-circuit string comparison, potentially leaking timing information about valid API key prefixes.

Impact: Low practical risk - requires many precise timing measurements. Standard network jitter likely masks the signal.

Mitigation:
- Use constant-time comparison for API keys
- Hash API keys at configuration load and compare hashes

---
8. No Short Code Character Validation on Lookup

File: haskell/src/API.hs:138-141

```
if T.length code /= 8
   then respond $ errorResponse status404 "Not found"
   else do
   maybeUrl <- get (ShortCode code) store  -- No character validation
```

Vulnerability: Only length is validated, not character set. Non-Base62 characters are passed to database query. While sqlite-simple uses parameterized queries (safe from
SQLi), this could cause:
- Unnecessary database queries
- Log injection if codes are logged
- Inconsistent behavior

Mitigation:
- Validate that code contains only Base62 characters before database lookup

---
9. Rate Limit State Not Persistent

File: haskell/src/RateLimit.hs

Observation: Rate limit state is entirely in-memory and per-process. Restarting the service resets all rate limits. With multiple instances (e.g., load-balanced deployment),
rate limiting is per-instance, not global.

Impact: Attackers can bypass rate limits by:
- Triggering service restart
- Distributing requests across multiple instances

Mitigation:
- Document limitation clearly
- Consider Redis-backed rate limiting for production deployments
- Add distributed rate limiting as optional feature

---
10. Location Header Injection Risk

File: haskell/src/API.hs:147-150

```
Just (ValidURL url) ->
   respond $ responseLBS status302
   [ ("Location", TE.encodeUtf8 url)  -- Direct injection into header
   , ("Cache-Control", "public, max-age=3600")
   ]
```

Observation: While SPARK validation prevents most dangerous URLs, if validation ever has gaps, the URL goes directly into the Location header. HTTP header injection via
newlines (CRLF) could be possible.

Current Protection: SPARK core likely rejects URLs with control characters (needs verification in core.ads).

Mitigation:
- Explicitly validate no CRLF (\r\n) in URL before header injection
- Add test case for CRLF injection attempts

---
Positive Security Features

The analysis identified several strong security practices:

1. Formally Verified Core Logic: SPARK/Ada with GNATprove for URL validation and short code generation - exceptional for a URL shortener
2. Defense in Depth:
   - Rate limiting per-IP
   - Optional Proof-of-Work
   - API key authentication
   - SSRF protection (partial)
3. Deployment Hardening:
   - Docker: no-new-privileges, cap_drop: ALL, read-only filesystem, memory limits
   - Systemd: ProtectSystem=strict, MemoryDenyWriteExecute=true, separate users
4. Parameterized SQL Queries: sqlite-simple library prevents SQL injection
5. Newtype Wrappers: RawURL vs ValidURL type distinction prevents accidental use of unvalidated input
6. Deterministic Short Codes: HMAC-based generation prevents enumeration
7. Secret Management: Docker secrets and file-based secrets with restricted permissions

---
Threat Model Gaps

Not Addressed by Current Design:

1. Malicious Destination URLs: No content scanning for phishing/malware links
2. Audit Logging: No record of who created what links
3. Link Expiration: No TTL mechanism for short codes
4. Abuse Reporting: No mechanism to report/disable malicious links
5. Rate Limiting Evasion: IPv6 rotation, proxy pools easily bypass current limits

---
Recommended Prioritized Actions
```
┌──────────┬──────────────────────────┬───────────────────────────────────────────────────┐
│ Priority │         Finding          │                      Action                       │
├──────────┼──────────────────────────┼───────────────────────────────────────────────────┤
│ P0       │ Insecure Default Secret  │ Fail startup if HADLINK_SECRET not set            │
├──────────┼──────────────────────────┼───────────────────────────────────────────────────┤
│ P0       │ X-Forwarded-For Spoofing │ Add config to disable XFF trust / validate format │
├──────────┼──────────────────────────┼───────────────────────────────────────────────────┤
│ P1       │ SSRF IPv6 Bypass         │ Add IPv6 private range checks                     │
├──────────┼──────────────────────────┼───────────────────────────────────────────────────┤
│ P1       │ Deploy Script Injection  │ Validate .env before sourcing                     │
├──────────┼──────────────────────────┼───────────────────────────────────────────────────┤
│ P2       │ PoW Replay               │ Add server challenge or timestamp                 │
├──────────┼──────────────────────────┼───────────────────────────────────────────────────┤
│ P2       │ FFI Bounds               │ Enable bounds checking in FFI                     │
├──────────┼──────────────────────────┼───────────────────────────────────────────────────┤
│ P3       │ Timing Attack            │ Constant-time API key comparison                  │
├──────────┼──────────────────────────┼───────────────────────────────────────────────────┤
│ P3       │ Short Code Validation    │ Validate Base62 characters on lookup              │
└──────────┴──────────────────────────┴───────────────────────────────────────────────────┘
```
---
Conclusion

Hadlink demonstrates thoughtful security architecture, particularly with the formally verified SPARK core. The most critical issues are the potential for rate limit bypass
via header spoofing and the insecure default secret. Both have straightforward fixes.

The SSRF protections, while better than most URL shorteners, have gaps for IPv6 and DNS-based attacks that should be addressed before production deployment.

For a project explicitly designed for "high-assurance" use cases, addressing the P0 and P1 issues would bring the implementation in line with the design goals.
