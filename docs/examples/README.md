# Examples

Integration examples and configuration templates for hadlink.

For deployment instructions, see [DEPLOYMENT.md](../DEPLOYMENT.md).

## Contents

- [CI Integration](#ci-integration)
- [Configuration](#configuration)
- [Monitoring Integration](#monitoring-integration)
- [QR Code Generation](#qr-code-generation)
- [Testing the API](#testing-the-api)
- [Advanced Usage](#advanced-usage)

---

## CI Integration

### Bash Script for CI/CD Pipelines

See [`ci-integration.sh`](ci-integration.sh) for a complete example of integrating hadlink into CI/CD pipelines.

Usage:
```bash
# In your CI pipeline
export HADLINK_API="https://hadlink.home/api/create"
export HADLINK_KEY="ci"
export BUILD_URL="http://jenkins.local/job/foo/123/console"

./ci-integration.sh
```

The script creates a short link and uses it in notifications.

### GitHub Actions Example

```yaml
name: Build
on: [push]

jobs:
  build:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v3

      - name: Build
        run: make build

      - name: Create short link
        env:
          HADLINK_API: ${{ secrets.HADLINK_API }}
          HADLINK_KEY: ${{ secrets.HADLINK_KEY }}
        run: |
          BUILD_URL="${{ github.server_url }}/${{ github.repository }}/actions/runs/${{ github.run_id }}"
          SHORT=$(curl -s "$HADLINK_API" \
            -H "X-API-Key: $HADLINK_KEY" \
            -d "url=$BUILD_URL" | jq -r '.short')
          echo "Build results: $SHORT"
```

### GitLab CI Example

```yaml
build:
  script:
    - make build
  after_script:
    - |
      SHORT=$(curl -s "$HADLINK_API" \
        -H "X-API-Key: $HADLINK_KEY" \
        -d "url=$CI_PIPELINE_URL" | jq -r '.short')
      echo "Pipeline: $SHORT"
```

---

## Configuration

### Example Configuration File

See [`config.example.yaml`](config.example.yaml) for a complete configuration template.

Copy and customize:
```bash
cp docs/examples/config.example.yaml config.yaml
# Edit config.yaml with your settings
```

### Minimal Configuration

```yaml
secret: "YOUR_SECRET_HERE"  # Required - generate with: openssl rand -hex 16
pow_difficulty: 0
rate_limit:
  per_ip: 10
  window: 60
storage:
  type: sqlite
  path: /var/lib/hadlink/hadlink.db
server:
  mode: shorten
  port: 8443
  bind: "127.0.0.1"
  trust_proxy: false  # Set true only behind trusted reverse proxy
```

### Behind a Reverse Proxy

When running behind nginx, Caddy, or another reverse proxy, enable X-Forwarded-For trust:

```bash
# Environment variable
export HADLINK_TRUST_PROXY=true
```

**Important**: Only enable this when:
- hadlink is behind a trusted reverse proxy (nginx, Caddy, etc.)
- The proxy correctly sets the X-Forwarded-For header
- Direct access to hadlink is blocked (only proxy can reach it)

When disabled (default), rate limiting uses the direct socket address, which would be the proxy's IP if behind a reverse proxy.

---

## Monitoring Integration

### Grafana Alert Example

```yaml
# Grafana alert annotation
message: |
  Disk usage critical on {{ $labels.instance }}
  Details: {{ $labels.short_url }}
```

Where `short_url` is pre-generated for each dashboard panel.

### Uptime Kuma Integration

Use hadlink to create stable links for incident response:

```bash
# In alert webhook
INCIDENT_URL="https://status.example.com/incident/123"
SHORT=$(curl -s "$HADLINK_API" \
  -H "X-API-Key: monitoring" \
  -d "url=$INCIDENT_URL" | jq -r '.short')

# Include $SHORT in SMS/email notifications
```

---

## QR Code Generation

Create QR codes for physical infrastructure:

```bash
#!/bin/bash
# Generate QR code for router admin page

ADMIN_URL="http://192.168.1.1"
SHORT=$(curl -s "$HADLINK_API" \
  -H "X-API-Key: infra" \
  -d "url=$ADMIN_URL" | jq -r '.short')

# Generate QR code
qrencode -o router-admin.png "$SHORT"

# Print and affix to device
```

---

## Testing the API

See the [API Specification](../API.md) for complete endpoint documentation.

### Create a short link

```bash
curl -X POST http://localhost:8443/api/create \
  -H "X-API-Key: test" \
  -d "url=https://example.com/very/long/path"
```

Response:
```json
{
  "short": "http://localhost:8080/8F3kP2Q"
}
```

### Resolve a short link

```bash
curl -I http://localhost:8080/8F3kP2Q
```

Response:
```
HTTP/1.1 302 Found
Location: https://example.com/very/long/path
```

---

## Advanced Usage

### Namespace Separation

Use different API keys for different services:

```yaml
api_keys:
  - name: ci
    key: "ci-secret-key"
  - name: monitoring
    key: "monitoring-secret-key"
  - name: infra
    key: "infra-secret-key"
```

Then track usage by key in logs.

### Rate Limit Tuning

For high-volume automated systems:

```yaml
rate_limit:
  per_ip: 100
  window: 60
  per_subnet: 500
```

Or disable for authenticated keys in code.
