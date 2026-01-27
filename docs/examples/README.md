# Examples

Integration examples and configuration templates for hadlink.

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
secret: "CHANGE_ME"
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
```

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

## Home Lab Setup

### Using deploy.sh (Recommended)

```bash
# Docker deployment
./deploy/deploy.sh docker --generate-secret

# With proof-of-work enabled
./deploy/deploy.sh docker --generate-secret --pow-difficulty 8

# Systemd deployment (requires root)
sudo ./deploy/deploy.sh systemd --generate-secret
```

### Manual Docker Setup

```bash
cd deploy/docker

# Generate secret (must be exactly 32 characters)
openssl rand -hex 16 | tr -d '\n' > secret.key
chmod 600 secret.key

# Build image (if not already built)
docker build -t hadlink:latest -f Dockerfile ../..

# Start services
docker compose up -d

# Verify
docker compose ps
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

# Copy service files
sudo cp deploy/systemd/*.service /etc/systemd/system/
sudo mkdir -p /etc/hadlink

# Create user and directories
sudo useradd -r -s /sbin/nologin hadlink
sudo mkdir -p /var/lib/hadlink
sudo chown hadlink:hadlink /var/lib/hadlink

# Create environment file
sudo cp deploy/systemd/hadlink.conf /etc/hadlink/

# Generate secret and create secret.conf (secret must be exactly 32 characters)
SECRET=$(openssl rand -hex 16)
echo "HADLINK_SECRET=${SECRET}" | sudo tee /etc/hadlink/secret.conf > /dev/null
sudo chmod 600 /etc/hadlink/secret.conf

# Start services
sudo systemctl daemon-reload
sudo systemctl enable --now hadlink-shorten hadlink-redirect
```

## Testing the API

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

## Troubleshooting

### Connection Refused

Check that the daemon is running:
```bash
systemctl status hadlink-shorten
```

### Rate Limited

Check your API key and rate limit configuration.

### Invalid URL

Ensure URLs use http:// or https:// scheme and contain no credentials.
