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
./deploy/deploy.sh docker start --generate-secret

# With proof-of-work enabled
./deploy/deploy.sh docker start --generate-secret --pow-difficulty 8

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

## Development with Docker

The default Docker configuration runs hadlink in production mode without access to source code. For development, you can bind-mount the source directory to enable code editing and rebuilding inside the container.

### Quick Debugging Shell

For debugging without source access:

```bash
./deploy/deploy.sh docker shell
```

This opens a shell in the container for inspecting logs, database, and runtime state. Changes do not persist.

### Development Setup with Bind Mount

To enable persistent development inside Docker:

1. Create a `docker-compose.override.yml` in `deploy/docker/`:

```yaml
services:
  shorten:
    volumes:
      - ../..:/hadlink:rw
    # Use builder image for development tools
    image: hadlink:builder
```

2. Build the builder image (includes Alire, Stack, redo):

```bash
cd deploy/docker
docker build --target builder -t hadlink:builder -f Dockerfile ../..
```

3. Start with the override:

```bash
docker compose up -d
./deploy/deploy.sh docker shell
```

4. Inside the shell, you can now edit and rebuild:

```bash
redo all          # Rebuild everything
redo test         # Run tests
redo prove        # Run SPARK proofs
redo style        # Check code style
```

Changes to source files persist on the host. After rebuilding, restart the service:

```bash
exit
./deploy/deploy.sh docker restart
```

**Note:** The bind mount gives the container write access to your source code. This is appropriate for development but should not be used in production.

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

### Service Won't Start

**"HADLINK_SECRET environment variable is not set"**
```bash
# Generate and set a secret key
export HADLINK_SECRET=$(openssl rand -hex 16)
```

**"HADLINK_SECRET is set to the insecure default value"**
```bash
# You must use a unique secret, not the example value
export HADLINK_SECRET=$(openssl rand -hex 16)
```

### Connection Refused

Check that the daemon is running:
```bash
systemctl status hadlink-shorten
```

### Rate Limited

Check your API key and rate limit configuration.

If behind a reverse proxy, ensure `HADLINK_TRUST_PROXY=true` is set, otherwise all requests appear to come from the proxy's IP.

### Invalid URL

Ensure URLs use http:// or https:// scheme and contain no credentials.

Private/internal addresses are rejected:
- IPv4: 10.x.x.x, 172.16-31.x.x, 192.168.x.x, 127.x.x.x, 169.254.x.x, 0.0.0.0
- IPv6: ::1, ::, fe80::, fc00::, fd00::, ::ffff:private
