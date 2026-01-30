# Deployment Guide

Detailed deployment instructions for hadlink. For quick Docker setup, see the main [README](../README.md#deployment).

## Contents

- [Docker Deployment](#docker-deployment)
- [Systemd Deployment](#systemd-deployment)
- [Manual Docker Setup](#manual-docker-setup)
- [Manual Systemd Setup](#manual-systemd-setup)
- [Development with Docker](#development-with-docker)
- [Troubleshooting](#troubleshooting)

---

## Docker Deployment

Using the deployment script (recommended):

```bash
# Docker deployment with default settings
./deploy/deploy.sh docker start --generate-secret

# With proof-of-work enabled
./deploy/deploy.sh docker start --generate-secret --pow-difficulty 8

# Stop/remove Docker deployment
./deploy/deploy.sh docker stop
./deploy/deploy.sh docker remove              # Keeps data volume
./deploy/deploy.sh docker remove --remove-data  # Removes everything

# Show all options
./deploy/deploy.sh --help
```

---

## Systemd Deployment

Using the deployment script:

```bash
# Systemd deployment (requires root)
sudo ./deploy/deploy.sh systemd start --generate-secret

# Manage systemd deployment
sudo ./deploy/deploy.sh systemd stop
sudo ./deploy/deploy.sh systemd update        # Reload config and restart
sudo ./deploy/deploy.sh systemd uninstall     # Removes services, keeps database
sudo ./deploy/deploy.sh systemd uninstall --remove-data  # Removes everything
```

---

## Manual Docker Setup

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

---

## Manual Systemd Setup

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

---

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

---

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
