#!/bin/bash
# hadlink deployment script
# Usage: ./deploy.sh <docker|systemd> [options]

set -e

# Default configuration
DEPLOY_METHOD=""
POW_DIFFICULTY=0
POW_DIFFICULTY_AUTH=0
RATE_LIMIT=10
SHORTEN_PORT=8443
REDIRECT_PORT=8080
SECRET_FILE=""
GENERATE_SECRET=false
DATA_DIR=""
INSTALL_DIR="/usr/local"

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

usage() {
    cat << EOF
Usage: $(basename "$0") <docker|systemd> [options]

Deployment Methods:
  docker      Deploy using Docker and docker-compose
  systemd     Deploy using systemd service units

Options:
  --pow-difficulty <N>      Proof-of-work difficulty for anonymous requests (default: 0 = disabled)
  --pow-difficulty-auth <N> Proof-of-work difficulty for authenticated requests (default: 0 = bypass)
  --rate-limit <N>          Rate limit per IP per minute (default: 10)
  --shorten-port <PORT>     Port for shorten service (default: 8443)
  --redirect-port <PORT>    Port for redirect service (default: 8080)
  --secret-file <PATH>      Path to secret key file (required for systemd, optional for docker)
  --generate-secret         Generate a new secret key
  --data-dir <PATH>         Data directory for database (default: ./data for docker, /var/lib/hadlink for systemd)
  --install-dir <PATH>      Installation directory for binaries (systemd only, default: /usr/local)
  -h, --help                Show this help message

Examples:
  # Docker deployment with default settings
  ./deploy.sh docker --generate-secret

  # Docker with proof-of-work enabled
  ./deploy.sh docker --generate-secret --pow-difficulty 8 --pow-difficulty-auth 2

  # Systemd deployment
  sudo ./deploy.sh systemd --generate-secret --data-dir /var/lib/hadlink

  # Systemd with custom ports and existing secret
  sudo ./deploy.sh systemd --secret-file /etc/hadlink/secret.key --shorten-port 9443

EOF
    exit 1
}

log_info() {
    echo -e "${GREEN}[INFO]${NC} $1"
}

log_warn() {
    echo -e "${YELLOW}[WARN]${NC} $1"
}

log_error() {
    echo -e "${RED}[ERROR]${NC} $1"
}

check_root() {
    if [ "$EUID" -ne 0 ]; then
        log_error "Systemd deployment requires root privileges"
        echo "Please run with: sudo $0 $*"
        exit 1
    fi
}

generate_secret() {
    local secret_path="$1"
    log_info "Generating secret key at $secret_path"
    # Generate exactly 32 hex characters (safe for env vars, no newline)
    openssl rand -hex 16 | tr -d '\n' > "$secret_path"
    chmod 600 "$secret_path"
    log_info "Secret key generated successfully (32 hex chars)"
}

deploy_docker() {
    log_info "Starting Docker deployment..."

    SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
    DOCKER_DIR="$SCRIPT_DIR/docker"

    if [ ! -f "$DOCKER_DIR/docker-compose.yml" ]; then
        log_error "docker-compose.yml not found at $DOCKER_DIR"
        exit 1
    fi

    cd "$DOCKER_DIR"

    # Set default data directory for docker
    if [ -z "$DATA_DIR" ]; then
        DATA_DIR="./data"
    fi

    # Create data directory
    mkdir -p "$DATA_DIR"

    # Handle secret file
    if [ "$GENERATE_SECRET" = true ]; then
        generate_secret "./secret.key"
        SECRET_FILE="./secret.key"
    elif [ -z "$SECRET_FILE" ]; then
        if [ ! -f "./secret.key" ]; then
            log_error "No secret file specified and ./secret.key does not exist"
            echo "Use --generate-secret or --secret-file <path>"
            exit 1
        fi
        SECRET_FILE="./secret.key"
    else
        if [ ! -f "$SECRET_FILE" ]; then
            log_error "Secret file not found: $SECRET_FILE"
            exit 1
        fi
        # Copy secret to docker directory if not already there
        if [ "$SECRET_FILE" != "./secret.key" ]; then
            cp "$SECRET_FILE" "./secret.key"
            chmod 600 "./secret.key"
        fi
    fi

    # Detect docker compose command (v2 uses 'docker compose', v1 uses 'docker-compose')
    if docker compose version >/dev/null 2>&1; then
        COMPOSE_CMD="docker compose"
    elif command -v docker-compose >/dev/null 2>&1; then
        COMPOSE_CMD="docker-compose"
    else
        log_error "Neither 'docker compose' nor 'docker-compose' found"
        exit 1
    fi
    log_info "Using compose command: $COMPOSE_CMD"

    # Check if image exists, build if not
    if ! docker image inspect hadlink:latest >/dev/null 2>&1; then
        log_info "Building Docker image..."
        docker build -t hadlink:latest -f Dockerfile ../..
    else
        log_info "Using existing hadlink:latest image"
    fi

    # Create docker-compose override file with custom settings
    cat > docker-compose.override.yml << EOF
services:
  redirect:
    ports:
      - "${REDIRECT_PORT}:8080"
    environment:
      - HADLINK_PORT=8080

  shorten:
    ports:
      - "127.0.0.1:${SHORTEN_PORT}:8443"
    environment:
      - HADLINK_POW_DIFFICULTY=${POW_DIFFICULTY}
      - HADLINK_POW_DIFFICULTY_AUTH=${POW_DIFFICULTY_AUTH}
      - HADLINK_RATE_LIMIT=${RATE_LIMIT}
      - HADLINK_PORT=8443
EOF

    log_info "Starting containers..."
    $COMPOSE_CMD up -d

    log_info "Docker deployment complete"
    echo ""
    echo "Services:"
    echo "  Redirect: http://localhost:${REDIRECT_PORT}"
    echo "  Shorten:  http://127.0.0.1:${SHORTEN_PORT}"
    echo ""
    echo "Test with:"
    echo "  curl -X POST http://127.0.0.1:${SHORTEN_PORT}/api/create \\"
    echo "    -H 'X-API-Key: test' \\"
    echo "    -d 'url=https://example.com'"
}

deploy_systemd() {
    check_root

    log_info "Starting systemd deployment..."

    SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
    SYSTEMD_DIR="$SCRIPT_DIR/systemd"

    # Set default data directory for systemd
    if [ -z "$DATA_DIR" ]; then
        DATA_DIR="/var/lib/hadlink"
    fi

    # Check for binaries
    if [ ! -f "$INSTALL_DIR/bin/hadlink" ]; then
        log_warn "hadlink binary not found at $INSTALL_DIR/bin/hadlink"
        echo "Please install the binary first:"
        echo "  sudo cp hadlink $INSTALL_DIR/bin/"
        echo "  sudo cp libHadlink_Core.so $INSTALL_DIR/lib/"
        echo "  sudo ldconfig"
        exit 1
    fi

    if [ ! -f "$INSTALL_DIR/lib/libHadlink_Core.so" ]; then
        log_warn "libHadlink_Core.so not found at $INSTALL_DIR/lib/libHadlink_Core.so"
        echo "Please install the library first:"
        echo "  sudo cp libHadlink_Core.so $INSTALL_DIR/lib/"
        echo "  sudo ldconfig"
        exit 1
    fi

    # Create hadlink user if it doesn't exist
    if ! id -u hadlink >/dev/null 2>&1; then
        log_info "Creating hadlink user..."
        useradd -r -s /sbin/nologin hadlink
    fi

    # Create directories
    log_info "Creating directories..."
    mkdir -p "$DATA_DIR"
    mkdir -p /etc/hadlink
    chown hadlink:hadlink "$DATA_DIR"

    # Handle secret file
    if [ "$GENERATE_SECRET" = true ]; then
        generate_secret "/etc/hadlink/secret.key"
        SECRET_FILE="/etc/hadlink/secret.key"
    elif [ -z "$SECRET_FILE" ]; then
        if [ ! -f "/etc/hadlink/secret.key" ]; then
            log_error "No secret file specified and /etc/hadlink/secret.key does not exist"
            echo "Use --generate-secret or --secret-file <path>"
            exit 1
        fi
        SECRET_FILE="/etc/hadlink/secret.key"
    else
        if [ ! -f "$SECRET_FILE" ]; then
            log_error "Secret file not found: $SECRET_FILE"
            exit 1
        fi
        # Copy secret to /etc/hadlink if not already there
        if [ "$SECRET_FILE" != "/etc/hadlink/secret.key" ]; then
            cp "$SECRET_FILE" "/etc/hadlink/secret.key"
            chmod 600 "/etc/hadlink/secret.key"
        fi
    fi

    # Create environment configuration
    log_info "Creating environment configuration..."
    cat > /etc/hadlink/hadlink.conf << EOF
# hadlink environment configuration
# Generated by deploy.sh on $(date)

LD_LIBRARY_PATH=${INSTALL_DIR}/lib
HADLINK_STORAGE=${DATA_DIR}/hadlink.db
EOF

    # Create secret configuration for shorten service
    # Read the secret from the key file and put it in the environment file
    SECRET_VALUE=$(cat /etc/hadlink/secret.key)
    cat > /etc/hadlink/secret.conf << EOF
# hadlink secret configuration
# Generated by deploy.sh on $(date)

HADLINK_SECRET=${SECRET_VALUE}
HADLINK_POW_DIFFICULTY=${POW_DIFFICULTY}
HADLINK_POW_DIFFICULTY_AUTH=${POW_DIFFICULTY_AUTH}
HADLINK_RATE_LIMIT=${RATE_LIMIT}
EOF
    chmod 600 /etc/hadlink/secret.conf
    # Remove the key file as it's now in secret.conf
    rm -f /etc/hadlink/secret.key

    # Install service files with custom ports
    log_info "Installing service files..."

    # Redirect service
    sed -e "s|Environment=HADLINK_PORT=8080|Environment=HADLINK_PORT=${REDIRECT_PORT}|g" \
        -e "s|Environment=HADLINK_STORAGE=.*|Environment=HADLINK_STORAGE=${DATA_DIR}/hadlink.db|g" \
        -e "s|ExecStart=.*|ExecStart=${INSTALL_DIR}/bin/hadlink redirect|g" \
        -e "s|ReadOnlyPaths=.*|ReadOnlyPaths=${DATA_DIR}|g" \
        "$SYSTEMD_DIR/hadlink-redirect.service" > /etc/systemd/system/hadlink-redirect.service

    # Shorten service
    sed -e "s|Environment=HADLINK_PORT=8443|Environment=HADLINK_PORT=${SHORTEN_PORT}|g" \
        -e "s|Environment=HADLINK_STORAGE=.*|Environment=HADLINK_STORAGE=${DATA_DIR}/hadlink.db|g" \
        -e "s|ExecStart=.*|ExecStart=${INSTALL_DIR}/bin/hadlink shorten|g" \
        -e "s|ReadWritePaths=.*|ReadWritePaths=${DATA_DIR}|g" \
        "$SYSTEMD_DIR/hadlink-shorten.service" > /etc/systemd/system/hadlink-shorten.service

    # Reload systemd
    log_info "Reloading systemd..."
    systemctl daemon-reload

    # Enable and start services
    log_info "Enabling and starting services..."
    systemctl enable hadlink-redirect hadlink-shorten
    systemctl restart hadlink-shorten
    systemctl restart hadlink-redirect

    # Check status
    sleep 2
    if systemctl is-active --quiet hadlink-redirect && systemctl is-active --quiet hadlink-shorten; then
        log_info "Systemd deployment complete"
        echo ""
        echo "Services:"
        echo "  Redirect: http://localhost:${REDIRECT_PORT} (systemctl status hadlink-redirect)"
        echo "  Shorten:  http://localhost:${SHORTEN_PORT} (systemctl status hadlink-shorten)"
        echo ""
        echo "Test with:"
        echo "  curl -X POST http://localhost:${SHORTEN_PORT}/api/create \\"
        echo "    -H 'X-API-Key: test' \\"
        echo "    -d 'url=https://example.com'"
    else
        log_error "One or more services failed to start"
        echo "Check logs with:"
        echo "  journalctl -u hadlink-redirect -n 50"
        echo "  journalctl -u hadlink-shorten -n 50"
        exit 1
    fi
}

# Parse arguments
if [ $# -eq 0 ]; then
    usage
fi

# Handle help flag before deployment method
case "$1" in
    -h|--help)
        usage
        ;;
esac

DEPLOY_METHOD="$1"
shift

while [ $# -gt 0 ]; do
    case "$1" in
        --pow-difficulty)
            POW_DIFFICULTY="$2"
            shift 2
            ;;
        --pow-difficulty-auth)
            POW_DIFFICULTY_AUTH="$2"
            shift 2
            ;;
        --rate-limit)
            RATE_LIMIT="$2"
            shift 2
            ;;
        --shorten-port)
            SHORTEN_PORT="$2"
            shift 2
            ;;
        --redirect-port)
            REDIRECT_PORT="$2"
            shift 2
            ;;
        --secret-file)
            SECRET_FILE="$2"
            shift 2
            ;;
        --generate-secret)
            GENERATE_SECRET=true
            shift
            ;;
        --data-dir)
            DATA_DIR="$2"
            shift 2
            ;;
        --install-dir)
            INSTALL_DIR="$2"
            shift 2
            ;;
        -h|--help)
            usage
            ;;
        *)
            log_error "Unknown option: $1"
            usage
            ;;
    esac
done

# Validate deployment method
case "$DEPLOY_METHOD" in
    docker)
        deploy_docker
        ;;
    systemd)
        deploy_systemd
        ;;
    *)
        log_error "Unknown deployment method: $DEPLOY_METHOD"
        usage
        ;;
esac
