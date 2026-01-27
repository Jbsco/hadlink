#!/bin/bash
# hadlink deployment script
# Usage: ./deploy.sh <docker|systemd> <action> [options]

set -e

# Default configuration
DEPLOY_METHOD=""
ACTION=""
POW_DIFFICULTY=0
POW_DIFFICULTY_AUTH=0
RATE_LIMIT=10
SHORTEN_PORT=8443
REDIRECT_PORT=8080
SECRET_FILE=""
GENERATE_SECRET=false
DATA_DIR=""
INSTALL_DIR="/usr/local"
REMOVE_DATA=false
FORCE=false

# Load existing .env if present (for Docker restarts to retain settings)
SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
if [ -f "$SCRIPT_DIR/docker/.env" ]; then
    source "$SCRIPT_DIR/docker/.env" 2>/dev/null || true
fi

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

usage() {
    cat << EOF
Usage: $(basename "$0") <docker|systemd> <action> [options]

Docker Actions:
  start       Start hadlink containers (default if no action specified)
  stop        Stop hadlink containers
  remove      Remove hadlink containers and networks (add --remove-data for volumes)

Systemd Actions:
  start       Install and start hadlink services (default if no action specified)
  stop        Stop hadlink services
  update      Reload configuration and restart services
  uninstall   Remove hadlink services (add --remove-data to also remove database)

Options:
  --pow-difficulty <N>      Proof-of-work difficulty for anonymous requests (default: 0 = disabled)
  --pow-difficulty-auth <N> Proof-of-work difficulty for authenticated requests (default: 0 = bypass)
  --rate-limit <N>          Rate limit per IP per minute (default: 10)
  --shorten-port <PORT>     Port for shorten service (default: 8443)
  --redirect-port <PORT>    Port for redirect service (default: 8080)
  --secret-file <PATH>      Path to secret key file
  --generate-secret         Generate a new secret key
  --data-dir <PATH>         Data directory (default: ./data for docker, /var/lib/hadlink for systemd)
  --install-dir <PATH>      Binary installation directory (systemd only, default: /usr/local)
  --remove-data             Also remove database/volumes (for remove/uninstall actions)
  --force                   Skip confirmation prompts
  -h, --help                Show this help message

Examples:
  # Docker
  ./deploy.sh docker start --generate-secret
  ./deploy.sh docker stop
  ./deploy.sh docker remove
  ./deploy.sh docker remove --remove-data    # Also removes volumes

  # Systemd
  sudo ./deploy.sh systemd start --generate-secret
  sudo ./deploy.sh systemd stop
  sudo ./deploy.sh systemd update
  sudo ./deploy.sh systemd uninstall
  sudo ./deploy.sh systemd uninstall --remove-data  # Also removes database

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

confirm() {
    if [ "$FORCE" = true ]; then
        return 0
    fi
    local prompt="$1"
    echo -e "${YELLOW}$prompt${NC}"
    read -r -p "Continue? [y/N] " response
    case "$response" in
        [yY][eE][sS]|[yY])
            return 0
            ;;
        *)
            return 1
            ;;
    esac
}

check_root() {
    if [ "$EUID" -ne 0 ]; then
        log_error "This action requires root privileges"
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

get_compose_cmd() {
    if docker compose version >/dev/null 2>&1; then
        echo "docker compose"
    elif command -v docker-compose >/dev/null 2>&1; then
        echo "docker-compose"
    else
        log_error "Neither 'docker compose' nor 'docker-compose' found"
        exit 1
    fi
}

# =============================================================================
# Docker Functions
# =============================================================================

docker_start() {
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

    COMPOSE_CMD=$(get_compose_cmd)
    log_info "Using compose command: $COMPOSE_CMD"

    # Check if image exists, build if not
    if ! docker image inspect hadlink:latest >/dev/null 2>&1; then
        log_info "Building Docker image..."
        docker build -t hadlink:latest -f Dockerfile ../..
    else
        log_info "Using existing hadlink:latest image"
    fi

    # Write current settings to .env (includes any existing values loaded at startup)
    cat > .env << EOF
# hadlink Docker configuration
# Generated by deploy.sh on $(date)

# Service ports
REDIRECT_PORT=${REDIRECT_PORT}
SHORTEN_PORT=${SHORTEN_PORT}

# Proof-of-work difficulty (0 = disabled)
POW_DIFFICULTY=${POW_DIFFICULTY}
POW_DIFFICULTY_AUTH=${POW_DIFFICULTY_AUTH}

# Rate limit per IP per minute
RATE_LIMIT=${RATE_LIMIT}
EOF
    log_info "Configuration saved to .env"

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

docker_stop() {
    log_info "Stopping Docker containers..."

    SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
    DOCKER_DIR="$SCRIPT_DIR/docker"

    if [ ! -f "$DOCKER_DIR/docker-compose.yml" ]; then
        log_error "docker-compose.yml not found at $DOCKER_DIR"
        exit 1
    fi

    cd "$DOCKER_DIR"
    COMPOSE_CMD=$(get_compose_cmd)

    $COMPOSE_CMD stop

    log_info "Containers stopped"
    echo "To start again: $0 docker start"
}

docker_remove() {
    log_info "Removing Docker deployment..."

    SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
    DOCKER_DIR="$SCRIPT_DIR/docker"

    if [ ! -f "$DOCKER_DIR/docker-compose.yml" ]; then
        log_error "docker-compose.yml not found at $DOCKER_DIR"
        exit 1
    fi

    cd "$DOCKER_DIR"
    COMPOSE_CMD=$(get_compose_cmd)

    echo "This will remove:"
    echo "  - hadlink containers (hadlink-shorten-1, hadlink-redirect-1)"
    echo "  - hadlink networks (hadlink_internal, hadlink_public)"
    if [ "$REMOVE_DATA" = true ]; then
        echo "  - hadlink volume (hadlink_hadlink-data) [DATABASE WILL BE DELETED]"
    fi
    echo ""

    if ! confirm "Remove hadlink Docker deployment?"; then
        log_info "Cancelled"
        exit 0
    fi

    if [ "$REMOVE_DATA" = true ]; then
        $COMPOSE_CMD down -v
        log_info "Containers, networks, and volumes removed"
    else
        $COMPOSE_CMD down
        log_info "Containers and networks removed (volume preserved)"
        echo "To also remove the data volume, use: $0 docker remove --remove-data"
    fi

}

# =============================================================================
# Systemd Functions
# =============================================================================

systemd_start() {
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
        if [ ! -f "/etc/hadlink/secret.key" ] && [ ! -f "/etc/hadlink/secret.conf" ]; then
            log_error "No secret file specified and no existing secret found"
            echo "Use --generate-secret or --secret-file <path>"
            exit 1
        fi
        if [ -f "/etc/hadlink/secret.key" ]; then
            SECRET_FILE="/etc/hadlink/secret.key"
        fi
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
    if [ -f "/etc/hadlink/secret.key" ]; then
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
    fi

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

systemd_stop() {
    check_root

    log_info "Stopping hadlink services..."

    systemctl stop hadlink-redirect hadlink-shorten 2>/dev/null || true

    log_info "Services stopped"
    echo "To start again: sudo $0 systemd start"
}

systemd_update() {
    check_root

    log_info "Updating hadlink services..."

    # Reload systemd in case service files changed
    systemctl daemon-reload

    # Restart services
    systemctl restart hadlink-shorten hadlink-redirect

    sleep 2
    if systemctl is-active --quiet hadlink-redirect && systemctl is-active --quiet hadlink-shorten; then
        log_info "Services updated and running"
    else
        log_error "One or more services failed to restart"
        echo "Check logs with:"
        echo "  journalctl -u hadlink-redirect -n 50"
        echo "  journalctl -u hadlink-shorten -n 50"
        exit 1
    fi
}

systemd_uninstall() {
    check_root

    log_info "Preparing to uninstall hadlink services..."

    # Determine what exists and will be removed
    echo ""
    echo "The following hadlink files will be removed:"
    echo ""

    # Service files
    if [ -f /etc/systemd/system/hadlink-redirect.service ]; then
        echo "  /etc/systemd/system/hadlink-redirect.service"
    fi
    if [ -f /etc/systemd/system/hadlink-shorten.service ]; then
        echo "  /etc/systemd/system/hadlink-shorten.service"
    fi

    # Config directory
    if [ -d /etc/hadlink ]; then
        echo "  /etc/hadlink/ (configuration directory)"
    fi

    # Data directory (only if --remove-data)
    if [ "$REMOVE_DATA" = true ]; then
        if [ -z "$DATA_DIR" ]; then
            DATA_DIR="/var/lib/hadlink"
        fi
        if [ -d "$DATA_DIR" ]; then
            echo "  $DATA_DIR/ (DATABASE WILL BE DELETED)"
        fi
    fi

    echo ""
    echo "The following will NOT be removed (remove manually if desired):"
    echo "  - /usr/local/bin/hadlink (binary)"
    echo "  - /usr/local/lib/libHadlink_Core.so (library)"
    echo "  - hadlink user account"
    if [ "$REMOVE_DATA" != true ]; then
        if [ -z "$DATA_DIR" ]; then
            DATA_DIR="/var/lib/hadlink"
        fi
        echo "  - $DATA_DIR/ (database - use --remove-data to include)"
    fi
    echo ""

    if ! confirm "Proceed with uninstall?"; then
        log_info "Cancelled"
        exit 0
    fi

    # Stop services
    log_info "Stopping services..."
    systemctl stop hadlink-redirect hadlink-shorten 2>/dev/null || true

    # Disable services
    log_info "Disabling services..."
    systemctl disable hadlink-redirect hadlink-shorten 2>/dev/null || true

    # Remove service files (only these specific files)
    log_info "Removing service files..."
    rm -f /etc/systemd/system/hadlink-redirect.service
    rm -f /etc/systemd/system/hadlink-shorten.service

    # Reload systemd
    systemctl daemon-reload

    # Remove config directory
    if [ -d /etc/hadlink ]; then
        log_info "Removing configuration directory..."
        rm -rf /etc/hadlink
    fi

    # Remove data directory if requested
    if [ "$REMOVE_DATA" = true ]; then
        if [ -z "$DATA_DIR" ]; then
            DATA_DIR="/var/lib/hadlink"
        fi
        if [ -d "$DATA_DIR" ]; then
            log_info "Removing data directory..."
            rm -rf "$DATA_DIR"
        fi
    fi

    log_info "Uninstall complete"
    echo ""
    echo "To fully remove hadlink, also run:"
    echo "  sudo rm -f /usr/local/bin/hadlink"
    echo "  sudo rm -f /usr/local/lib/libHadlink_Core.so"
    echo "  sudo ldconfig"
    echo "  sudo userdel hadlink"
    if [ "$REMOVE_DATA" != true ]; then
        echo "  sudo rm -rf /var/lib/hadlink  # (database)"
    fi
}

# =============================================================================
# Main
# =============================================================================

# Parse arguments
if [ $# -eq 0 ]; then
    usage
fi

# Handle help flag before anything else
case "$1" in
    -h|--help)
        usage
        ;;
esac

DEPLOY_METHOD="$1"
shift

# Check for action (second positional argument)
if [ $# -gt 0 ] && [[ ! "$1" =~ ^-- ]]; then
    ACTION="$1"
    shift
fi

# Default action is 'start'
if [ -z "$ACTION" ]; then
    ACTION="start"
fi

# Parse remaining options
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
        --remove-data)
            REMOVE_DATA=true
            shift
            ;;
        --force)
            FORCE=true
            shift
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

# Execute the appropriate function
case "$DEPLOY_METHOD" in
    docker)
        case "$ACTION" in
            start)
                docker_start
                ;;
            stop)
                docker_stop
                ;;
            remove)
                docker_remove
                ;;
            *)
                log_error "Unknown docker action: $ACTION"
                echo "Valid actions: start, stop, remove"
                exit 1
                ;;
        esac
        ;;
    systemd)
        case "$ACTION" in
            start)
                systemd_start
                ;;
            stop)
                systemd_stop
                ;;
            update)
                systemd_update
                ;;
            uninstall)
                systemd_uninstall
                ;;
            *)
                log_error "Unknown systemd action: $ACTION"
                echo "Valid actions: start, stop, update, uninstall"
                exit 1
                ;;
        esac
        ;;
    *)
        log_error "Unknown deployment method: $DEPLOY_METHOD"
        usage
        ;;
esac
