# Multi-stage Dockerfile for hadlink
# Stage 1: Build SPARK core and Haskell binary
# Stage 2: Minimal runtime image

# =============================================================================
# Build Stage
# =============================================================================
FROM ubuntu:22.04 AS builder

# Avoid interactive prompts during package installation
ENV DEBIAN_FRONTEND=noninteractive

# Install base dependencies
RUN apt-get update && apt-get install -y --no-install-recommends \
    ca-certificates \
    curl \
    git \
    unzip \
    build-essential \
    libgmp-dev \
    libffi-dev \
    libsqlite3-dev \
    zlib1g-dev \
    libnuma-dev \
    && rm -rf /var/lib/apt/lists/*

# Install Alire (Ada/SPARK package manager)
ARG ALIRE_VERSION=2.0.1
RUN curl -L -o alire.zip \
    "https://github.com/alire-project/alire/releases/download/v${ALIRE_VERSION}/alr-${ALIRE_VERSION}-bin-x86_64-linux.zip" \
    && unzip alire.zip -d /opt \
    && mv /opt/bin/alr /usr/local/bin/alr \
    && rm -rf alire.zip /opt/bin

# Install Stack (Haskell build tool) - direct binary download
ARG STACK_VERSION=3.9.1
RUN curl -L -o stack.tar.gz \
    "https://github.com/commercialhaskell/stack/releases/download/v${STACK_VERSION}/stack-${STACK_VERSION}-linux-x86_64.tar.gz" \
    && tar -xzf stack.tar.gz \
    && mv stack-${STACK_VERSION}-linux-x86_64/stack /usr/local/bin/stack \
    && rm -rf stack.tar.gz stack-${STACK_VERSION}-linux-x86_64 \
    && chmod +x /usr/local/bin/stack

# Install redo (build system)
RUN git clone --depth 1 https://github.com/dinkelk/redo.git /opt/redo \
    && cd /opt/redo && ./do \
    && ln -s /opt/redo/bin/redo /usr/local/bin/redo \
    && ln -s /opt/redo/bin/redo-ifchange /usr/local/bin/redo-ifchange

# Copy source code
WORKDIR /build
COPY . .

# Build SPARK core
WORKDIR /build/spark-core
SHELL ["/bin/bash", "-c"]
RUN alr toolchain --select gnat_native gprbuild \
    && alr build --stop-after=generation || true \
    && source <(alr printenv) \
    && alr build

# Build Haskell
WORKDIR /build/haskell
RUN stack build --system-ghc --no-install-ghc || stack build

# Find and stage the built artifacts
RUN mkdir -p /dist/lib /dist/bin \
    && cp /build/spark-core/lib/libHadlink_Core.so /dist/lib/ \
    && (cp /build/haskell/.stack-work/install/*/bin/hadlink /dist/bin/ 2>/dev/null \
        || find /build/haskell/.stack-work -name hadlink -type f -executable -exec cp {} /dist/bin/ \;)

# =============================================================================
# Runtime Stage
# =============================================================================
FROM debian:bookworm-slim

# Install minimal runtime dependencies
RUN apt-get update && apt-get install -y --no-install-recommends \
    ca-certificates \
    curl \
    libgmp10 \
    libffi8 \
    libsqlite3-0 \
    && rm -rf /var/lib/apt/lists/*

# Create non-root user
RUN useradd -r -s /sbin/nologin -u 1000 hadlink \
    && mkdir -p /data /var/lib/hadlink \
    && chown hadlink:hadlink /data /var/lib/hadlink

# Copy built artifacts
COPY --from=builder /dist/bin/hadlink /usr/local/bin/hadlink
COPY --from=builder /dist/lib/libHadlink_Core.so /usr/local/lib/

# Set library path
ENV LD_LIBRARY_PATH=/usr/local/lib

# Update shared library cache
RUN ldconfig

# Set ownership and permissions
RUN chmod 755 /usr/local/bin/hadlink \
    && chmod 644 /usr/local/lib/libHadlink_Core.so

# Switch to non-root user
USER hadlink

# Default environment variables
ENV HADLINK_STORAGE=/data/hadlink.db
ENV HADLINK_PORT=8080

# Expose both service ports
EXPOSE 8080 8443

# Health check - verify service is responding (any HTTP response is OK)
HEALTHCHECK --interval=30s --timeout=5s --start-period=10s --retries=3 \
    CMD curl -s -o /dev/null http://localhost:${HADLINK_PORT}/ || exit 1

# Entrypoint allows mode selection: "redirect" or "shorten"
ENTRYPOINT ["/usr/local/bin/hadlink"]
CMD ["redirect"]
