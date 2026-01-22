#!/bin/bash
# Generate secret key for deployment

exec >&2

SECRET_FILE="deploy/docker/secret.key"

if [ -f "$SECRET_FILE" ]; then
    echo "Secret key already exists at $SECRET_FILE"
    echo "Remove it first if you want to regenerate"
    exit 1
fi

mkdir -p "$(dirname "$SECRET_FILE")"
openssl rand -base64 32 > "$SECRET_FILE"
chmod 600 "$SECRET_FILE"

echo "Generated secret key: $SECRET_FILE"
echo "Keep this file secure and never commit it to version control"

touch "$3"
