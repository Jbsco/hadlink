#!/bin/bash
# Example CI integration script for hadlink
# This demonstrates how to create short links from CI/CD pipelines

set -euo pipefail

# Configuration
HADLINK_API="${HADLINK_API:-https://hadlink.home/api/create}"
HADLINK_KEY="${HADLINK_KEY:-ci}"

# Create a short link for a build artifact
create_link() {
    local long_url="$1"
    
    local response
    response=$(curl -s -w "\n%{http_code}" \
        -X POST "$HADLINK_API" \
        -H "X-API-Key: $HADLINK_KEY" \
        -d "url=$long_url")
    
    local body=$(echo "$response" | head -n1)
    local status=$(echo "$response" | tail -n1)
    
    if [ "$status" = "200" ]; then
        echo "$body" | jq -r '.short'
    else
        echo "Error creating short link: HTTP $status" >&2
        return 1
    fi
}

# Example usage in CI pipeline
main() {
    local build_url="${BUILD_URL:-http://jenkins.local/job/foo/123/console}"
    
    echo "Creating short link for: $build_url"
    
    local short_link
    if short_link=$(create_link "$build_url"); then
        echo "Build output: $short_link"
        
        # Use in notification
        echo "Build completed successfully"
        echo "View console: $short_link"
    else
        echo "Failed to create short link, using original URL"
        echo "View console: $build_url"
    fi
}

main "$@"
