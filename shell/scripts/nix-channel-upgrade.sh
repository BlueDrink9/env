#!/usr/bin/env bash

set -euo pipefail

if [ $# -ne 2 ]; then
  echo "Usage: $0 <old_version> <new_version>"
  echo "Example: $0 24.11 25.05"
  exit 1
fi

OLD_VERSION="$1"
NEW_VERSION="$2"

update_channels() {
  local prefix="$1"
  local sudo_cmd="$2"

  echo "Listing Nix channels for ${prefix}user..."
  $sudo_cmd nix-channel --list | while read -r name url; do
    if [[ "$url" == *"$OLD_VERSION"* ]]; then
      echo "Updating channel '$name' from version $OLD_VERSION to $NEW_VERSION"

      # Build new URL
      new_url="${url/$OLD_VERSION/$NEW_VERSION}"

      # Remove and re-add
      $sudo_cmd nix-channel --remove "$name"
      $sudo_cmd nix-channel --add "$new_url" "$name"
    else
      echo "Skipping channel '$name' (does not match version $OLD_VERSION)"
    fi
  done

  echo "Refreshing channels for ${prefix}user..."
  $sudo_cmd nix-channel --update
}

# First, update user channels
update_channels "$USER" ""

# Then, update root (sudo) channels
if command -v sudo >/dev/null 2>&1; then
  echo
  echo "Now updating channels for root using sudo..."
  update_channels "root " "sudo"
else
  echo "sudo not available, skipping root channel updates."
fi
