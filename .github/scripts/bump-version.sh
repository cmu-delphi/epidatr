#!/usr/bin/env bash
# Bump the Version field in DESCRIPTION and print the new version.
# Usage: bump-version.sh [patch|minor|major] [path/to/DESCRIPTION]
set -euo pipefail

level=${1:-patch}
file=${2:-DESCRIPTION}

current=$(sed -n 's/^Version: //p' "$file")
if [[ ! $current =~ ^[0-9]+\.[0-9]+\.[0-9]+$ ]]; then
  echo "Expected a major.minor.patch Version in $file, got '$current'" >&2
  exit 1
fi
IFS=. read -r major minor patch <<<"$current"
case $level in
  major) major=$((major + 1)); minor=0; patch=0 ;;
  minor) minor=$((minor + 1)); patch=0 ;;
  patch) patch=$((patch + 1)) ;;
  *) echo "Unknown bump level '$level' (expected patch, minor, or major)" >&2; exit 1 ;;
esac

new="$major.$minor.$patch"
# perl -i behaves the same on GNU and BSD/macOS, unlike sed -i.
perl -pi -e "s/^Version: .*/Version: $new/" "$file"
echo "$new"
