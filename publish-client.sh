#!/bin/bash
set -e

echo "==> Building client..."
cd "$(dirname "$0")/client"
npm run build

echo "==> Package contents:"
npm pack --dry-run

if [ "$1" = "--publish" ]; then
  echo "==> Publishing to npm..."
  npm publish --access public
else
  echo ""
  echo "Dry run complete. Run with --publish to publish to npm."
fi
