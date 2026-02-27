#!/bin/bash
set -e

echo "==> Building clientJS/fullLinkJS..."
sbt clientJS/fullLinkJS

echo "==> Copying artifact to client/npm/..."
cp client/js/target/scala-3.8.1/petradb-client-opt/main.js client/npm/main.js

echo "==> Package contents:"
cd client/npm
npm pack --dry-run

if [ "$1" = "--publish" ]; then
  echo "==> Publishing to npm..."
  npm publish --access public
else
  echo ""
  echo "Dry run complete. Run with --publish to publish to npm."
fi
