#!/bin/bash
set -e

echo "==> Building serverJS/fullLinkJS..."
sbt serverJS/fullLinkJS

echo "==> Copying artifact to npm-server/bin/..."
cp server/js/target/scala-3.8.1/petradb-server-opt/main.js npm-server/bin/main.js

echo "==> Package contents:"
cd npm-server
npm pack --dry-run

if [ "$1" = "--publish" ]; then
  echo "==> Publishing to npm..."
  npm publish --access public
else
  echo ""
  echo "Dry run complete. Run with --publish to publish to npm."
fi
