#!/bin/bash
set -e

echo "==> Building fullLinkJS..."
sbt engineJS/fullLinkJS

echo "==> Running tests..."
node --experimental-strip-types --test ts-test/petradb.test.ts

echo "==> Copying artifacts to npm/..."
cp engine/js/target/scala-3.8.1/petradb-engine-opt/main.js npm/main.js

echo "==> Package contents:"
cd npm
npm pack --dry-run

if [ "$1" = "--publish" ]; then
  echo "==> Publishing to npm..."
  npm publish --access public
else
  echo ""
  echo "Dry run complete. Run with --publish to publish to npm."
fi
