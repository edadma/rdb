#!/bin/bash
set -e

echo "==> Building fullOptJS..."
sbt rdbJS/fullOptJS

echo "==> Running tests..."
node --experimental-strip-types --test ts-test/rdb.test.ts

echo "==> Copying artifacts to npm/..."
cp js/target/scala-3.8.1/rdb-opt/main.js npm/main.js
cp shared/src/index.d.ts npm/index.d.ts

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
