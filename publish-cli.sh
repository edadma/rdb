#!/bin/bash
set -e

echo "==> Building cliJS/fullLinkJS..."
sbt cliJS/fullLinkJS

echo "==> Copying artifact to npm-cli/bin/..."
cp cli/js/target/scala-3.8.1/petradb-cli-opt/main.js npm-cli/bin/main.js

echo "==> Package contents:"
cd npm-cli
npm pack --dry-run

if [ "$1" = "--publish" ]; then
  echo "==> Publishing to npm..."
  npm publish --access public
else
  echo ""
  echo "Dry run complete. Run with --publish to publish to npm."
fi
