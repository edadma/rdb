#!/bin/bash
set -e

SCALA_VERSION="3.8.2"

case "$1" in
  engine)
    MODULE=engineJS
    SRC="engine/js/target/scala-${SCALA_VERSION}/petradb-engine-opt/main.js"
    DEST="engine/npm/main.js"
    PKG_DIR="engine/npm"
    ;;
  client)
    MODULE=clientJS
    SRC="client/js/target/scala-${SCALA_VERSION}/petradb-client-opt/main.js"
    DEST="client/npm/main.js"
    PKG_DIR="client/npm"
    ;;
  cli)
    MODULE=cliJS
    SRC_DIR="cli/js/target/scala-${SCALA_VERSION}/petradb-cli-opt"
    DEST_DIR="cli/npm/bin"
    PKG_DIR="cli/npm"
    MULTI=true
    ;;
  server)
    MODULE=serverJS
    SRC="server/js/target/scala-${SCALA_VERSION}/petradb-server-opt/main.js"
    DEST="server/npm/bin/main.js"
    PKG_DIR="server/npm"
    ;;
  *)
    echo "Usage: ./publish.sh <engine|client|cli|server> [--publish]"
    exit 1
    ;;
esac

echo "==> Building ${MODULE}/fullLinkJS..."
sbt ${MODULE}/fullLinkJS

echo "==> Copying artifact..."
if [ "$MULTI" = "true" ]; then
  cp "$SRC_DIR"/*.js "$DEST_DIR/"
else
  cp "$SRC" "$DEST"
fi

echo "==> Package contents:"
cd "$PKG_DIR"
npm pack --dry-run

if [ "$2" = "--publish" ]; then
  echo "==> Publishing to npm..."
  npm publish --access public
else
  echo ""
  echo "Dry run complete. Run with --publish to publish to npm."
fi
