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
    SRC="client/js/target/scala-${SCALA_VERSION}/petradb-client-opt/client.js"
    DEST="client/npm/client.js"
    PKG_DIR="client/npm"
    ;;
  cli)
    MODULE=cliJS
    SRC_DIR="cli/js/target/scala-${SCALA_VERSION}/petradb-cli-fastopt"
    DEST_DIR="cli/npm/bin"
    PKG_DIR="cli/npm"
    MULTI=true
    LINK_TASK=fastLinkJS
    ;;
  server)
    MODULE=serverJS
    SRC="server/js/target/scala-${SCALA_VERSION}/petradb-server-opt/main.js"
    DEST="server/npm/bin/main.js"
    PKG_DIR="server/npm"
    ;;
  knex)
    PKG_DIR="knex"
    TS_ONLY=true
    ;;
  lucid)
    PKG_DIR="lucid"
    TS_ONLY=true
    ;;
  drizzle)
    PKG_DIR="drizzle"
    TS_ONLY=true
    ;;
  *)
    echo "Usage: ./publish.sh <engine|client|cli|server|knex|lucid|drizzle> [--publish]"
    exit 1
    ;;
esac

if [ "$TS_ONLY" = "true" ]; then
  echo "==> Building TypeScript..."
  cd "$PKG_DIR"
  npx tsc
else
  LINK_TASK="${LINK_TASK:-fullLinkJS}"
  echo "==> Building ${MODULE}/${LINK_TASK}..."
  sbt ${MODULE}/${LINK_TASK}

  echo "==> Copying artifact..."
  if [ "$MULTI" = "true" ]; then
    cp "$SRC_DIR"/*.js "$DEST_DIR/"
  else
    cp "$SRC" "$DEST"
  fi

  cd "$PKG_DIR"
fi

echo "==> Package contents:"
npm pack --dry-run

if [ "$2" = "--publish" ]; then
  echo "==> Publishing to npm..."
  npm publish --access public
else
  echo ""
  echo "Dry run complete. Run with --publish to publish to npm."
fi
