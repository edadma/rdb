#!/bin/bash
# Build and run PetraDB C API tests
#
# Usage: ./run.sh
#
# Prerequisites: sbt engineNative/nativeLink must have been run first

set -e

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
LIB_DIR="$SCRIPT_DIR/../../native/target/scala-3.8.2"
HEADER_DIR="$SCRIPT_DIR/.."

if [ ! -f "$LIB_DIR/libpetradb-engine.so" ] && [ ! -f "$LIB_DIR/libpetradb-engine.dylib" ]; then
  echo "Shared library not found. Building..."
  cd "$SCRIPT_DIR/../../.."
  sbt engineNative/nativeLink
fi

echo "==> Compiling test_petradb.c"
gcc -o "$SCRIPT_DIR/test_petradb" \
    "$SCRIPT_DIR/test_petradb.c" \
    -I"$HEADER_DIR" \
    -L"$LIB_DIR" \
    -lpetradb-engine \
    -Wl,-rpath,"$LIB_DIR"

echo "==> Running tests"
"$SCRIPT_DIR/test_petradb"
