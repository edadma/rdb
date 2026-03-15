#!/bin/bash
# Build PetraDB as a shared C library (.so / .dylib)
#
# Usage: ./build-clib.sh
#
# Prerequisites: sbt, clang, Scala Native toolchain
#
# Output: libpetradb.so (Linux) or libpetradb.dylib (macOS)

set -e

cd "$(dirname "$0")/../.."

echo "==> Compiling engine native with shared library target..."
sbt 'set engineNative / nativeConfig ~= { _.withBuildTarget(scalanative.build.BuildTarget.libraryDynamic) }' \
    'engineNative/nativeLink'

# Find the output
NATIVE_OUT=$(find engine/native/target -name "*.so" -o -name "*.dylib" 2>/dev/null | head -1)

if [ -z "$NATIVE_OUT" ]; then
  echo "ERROR: Could not find shared library output"
  exit 1
fi

echo "==> Built: $NATIVE_OUT"
echo "==> Header: engine/native/petradb.h"
echo ""
echo "To use from C:"
echo "  gcc -o myapp myapp.c -L$(dirname $NATIVE_OUT) -lpetradb"
