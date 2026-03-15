#!/bin/bash
# Build PetraDB Python package with bundled native library.
#
# Usage: ./build.sh [--publish]
#
# Prerequisites:
#   - sbt engineNative/nativeLink (or pre-built .so)
#   - pip install build twine

set -e

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
cd "$SCRIPT_DIR"

# Find the shared library
SO_PATH="../engine/native/target/scala-3.8.2/libpetradb-engine.so"
DYLIB_PATH="../engine/native/target/scala-3.8.2/libpetradb-engine.dylib"

if [ -f "$SO_PATH" ]; then
  echo "==> Bundling libpetradb-engine.so"
  cp "$SO_PATH" petradb/libpetradb-engine.so
elif [ -f "$DYLIB_PATH" ]; then
  echo "==> Bundling libpetradb-engine.dylib"
  cp "$DYLIB_PATH" petradb/libpetradb-engine.dylib
else
  echo "ERROR: Native library not found. Run 'sbt engineNative/nativeLink' first."
  exit 1
fi

# Clean previous builds
rm -rf dist/ build/ *.egg-info petradb/*.egg-info

echo "==> Building wheel and sdist..."
python3 -m build

echo ""
echo "==> Package contents:"
ls -lh dist/

if [ "$1" = "--publish" ]; then
  echo ""
  echo "==> Publishing to PyPI..."
  python3 -m twine upload dist/*
else
  echo ""
  echo "Dry run complete. Run with --publish to upload to PyPI."
  echo "  ./build.sh --publish"
fi
