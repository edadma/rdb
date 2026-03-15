#!/bin/bash
# Build PetraDB Python package with bundled native library.
#
# Usage: ./build.sh [--publish]
#
# Creates a venv with build tools automatically.

set -e

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
cd "$SCRIPT_DIR"

# Set up venv with build tools
VENV_DIR="$SCRIPT_DIR/.venv"
if [ ! -d "$VENV_DIR" ]; then
  echo "==> Creating build venv..."
  python3 -m venv "$VENV_DIR"
  "$VENV_DIR/bin/pip" install --quiet build twine
fi

# Use venv's python
PYTHON="$VENV_DIR/bin/python3"

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
"$PYTHON" -m build

echo ""
echo "==> Package contents:"
ls -lh dist/

if [ "$1" = "--publish" ]; then
  echo ""
  echo "==> Publishing to PyPI..."
  "$PYTHON" -m twine upload dist/*
else
  echo ""
  echo "Dry run complete. Run with --publish to upload to PyPI."
  echo "  ./build.sh --publish"
fi
