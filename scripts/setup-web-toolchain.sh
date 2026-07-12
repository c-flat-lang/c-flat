#!/usr/bin/env bash
# One-time setup for the real-raylib web toolchain:
#   - emscripten (emsdk)
#   - raylib source, built for the web (PLATFORM=PLATFORM_WEB via emcc)
#
# Everything is vendored under web/vendor/ (gitignored). Large downloads
# (hundreds of MB). Re-running is idempotent-ish: it skips clones that exist.
#
# After this finishes, `source web/vendor/emsdk/emsdk_env.sh` puts `emcc` on
# PATH, and web/build-raylib-host.sh can build the raylib host module.
set -euo pipefail

SCRIPT_DIR="$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" &>/dev/null && pwd)"
ROOT="$(cd -- "$SCRIPT_DIR/.." &>/dev/null && pwd)"
VENDOR="$ROOT/web/vendor"
mkdir -p "$VENDOR"

EMSDK_DIR="$VENDOR/emsdk"
RAYLIB_DIR="$VENDOR/raylib"

echo "==> emscripten (emsdk)"
if [[ ! -d "$EMSDK_DIR" ]]; then
  git clone --depth 1 https://github.com/emscripten-core/emsdk "$EMSDK_DIR"
fi
"$EMSDK_DIR/emsdk" install latest
"$EMSDK_DIR/emsdk" activate latest
# shellcheck disable=SC1091
source "$EMSDK_DIR/emsdk_env.sh"
echo "emcc: $(command -v emcc)"
emcc --version | head -1

echo "==> raylib source"
if [[ ! -d "$RAYLIB_DIR" ]]; then
  git clone --depth 1 https://github.com/raysan5/raylib "$RAYLIB_DIR"
fi

echo "==> building raylib for web (PLATFORM=PLATFORM_WEB)"
# Produces libraylib.web.a (or libraylib.a) in raylib/src.
make -C "$RAYLIB_DIR/src" PLATFORM=PLATFORM_WEB -B

LIB="$(ls "$RAYLIB_DIR"/src/libraylib.web.a "$RAYLIB_DIR"/src/libraylib.a 2>/dev/null | head -1 || true)"
if [[ -z "$LIB" ]]; then
  echo "ERROR: raylib web build did not produce a libraylib.*.a" >&2
  exit 1
fi

echo
echo "Done."
echo "  RAYLIB_SRC = $RAYLIB_DIR/src"
echo "  RAYLIB_LIB = $LIB"
echo
echo "Next:"
echo "  source \"$EMSDK_DIR/emsdk_env.sh\"   # put emcc on PATH in your shell"
echo "  ./web/build-raylib-host.sh          # build the raylib host wasm module"
