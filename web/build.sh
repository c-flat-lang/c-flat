#!/usr/bin/env bash
# Build a self-contained native-raylib web project from a c-flat source file.
#
#   web/build.sh <path/to/file.cb>
#
# The program must be a normal raylib program: a main() with a
# `while (!window_should_close()) { begin_drawing(); ...; end_drawing(); }` loop.
# It is compiled to wasm, asyncified so end_drawing() yields to the browser each
# frame, and paired with the REAL raylib compiled to wasm (the emscripten host
# module). Output: ./bin/web/<name>/ containing
#   index.html               - the harness/driver
#   raylib_host_bridge.js    - forwards the c-flat `core` imports to real raylib
#   raylib_host.js / .wasm    - real raylib (emscripten)
#   <name>.wasm              - the compiled c-flat program (asyncified)
#
# Prereqs (one-time):
#   scripts/setup-web-toolchain.sh          # emsdk + raylib-web
#   source web/vendor/emsdk/emsdk_env.sh
#   web/build-raylib-host.sh                # -> web/raylib_host.{js,wasm}
set -euo pipefail

SCRIPT_DIR="$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" &>/dev/null && pwd)"
ROOT="$(cd -- "$SCRIPT_DIR/.." &>/dev/null && pwd)"

if [[ $# -lt 1 ]]; then
  echo "usage: $0 <path/to/file.cb>" >&2
  exit 1
fi

SRC="$1"
if [[ ! -f "$SRC" ]]; then
  echo "error: no such file: $SRC" >&2
  exit 1
fi

CFLAT="${CFLAT:-$ROOT/target/debug/cflat}"
export CFLAT_STD_PATH="${CFLAT_STD_PATH:-$ROOT/std}"

command -v wasm-opt >/dev/null 2>&1 || {
  echo "error: wasm-opt not found (brew install binaryen)" >&2
  exit 1
}

# The real raylib host module must have been built.
if [[ ! -f "$SCRIPT_DIR/raylib_host.js" || ! -f "$SCRIPT_DIR/raylib_host.wasm" ]]; then
  echo "error: raylib host module not built. Do this first:" >&2
  echo "         scripts/setup-web-toolchain.sh    # one-time: emsdk + raylib-web" >&2
  echo "         source web/vendor/emsdk/emsdk_env.sh" >&2
  echo "         web/build-raylib-host.sh" >&2
  exit 1
fi

name="$(basename "${SRC%.cb}")"
outdir="$ROOT/bin/web/$name"
raw_wasm="$ROOT/bin/$name.wasm"

# 1. Compile c-flat -> wasm.
"$CFLAT" --target=wasm32 "$SRC"

# 2. Require a main() entry (the blocking-loop model).
if ! wasm-opt "$raw_wasm" --print 2>/dev/null | grep -q '(export "main"'; then
  echo "error: a web program must export main() — a main() with a" >&2
  echo "       while (!window_should_close()) { ...begin/end_drawing... } loop." >&2
  exit 1
fi

# 3. Stage the output directory fresh.
rm -rf "$outdir"
mkdir -p "$outdir"

# 4. Asyncify the program (so end_drawing() yields each frame) and assemble the
#    project against the real raylib host module.
wasm-opt "$raw_wasm" \
  --asyncify \
  --pass-arg=asyncify-imports@core.EndDrawing \
  -o "$outdir/$name.wasm"

sed "s/__WASM_FILE__/$name.wasm/g" "$SCRIPT_DIR/templates/index.html" >"$outdir/index.html"
cp "$SCRIPT_DIR/raylib_host_bridge.js" "$outdir/raylib_host_bridge.js"
cp "$SCRIPT_DIR/raylib_host.js" "$outdir/raylib_host.js"
cp "$SCRIPT_DIR/raylib_host.wasm" "$outdir/raylib_host.wasm"

# 5. Assets: if a sibling "<name>.assets/" dir exists, copy it to the served
#    assets/ folder and emit assets.json (the manifest the harness fetches, then
#    writes each file into emscripten's FS before the program runs).
assets_src="${SRC%.cb}.assets"
if [[ -d "$assets_src" ]]; then
  mkdir -p "$outdir/assets"
  cp -R "$assets_src"/. "$outdir/assets/"
  # Manifest = paths relative to the assets dir (recurses into subfolders).
  ( cd "$assets_src" && find . -type f | sed 's#^\./##' ) \
    | sort \
    | awk 'BEGIN{printf "["} {printf "%s\"%s\"", (NR>1?",":""), $0} END{print "]"}' \
    >"$outdir/assets.json"
  echo "Built (native raylib): $outdir"
  echo "  index.html  raylib_host_bridge.js  raylib_host.{js,wasm}  $name.wasm  assets/ (assets.json)"
else
  echo "Built (native raylib): $outdir"
  echo "  index.html  raylib_host_bridge.js  raylib_host.{js,wasm}  $name.wasm"
fi
echo
echo "Serve with:  python3 -m http.server -d \"$outdir\" 8000"
echo "Then open:   http://localhost:8000/"
