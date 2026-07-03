#!/usr/bin/env bash
# Build a self-contained web project from a c-flat source file.
#
#   web/build.sh <path/to/file.cb>
#
# Produces ./bin/web/<name>/ containing:
#   index.html    - the browser harness
#   raylib.js     - the Canvas-2D raylib shim (copied from web/raylib.js)
#   <name>.wasm   - the compiled c-flat program
#
# The harness model is auto-detected from the wasm exports:
#   - exports init + step  -> frame-callback harness (no asyncify)
#   - exports main         -> blocking-loop harness (wasm-opt --asyncify at
#                             end_drawing, so `while (!window_should_close())`
#                             yields each frame)
#
# Requires: cargo-built ./target/debug/cflat and `wasm-opt` (brew install binaryen).
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

name="$(basename "${SRC%.cb}")"
outdir="$ROOT/bin/web/$name"
raw_wasm="$ROOT/bin/$name.wasm"

# 1. Compile c-flat -> wasm.
"$CFLAT" --target=wasm32 "$SRC"

# 2. Detect the harness model from the module's exports.
exports="$(wasm-opt "$raw_wasm" --print 2>/dev/null | grep -oE '\(export "[^"]+"' || true)"
if grep -q '"init"' <<<"$exports" && grep -q '"step"' <<<"$exports"; then
  model="callback"
elif grep -q '"main"' <<<"$exports"; then
  model="loop"
else
  echo "error: wasm exports neither (init+step) nor main; nothing to drive" >&2
  exit 1
fi

# 3. Stage the output directory fresh.
rm -rf "$outdir"
mkdir -p "$outdir"

# 4. Place the program wasm — asyncified for the blocking-loop model.
if [[ "$model" == "loop" ]]; then
  wasm-opt "$raw_wasm" \
    --asyncify \
    --pass-arg=asyncify-imports@core.EndDrawing \
    -o "$outdir/$name.wasm"
  template="$SCRIPT_DIR/templates/loop.html"
else
  cp "$raw_wasm" "$outdir/$name.wasm"
  template="$SCRIPT_DIR/templates/callback.html"
fi

# 5. Emit index.html (from the template, with the wasm filename filled in) and
#    the raylib shim as raylib.js.
sed "s/__WASM_FILE__/$name.wasm/g" "$template" >"$outdir/index.html"
cp "$SCRIPT_DIR/raylib.js" "$outdir/raylib.js"

echo "Built ($model model): $outdir"
echo "  index.html  raylib.js  $name.wasm"
echo
echo "Serve with:  python3 -m http.server -d \"$outdir\" 8000"
echo "Then open:   http://localhost:8000/"
