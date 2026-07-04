#!/usr/bin/env bash
#
# install.sh — build/install the c-flat compiler and stage its std lib.
#
#   ~/.cargo/bin/c-flat            (binary)
#   ~/.cargo/lib/c-flat/std/       (std lib source)
#
# Usage:
#   ./install.sh                      # plain install, no extra features
#   ./install.sh --wasm-runtime        # install with wasm-runtime feature
#   ./install.sh --debug               # install with debug feature
#   ./install.sh --wasm-runtime --debug
#   ./install.sh --features wasm-runtime,debug   # equivalent, explicit form
#
# Env overrides:
#   CARGO_INSTALL_ROOT   Same var cargo itself honors for install location.
#                         Defaults to ~/.cargo (cargo's own default), so
#                         binaries land in $CARGO_INSTALL_ROOT/bin.
#   STD_SRC_DIR           Path to std sources to copy. Defaults to ./std
#                         relative to this script.

set -euo pipefail

SCRIPT_DIR="$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" &>/dev/null && pwd)"
STD_SRC_DIR="${STD_SRC_DIR:-"$SCRIPT_DIR/std"}"

CARGO_INSTALL_ROOT="${CARGO_INSTALL_ROOT:-"$HOME/.cargo"}"
STD_DEST_DIR="$CARGO_INSTALL_ROOT/lib/c-flat/std"

VALID_FEATURES=("wasm-runtime" "debug")
features=()
skip_install_std=1

usage() {
    echo "Usage: $0 [--wasm-runtime] [--debug] [--features f1,f2,...]"
    echo
    echo "Valid features: ${VALID_FEATURES[*]}"
    exit 1
}

is_valid_feature() {
    local f="$1"
    for v in "${VALID_FEATURES[@]}"; do
        [[ "$f" == "$v" ]] && return 0
    done
    return 1
}

while [[ $# -gt 0 ]]; do
    case "$1" in
        --wasm-runtime)
            features+=("wasm-runtime")
            shift
            ;;
        --debug)
            features+=("debug")
            shift
            ;;
        --features)
            [[ $# -ge 2 ]] || { echo "error: --features requires an argument" >&2; usage; }
            IFS=',' read -ra parsed <<< "$2"
            for f in "${parsed[@]}"; do
                f="$(echo -n "$f" | xargs)" # trim whitespace
                [[ -z "$f" ]] && continue
                if ! is_valid_feature "$f"; then
                    echo "error: unknown feature '$f' (valid: ${VALID_FEATURES[*]})" >&2
                    exit 1
                fi
                features+=("$f")
            done
            shift 2
            ;;
        -h|--help)
            usage
            ;;
        --skip-install-std)
            skip_install_std=0
            shift
            ;;
        *)
            echo "error: unknown argument '$1'" >&2
            usage
            ;;
    esac
done

# De-duplicate features while preserving order.
if [[ ${#features[@]} -gt 0 ]]; then
    declare -A seen=()
    deduped=()
    for f in "${features[@]}"; do
        if [[ -z "${seen[$f]:-}" ]]; then
            seen["$f"]=1
            deduped+=("$f")
        fi
    done
    features=("${deduped[@]}")
fi

if [[ ! -d "$STD_SRC_DIR" ]]; then
    echo "error: std source dir not found at '$STD_SRC_DIR'" >&2
    echo "       set STD_SRC_DIR to override" >&2
    exit 1
fi

if [[ $skip_install_std -eq 1 ]]; then
    echo "==> Staging std lib"
    echo "    from: $STD_SRC_DIR"
    echo "    to:   $STD_DEST_DIR"
    mkdir -p "$(dirname -- "$STD_DEST_DIR")"
    rm -rf "$STD_DEST_DIR"
    cp -R "$STD_SRC_DIR" "$STD_DEST_DIR"
fi

echo "==> Running cargo install"
cargo_args=(install --path "$SCRIPT_DIR")

if [[ ${#features[@]} -gt 0 ]]; then
    feature_str="$(IFS=,; echo "${features[*]}")"
    echo "    features: $feature_str"
    cargo_args+=(--features "$feature_str")
else
    echo "    features: (none)"
fi

cargo "${cargo_args[@]}"

echo "==> Done"
echo "    std lib installed at: $STD_DEST_DIR"
