#!/usr/bin/env bash
# Build the "raylib host" module: real raylib (compiled for web) + our scalar-arg
# shim, as an ES-module emscripten build. Produces:
#   web/raylib_host.js    (ES module factory: `createRaylibHost`)
#   web/raylib_host.wasm  (real raylib + emscripten runtime)
#
# Prereq: run scripts/setup-web-toolchain.sh once, then have emcc on PATH
#   source web/vendor/emsdk/emsdk_env.sh
set -euo pipefail

SCRIPT_DIR="$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" &>/dev/null && pwd)"
ROOT="$(cd -- "$SCRIPT_DIR/.." &>/dev/null && pwd)"
VENDOR="$ROOT/web/vendor"

# Auto-source emsdk env if emcc isn't already on PATH.
if ! command -v emcc >/dev/null 2>&1; then
  if [[ -f "$VENDOR/emsdk/emsdk_env.sh" ]]; then
    # shellcheck disable=SC1091
    source "$VENDOR/emsdk/emsdk_env.sh"
  fi
fi
command -v emcc >/dev/null 2>&1 || {
  echo "error: emcc not found. Run scripts/setup-web-toolchain.sh, then" >&2
  echo "       source web/vendor/emsdk/emsdk_env.sh" >&2
  exit 1
}

RAYLIB_SRC="${RAYLIB_SRC:-$VENDOR/raylib/src}"
RAYLIB_LIB="${RAYLIB_LIB:-$(ls "$RAYLIB_SRC"/libraylib.web.a "$RAYLIB_SRC"/libraylib.a 2>/dev/null | head -1 || true)}"
[[ -f "$RAYLIB_SRC/raylib.h" ]] || { echo "error: raylib.h not found in $RAYLIB_SRC" >&2; exit 1; }
[[ -n "$RAYLIB_LIB" && -f "$RAYLIB_LIB" ]] || { echo "error: libraylib web .a not found in $RAYLIB_SRC" >&2; exit 1; }

EXPORTS='_cf_init_window,_cf_clear_background,_cf_draw_rectangle,_cf_draw_text,_cf_check_collision_recs,_cf_load_texture,_cf_unload_texture,_cf_draw_texture,_cf_draw_poly,_cf_draw_poly_lines,_cf_draw_poly_lines_ex,_BeginDrawing,_EndDrawing,_CloseWindow,_SetTargetFPS,_WindowShouldClose,_IsKeyPressed,_IsKeyDown,_IsKeyUp,_IsKeyPressed,_IsMouseButtonPressed,_IsMouseButtonDown,_IsMouseButtonReleased,_IsMouseButtonUp,_IsGamepadButtonPressed,_GetFrameTime,_InitAudioDevice,_CloseAudioDevice,_IsAudioDeviceReady,_LoadWave,_LoadWaveFromMemory,_IsWaveValid,_LoadSound,_LoadSoundFromWave,_LoadSoundAlias,_IsSoundValid,_UpdateSound,_UnloadWave,_UnloadSound,_UnloadSoundAlias,_ExportWave,_ExportWaveAsCode,_PlaySound,_StopSound,_PauseSound,_ResumeSound,_IsSoundPlaying,_SetSoundVolume,_SetSoundPitch,_SetSoundPan,_WaveCopy,_WaveCrop,_WaveFormat,_LoadWaveSamples,_UnloadWaveSamples,_cf_load_sound,_cf_play_sound,_cf_stop_sound,_cf_pause_sound,_cf_resume_sound,_cf_is_sound_playing,_cf_set_sound_volume,_cf_set_sound_pitch,_cf_set_sound_pan,_cf_unload_sound,_cf_is_sound_valid,_MeasureText,_GetMouseX,_SetRandomSeed,_GetRandomValue,_GetMouseY,_malloc,_free'

emcc "$SCRIPT_DIR/raylib_shim.c" \
  -I"$RAYLIB_SRC" "$RAYLIB_LIB" \
  -o "$SCRIPT_DIR/raylib_host.js" \
  -sUSE_GLFW=3 \
  -sMODULARIZE=1 -sEXPORT_ES6=1 -sENVIRONMENT=web \
  -sALLOW_MEMORY_GROWTH=0 -sINITIAL_MEMORY=67108864 \
  -sFORCE_FILESYSTEM=1 \
  -sEXPORTED_FUNCTIONS="$EXPORTS" \
  -sEXPORTED_RUNTIME_METHODS=stringToUTF8,lengthBytesUTF8,FS,getValue,setValue \
  -O2

echo "Built $SCRIPT_DIR/raylib_host.js + raylib_host.wasm"
