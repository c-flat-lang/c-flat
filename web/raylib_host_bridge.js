// Bridge: satisfy the c-flat wasm module's `core` raylib imports by forwarding
// them to the REAL raylib compiled to wasm via emscripten (raylib_host.js).
//
// c-flat passes structs/strings as pointers into ITS OWN linear memory. This
// bridge reads those bytes out and calls the emscripten module's scalar-arg
// wrappers (see web/raylib_shim.c). Pure scalar/void raylib functions are
// forwarded directly (Module._Name(...)).
//
// Returns { core, ready, module }:
//   core    - the import object for the c-flat module's "core" namespace
//   ready   - promise resolving once the emscripten raylib module is up
//   module  - the emscripten Module (exposed so the driver can wrap EndDrawing
//             to interleave the real GL flush with its frame-yield)
import createRaylibHost from "./raylib_host.js";

export function makeRaylibHost(getCflatMemory, canvas) {
  let module = null;

  // Hand emscripten our canvas so raylib's WebGL renders into it.
  const ready = createRaylibHost({ canvas }).then((m) => {
    module = m;
    return m;
  });

  // --- reads out of the c-flat module's memory ---
  const cfU8 = () => new Uint8Array(getCflatMemory().buffer);
  const cfDV = () => new DataView(getCflatMemory().buffer);

  const readColor = (ptr) => {
    const m = cfU8();
    return [m[ptr], m[ptr + 1], m[ptr + 2], m[ptr + 3]];
  };
  const readRect = (ptr) => {
    const d = cfDV();
    return [
      d.getFloat32(ptr, true),
      d.getFloat32(ptr + 4, true),
      d.getFloat32(ptr + 8, true),
      d.getFloat32(ptr + 12, true),
    ];
  };
  const readCStr = (ptr) => {
    const m = cfU8();
    let end = ptr;
    while (end < m.length && m[end] !== 0) end++;
    return new TextDecoder().decode(m.subarray(ptr, end));
  };

  // Copy a JS string into the emscripten heap; returns a pointer to free later.
  const toHostStr = (str) => {
    const n = module.lengthBytesUTF8(str) + 1;
    const ptr = module._malloc(n);
    module.stringToUTF8(str, ptr, n);
    return ptr;
  };
  const withHostStr = (str, fn) => {
    const ptr = toHostStr(str);
    try {
      return fn(ptr);
    } finally {
      module._free(ptr);
    }
  };

  const core = {
    // Window / lifecycle
    InitWindow: (w, h, titlePtr) =>
      withHostStr(readCStr(titlePtr), (p) => module._cf_init_window(w, h, p)),
    CloseWindow: () => module._CloseWindow(),
    SetTargetFPS: (fps) => module._SetTargetFPS(fps),
    WindowShouldClose: () => module._WindowShouldClose(),

    // Drawing
    BeginDrawing: () => module._BeginDrawing(),
    EndDrawing: () => module._EndDrawing(),
    ClearBackground: (cPtr) => {
      const [r, g, b, a] = readColor(cPtr);
      module._cf_clear_background(r, g, b, a);
    },
    DrawRectangle: (x, y, w, h, cPtr) => {
      const [r, g, b, a] = readColor(cPtr);
      module._cf_draw_rectangle(x, y, w, h, r, g, b, a);
    },
    DrawText: (textPtr, x, y, fontSize, cPtr) => {
      const [r, g, b, a] = readColor(cPtr);
      withHostStr(readCStr(textPtr), (p) =>
        module._cf_draw_text(p, x, y, fontSize, r, g, b, a),
      );
    },

    // Input / timing (raylib handles keycodes + timing internally)
    IsKeyPressed: (key) => module._IsKeyPressed(key),
    IsGamepadButtonPressed: (pad, btn) => module._IsGamepadButtonPressed(pad, btn),
    GetFrameTime: () => module._GetFrameTime(),
    CheckCollisionRecs: (aPtr, bPtr) => {
      const a = readRect(aPtr);
      const b = readRect(bPtr);
      return module._cf_check_collision_recs(a[0], a[1], a[2], a[3], b[0], b[1], b[2], b[3]);
    },

    // Console output (reads from c-flat memory).
    write_char: (c) => globalThis.__cflat_log?.(String.fromCharCode(c)),
    write_int: (n) => globalThis.__cflat_log?.(String(n)),
    write: (ptr, len) => {
      const s = new TextDecoder().decode(cfU8().subarray(ptr, ptr + len));
      globalThis.__cflat_log?.(s);
      return 0;
    },
  };

  return { core, ready, get module() { return module; } };
}
