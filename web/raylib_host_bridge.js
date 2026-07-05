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

export function makeRaylibHost(getCflatExports, canvas) {
  let module = null;

  // Hand emscripten our canvas so raylib's WebGL renders into it.
  const ready = createRaylibHost({ canvas }).then((m) => {
    module = m;
    return m;
  });

  // --- reads out of the c-flat module's memory ---
  const cfMem = () => getCflatExports().memory;
  const cfU8 = () => new Uint8Array(cfMem().buffer);
  const cfDV = () => new DataView(cfMem().buffer);

  const cfAlloc = (n) => {
    const sp = getCflatExports().__stack_pointer;
    const p = (sp.value + 3) & ~3;
    sp.value = p + n;
    return p;
  };

  // Read a Texture2D (id:u32, width,height,mipmaps,format:i32) from c-flat memory.
  const readTexture = (ptr) => {
    const d = cfDV();
    return [
      d.getUint32(ptr, true),
      d.getInt32(ptr + 4, true),
      d.getInt32(ptr + 8, true),
      d.getInt32(ptr + 12, true),
      d.getInt32(ptr + 16, true),
    ];
  };

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

  async function preloadAssets(manifest, baseUrl = "./assets/") {
    if (!manifest || manifest.length === 0) return;
    await ready;
    for (const rel of manifest) {
      const resp = await fetch(baseUrl + rel);
      if (!resp.ok) throw new Error(`asset "${rel}": HTTP ${resp.status}`);
      const bytes = new Uint8Array(await resp.arrayBuffer());
      const fsPath = "/assets/" + rel;
      const dir = fsPath.slice(0, fsPath.lastIndexOf("/"));
      module.FS.mkdirTree(dir);
      module.FS.writeFile(fsPath, bytes);
    }
  }

  const core = {
    // Window / lifecycle
    InitWindow: (w, h, titlePtr) =>
      withHostStr(readCStr(titlePtr), (p) => module._cf_init_window(w, h, p)),
    CloseWindow: () => module._CloseWindow(),
    // Force target FPS 0: otherwise raylib's WaitTime() calls emscripten_sleep(),
    // which fights the c-flat module's Asyncify unwind. rAF paces the frames.
    SetTargetFPS: (_fps) => module._SetTargetFPS(0),
    // Do NOT forward to raylib — its web WindowShouldClose() also calls
    // emscripten_sleep(), corrupting the Asyncify stack between frames. The loop
    // runs until the browser tab is closed.
    WindowShouldClose: () => 0,

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

    IsKeyPressed: (key) => module._IsKeyPressed(key),
    IsKeyDown: (key) => module._IsKeyDown(key),
    IsKeyUp: (key) => module._IsKeyUp(key),
    IsMouseButtonPressed: (mouse) => module._IsMouseButtonPressed(mouse),
    IsMouseButtonDown: (mouse) => module._IsMouseButtonDown(mouse),
    IsMouseButtonReleased: (mouse) => module._IsMouseButtonReleased(mouse),
    IsMouseButtonUp: (mouse) => module._IsMouseButtonUp(mouse),
    IsGamepadButtonPressed: (pad, btn) =>
      module._IsGamepadButtonPressed(pad, btn),
    GetFrameTime: () => module._GetFrameTime(),
    CheckCollisionRecs: (aPtr, bPtr) => {
      const a = readRect(aPtr);
      const b = readRect(bPtr);
      return module._cf_check_collision_recs(
        a[0],
        a[1],
        a[2],
        a[3],
        b[0],
        b[1],
        b[2],
        b[3],
      );
    },

    LoadTexture: (pathPtr) => {
      const path = readCStr(pathPtr);
      const outPtr = module._malloc(20);
      const hostPath = toHostStr(path);
      try {
        module._cf_load_texture(hostPath, outPtr);
        const cfPtr = cfAlloc(20);
        const dv = cfDV();
        for (let k = 0; k < 5; k++) {
          dv.setInt32(
            cfPtr + 4 * k,
            module.getValue(outPtr + 4 * k, "i32"),
            true,
          );
        }
        return cfPtr;
      } finally {
        module._free(hostPath);
        module._free(outPtr);
      }
    },
    UnloadTexture: (texPtr) => {
      const t = readTexture(texPtr);
      module._cf_unload_texture(t[0], t[1], t[2], t[3], t[4]);
    },
    DrawTexture: (texPtr, x, y, colorPtr) => {
      const t = readTexture(texPtr);
      const [r, g, b, a] = readColor(colorPtr);
      module._cf_draw_texture(t[0], t[1], t[2], t[3], t[4], x, y, r, g, b, a);
    },

    // SOUND
    InitAudioDevice: () => module._InitAudioDevice(),
    CloseAudioDevice: () => module._CloseAudioDevice(),
    IsAudioDeviceReady: () => module._IsAudioDeviceReady(),
    LoadSound: (pathPtr) => {
      const path = readCStr(pathPtr);
      const hostPath = toHostStr(path);
      try {
        const h = module._cf_load_sound(hostPath);
        const cfPtr = cfAlloc(4);
        cfDV().setInt32(cfPtr, h, true);
        return cfPtr; // Sound { handle: s32 }
      } finally {
        module._free(hostPath);
      }
    },
    PlaySound: (soundPtr) =>
      module._cf_play_sound(cfDV().getInt32(soundPtr, true)),
    StopSound: (soundPtr) =>
      module._cf_stop_sound(cfDV().getInt32(soundPtr, true)),
    PauseSound: (soundPtr) =>
      module._cf_pause_sound(cfDV().getInt32(soundPtr, true)),
    ResumeSound: (soundPtr) =>
      module._cf_resume_sound(cfDV().getInt32(soundPtr, true)),
    IsSoundPlaying: (soundPtr) =>
      module._cf_is_sound_playing(cfDV().getInt32(soundPtr, true)),
    IsSoundValid: (soundPtr) =>
      module._cf_is_sound_valid(cfDV().getInt32(soundPtr, true)),
    UnloadSound: (soundPtr) =>
      module._cf_unload_sound(cfDV().getInt32(soundPtr, true)),
    SetSoundVolume: (soundPtr, volume) =>
      module._cf_set_sound_volume(cfDV().getInt32(soundPtr, true), volume),
    SetSoundPitch: (soundPtr, pitch) =>
      module._cf_set_sound_pitch(cfDV().getInt32(soundPtr, true), pitch),
    SetSoundPan: (soundPtr, pan) =>
      module._cf_set_sound_pan(cfDV().getInt32(soundPtr, true), pan),

    // Wave
    LoadWave: (sound_name) => module._LoadWave(sound_name),
    LoadWaveFromMemory: (a, b, c) => module._LoadWaveFromMemory(a, b, c),
    IsWaveValid: (wave) => module._IsWaveValid(wave),
    LoadSoundFromWave: (wave) => module._LoadSoundFromWave(wave),
    LoadSoundAlias: (sound) => module._LoadSoundAlias(sound),
    UpdateSound: (sound, sound_ptr, id) =>
      module._UpdateSound(sound, sound_ptr, id),
    UnloadWave: (wave) => module._UnloadWave(wave),
    UnloadSoundAlias: (sound) => module._UnloadSoundAlias(sound),
    ExportWave: (wave, name) => module._ExportWave(wave, name),
    ExportWaveAsCode: (wave, name) => module._ExportWaveAsCode(wave, name),
    WaveCopy: (wave) => module._WaveCopy(wave),
    WaveCrop: (wave, x, y) => module._WaveCrop(wave, x, y),
    WaveFormat: (wave, a, b, c) => module._WaveFormat(wave, a, b, c),
    LoadWaveSamples: (wave) => module._LoadWaveSamples(wave),
    UnloadWaveSamples: (v) => module._UnloadWaveSamples(v),

    write_bool: (n) => globalThis.__cflat_log(n ? "true" : "false"),
    write_u8: (n) => globalThis.__cflat_log(String(n)),
    write_char: (c) => globalThis.__cflat_log(String.fromCharCode(c)),
    write_s32: (n) => globalThis.__cflat_log(String(n)),
    write_u32: (n) => globalThis.__cflat_log(String(n)),
    write_f32: (n) => globalThis.__cflat_log(String(n)),
    write_f64: (n) => globalThis.__cflat_log(String(n)),
    write: (ptr, len) => {
      const s = new TextDecoder().decode(cfU8().subarray(ptr, ptr + len));
      globalThis.__cflat_log(s);
      return 0;
    },
  };

  return {
    core,
    ready,
    preloadAssets,
    get module() {
      return module;
    },
  };
}
