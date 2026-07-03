export function makeRaylib(getMemory, canvas) {
  const ctx = canvas.getContext("2d");

  const u8 = () => new Uint8Array(getMemory().buffer);
  const dv = () => new DataView(getMemory().buffer);

  function readColor(ptr) {
    const m = u8();
    return `rgba(${m[ptr]}, ${m[ptr + 1]}, ${m[ptr + 2]}, ${m[ptr + 3] / 255})`;
  }

  function readRect(ptr) {
    const d = dv();
    return {
      x: d.getFloat32(ptr, true),
      y: d.getFloat32(ptr + 4, true),
      w: d.getFloat32(ptr + 8, true),
      h: d.getFloat32(ptr + 12, true),
    };
  }

  function readCStr(ptr) {
    const m = u8();
    let end = ptr;
    while (end < m.length && m[end] !== 0) end++;
    return new TextDecoder().decode(m.subarray(ptr, end));
  }

  const pressedThisFrame = new Set();
  window.addEventListener("keydown", (e) => {
    if (!e.repeat) pressedThisFrame.add(e.keyCode);
  });

  let lastTime = performance.now();
  let frameDelta = 0;

  function beginFrame(now) {
    frameDelta = (now - lastTime) / 1000;
    lastTime = now;
  }
  function endFrame() {
    pressedThisFrame.clear();
  }

  const core = {
    InitWindow: (w, h, titlePtr) => {
      canvas.width = w;
      canvas.height = h;
      try {
        document.title = readCStr(titlePtr);
      } catch (_) {}
    },
    CloseWindow: () => {},
    SetTargetFPS: (_fps) => {},
    WindowShouldClose: () => 0,

    BeginDrawing: () => {},
    EndDrawing: () => {},
    ClearBackground: (colorPtr) => {
      ctx.fillStyle = readColor(colorPtr);
      ctx.fillRect(0, 0, canvas.width, canvas.height);
    },
    DrawRectangle: (x, y, w, h, colorPtr) => {
      ctx.fillStyle = readColor(colorPtr);
      ctx.fillRect(x, y, w, h);
    },
    DrawText: (textPtr, x, y, fontSize, colorPtr) => {
      ctx.fillStyle = readColor(colorPtr);
      ctx.font = `${fontSize}px monospace`;
      ctx.textBaseline = "top";
      ctx.fillText(readCStr(textPtr), x, y);
    },

    IsKeyPressed: (key) => (pressedThisFrame.has(key) ? 1 : 0),
    IsGamepadButtonPressed: (_pad, _btn) => 0,
    GetFrameTime: () => frameDelta,
    CheckCollisionRecs: (aPtr, bPtr) => {
      const a = readRect(aPtr);
      const b = readRect(bPtr);
      const hit =
        a.x < b.x + b.w &&
        a.x + a.w > b.x &&
        a.y < b.y + b.h &&
        a.y + a.h > b.y;
      return hit ? 1 : 0;
    },

    write_char: (c) => {
      const s = String.fromCharCode(c);
      globalThis.__cflat_log?.(s);
    },
    write_int: (n) => globalThis.__cflat_log?.(String(n)),
    write: (ptr, len) => {
      const s = new TextDecoder().decode(u8().subarray(ptr, ptr + len));
      globalThis.__cflat_log?.(s);
      return 0;
    },
  };

  return { core, beginFrame, endFrame };
}
