import { readFile } from "node:fs/promises";
import { fileURLToPath } from "node:url";
import path from "node:path";

const here = path.dirname(fileURLToPath(import.meta.url));
const wasmPath = process.argv[2] ?? path.join(here, "raylib.wasm");

const DATA_ADDR = 0x30000;
const STACK_SIZE = 0x8000;

let instance = null;
let exports = null;
let sleeping = false;
let frame = null;

const mem = () => new Uint8Array(exports.memory.buffer);
const readColor = (p) => {
  const m = mem();
  return [m[p], m[p + 1], m[p + 2], m[p + 3]];
};

const core = {
  InitWindow: () => {},
  CloseWindow: () => {},
  SetTargetFPS: () => {},
  WindowShouldClose: () => 0,
  BeginDrawing: () => {},
  ClearBackground: (p) => {
    frame.clear = readColor(p);
  },
  DrawRectangle: (x, y, w, h, p) => {
    frame.draw = { x, y, w, h, color: readColor(p) };
  },
  DrawText: () => {},
  IsKeyPressed: () => 0,
  IsGamepadButtonPressed: () => 0,
  GetFrameTime: () => 0.016,
  CheckCollisionRecs: () => 0,
  write_char: () => {},
  write_int: () => {},
  write: () => 0,
  // Yield point, identical logic to loop.html.
  EndDrawing: () => {
    if (!sleeping) {
      exports.asyncify_start_unwind(DATA_ADDR);
      sleeping = true;
    } else {
      exports.asyncify_stop_rewind();
      sleeping = false;
    }
  },
};

const assert = (cond, msg) => {
  if (!cond) {
    console.error("FAIL:", msg);
    process.exit(1);
  }
  console.log("ok  -", msg);
};

const bytes = await readFile(wasmPath);
const { instance: inst } = await WebAssembly.instantiate(bytes, { core });
instance = inst;
exports = instance.exports;

for (const fn of [
  "main",
  "asyncify_start_unwind",
  "asyncify_stop_unwind",
  "asyncify_start_rewind",
  "asyncify_stop_rewind",
  "asyncify_get_state",
]) {
  assert(typeof exports[fn] === "function", `export ${fn} present`);
}

const view = new Int32Array(exports.memory.buffer);
view[DATA_ADDR >> 2] = DATA_ADDR + 8;
view[(DATA_ADDR + 4) >> 2] = DATA_ADDR + 8 + STACK_SIZE;

const N = 6;
const frames = [];

frame = {};
exports.main();
assert(
  exports.asyncify_get_state() === 1,
  "frame 0: main() yielded (unwinding)",
);
exports.asyncify_stop_unwind();
frames.push(frame);

for (let i = 1; i < N; i++) {
  frame = {};
  exports.asyncify_start_rewind(DATA_ADDR);
  exports.main();
  assert(
    exports.asyncify_get_state() === 1,
    `frame ${i}: loop yielded again (still running)`,
  );
  exports.asyncify_stop_unwind();
  frames.push(frame);
}

frames.forEach((f, i) => {
  assert(
    f.clear && f.draw,
    `frame ${i}: ClearBackground + DrawRectangle both fired`,
  );
});

assert(
  JSON.stringify(frames[0].clear) === JSON.stringify([20, 20, 30, 255]),
  `bg color = [20,20,30,255] (got ${frames[0].clear})`,
);
assert(
  JSON.stringify(frames[0].draw.color) === JSON.stringify([230, 80, 60, 255]),
  `fg color = [230,80,60,255] (got ${frames[0].draw.color})`,
);
assert(
  frames[0].draw.w === 32 && frames[0].draw.h === 32,
  "rectangle is 32x32",
);

const pos = frames.map((f) => [f.draw.x, f.draw.y]);
assert(
  pos[0][0] === 36 && pos[0][1] === 35,
  `frame 0 at (36,35) (got ${pos[0]})`,
);
for (let i = 1; i < N; i++) {
  const dx = pos[i][0] - pos[i - 1][0];
  const dy = pos[i][1] - pos[i - 1][1];
  assert(
    dx === 4 && dy === 3,
    `frame ${i} advanced by (4,3) (got (${dx},${dy}))`,
  );
}

console.log("\nAll checks passed. Positions:", JSON.stringify(pos));
