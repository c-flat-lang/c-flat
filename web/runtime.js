import { makeRaylibHost } from "./raylib_host_bridge.js";

const ASYNCIFY_EXPORTS = [
  "main",
  "asyncify_start_unwind",
  "asyncify_stop_unwind",
  "asyncify_start_rewind",
  "asyncify_stop_rewind",
  "asyncify_get_state",
];

export async function loadRaylibProgram(
  wasmBytes,
  {
    canvas,
    log,
    shouldClose,
    locateFile,
    keyboardFocusOnly,
    extraImports = {},
    stackSize = 0x8000,
  } = {},
) {
  let instance = null;
  let exports = null;
  let dataAddr = 0;
  let sleeping = false;

  const host = makeRaylibHost(() => instance.exports, {
    canvas,
    log,
    shouldClose,
    locateFile,
    keyboardFocusOnly,
  });
  const { core } = host;

  let windowOpen = false;
  const realInitWindow = core.InitWindow;
  core.InitWindow = (...args) => {
    realInitWindow(...args);
    windowOpen = true;
  };
  const realCloseWindow = core.CloseWindow;
  core.CloseWindow = () => {
    windowOpen = false;
    realCloseWindow();
  };
  const closeWindowIfOpen = () => {
    if (!windowOpen) return;
    try {
      core.CloseWindow();
    } catch {
      windowOpen = false;
    }
  };

  const realEndDrawing = core.EndDrawing;
  core.EndDrawing = () => {
    if (!sleeping) {
      realEndDrawing();
      new Int32Array(exports.memory.buffer)[dataAddr >> 2] = dataAddr + 8;
      exports.asyncify_start_unwind(dataAddr);
      sleeping = true;
    } else {
      exports.asyncify_stop_rewind();
      sleeping = false;
    }
  };

  try {
    await host.ready;
  } catch (e) {
    throw new Error("Failed to init raylib host: " + e);
  }

  try {
    const res = await WebAssembly.instantiate(wasmBytes, {
      core: { ...core, ...extraImports },
    });
    instance = res.instance;
  } catch (e) {
    throw new Error("Failed to load/instantiate c-flat wasm: " + e);
  }
  exports = instance.exports;

  for (const fn of ASYNCIFY_EXPORTS) {
    if (typeof exports[fn] !== "function") {
      throw new Error(
        `c-flat wasm missing export ${fn} — was it built with wasm-opt --asyncify?`,
      );
    }
  }

  const pageIndex = exports.memory.grow(1);
  dataAddr = pageIndex * 65536;
  const view = new Int32Array(exports.memory.buffer);
  view[dataAddr >> 2] = dataAddr + 8;
  view[(dataAddr + 4) >> 2] = dataAddr + 8 + stackSize;

  function run() {
    exports.main();
    if (exports.asyncify_get_state() === 1) {
      exports.asyncify_stop_unwind();
      requestAnimationFrame(() => {
        exports.asyncify_start_rewind(dataAddr);
        run();
      });
    }
  }

  function runUntilExit({ signal } = {}) {
    return new Promise((resolve, reject) => {
      const finish = () => {
        closeWindowIfOpen();
        resolve();
      };
      const fail = (e) => {
        closeWindowIfOpen();
        reject(e);
      };
      const step = () => {
        try {
          exports.main();
          if (exports.asyncify_get_state() === 1) {
            exports.asyncify_stop_unwind();
            requestAnimationFrame(() => {
              if (signal?.aborted) return finish();
              try {
                exports.asyncify_start_rewind(dataAddr);
              } catch (e) {
                return fail(e);
              }
              step();
            });
          } else {
            finish();
          }
        } catch (e) {
          fail(e);
        }
      };
      if (signal?.aborted) return finish();
      step();
    });
  }

  return { host, instance, run, runUntilExit };
}
