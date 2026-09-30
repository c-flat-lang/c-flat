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
  });
  const { core } = host;

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

  function runUntilExit() {
    return new Promise((resolve, reject) => {
      const step = () => {
        try {
          exports.main();
          if (exports.asyncify_get_state() === 1) {
            exports.asyncify_stop_unwind();
            requestAnimationFrame(() => {
              try {
                exports.asyncify_start_rewind(dataAddr);
              } catch (e) {
                return reject(e);
              }
              step();
            });
          } else {
            resolve();
          }
        } catch (e) {
          reject(e);
        }
      };
      step();
    });
  }

  return { host, instance, run, runUntilExit };
}
