let memoryBuffer;
const imports = {
  core: {
    write: (ptr, len) => {
      console.log(ptr, len);
      const stringData = new TextDecoder("utf-8").decode(
        memoryBuffer.slice(ptr, ptr + len),
      );
      console.log(stringData);
    },
  },
};

async function getWasmBuffer(name) {
  if (typeof window !== "undefined" && typeof window.document !== "undefined") {
    const result = await fetch(name);
    return await result.arrayBuffer();
  } else {
    const fs = require("fs");
    return fs.readFileSync(name);
  }
}

async function main() {
  const buffer = await getWasmBuffer("examples/test.wasm");
  try {
    const module = await WebAssembly.instantiate(buffer, imports);
    const memory = module.instance.exports.memory;
    if (memory) {
      memoryBuffer = new Uint8Array(memory.buffer);
    }

    // Instead of calling `main`, call `add`
    const result = module.instance.exports.add(42, 58);
    console.log("Result from wasm add:", result);
  } catch (e) {
    console.error("Error instantiating the wasm module:");
    console.error(e.message);
  }
}

main();
