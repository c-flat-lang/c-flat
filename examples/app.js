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
    write_i32: (number) => {
      console.log(number);
    },
  },
};

function isBrowser() {
  return (
    typeof window !== "undefined" && typeof window.document !== "undefined"
  );
}
async function getWasmBuffer(name) {
  if (isBrowser()) {
    const result = await fetch(name);
    return await result.arrayBuffer();
  } else {
    const fs = require("fs");
    return fs.readFileSync(name);
  }
}

async function main(filename) {
  const buffer = await getWasmBuffer(filename);
  try {
    const module = await WebAssembly.instantiate(buffer, imports);
    const memory = module.instance.exports.memory;
    if (memory) {
      memoryBuffer = new Uint8Array(memory.buffer);
    }

    const result = module.instance.exports.main();
    if (isBrowser()) {
      const resultElement = document.getElementById("result");
      console.log(resultElement);
      resultElement.innerHTML = `Result from wasm main: ${result}`;
    } else {
      console.log("Result from wasm main:", result);
    }
  } catch (e) {
    console.error("Error instantiating the wasm module:");
    console.error(e.message);
  }
}

if (!isBrowser()) {
  console.log("Running in node.js");
  // get args
  const args = process.argv.slice(2);
  const filename = args[0];
  main(filename);
} else {
  document
    .getElementById("run")
    .addEventListener("click", () => main("basic.wasm"));
}
