import CodeMirror, { inputStyles } from "codemirror/lib/codemirror.js";
import "codemirror/lib/codemirror.css";
import "codemirror/theme/dracula.css";
import "codemirror/addon/mode/simple.js";
import "codemirror/mode/rust/rust.js";
import "codemirror/keymap/vim.js";
import init, { compile_source, Cli, Target } from "../pkg/cflat.js";

async function run(buffer) {
  let memoryBuffer;
  const imports = {
    core: {
      write: (ptr, len) => {
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

  try {
    const module = await WebAssembly.instantiate(buffer, imports);
    const memory = module.instance.exports.memory;
    if (memory) memoryBuffer = new Uint8Array(memory.buffer);

    return module.instance.exports.main();
  } catch (err) {
    console.error("Error instantiating WASM module:", err);
    return `Error: ${err}`;
  }
}

async function main() {
  await init();

  const editor = CodeMirror.fromTextArea(document.getElementById("editor"), {
    value: "",
    mode: "rust",
    theme: "gruvbox-dark",
    lineNumbers: true,
    keyMap: "vim",
    showCursorWhenSelecting: true,
    inputStyles: "contenteditable",
  });

  const vimStatus = document.getElementById("vim-status");
  let keys = "";
  CodeMirror.on(editor, "vim-keypress", function (key) {
    keys = keys + key;
    vimStatus.innerHTML = keys;
  });
  CodeMirror.on(editor, "vim-mode-change", function (e) {});
  CodeMirror.on(editor, "vim-command-done", function (e) {
    keys = "";
    vimStatus.innerHTML = keys;
  });

  const outputEl = document.getElementById("output");
  const runBtn = document.getElementById("run-btn");

  runBtn.addEventListener("click", async () => {
    const source = editor.getValue();
    const options = new Cli(Target.Wasm32, "example.cb");

    try {
      const wasmBytes = compile_source(source, options);
      outputEl.textContent = `Compiled ${wasmBytes.length} bytes!\nRunning...`;
      const output = await run(wasmBytes);
      outputEl.textContent += `\n\nOutput:\n${output}`;
    } catch (err) {
      outputEl.textContent = `Error:\n${err}`;
    }
  });
}

main();
