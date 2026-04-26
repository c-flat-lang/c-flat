"use client";

import { useEffect, useRef, useState } from "react";

interface RunResult {
  output: string;
  error: string | null;
}

export function useCompiler() {
  // eslint-disable-next-line @typescript-eslint/no-explicit-any
  const mod = useRef<any>(null);
  const [ready, setReady] = useState(false);

  useEffect(() => {
    async function load() {
      const m = await import("../../lib/cflat/cflat.js");
      await m.default();
      mod.current = m;
      setReady(true);
    }
    load().catch(console.error);
  }, []);

  async function compile(source: string): Promise<Uint8Array> {
    if (!mod.current) throw new Error("Compiler not loaded");
    const { compile_source, Cli, Target } = mod.current;
    const cli = new Cli(Target.Wasm32, "fib.cb");
    return compile_source(source, cli);
  }

  async function run(wasmBytes: Uint8Array): Promise<RunResult> {
    let memoryBuffer: Uint8Array | null = null;
    const lines: string[] = [];

    const imports = {
      core: {
        write: (ptr: number, len: number) => {
          if (!memoryBuffer) return;
          lines.push(
            new TextDecoder().decode(memoryBuffer.slice(ptr, ptr + len)),
          );
        },
        write_i32: (n: number) => lines.push(String(n)),
        write_int: (n: number) => lines.push(String(n)),
        writenl: () => lines.push("\n"),
        write_char: (c: number) => lines.push(String.fromCharCode(c)),
      },
    };

    try {
      const result = await WebAssembly.instantiate(wasmBytes, imports);
      const instance = (
        result as unknown as WebAssembly.WebAssemblyInstantiatedSource
      ).instance;
      const memory = instance.exports.memory as WebAssembly.Memory | undefined;
      if (memory) memoryBuffer = new Uint8Array(memory.buffer);
      (instance.exports.main as () => void)();
      return { output: lines.join(""), error: null };
    } catch (err) {
      return { output: "", error: String(err) };
    }
  }

  return { ready, compile, run };
}
