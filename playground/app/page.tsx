"use client";

import dynamic from "next/dynamic";
import { useRef, useState } from "react";
import Output from "./components/Output";
import Toolbar from "./components/Toolbar";
import { useCompiler } from "./hooks/useCompiler";
import type { EditorHandle } from "./components/Editor";

const Editor = dynamic(() => import("./components/Editor"), { ssr: false });

type Status = "idle" | "compiling" | "running" | "done";

export default function Home() {
  const editorRef = useRef<EditorHandle>(null);
  const [output, setOutput] = useState("");
  const [error, setError] = useState<string | null>(null);
  const [status, setStatus] = useState<Status>("idle");
  const [vimMode] = useState("NORMAL");
  const { ready, compile, run } = useCompiler();

  async function handleRun() {
    const source = editorRef.current?.getValue() ?? "";
    setOutput("");
    setError(null);

    try {
      setStatus("compiling");
      const wasmBytes = await compile(source);
      setStatus("running");
      const result = await run(wasmBytes);
      if (result.error) {
        setError(result.error);
      } else {
        setOutput(result.output);
      }
    } catch (err) {
      setError(String(err));
    } finally {
      setStatus("done");
    }
  }

  return (
    <div className="flex flex-col h-screen bg-[#1e1e1e] text-white overflow-hidden">
      <div className="flex flex-1 min-h-0">
        <div className="flex flex-col flex-1 min-w-0 border-r border-[#3e3e3e]">
          <div className="px-3 py-1 bg-[#252526] border-b border-[#3e3e3e] text-xs text-[#858585] font-mono">
            fib.cb
          </div>
          <div className="flex-1 min-h-0">
            <Editor ref={editorRef} />
          </div>
        </div>
        <div className="flex flex-col w-[40%] min-w-[300px]">
          <Output output={output} error={error} status={status} />
        </div>
      </div>
      <Toolbar
        onRun={handleRun}
        ready={ready}
        status={status}
        vimMode={vimMode}
      />
    </div>
  );
}
