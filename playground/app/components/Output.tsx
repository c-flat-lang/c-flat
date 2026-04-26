"use client";

interface OutputProps {
  output: string;
  error: string | null;
  status: "idle" | "compiling" | "running" | "done";
}

export default function Output({ output, error, status }: OutputProps) {
  return (
    <div className="flex flex-col h-full bg-[#1e1e1e] border-l border-[#3e3e3e]">
      <div className="flex items-center px-3 py-1 bg-[#252526] border-b border-[#3e3e3e] text-xs text-[#858585] font-mono">
        OUTPUT
        {status === "compiling" && (
          <span className="ml-2 text-yellow-400">compiling…</span>
        )}
        {status === "running" && (
          <span className="ml-2 text-blue-400">running…</span>
        )}
      </div>
      <pre className="flex-1 overflow-auto p-3 font-mono text-sm whitespace-pre-wrap">
        {error ? (
          <span className="text-red-400">{error}</span>
        ) : (
          <span className="text-[#d4d4d4]">{output}</span>
        )}
      </pre>
    </div>
  );
}
