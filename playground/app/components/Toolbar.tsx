"use client";

interface ToolbarProps {
  onRun: () => void;
  ready: boolean;
  status: "idle" | "compiling" | "running" | "done";
  vimMode: string;
}

export default function Toolbar({ onRun, ready, status, vimMode }: ToolbarProps) {
  const busy = status === "compiling" || status === "running";

  return (
    <div className="flex items-center justify-between px-3 h-8 bg-[#007acc] text-white text-xs font-mono select-none shrink-0">
      {/* Left: vim mode */}
      <span className="w-24 text-yellow-200 font-bold uppercase tracking-wider">
        {vimMode || "NORMAL"}
      </span>

      {/* Center: title */}
      <span className="text-white/80">C-Flat Playground</span>

      {/* Right: run button */}
      <button
        onClick={onRun}
        disabled={!ready || busy}
        className="px-3 py-0.5 rounded bg-white/20 hover:bg-white/30 disabled:opacity-40 disabled:cursor-not-allowed transition-colors text-white font-semibold"
      >
        {busy ? (status === "compiling" ? "Compiling…" : "Running…") : "▶ Run"}
      </button>
    </div>
  );
}
