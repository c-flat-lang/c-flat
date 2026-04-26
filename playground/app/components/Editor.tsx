"use client";

import { useEffect, useRef, forwardRef, useImperativeHandle } from "react";
import { EditorState } from "@codemirror/state";
import { EditorView, keymap, lineNumbers, drawSelection, highlightActiveLine } from "@codemirror/view";
import { defaultKeymap, historyKeymap, history } from "@codemirror/commands";
import { syntaxHighlighting, defaultHighlightStyle, bracketMatching } from "@codemirror/language";
import { oneDark } from "@codemirror/theme-one-dark";
import { vim } from "@replit/codemirror-vim";

const DEFAULT_SOURCE = `pub fn fib(n: s32, a: s32, b: s32) s32 {
  let is_zero = n == 0;
  if is_zero {
    return a;
  }
  return fib(n - 1, b, a + b);
}

pub fn main() s32 {
  let value = fib(10, 0, 1);
  write_int(value);
  write_char(10);
  return 0;
}
`;

export interface EditorHandle {
  getValue: () => string;
}

export default forwardRef<EditorHandle>(function Editor(_props, ref) {
  const containerRef = useRef<HTMLDivElement>(null);
  const viewRef = useRef<EditorView | null>(null);

  useImperativeHandle(ref, () => ({
    getValue: () => viewRef.current?.state.doc.toString() ?? "",
  }));

  useEffect(() => {
    if (!containerRef.current) return;

    const state = EditorState.create({
      doc: DEFAULT_SOURCE,
      extensions: [
        vim(),
        history(),
        lineNumbers(),
        drawSelection(),
        highlightActiveLine(),
        bracketMatching(),
        syntaxHighlighting(defaultHighlightStyle),
        oneDark,
        keymap.of([...defaultKeymap, ...historyKeymap]),
        EditorView.theme({
          "&": { height: "100%", fontSize: "14px" },
          ".cm-scroller": { fontFamily: "monospace", overflow: "auto" },
          ".cm-content": { padding: "8px 0" },
        }),
      ],
    });

    const view = new EditorView({ state, parent: containerRef.current });
    viewRef.current = view;

    return () => view.destroy();
  }, []);

  return <div ref={containerRef} className="h-full w-full overflow-hidden" />;
});
