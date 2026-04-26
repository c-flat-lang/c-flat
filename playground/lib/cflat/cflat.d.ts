/* tslint:disable */
/* eslint-disable */

export class Cli {
    free(): void;
    [Symbol.dispose](): void;
    constructor(target: Target, file_path: string, debug_mode?: DebugMode | null);
    readonly debug_mode: DebugMode | undefined;
    readonly file_path: string;
    readonly target: Target;
}

export enum DebugMode {
    Token = 0,
    Ast = 1,
    SymbolTable = 2,
    Ir = 3,
    LoweredIr = 4,
    Emit = 5,
    ControlFlowGraph = 6,
    LivenessAnalysis = 7,
    DetectLoops = 8,
    PhiNodeElimination = 9,
    LocalFunctionVariables = 10,
    StructuringIr = 11,
}

export enum Target {
    Wasm32 = 0,
    X86_64Linux = 1,
    Bitbeat = 2,
}

export function compile_source(source: string, cli_options: Cli): Uint8Array;

export type InitInput = RequestInfo | URL | Response | BufferSource | WebAssembly.Module;

export interface InitOutput {
    readonly memory: WebAssembly.Memory;
    readonly __wbg_cli_free: (a: number, b: number) => void;
    readonly cli_debug_mode: (a: number) => number;
    readonly cli_file_path: (a: number) => [number, number];
    readonly cli_new: (a: number, b: number, c: number, d: number) => number;
    readonly cli_target: (a: number) => number;
    readonly compile_source: (a: number, b: number, c: number) => [number, number, number];
    readonly __wbindgen_externrefs: WebAssembly.Table;
    readonly __wbindgen_free: (a: number, b: number, c: number) => void;
    readonly __wbindgen_malloc: (a: number, b: number) => number;
    readonly __wbindgen_realloc: (a: number, b: number, c: number, d: number) => number;
    readonly __externref_table_dealloc: (a: number) => void;
    readonly __wbindgen_start: () => void;
}

export type SyncInitInput = BufferSource | WebAssembly.Module;

/**
 * Instantiates the given `module`, which can either be bytes or
 * a precompiled `WebAssembly.Module`.
 *
 * @param {{ module: SyncInitInput }} module - Passing `SyncInitInput` directly is deprecated.
 *
 * @returns {InitOutput}
 */
export function initSync(module: { module: SyncInitInput } | SyncInitInput): InitOutput;

/**
 * If `module_or_path` is {RequestInfo} or {URL}, makes a request and
 * for everything else, calls `WebAssembly.instantiate` directly.
 *
 * @param {{ module_or_path: InitInput | Promise<InitInput> }} module_or_path - Passing `InitInput` directly is deprecated.
 *
 * @returns {Promise<InitOutput>}
 */
export default function __wbg_init (module_or_path?: { module_or_path: InitInput | Promise<InitInput> } | InitInput | Promise<InitInput>): Promise<InitOutput>;
