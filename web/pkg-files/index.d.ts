export type CoreImport = (...args: any[]) => any;

export interface RaylibHostOptions {
  canvas: HTMLCanvasElement;
  log?: (s: string) => void;
  shouldClose?: () => boolean;
  locateFile?: (path: string, prefix: string) => string;
}

export interface RaylibHost {
  core: Record<string, CoreImport>;
  ready: Promise<unknown>;
  readonly module: any;
  preloadAssets(manifest: string[], baseUrl?: string): Promise<void>;
}

export function makeRaylibHost(
  getCflatExports: () => WebAssembly.Exports,
  canvasOrOpts: HTMLCanvasElement | RaylibHostOptions,
): RaylibHost;

export interface RaylibProgramOptions extends RaylibHostOptions {
  extraImports?: Record<string, CoreImport>;
  stackSize?: number;
}

export interface RaylibProgram {
  host: RaylibHost;
  instance: WebAssembly.Instance;
  run(): void;
  runUntilExit(): Promise<void>;
}

export function loadRaylibProgram(
  wasmBytes: BufferSource,
  options: RaylibProgramOptions,
): Promise<RaylibProgram>;
