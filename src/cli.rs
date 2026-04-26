use bitbox::Target;

#[cfg(not(feature = "wasm"))]
use std::str::FromStr;

#[cfg(feature = "wasm")]
use wasm_bindgen::prelude::*;

trait HasArg {
    fn has_arg(&self, expected: &str) -> bool;
    fn has_prefix(&self, expected: &str) -> bool;
}

impl HasArg for Vec<String> {
    fn has_arg(&self, expected: &str) -> bool {
        self.iter().any(|arg| arg == expected)
    }
    fn has_prefix(&self, expected: &str) -> bool {
        self.iter().any(|arg| arg.starts_with(expected))
    }
}

#[cfg(feature = "wasm")]
#[wasm_bindgen]
#[derive(Debug, Clone)]
pub struct Cli {
    debug_mode: Option<DebugMode>,
    target: Target,
    file_path: String,
    run: bool,
}

#[cfg(not(feature = "wasm"))]
#[derive(Debug, Clone)]
pub struct Cli {
    pub debug_mode: Option<DebugMode>,
    pub target: Target,
    pub file_path: String,
    pub run: bool,
}

#[cfg_attr(feature = "wasm", wasm_bindgen)]
impl Cli {
    #[cfg_attr(feature = "wasm", wasm_bindgen(constructor))]
    pub fn new(target: Target, file_path: String, debug_mode: Option<DebugMode>) -> Self {
        Self {
            target,
            file_path,
            debug_mode,
            run: false,
        }
    }

    #[cfg_attr(feature = "wasm", wasm_bindgen(getter))]
    pub fn target(&self) -> Target {
        self.target
    }

    #[cfg_attr(feature = "wasm", wasm_bindgen(getter))]
    pub fn file_path(&self) -> String {
        self.file_path.clone()
    }

    #[cfg_attr(feature = "wasm", wasm_bindgen(getter))]
    pub fn debug_mode(&self) -> Option<DebugMode> {
        self.debug_mode
    }

    #[cfg(not(feature = "wasm"))]
    pub fn parse() -> Self {
        let mut args: Vec<String> = std::env::args().skip(1).collect();

        if args.iter().any(|a| a == "-h" || a == "--help") {
            print_help();
            std::process::exit(0);
        }

        let file_path = args.pop().unwrap_or_else(|| {
            print_help();
            std::process::exit(1);
        });

        let mut run = false;

        if args.has_arg("run") {
            run = true;
        }

        let mut debug_mode = None;

        if args.has_arg("-t") {
            debug_mode = Some(DebugMode::Token);
        } else if args.has_arg("-a") {
            debug_mode = Some(DebugMode::Ast);
        } else if args.has_arg("-s") {
            debug_mode = Some(DebugMode::SymbolTable);
        } else if args.has_arg("-ir") {
            debug_mode = Some(DebugMode::Ir);
        } else if args.has_prefix("--dump-after=") {
            let value = args
                .iter()
                .find(|arg| arg.starts_with("--dump-after="))
                .unwrap();
            let Some(vlaue) = value.strip_prefix("--dump-after=") else {
                eprintln!("Invalid argument: {}", value);
                std::process::exit(1);
            };
            let mode = DebugMode::from_dump_after(vlaue);
            debug_mode = Some(mode);
        }

        let mut target = Target::default();

        let i = 0;
        while i < args.len() {
            let arg = &args[i];

            if let Some(mode) = DebugMode::from_flag(arg) {
                debug_mode = Some(mode);
                args.remove(i);
                continue;
            }

            if arg.starts_with("run") {
                run = true;
                args.remove(i);
                continue;
            }

            if let Some(value) = arg.strip_prefix("--dump-after=") {
                debug_mode = Some(DebugMode::from_dump_after(value));
                args.remove(i);
                continue;
            }

            if let Some(value) = arg.strip_prefix("--target=") {
                target = Target::from_str(value).unwrap_or_else(|err| {
                    eprintln!("{} '{}'", err, value);
                    std::process::exit(1);
                });
                args.remove(i);
                continue;
            }

            unknown_arg(arg);
        }

        Self {
            debug_mode,
            target,
            file_path,
            run,
        }
    }
}

fn print_help() {
    let bin = std::env::args().next().unwrap();
    eprintln!("Usage: {bin} <filename>");
    eprintln!("Options:");
    eprintln!("  -t            Print tokens");
    eprintln!("  -a            Print AST");
    eprintln!("  -s            Print symbol table");
    eprintln!("  -ir           Print IR");
    print_target_help();
    print_dump_after_help();
    eprintln!("  -h, --help    Print this help message");
}

fn print_target_help() {
    eprintln!("  --target=TRIPLE");
    eprintln!("    wasm32");
    eprintln!("    x86_64-linux");
    eprintln!("    bitbeat");
}

fn print_dump_after_help() {
    eprintln!("  --dump-after=PASS");
    eprintln!("    lowering-ir");
    eprintln!("    emit");
    eprintln!("    control-flow-graph");
    eprintln!("    liveness-analysis");
    eprintln!("    detect-loops");
    eprintln!("    phi-node-elimination");
    eprintln!("    local-function-variables");
    eprintln!("    structuring-ir");
}

fn unknown_arg(arg: &str) -> ! {
    eprintln!("Unknown argument: {arg}");
    eprintln!("Run with --help to see valid options.");
    std::process::exit(1);
}

#[cfg_attr(feature = "wasm", wasm_bindgen)]
#[derive(Debug, Clone, Copy)]
pub enum DebugMode {
    Token,
    Ast,
    SymbolTable,
    Ir,
    LoweredIr,
    Emit,
    ControlFlowGraph,
    LivenessAnalysis,
    DetectLoops,
    PhiNodeElimination,
    LocalFunctionVariables,
    StructuringIr,
}

impl DebugMode {
    fn from_flag(flag: &str) -> Option<Self> {
        Some(match flag {
            "-t" => Self::Token,
            "-a" => Self::Ast,
            "-s" => Self::SymbolTable,
            "-ir" => Self::Ir,
            _ => return None,
        })
    }

    fn from_dump_after(value: &str) -> Self {
        match value {
            "lowering-ir" => Self::LoweredIr,
            "emit" => Self::Emit,
            "control-flow-graph" => Self::ControlFlowGraph,
            "liveness-analysis" => Self::LivenessAnalysis,
            "detect-loops" => Self::DetectLoops,
            "phi-node-elimination" => Self::PhiNodeElimination,
            "local-function-variables" => Self::LocalFunctionVariables,
            "structuring-ir" => Self::StructuringIr,
            _ => {
                eprintln!("Unknown debug mode: {value}");
                eprintln!("Options:");
                eprintln!("   lowering-ir");
                eprintln!("   emit");
                eprintln!("   control-flow-graph");
                eprintln!("   liveness-analysis");
                eprintln!("   detect-loops");
                eprintln!("   phi-node-elimination");
                eprintln!("   local-function-variables");
                eprintln!("   structuring-ir");
                std::process::exit(1);
            }
        }
    }
}

impl From<DebugMode> for Option<bitbox::passes::DebugPass> {
    fn from(mode: DebugMode) -> Self {
        use bitbox::passes::DebugPass::*;
        Some(match mode {
            DebugMode::LoweredIr => LoweredIr,
            DebugMode::Emit => Emit,
            DebugMode::ControlFlowGraph => ControlFlowGraph,
            DebugMode::LivenessAnalysis => LivenessAnalysis,
            DebugMode::DetectLoops => DetectLoops,
            DebugMode::PhiNodeElimination => PhiNodeElimination,
            DebugMode::LocalFunctionVariables => LocalFunctionVariables,
            DebugMode::StructuringIr => StructuringIr,
            _ => return None,
        })
    }
}
