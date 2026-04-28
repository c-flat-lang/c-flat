use bitbox::Target;

#[cfg(not(feature = "wasm"))]
use std::str::FromStr;

#[cfg(feature = "wasm")]
use wasm_bindgen::prelude::*;

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
        let mut debug_mode = None;
        let mut target = Target::default();

        let i = 0;
        while i < args.len() {
            let arg = &args[i];

            if arg == "run" {
                run = true;
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

            if let Some(value) = arg.strip_prefix("--dump-after=") {
                debug_mode = Some(DebugMode::from_dump_after(value));
                args.remove(i);
                continue;
            }

            if let Some(mode) = DebugMode::from_flag(arg) {
                debug_mode = Some(mode);
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
    eprintln!("Usage: {bin} [options] <filename>");
    eprintln!("Options:");
    eprintln!("  run           Compile and run the program");
    eprintln!("  -t            Print tokens");
    eprintln!("  -a            Print AST");
    eprintln!("  --check       Type check code");
    eprintln!("  -s            Print symbol table");
    eprintln!("  -ir           Print IR");
    eprintln!("  --target=TRIPLE");
    eprintln!("    wasm32");
    eprintln!("    x86_64-linux");
    eprintln!("    bitbeat");
    eprintln!("  --dump-after=PASS");
    for pass in DebugMode::DUMP_AFTER_PASSES {
        eprintln!("    {pass}");
    }
    eprintln!("  -h, --help    Print this help message");
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
    TypeChecker,
    Ir,
    LoweredIr,
    Emit,
    ControlFlowGraph,
    LivenessAnalysis,
    DetectLoops,
    PhiNodeElimination,
    LocalFunctionVariables,
    StructuringIr,
    VirtRegRewrite,
}

impl DebugMode {
    const DUMP_AFTER_PASSES: &'static [&'static str] = &[
        "lowering-ir",
        "emit",
        "control-flow-graph",
        "liveness-analysis",
        "detect-loops",
        "phi-node-elimination",
        "local-function-variables",
        "structuring-ir",
        "virt-reg-rewrite",
    ];

    fn from_flag(flag: &str) -> Option<Self> {
        Some(match flag {
            "-t" => Self::Token,
            "-a" => Self::Ast,
            "-s" => Self::SymbolTable,
            "--check" => Self::TypeChecker,
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
            "virt-reg-rewrite" => Self::VirtRegRewrite,
            _ => {
                eprintln!("Unknown pass: {value}");
                eprintln!("Valid options:");
                for pass in Self::DUMP_AFTER_PASSES {
                    eprintln!("  {pass}");
                }
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
            DebugMode::VirtRegRewrite => VirtRegRewrite,
            _ => return None,
        })
    }
}
