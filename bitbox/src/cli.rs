use bitbox::Target;
use std::str::FromStr;

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

fn help_dump_after() {
    eprintln!("    --dump-after  Print debug info after pass");
    eprintln!("                  options:");
    eprintln!("                    lowering-ir");
    eprintln!("                    emit");
    eprintln!("                    control-flow-graph");
    eprintln!("                    liveness-analysis");
    eprintln!("                    detect-loops");
    eprintln!("                    phi-node-elimination");
    eprintln!("                    local-function-variables");
    eprintln!("                    structuring-ir");
}

#[derive(Debug, Clone)]
pub struct Cli {
    pub debug_mode: Option<DebugMode>,
    pub target: Target,
    pub file_path: String,
}

impl Cli {
    pub fn parse() -> Self {
        let args = std::env::args().skip(1).collect::<Vec<_>>();

        if args.iter().any(|arg| arg == "-h" || arg == "--help") {
            eprintln!("Usage: {} <filename>", std::env::args().next().unwrap());
            eprintln!("  Options:");
            eprintln!("    -t            Print tokens");
            eprintln!("    -a            Print AST");
            eprintln!("    -s            Print symbol table");
            eprintln!("    -ir           Print IR");
            eprintln!("    --target      Target triple");
            eprintln!("    --dump-after  Print debug info after pass");
            help_dump_after();
            eprintln!("    -h, --help    Print this help message");

            std::process::exit(0);
        }

        let Some(file_path) = args.last() else {
            eprintln!("Usage: {} <filename>", std::env::args().next().unwrap());
            std::process::exit(1);
        };

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
            let arg = args
                .iter()
                .find(|arg| arg.starts_with("--dump-after="))
                .unwrap();
            let Some(value) = arg.strip_prefix("--dump-after=") else {
                eprintln!("Invalid argument: {}", arg);
                help_dump_after();
                std::process::exit(1);
            };
            let mode = DebugMode::from_str(value).unwrap_or_else(|_| {
                eprintln!("Unknown debug mode: {}", value);
                help_dump_after();
                std::process::exit(1);
            });
            debug_mode = Some(mode);
        }

        let mut target = Target::default();
        if let Some(arg) = args.iter().find(|arg| arg.starts_with("--target")) {
            let value = arg.strip_prefix("--target=").unwrap();
            target = match Target::from_str(value) {
                Ok(target) => target,
                Err(err) => {
                    eprintln!("{} '{}'", err, value);
                    std::process::exit(1);
                }
            };
        }
        Self {
            debug_mode,
            target,
            file_path: file_path.to_string(),
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub enum DebugMode {
    Ast,
    ControlFlowGraph,
    DetectLoops,
    Emit,
    Ir,
    LivenessAnalysis,
    LocalFunctionVariables,
    LoweredIr,
    PhiNodeElimination,
    SymbolTable,
    Token,
    StructuringIr,
}

impl FromStr for DebugMode {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "ast" => Ok(DebugMode::Ast),
            "control-flow-graph" => Ok(DebugMode::ControlFlowGraph),
            "detect-loops" => Ok(DebugMode::DetectLoops),
            "emit" => Ok(DebugMode::Emit),
            "ir" => Ok(DebugMode::Ir),
            "liveness-analysis" => Ok(DebugMode::LivenessAnalysis),
            "local-function-variables" => Ok(DebugMode::LocalFunctionVariables),
            "lowering-ir" => Ok(DebugMode::LoweredIr),
            "phi-node-elimination" => Ok(DebugMode::PhiNodeElimination),
            "symbol-table" => Ok(DebugMode::SymbolTable),
            "token" => Ok(DebugMode::Token),
            "structuring-ir" => Ok(DebugMode::StructuringIr),
            _ => Err(()),
        }
    }
}

impl From<DebugMode> for Option<bitbox::passes::DebugPass> {
    fn from(value: DebugMode) -> Option<bitbox::passes::DebugPass> {
        match value {
            DebugMode::LoweredIr => Some(bitbox::passes::DebugPass::LoweredIr),
            DebugMode::Emit => Some(bitbox::passes::DebugPass::Emit),
            DebugMode::ControlFlowGraph => Some(bitbox::passes::DebugPass::ControlFlowGraph),
            DebugMode::LivenessAnalysis => Some(bitbox::passes::DebugPass::LivenessAnalysis),
            DebugMode::DetectLoops => Some(bitbox::passes::DebugPass::DetectLoops),
            DebugMode::PhiNodeElimination => Some(bitbox::passes::DebugPass::PhiNodeElimination),
            DebugMode::LocalFunctionVariables => {
                Some(bitbox::passes::DebugPass::LocalFunctionVariables)
            }
            DebugMode::StructuringIr => Some(bitbox::passes::DebugPass::StructuringIr),
            _ => None,
        }
    }
}
