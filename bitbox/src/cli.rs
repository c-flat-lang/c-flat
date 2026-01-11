use bitbox::Target;
use std::str::FromStr;

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
            eprintln!("                  options: lowering-ir");
            eprintln!("                  options: emit-wasm32");
            eprintln!("                  options: emit-bitbeat");
            eprintln!("                  options: control-flow-graph");
            eprintln!("                  options: liveness-analysis");
            eprintln!("    -h, --help    Print this help message");

            std::process::exit(0);
        }

        let Some(file_path) = args.last() else {
            eprintln!("Usage: {} <filename>", std::env::args().next().unwrap());
            std::process::exit(1);
        };

        let mut debug_mode = None;
        if args.iter().any(|arg| arg == "-t") {
            debug_mode = Some(DebugMode::Token);
        } else if args.iter().any(|arg| arg == "-a") {
            debug_mode = Some(DebugMode::Ast);
        } else if args.iter().any(|arg| arg == "-s") {
            debug_mode = Some(DebugMode::SymbolTable);
        } else if args.iter().any(|arg| arg == "-ir") {
            debug_mode = Some(DebugMode::Ir);
        } else if args.iter().any(|arg| arg == "--dump-after=lowering-ir") {
            debug_mode = Some(DebugMode::LoweredIr);
        } else if args.iter().any(|arg| arg == "--dump-after=emit-wasm32") {
            debug_mode = Some(DebugMode::EmitWasm32);
        } else if args.iter().any(|arg| arg == "--dump-after=emit-bitbeat") {
            debug_mode = Some(DebugMode::EmitBitbeat);
        } else if args
            .iter()
            .any(|arg| arg == "--dump-after=control-flow-graph")
        {
            debug_mode = Some(DebugMode::ControlFlowGraph);
        } else if args
            .iter()
            .any(|arg| arg == "--dump-after=liveness-analysis")
        {
            debug_mode = Some(DebugMode::LivenessAnalysis);
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
    EmitBitbeat,
    EmitWasm32,
    Ir,
    LivenessAnalysis,
    LoweredIr,
    SymbolTable,
    Token,
}

impl From<DebugMode> for Option<bitbox::passes::DebugPass> {
    fn from(value: DebugMode) -> Option<bitbox::passes::DebugPass> {
        match value {
            DebugMode::LoweredIr => Some(bitbox::passes::DebugPass::LoweredIr),
            DebugMode::EmitWasm32 => Some(bitbox::passes::DebugPass::EmitWasm32),
            DebugMode::EmitBitbeat => Some(bitbox::passes::DebugPass::EmitBitbeat),
            DebugMode::ControlFlowGraph => Some(bitbox::passes::DebugPass::ControlFlowGraph),
            DebugMode::LivenessAnalysis => Some(bitbox::passes::DebugPass::LivenessAnalysis),
            _ => None,
        }
    }
}
