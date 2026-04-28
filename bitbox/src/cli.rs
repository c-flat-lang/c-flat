use bitbox::Target;
use std::str::FromStr;

#[derive(Debug, Clone)]
pub struct Cli {
    pub debug_mode: Option<DebugMode>,
    pub target: Target,
    pub file_path: String,
    pub link: Option<String>,
}

impl Cli {
    pub fn parse() -> Self {
        let mut args = std::env::args().skip(1).collect::<Vec<_>>();

        if args.iter().any(|arg| arg == "-h" || arg == "--help") {
            print_help();
            std::process::exit(0);
        }

        let file_path = args.pop().unwrap_or_else(|| {
            print_help();
            std::process::exit(1);
        });

        let mut debug_mode = None;
        let mut target = Target::default();
        let mut link = None;

        let i = 0;
        while i < args.len() {
            let arg = &args[i];

            if arg == "--link" {
                if i + 1 >= args.len() {
                    eprintln!("Expected argument after 'link'");
                    std::process::exit(1);
                }
                link = Some(args[i + 1].clone());
                args.drain(i..=i + 1);
                continue;
            } else if arg.starts_with("--link=") {
                let Some(value) = arg.strip_prefix("--link=") else {
                    eprintln!("Expected argument after 'link'");
                    std::process::exit(1);
                };
                link = Some(value.to_string());
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
            link,
        }
    }
}

fn print_help() {
    let bin = std::env::args().next().unwrap();
    eprintln!("Usage: {bin} [options] <filename>");
    eprintln!("Options:");
    eprintln!("  -t                 Print tokens");
    eprintln!("  -a                 Print AST");
    eprintln!("  -s                 Print symbol table");
    eprintln!("  -ir                Print IR");
    eprintln!("  --link [options]   Link with another file (e.g. a C library)");
    eprintln!("  --target=TRIPLE    Compile for a specific target. Valid options:");
    eprintln!("    wasm32");
    eprintln!("    x86_64-linux");
    eprintln!("    bitbeat");
    eprintln!("  --dump-after=PASS  Dump IR after a specific pass. Valid options:");
    for pass in DebugMode::DUMP_AFTER_PASSES {
        eprintln!("    {pass}");
    }
    eprintln!("  -h, --help         Print this help message");
}

fn unknown_arg(arg: &str) -> ! {
    eprintln!("Unknown argument: {arg}");
    eprintln!("Run with --help to see valid options.");
    std::process::exit(1);
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
    fn from(value: DebugMode) -> Option<bitbox::passes::DebugPass> {
        use bitbox::passes::DebugPass::*;
        Some(match value {
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
