mod cli;
mod error;
mod stage;
use stage::Stage;

use bitbox::{Target, passes::DebugPass};
use cli::{Cli, DebugMode};

use crate::error::CompilerError;

use std::str::FromStr;

fn front_end_compiler(cli_options: &Cli, src: &str) -> Result<bitbox::ir::Module, CompilerError> {
    let tokens = stage::lexer::Lexer::default().run(src);

    if let Some(DebugMode::Token) = cli_options.debug_mode {
        for token in &tokens {
            eprintln!("{:?}", token);
        }
        std::process::exit(0);
    }

    let mut ast = stage::parser::Parser::default().run(tokens)?;

    if let Some(DebugMode::Ast) = cli_options.debug_mode {
        eprintln!("{:#?}", ast);
        std::process::exit(0);
    }

    let symbol_table = stage::semantic_analyzer::SemanticAnalyzer::default().run(&mut ast)?;

    if let Some(DebugMode::SymbolTable) = cli_options.debug_mode {
        eprintln!("{:#?}", symbol_table);
        std::process::exit(0);
    }

    let module = stage::ir_builder::IRBuilder::default().run((symbol_table, ast))?;

    if let Some(DebugMode::Ir) = cli_options.debug_mode {
        eprintln!("{}", module);
        std::process::exit(0);
    }

    Ok(module)
}

fn main() {
    let args = std::env::args().skip(1).collect::<Vec<_>>();
    let mut cli_options = Cli::default();

    if args
        .iter()
        .find(|arg| arg == &"-h" || arg == &"--help")
        .is_some()
    {
        eprintln!("Usage: {} <filename>", std::env::args().next().unwrap());
        eprintln!("  Options:");
        eprintln!("    -t            Print tokens");
        eprintln!("    -a            Print AST");
        eprintln!("    -s            Print symbol table");
        eprintln!("    -ir           Print IR");
        eprintln!("    --target      Target triple");
        eprintln!("    -h, --help    Print this help message");
        std::process::exit(0);
    }

    let Some(file_path) = args.last() else {
        eprintln!("Usage: {} <filename>", std::env::args().next().unwrap());
        std::process::exit(1);
    };

    if let Some(_) = args.iter().find(|arg| arg == &"-t") {
        cli_options.debug_mode = Some(DebugMode::Token);
    } else if let Some(_) = args.iter().find(|arg| arg == &"-a") {
        cli_options.debug_mode = Some(DebugMode::Ast);
    } else if let Some(_) = args.iter().find(|arg| arg == &"-s") {
        cli_options.debug_mode = Some(DebugMode::SymbolTable);
    } else if let Some(_) = args.iter().find(|arg| arg == &"-ir") {
        cli_options.debug_mode = Some(DebugMode::Ir);
    } else if let Some(_) = args.iter().find(|arg| arg == &"--dump-after=lowering") {
        cli_options.debug_mode = Some(DebugMode::LoweredIr);
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

    let source = std::fs::read_to_string(&file_path).unwrap();

    let mut module = match front_end_compiler(&cli_options, &source) {
        Ok(module) => module,
        Err(err) => {
            println!("{:?}", err);
            return;
        }
    };
    let mut ctx = bitbox::backend::Context::default();
    let compiler_debug_mode: Option<DebugPass> = cli_options.debug_mode.and_then(Into::into);
    if let Err(error) =
        bitbox::Compiler::new(file_path, target, compiler_debug_mode).run(&mut module, &mut ctx)
    {
        eprintln!("{}", error);
        std::process::exit(1);
    }
}
