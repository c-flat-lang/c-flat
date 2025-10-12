mod error;
mod stage;
use stage::Stage;

use bitbox::Target;

use crate::error::CompilerError;

use std::str::FromStr;

fn front_end_compiler(src: &str) -> Result<bitbox::ir::Module, CompilerError> {
    let tokens = stage::lexer::Lexer::default().run(src);
    let mut ast = stage::parser::Parser::default().run(tokens)?;
    let symbol_table = stage::semantic_analyzer::SemanticAnalyzer::default().run(&mut ast)?;
    stage::ir_builder::IRBuilder::default().run((symbol_table, ast))
}

fn main() {
    let args = std::env::args().skip(1).collect::<Vec<_>>();

    if args
        .iter()
        .find(|arg| arg == &"-h" || arg == &"--help")
        .is_some()
    {
        eprintln!("Usage: {} <filename>", std::env::args().next().unwrap());
        eprintln!("  Options:");
        eprintln!("    -t            Print tokens");
        eprintln!("    -a            Print AST");
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
        todo!("print tokens")
    } else if let Some(_) = args.iter().find(|arg| arg == &"-a") {
        todo!("print ast")
    } else if let Some(_) = args.iter().find(|arg| arg == &"-ir") {
        todo!("print ir")
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

    let mut module = match front_end_compiler(&source) {
        Ok(module) => module,
        Err(err) => {
            println!("{:?}", err);
            return;
        }
    };
    let mut ctx = bitbox::backend::Context::default();
    bitbox::Compiler::new(file_path, target).run(&mut module, &mut ctx);
}
