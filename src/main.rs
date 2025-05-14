#![allow(unused)]
mod ast;
mod bbir_emitter;
mod error;
mod lexer;
mod llvm_codgen;
mod parser;
mod semantic_analysis;

use bitbox::Target;
use std::str::FromStr;

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
        eprintln!("    --target      Target triple");
        eprintln!("    -h, --help    Print this help message");
        std::process::exit(0);
    }

    let Some(file_path) = args.last() else {
        eprintln!("Usage: {} <filename>", std::env::args().next().unwrap());
        std::process::exit(1);
    };

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

    if args.iter().find(|arg| arg == &"-t").is_some() {
        let tokens = lexer::tokenize(&source);
        eprintln!("{tokens:#?}");
        return;
    }

    let mut ast = match parser::Parser::new(&source).parse() {
        Ok(ast) => ast,
        Err(err) => {
            let report = err.report(&file_path, &source);
            eprintln!("{}", report);
            std::process::exit(1);
        }
    };

    ast.sort_by(|a, b| match (a, b) {
        (ast::Item::Function(a), ast::Item::Function(b)) => {
            if a.name.lexeme == "main" {
                std::cmp::Ordering::Greater
            } else if b.name.lexeme == "main" {
                std::cmp::Ordering::Less
            } else {
                a.name.lexeme.cmp(&b.name.lexeme)
            }
        }
        _ => std::cmp::Ordering::Equal,
    });

    if args.iter().find(|arg| arg == &"-a").is_some() {
        eprintln!("{ast:#?}");
        return;
    }

    let program = bbir_emitter::emit(&mut ast);
    eprintln!("{:#?}", program);

    //if let Err(err) = bitbox::Compiler::default()
    //    .target(target)
    //    .program(program)
    //    .filename(file_path)
    //    .src(&source)
    //    .compile()
    //{
    //    eprintln!("{}", err);
    //    std::process::exit(1);
    //}
}
