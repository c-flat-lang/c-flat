mod ast;
mod error;
mod lexer;
mod llvm_codgen;
mod parser;

use inkwell::context::Context;
use inkwell::targets::{InitializationConfig, Target, TargetMachine};
use inkwell::OptimizationLevel;
use llvm_codgen::Codegen;
use std::path::Path;
use std::process::Command;

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
        eprintln!("    -h, --help    Print this help message");
        std::process::exit(0);
    }

    let Some(file_path) = args.last() else {
        eprintln!("Usage: {} <filename>", std::env::args().next().unwrap());
        std::process::exit(1);
    };

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

    // LLVM

    let context = Context::create();
    let module = context.create_module("test");
    let builder = context.create_builder();

    // Initialize native target
    Target::initialize_native(&InitializationConfig::default())
        .expect("Failed to initialize target");

    for item in ast {
        match item {
            ast::Item::Function(function) => {
                function.codegen(&context, &module, &builder).unwrap();
            }
            ast::Item::Type(type_def) => todo!("{type_def:#?}"),
            ast::Item::Use(use_item) => todo!("{use_item:#?}"),
        }
    }

    // Create target machine for object file generation
    let triple = TargetMachine::get_default_triple();
    let target = Target::from_triple(&triple).expect("Failed to get target");
    let target_machine = target
        .create_target_machine(
            &triple,
            "generic",
            "",
            OptimizationLevel::Default,
            inkwell::targets::RelocMode::Default,
            inkwell::targets::CodeModel::Default,
        )
        .expect("Failed to create target machine");

    let filename = file_path.split_once('.').unwrap_or_default().0;

    // Write object file
    target_machine
        .write_to_file(
            &module,
            inkwell::targets::FileType::Object,
            Path::new(&format!("{}.o", filename)),
        )
        .expect("Failed to write object file");

    println!("Generated object file: output.o");

    // Link the object file into an executable using GCC
    Command::new("gcc")
        .args([&format!("{filename}.o"), "-o", filename, "-static"])
        .status()
        .expect("Failed to link executable");

    println!("Executable created: output");
}
