pub mod cli;
pub mod error;
pub mod stage;
use stage::Stage;

use crate::error::CompilerError;
pub use bitbox::{self, Target};
#[cfg(feature = "wasm")]
pub use bitbox::{backend::CompilerResult, passes::DebugPass};
pub use cli::{Cli, DebugMode};
#[cfg(feature = "wasm")]
use wasm_bindgen::prelude::*;

#[cfg(feature = "wasm")]
pub fn front_end_compiler(
    src: &str,
    cli_options: Cli,
) -> Result<bitbox::ir::Module, CompilerError> {
    let tokens = stage::lexer::Lexer::default().run(src);

    if let Some(DebugMode::Token) = cli_options.debug_mode() {
        for token in &tokens {
            // eprintln!("{:?}", token);
        }
        //std::process::exit(0);
    }

    let mut ast = stage::parser::Parser::default().run(tokens)?;

    if let Some(DebugMode::Ast) = cli_options.debug_mode() {
        // eprintln!("{:#?}", ast);
        // std::process::exit(0);
    }

    let symbol_table = stage::semantic_analyzer::SemanticAnalyzer::default().run(&mut ast)?;

    if let Some(DebugMode::SymbolTable) = cli_options.debug_mode() {
        // eprintln!("{:#?}", symbol_table);
        // std::process::exit(0);
    }

    let module = stage::ir_builder::IRBuilder::default().run((symbol_table, ast))?;

    if let Some(DebugMode::Ir) = cli_options.debug_mode() {
        // eprintln!("{}", module);
        // std::process::exit(0);
    }

    Ok(module)
}

#[cfg(feature = "wasm")]
#[wasm_bindgen]
pub fn compile_source(source: &str, cli_options: Cli) -> Result<js_sys::Uint8Array, JsValue> {
    let mut module = match front_end_compiler(&source, cli_options.clone()) {
        Ok(module) => module,
        Err(err) => {
            println!("{:?}", err);
            return Err(JsValue::from_str(
                &err.report(&cli_options.file_path(), source),
            ));
        }
    };
    let compiler_debug_mode: Option<DebugPass> =
        cli_options.debug_mode().clone().and_then(Into::into);
    match bitbox::Compiler::new(
        &cli_options.file_path(),
        cli_options.target(),
        compiler_debug_mode,
    )
    .run(&mut module)
    {
        Ok(CompilerResult::Wasm32(bytes)) => Ok(js_sys::Uint8Array::from(&bytes[..])),
        Err(error) => Err(JsValue::from_str(&error.to_string())),
        _ => unimplemented!(),
    }
}

#[cfg(not(feature = "wasm"))]
pub fn front_end_compiler(
    src: &str,
    cli_options: &Cli,
) -> Result<bitbox::ir::Module, CompilerError> {
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
