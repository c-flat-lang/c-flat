pub mod cli;
pub mod error;
pub mod stage;
use stage::Stage;

use crate::error::Result;
pub use bitbox::{self, Target};
#[cfg(feature = "wasm")]
pub use bitbox::{backend::CompilerResult, passes::DebugPass};
pub use cli::{Cli, DebugMode};
#[cfg(feature = "wasm")]
use wasm_bindgen::prelude::*;

#[cfg(feature = "wasm")]
pub fn front_end_compiler(src: &str, cli_options: Cli) -> Result<bitbox::ir::Module> {
    console_error_panic_hook::set_once();
    let tokens = stage::lexer::Lexer.run(src);

    if let Some(DebugMode::Token) = cli_options.debug_mode() {
        for token in &tokens {
            let string = format!("{:?}", token);
            web_sys::console::log_1(&string.into());
        }
    }

    let ast = stage::parser::Parser::default().run(tokens)?;

    if let Some(DebugMode::Ast) = cli_options.debug_mode() {
        let string = format!("{:#?}", ast);
        web_sys::console::log_1(&string.into());
    }

    let mut ast = stage::monomorphize::Monomorphizer::default().run(ast)?;

    let symbol_table = stage::semantic_analyzer::SemanticAnalyzer::default().run(&mut ast)?;

    if let Some(DebugMode::SymbolTable) = cli_options.debug_mode() {
        let string = format!("{:#?}", symbol_table);
        web_sys::console::log_1(&string.into());
    }

    let module =
        stage::ir_builder::IRBuilder::new(cli_options.target()).run((symbol_table, ast))?;

    if let Some(DebugMode::Ir) = cli_options.debug_mode() {
        let string = format!("{}", module);
        web_sys::console::log_1(&string.into());
    }

    Ok(module)
}

#[cfg(feature = "wasm")]
#[wasm_bindgen]
pub fn compile_source(
    source: &str,
    cli_options: Cli,
) -> std::result::Result<js_sys::Uint8Array, JsValue> {
    let mut module = match front_end_compiler(&source, cli_options.clone()) {
        Ok(module) => module,
        Err(err) => {
            let string = format!("{:?}", err);
            web_sys::console::log_1(&string.into());
            return Err(JsValue::from_str(
                &err.report(&cli_options.file_path(), source),
            ));
        }
    };
    let compiler_debug_mode: Option<DebugPass> =
        cli_options.debug_mode().clone().and_then(Into::into);
    let mut compiler = bitbox::Compiler::new(
        &cli_options.file_path(),
        cli_options.target(),
        compiler_debug_mode,
    );
    match compiler.run(&mut module) {
        Ok(_) => {
            let CompilerResult::Wasm32(bytes) = compiler.results.unwrap_or_default() else {
                unimplemented!()
            };
            Ok(js_sys::Uint8Array::from(&bytes[..]))
        }
        Err(error) => {
            web_sys::console::log_1(&"ERROR:".into());
            Err(JsValue::from_str(&error.to_string()))
        }
    }
}

#[cfg(not(feature = "wasm"))]
pub fn front_end_compiler(cli_options: &Cli) -> Result<bitbox::ir::Module> {
    use crate::error::{Report, ScopedReport};
    use crate::stage::parser::ast::Item;
    use std::path::Path;

    let entry = Path::new(&cli_options.file_path);
    let loader = stage::module_loader::ModuleLoader::new(cli_options.unix_newlines);
    let program = loader.load(entry)?;

    if let Some(DebugMode::Token) = cli_options.debug_mode {
        for module in &program.modules {
            eprintln!("=== {} ===", module.path.display());
            for token in
                stage::lexer::Lexer.run((&module.path.to_str().unwrap_or_default(), &module.source))
            {
                eprintln!("{:?}", token);
            }
        }
        std::process::exit(0);
    }

    if let Some(DebugMode::Ast) = cli_options.debug_mode {
        for module in &program.modules {
            eprintln!("=== {} ===", module.path.display());
            eprintln!("{:#?}", module.items);
        }
        std::process::exit(0);
    }

    let entry_path = program.modules[0].path.display().to_string();
    let entry_source = program.modules[0].source.clone();

    let items: Vec<Item> = program
        .modules
        .into_iter()
        .flat_map(|module| module.items)
        .collect();

    let mut items = stage::monomorphize::Monomorphizer::default().run(items)?;

    let symbol_table = stage::semantic_analyzer::SemanticAnalyzer::default().run(&mut items)?;

    if let Some(DebugMode::SymbolTable) = cli_options.debug_mode {
        eprintln!("{:#?}", symbol_table);
        std::process::exit(0);
    }

    if let Some(DebugMode::TypeChecker) = cli_options.debug_mode {
        std::process::exit(0);
    }

    let module =
        stage::ir_builder::IRBuilder::new(cli_options.target).run((symbol_table, items))?;

    if let Some(DebugMode::Ir) = cli_options.debug_mode {
        eprintln!("{}", module);
        std::process::exit(0);
    }

    Ok(module)
}
