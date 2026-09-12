pub mod ir_builder;
pub mod lexer;
pub mod module_loader;
pub mod monomorphize;
pub mod parser;
pub mod pipelines;
pub mod semantic_analyzer;
pub mod type_interner;

use std::path::PathBuf;

use crate::{error::ErrorMessage, stage::semantic_analyzer::symbol_table::SymbolTable};
use bitbox::ir::Module;
use report::Result;

use crate::{
    DebugMode,
    stage::{
        module_loader::{LoadedModule, LoadedProgram},
        parser::ast::Item,
    },
};

pub trait Stage {
    fn name(&self) -> &'static str;
    fn debug_mode(&self) -> &'static [DebugMode];
    fn debug(&self, _ctx: &mut StageContext) -> StageOutput;
    fn run(&mut self, ctx: &mut StageContext) -> Result<()>;
    fn eprintln(&self, msg: &str) {
        eprintln!("{: >30}", msg);
    }
    fn execute(&mut self, ctx: &mut StageContext) -> Result<StageOutput> {
        if ctx.verbose {
            self.eprintln(self.name());
        }

        self.run(ctx)?;

        let output = if let Some(dm) = &ctx.debug_mode
            && self.debug_mode().contains(dm)
        {
            self.debug(ctx)
        } else {
            StageOutput::Nothing
        };

        Ok(output)
    }
}

#[derive(Debug, Default)]
pub struct StageContext {
    pub target: crate::bitbox::Target,
    pub debug_mode: Option<DebugMode>,
    pub unix_newlines: bool,
    pub verbose: bool,

    #[cfg(not(feature = "wasm"))]
    pub entry: PathBuf,

    #[cfg(feature = "wasm")]
    pub entry: String,
    #[cfg(feature = "wasm")]
    pub source: String,

    pub program: LoadedProgram,
    pub items: Vec<Item>,
    pub symbol_table: Option<SymbolTable>,
    pub module: Module,
}

impl StageContext {
    pub fn take_loaded_program_modules(&mut self) -> Vec<LoadedModule> {
        std::mem::take(&mut self.program.modules)
    }

    pub fn take_items(&mut self) -> Vec<Item> {
        std::mem::take(&mut self.items)
    }

    pub fn symbol_table_mut(&mut self) -> report::Result<&mut SymbolTable> {
        self.symbol_table.as_mut().ok_or_else(|| {
            Box::new(ErrorMessage(
                "Failed to get symbol table out of context".to_string(),
            )) as _
        })
    }

    pub fn symbol_table(&mut self) -> report::Result<SymbolTable> {
        self.symbol_table.clone().ok_or_else(|| {
            Box::new(ErrorMessage(
                "Failed to get symbol table out of context".to_string(),
            )) as _
        })
    }
}

pub enum StageOutput {
    Nothing,
    Output(String),
}
