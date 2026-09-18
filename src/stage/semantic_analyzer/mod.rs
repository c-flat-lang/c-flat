use crate::DebugMode;
use crate::stage::{Stage, StageContext, StageOutput};
pub mod symbol_table;
pub mod type_check;
mod type_resolver;

use crate::error::{Report, Result, ScopedReport};
use crate::stage::module_loader::LoadedModule;
use crate::stage::semantic_analyzer::symbol_table::SymbolTable;

#[derive(Debug, Default)]
pub struct SemanticAnalyzer {}

impl SemanticAnalyzer {
    /// Analyze a whole multi-module program.
    ///
    /// Signatures from every module are collected before any bodies so that
    /// cross-module (and forward) references resolve without needing a
    /// topological order. Type checking then runs per module against the shared
    /// table, with errors scoped to the module they came from.
    pub fn analyze_program(&self, modules: &mut [LoadedModule]) -> Result<SymbolTable> {
        eprintln!("{: >30}", "SemanticAnalyzer");
        let mut builder = symbol_table::SymbolTableBuilder::default();

        for module in modules.iter() {
            builder.set_source(module.path.display().to_string(), module.source.clone());
            builder.collect_signatures(&module.items);
        }
        for module in modules.iter() {
            builder.set_source(module.path.display().to_string(), module.source.clone());
            builder.collect_bodies(&module.items);
        }
        let mut symbol_table = builder.finish()?;

        eprintln!("{: >30}", "TypeChecker");
        for module in modules.iter_mut() {
            let filename = module.path.display().to_string();
            let source = module.source.clone();
            type_check::TypeChecker::new(&mut symbol_table)
                .check(&mut module.items)
                .map_err(|err| -> Box<dyn Report> {
                    Box::new(ScopedReport::new(filename, source, err))
                })?;
        }
        Ok(symbol_table)
    }
}

pub struct SymbolTableBuilderStage;

impl Stage for SymbolTableBuilderStage {
    fn name(&self) -> &'static str {
        "Building Symbol Table"
    }
    fn debug_mode(&self) -> &'static [DebugMode] {
        &[DebugMode::SymbolTable]
    }

    fn debug(&self, ctx: &mut StageContext) -> StageOutput {
        let output = format!("{:#?}", ctx.symbol_table);
        StageOutput::Output(output)
    }

    fn run(&mut self, ctx: &mut StageContext) -> Result<()> {
        let builder = symbol_table::SymbolTableBuilder::default();
        ctx.symbol_table = Some(builder.build(&ctx.items)?);
        Ok(())
    }
}

pub struct TypeCheckerStage;

impl Stage for TypeCheckerStage {
    fn name(&self) -> &'static str {
        "Type Checking"
    }
    fn debug_mode(&self) -> &'static [DebugMode] {
        &[DebugMode::TypeChecker]
    }

    fn debug(&self, _ctx: &mut StageContext) -> StageOutput {
        StageOutput::Nothing
    }

    fn run(&mut self, ctx: &mut StageContext) -> Result<()> {
        let mut items = ctx.take_items();
        let symbol_table = ctx.symbol_table_mut()?;
        type_check::TypeChecker::new(symbol_table).check(&mut items)?;
        ctx.items = items;
        Ok(())
    }
}
