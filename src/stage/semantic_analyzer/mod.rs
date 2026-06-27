pub mod symbol_table;
pub mod type_check;
mod type_resolver;

use super::Stage;
use crate::error::{Report, Result, ScopedReport};
use crate::stage::module_loader::LoadedModule;
use crate::stage::parser::ast::Item;
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

impl Stage<&mut Vec<Item>, Result<SymbolTable>> for SemanticAnalyzer {
    fn run(&mut self, input: &mut Vec<Item>) -> Result<SymbolTable> {
        eprintln!("{: >30}", "SemanticAnalyzer");
        let mut symbol_table = symbol_table::SymbolTableBuilder::default().build(input)?;
        eprintln!("{: >30}", "TypeChecker");
        type_check::TypeChecker::new(&mut symbol_table).check(input)?;
        Ok(symbol_table)
    }
}
