pub mod symbol_table;
pub mod type_check;
mod type_resolver;

use super::Stage;
use crate::error::Result;
use crate::stage::parser::ast::Item;
use crate::stage::semantic_analyzer::symbol_table::SymbolTable;

#[derive(Debug, Default)]
pub struct SemanticAnalyzer {}

impl Stage<&mut Vec<Item>, Result<SymbolTable>> for SemanticAnalyzer {
    fn run(&mut self, input: &mut Vec<Item>) -> Result<SymbolTable> {
        eprintln!("{: >30}", "SemanticAnalyzer");
        let mut symbol_table = symbol_table::SymbolTableBuilder::default().build(input)?;
        eprintln!("{: >30}", "TypeChecker");
        type_check::TypeChecker::new(&mut symbol_table).check(input)?;
        Ok(symbol_table)
    }
}
