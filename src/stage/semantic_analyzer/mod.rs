pub mod symbol_table;
pub mod type_check;

use super::Stage;
use crate::error::Result;
use crate::stage::parser::ast::Item;
use crate::stage::semantic_analyzer::symbol_table::SymbolTable;

#[derive(Debug, Default)]
pub struct SemanticAnalyzer {}

impl Stage<&mut Vec<Item>, Result<SymbolTable>> for SemanticAnalyzer {
    fn run(&mut self, input: &mut Vec<Item>) -> Result<SymbolTable> {
        let mut symbol_table = symbol_table::SymbolTableBuilder::default().build(input)?;
        type_check::TypeChecker::new(&mut symbol_table).check(input)?;
        Ok(symbol_table)
    }
}
