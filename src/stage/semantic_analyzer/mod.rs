pub mod symbol_table;
pub mod type_check;

use super::Stage;
use crate::error::CompilerError;
use crate::stage::parser::ast::Item;
use crate::stage::semantic_analyzer::symbol_table::SymbolTable;

#[derive(Debug, Default)]
pub struct SemanticAnalyzer {}

impl Stage<&mut Vec<Item>, Result<SymbolTable, CompilerError>> for SemanticAnalyzer {
    fn run(&mut self, input: &mut Vec<Item>) -> Result<SymbolTable, CompilerError> {
        let mut symbol_table = symbol_table::SymbolTableBuilder::new().build(&input)?;
        type_check::TypeChecker::new(&mut symbol_table).check(input)?;
        Ok(symbol_table)
    }
}
