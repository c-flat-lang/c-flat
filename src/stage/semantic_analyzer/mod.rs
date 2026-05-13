pub mod symbol_table;
pub mod type_check;
mod type_resolver;

use bitbox::Target;

use super::Stage;
use crate::error::Result;
use crate::stage::parser::ast::Item;
use crate::stage::semantic_analyzer::symbol_table::SymbolTable;
use type_resolver::TypeResolver;

#[derive(Debug)]
pub struct SemanticAnalyzer {
    target: Target,
}

impl SemanticAnalyzer {
    pub fn new(target: Target) -> Self {
        Self { target }
    }
    fn calculate_target_pointer_size(&self) -> u8 {
        match self.target {
            Target::Wasm32 => 32,
            Target::X86_64Linux => 64,
            Target::Bitbeat => 64,
        }
    }
}

impl Stage<&mut Vec<Item>, Result<SymbolTable>> for SemanticAnalyzer {
    fn run(&mut self, input: &mut Vec<Item>) -> Result<SymbolTable> {
        eprintln!("{: >30}", "SemanticAnalyzer");
        let mut symbol_table = symbol_table::SymbolTableBuilder::default().build(input)?;

        eprintln!("{: >30}", "TypeResolver");
        let target_pointer_size = self.calculate_target_pointer_size();
        let resolver = TypeResolver::new(&mut symbol_table, target_pointer_size);
        resolver.walk_items(input)?;

        eprintln!("{: >30}", "TypeChecker");
        type_check::TypeChecker::new(&mut symbol_table).check(input, target_pointer_size)?;
        Ok(symbol_table)
    }
}
