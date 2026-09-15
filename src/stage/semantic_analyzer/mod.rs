use crate::DebugMode;
use crate::stage::{Stage, StageContext, StageOutput};
pub mod symbol_table;
pub mod type_check;
mod type_resolver;

use crate::error::Result;

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
