use super::Stage;
use crate::stage::semantic_analyzer::symbol_table::SymbolTable;
use crate::{error::CompilerError, stage::parser::ast, stage::parser::ast::Item};
use bitbox::ir::Module;
use bitbox::ir::builder::ModuleBuilder;

#[derive(Debug, Default)]
pub struct IRBuilder {}

impl IRBuilder {
    fn build_function(&mut self, _function: ast::Function, _mb: &mut ModuleBuilder) {
        todo!("build functions")
        // let ast::Function {
        //     visibility,
        //     fn_token,
        //     name,
        //     params,
        //     return_type,
        //     body,
        // } = function;
        //
        // self.symbol_table.enter_scope(&name.lexeme);
        //
        // let mut function_builder = FunctionBuilder::new(name.lexeme.clone())
        //     .with_visibility(match visibility {
        //         ast::Visibility::Public => Visibility::Public,
        //         ast::Visibility::Private => Visibility::Private,
        //     })
        //     .with_params(
        //         params
        //             .iter()
        //             .map(|param| Variable {
        //                 name: param.name.lexeme.clone(),
        //                 ty: param.ty.into_bitbox_type(),
        //                 version: 0,
        //             })
        //             .collect(),
        //     )
        //     .with_return_type(return_type.into_bitbox_type());
        //
        // let mut assembler = function_builder.assembler();
        // assembler.create_block("entry");
        //
        // self.walk_block(&mut assembler, &function.body);
        //
        // self.symbol_table.exit_scope();
        // mb.push_function(function_builder.build());
    }
}

impl Stage<(SymbolTable, Vec<Item>), Result<Module, CompilerError>> for IRBuilder {
    fn run(
        &mut self,
        (_symbol_table, ast): (SymbolTable, Vec<Item>),
    ) -> Result<Module, CompilerError> {
        let mut mb = ModuleBuilder::default();
        for item in ast {
            match item {
                Item::Function(function) => self.build_function(function, &mut mb),
                Item::Type(_) => todo!("type def"),
                Item::Use(_) => todo!("use"),
            }
        }
        Ok(mb.build())
    }
}
