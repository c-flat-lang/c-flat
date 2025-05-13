use inkwell::{builder::Builder, context::Context, module::Module, values::PointerValue};
use std::collections::HashMap;

pub type CodegenResult<T> = std::result::Result<T, Box<dyn std::error::Error>>;

pub trait Codegen {
    type Output<'ctx>;
    fn codegen<'ctx>(
        &self,
        variable_context: &mut VariableContext<'ctx>,
        context: &'ctx Context,
        module: &Module<'ctx>,
        builder: &Builder<'ctx>,
    ) -> CodegenResult<Self::Output<'ctx>>;
}

pub type VariableContext<'ctx> = HashMap<String, (PointerValue<'ctx>, crate::ast::Type)>;
