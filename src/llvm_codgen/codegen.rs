use inkwell::{builder::Builder, context::Context, module::Module};

pub trait Codegen {
    fn codegen<'ctx>(&self, context: &'ctx Context, module: &Module<'ctx>, builder: &Builder<'ctx>);
}
