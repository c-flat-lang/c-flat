use inkwell::{builder::Builder, context::Context, module::Module};

pub type CodegenResult<T> = std::result::Result<T, Box<dyn std::error::Error>>;

pub trait Codegen {
    type Output<'ctx>;
    fn codegen<'ctx>(
        &self,
        context: &'ctx Context,
        module: &Module<'ctx>,
        builder: &Builder<'ctx>,
    ) -> CodegenResult<Self::Output<'ctx>>;
}
