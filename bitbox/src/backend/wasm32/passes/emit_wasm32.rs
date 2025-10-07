use crate::ir::Module;
use crate::passes::Pass;

#[derive(Debug)]
pub struct EmitWasm32Pass;

impl Pass for EmitWasm32Pass {
    fn run(
        &mut self,
        module: &mut Module,
        _ctx: &mut crate::backend::Context,
    ) -> Result<(), crate::error::Error> {
        eprintln!("EmitWasm32Pass");
        eprintln!("{}", module);
        Ok(())
    }
}
