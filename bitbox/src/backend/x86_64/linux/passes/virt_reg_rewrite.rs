pub struct VirtRegRewritePass;

impl crate::passes::Pass for VirtRegRewritePass {
    fn debug(
        &self,
        _module: &crate::ir::Module,
        _ctx: &crate::backend::Context,
        _debug_mode: Option<crate::passes::DebugPass>,
    ) -> bool {
        false
    }

    fn run(
        &mut self,
        _module: &mut crate::ir::Module,
        ctx: &mut crate::backend::Context,
    ) -> Result<(), crate::error::Error> {
        let x86_64 = ctx.output.get_mut_x86_64();
        for f in x86_64.functions.iter_mut() {
            for i in f.instructions.iter_mut() {
                eprintln!("{i}");
            }
        }
        Ok(())
    }
}
