pub mod control_flow_graph;
pub mod liveness;
pub mod local_function_variables;
pub mod lowering;

pub trait Pass {
    fn run(
        &mut self,
        module: &mut crate::ir::Module,
        ctx: &mut crate::backend::Context,
    ) -> Result<(), crate::error::Error>;
}
