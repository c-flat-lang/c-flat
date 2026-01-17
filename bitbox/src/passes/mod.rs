pub mod control_flow_graph;
pub mod liveness;
pub mod local_function_variables;
pub mod lowering;

#[derive(Debug, Clone, Copy)]
pub enum DebugPass {
    LoweredIr,
    EmitWasm32,
    EmitBitbeat,
    ControlFlowGraph,
    LivenessAnalysis,
    DetectLoops,
}

pub trait Pass {
    fn run(
        &mut self,
        module: &mut crate::ir::Module,
        ctx: &mut crate::backend::Context,
    ) -> Result<(), crate::error::Error>;

    fn debug(
        &self,
        _module: &crate::ir::Module,
        _ctx: &crate::backend::Context,
        _debug_mode: Option<DebugPass>,
    ) -> bool {
        false
    }
}
