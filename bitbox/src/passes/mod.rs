pub mod control_flow_graph;
pub mod liveness;
pub mod local_function_variables;
pub mod lowering;
pub mod phi_node_elimination;
pub mod structuring;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum DebugPass {
    ControlFlowGraph,
    DetectLoops,
    Emit,
    LivenessAnalysis,
    LocalFunctionVariables,
    LoweredIr,
    PhiNodeElimination,
    VirtRegRewrite,
    StructuringIr,
}

#[derive(Debug)]
pub enum PassOutput {
    Nothing,
    String(String),
}

pub trait Pass {
    fn debug_pass(&self) -> DebugPass;
    fn run(
        &mut self,
        module: &mut crate::ir::Module,
        ctx: &mut crate::backend::Context,
    ) -> Result<(), crate::error::Error>;

    fn debug(&self, _module: &crate::ir::Module, _ctx: &crate::backend::Context) -> PassOutput;

    fn execute(
        &mut self,
        module: &mut crate::ir::Module,
        ctx: &mut crate::backend::Context,
        debug_mode: Option<DebugPass>,
    ) -> Result<PassOutput, crate::error::Error> {
        eprintln!("{: >30}", format!("{:?}", self.debug_pass()));

        self.run(module, ctx)?;

        let output = if let Some(dm) = debug_mode
            && dm == self.debug_pass()
        {
            self.debug(module, ctx)
        } else {
            PassOutput::Nothing
        };

        Ok(output)
    }
}
