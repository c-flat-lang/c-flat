mod passes;
use crate::backend::Backend;
use crate::passes::control_flow_graph::ControlFlowGraphPass;
use crate::passes::liveness::LivenessAnalysisPass;
use crate::passes::local_function_variables::LocalFunctionVariablesPass;
use crate::passes::lowering::LoweringPass;
use crate::passes::Pass;
pub use bitbeat;
use passes::emit::EmitBitbeatPass;

#[derive(Debug, Default)]
pub struct BitbeatBackend;

impl Backend for BitbeatBackend {
    fn passes(&self) -> Vec<Box<dyn Pass>> {
        vec![
            Box::new(LoweringPass),
            Box::new(LocalFunctionVariablesPass),
            Box::new(ControlFlowGraphPass),
            Box::new(LivenessAnalysisPass),
            Box::new(EmitBitbeatPass),
        ]
    }
}
