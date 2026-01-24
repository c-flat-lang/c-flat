pub mod passes;
use crate::backend::Backend;
use crate::passes::Pass;
use crate::passes::control_flow_graph::ControlFlowGraphPass;
use crate::passes::liveness::LivenessAnalysisPass;
use crate::passes::local_function_variables::LocalFunctionVariablesPass;
use crate::passes::lowering::LoweringPass;
use crate::passes::phi_node_elimination::PhiNodeEliminationPass;
use passes::emit::EmitX86_64LinuxPass;

pub struct X86_64LinuxBackend;

impl Backend for X86_64LinuxBackend {
    fn passes(&self) -> Vec<Box<dyn Pass>> {
        vec![
            Box::new(LocalFunctionVariablesPass),
            Box::new(ControlFlowGraphPass),
            Box::new(LivenessAnalysisPass),
            Box::new(LoweringPass),
            Box::new(PhiNodeEliminationPass),
            Box::new(EmitX86_64LinuxPass),
        ]
    }
}
