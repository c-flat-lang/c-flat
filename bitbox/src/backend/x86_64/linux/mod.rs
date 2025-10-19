pub mod passes;
use crate::backend::Backend;
use crate::passes::control_flow_graph::ControlFlowGraphPass;
use crate::passes::liveness::LivenessAnalysisPass;
use crate::passes::local_function_variables::LocalFunctionVariablesPass;
use crate::passes::lowering::LoweringPass;
use crate::passes::Pass;
use passes::emit_x86_64_linux::EmitX86_64LinuxPass;

pub struct X86_64LinuxBackend;

impl Backend for X86_64LinuxBackend {
    fn passes(&self) -> Vec<Box<dyn Pass>> {
        vec![
            Box::new(LoweringPass),
            Box::new(LocalFunctionVariablesPass),
            Box::new(ControlFlowGraphPass),
            Box::new(LivenessAnalysisPass),
            Box::new(EmitX86_64LinuxPass),
        ]
    }
}
