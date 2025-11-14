pub mod passes;
use crate::backend::wasm32::passes::EmitWasm32Pass;
use crate::backend::Backend;
use crate::passes::control_flow_graph::ControlFlowGraphPass;
use crate::passes::liveness::LivenessAnalysisPass;
use crate::passes::local_function_variables::LocalFunctionVariablesPass;
use crate::passes::Pass;

pub struct Wasm32Backend;

impl Backend for Wasm32Backend {
    fn passes(&self) -> Vec<Box<dyn Pass>> {
        vec![
            Box::new(LocalFunctionVariablesPass),
            Box::new(ControlFlowGraphPass),
            Box::new(LivenessAnalysisPass),
            Box::new(EmitWasm32Pass),
        ]
    }
}
