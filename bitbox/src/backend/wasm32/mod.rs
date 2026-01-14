pub mod passes;
use crate::backend::Backend;
use crate::backend::wasm32::passes::EmitWasm32Pass;
use crate::passes::Pass;
use crate::passes::control_flow_graph::ControlFlowGraphPass;
use crate::passes::detect_loops::DetectLoopsPass;
use crate::passes::liveness::LivenessAnalysisPass;
use crate::passes::local_function_variables::LocalFunctionVariablesPass;

pub struct Wasm32Backend;

impl Backend for Wasm32Backend {
    fn passes(&self) -> Vec<Box<dyn Pass>> {
        vec![
            Box::new(LocalFunctionVariablesPass),
            Box::new(ControlFlowGraphPass),
            Box::new(LivenessAnalysisPass),
            Box::new(DetectLoopsPass),
            Box::new(EmitWasm32Pass),
        ]
    }
}
