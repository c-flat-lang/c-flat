pub mod bitbeat;
pub mod wasm32;
pub mod x86_64;

#[derive(Debug, Default)]
pub struct Context {
    pub cfg: crate::passes::control_flow_graph::ControlFlowGraph,
    pub liveness: crate::passes::liveness::LivenessAnalysisInfo,
}

pub trait Backend {
    fn passes(&self) -> Vec<Box<dyn crate::passes::Pass>>;
}
