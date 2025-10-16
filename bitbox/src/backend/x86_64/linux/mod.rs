use crate::backend::Backend;
use crate::passes::Pass;

use crate::passes::control_flow_graph::ControlFlowGraphPass;

pub struct X86_64LinuxBackend;
impl Backend for X86_64LinuxBackend {
    fn passes(&self) -> Vec<Box<dyn Pass>> {
        vec![Box::new(ControlFlowGraphPass)]
    }
}
