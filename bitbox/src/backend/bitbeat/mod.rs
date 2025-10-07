use crate::backend::Backend;
use crate::passes::Pass;

#[derive(Debug, Default)]
pub struct BitbeatBackend;

impl Backend for BitbeatBackend {
    fn passes(&self) -> Vec<Box<dyn Pass>> {
        vec![]
    }
}
