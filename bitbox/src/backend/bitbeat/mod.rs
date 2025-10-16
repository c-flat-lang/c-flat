use crate::backend::Backend;
use crate::passes::Pass;
pub use bitbeat;

#[derive(Debug, Default)]
pub struct BitbeatBackend;

impl Backend for BitbeatBackend {
    fn passes(&self) -> Vec<Box<dyn Pass>> {
        vec![]
    }
}
