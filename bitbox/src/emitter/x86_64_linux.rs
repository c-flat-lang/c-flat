use super::Emitter;
use crate::ir::{Constant, Function, Import};

#[derive(Debug)]
pub struct X86_64Linux;

impl Default for X86_64Linux {
    fn default() -> Self {
        Self
    }
}

impl Emitter for X86_64Linux {
    fn emit_import(&mut self, _import: &Import) {
        todo!("import")
    }

    fn emit_constant(&mut self, _constant: &Constant) {
        todo!("constant")
    }

    fn emit_function(&mut self, _function: &Function) {
        todo!("function")
    }
}
