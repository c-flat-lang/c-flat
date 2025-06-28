use super::Emitter;
use crate::ir::{Constant, Function, Import, Module};

#[derive(Debug)]
pub struct BitBeat {
    module: bitbeat::Module,
}

impl Default for BitBeat {
    fn default() -> Self {
        Self {
            module: bitbeat::Module::new("main"),
        }
    }
}

impl Emitter for BitBeat {
    fn startup(&mut self, _: &Module) {}
    fn finish(&mut self) -> Vec<u8> {
        todo!()
    }
    fn emit_import(&mut self, _import: &Import) {
        todo!()
    }

    fn emit_constant(&mut self, _constant: &Constant) {
        todo!()
    }

    fn emit_function(&mut self, function: &Function) {
        // for blocks in function.blocks.iter() {
        //     println!("{}", blocks);
        // }
        eprintln!("{}", function);
        let func = bitbeat::Function::new(&function.name).arity(function.params.len());
        self.module.add_function(func);
    }
}
