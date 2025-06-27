mod beatbox;
mod wasm32;
mod x86_64_linux;
use beatbox::BitBeat;
use wasm32::Wasm32;
use x86_64_linux::X86_64Linux;

use crate::ir::{Constant, Function, Import, Module};

#[derive(Debug, Default, Clone, Copy, PartialEq, Eq)]
pub enum Target {
    #[default]
    Bitbeat,
    Wasm32,
    X86_64Linux,
}

impl std::str::FromStr for Target {
    type Err = String;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "bitbeat" => Ok(Target::Bitbeat),
            "wasm32" => Ok(Target::Wasm32),
            "x86_64-linux" => Ok(Target::X86_64Linux),
            _ => Err(format!(
                "Unknown target '{}'\noptions: bitbeat, wasm32, x86_64-linux",
                s
            )),
        }
    }
}

pub trait Emitter {
    fn emit(&mut self, module: &Module) {
        self.emit_imports(&module.imports);
        self.emit_constants(&module.constants);
        self.emit_functions(&module.functions);
    }

    fn emit_imports(&mut self, imports: &[Import]) {
        for import in imports {
            self.emit_import(import);
        }
    }
    fn emit_constants(&mut self, constants: &[Constant]) {
        for constant in constants {
            self.emit_constant(constant);
        }
    }

    fn emit_functions(&mut self, functions: &[Function]) {
        for function in functions {
            self.emit_function(function);
        }
    }

    fn emit_import(&mut self, import: &Import);
    fn emit_constant(&mut self, constant: &Constant);
    fn emit_function(&mut self, function: &Function);
}

pub struct Compiler {
    pub emitter: Box<dyn Emitter>,
}

impl Compiler {
    pub fn new(target: Target) -> Self {
        Self {
            emitter: match target {
                Target::Bitbeat => Box::new(BitBeat::default()),
                Target::Wasm32 => Box::new(Wasm32::default()),
                Target::X86_64Linux => Box::new(X86_64Linux::default()),
            },
        }
    }

    pub fn build(mut self, program: &Module) {
        self.emitter.emit(program);
    }
}
