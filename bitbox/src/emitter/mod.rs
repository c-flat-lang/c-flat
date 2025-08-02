mod bitbeat;
mod wasm32;
mod x86_64_linux;
use bitbeat::BitBeat;
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

impl Target {
    pub fn file_extension(&self) -> &'static str {
        match self {
            Target::Bitbeat => "bb",
            Target::Wasm32 => "wasm",
            Target::X86_64Linux => "a",
        }
    }
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

impl std::fmt::Display for Target {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(match self {
            Target::Bitbeat => "bitbeat",
            Target::Wasm32 => "wasm32",
            Target::X86_64Linux => "x86_64-linux",
        })
    }
}

pub trait Emitter {
    fn startup(&mut self, _: &Module) {}
    fn to_bytes(&mut self) -> Vec<u8>;

    fn emit(&mut self, module: &Module) -> Vec<u8> {
        self.startup(module);
        self.emit_imports(&module.imports);
        self.emit_constants(&module.constants);
        self.emit_functions(&module.functions);
        self.to_bytes()
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

    pub fn build(mut self, program: &Module) -> Vec<u8> {
        self.emitter.emit(program)
    }
}
