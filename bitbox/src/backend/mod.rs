#![allow(unused)]
use std::{collections::HashMap, path::Path, process::Command};

use crate::{ir::Variable, Target};

pub mod bitbeat;
pub mod wasm32;
pub mod x86_64;

#[derive(Debug)]
pub enum Output {
    Wasm32(wasm32::passes::emit::Wasm32Module),
    Bitbeat(bitbeat::bitbeat::Module),
    X86_64(String),
}

impl<'ctx> Output {
    pub fn get_wasm32(&self) -> &wasm32::passes::emit::Wasm32Module {
        match &self {
            Self::Wasm32(module) => module,
            v => panic!("Not wasm32 but {:?}", v),
        }
    }

    pub fn get_mut_wasm32(&mut self) -> &mut wasm32::passes::emit::Wasm32Module {
        match self {
            Self::Wasm32(module) => module,
            v => panic!("Not wasm32 but {:?}", v),
        }
    }

    pub fn get_bitbeat(&self) -> &bitbeat::bitbeat::Module {
        match &self {
            Self::Bitbeat(module) => module,
            v => panic!("Not wasm32 but {:?}", v),
        }
    }

    pub fn get_mut_bitbeat(&mut self) -> &mut bitbeat::bitbeat::Module {
        match self {
            Self::Bitbeat(module) => module,
            v => panic!("Not wasm32 but {:?}", v),
        }
    }

    pub fn get_x86_64(&self) -> &str {
        match &self {
            Self::X86_64(module) => module,
            v => panic!("Not wasm32 but {:?}", v),
        }
    }

    pub fn get_mut_x86_64(&mut self) -> &mut String {
        match self {
            Self::X86_64(module) => module,
            v => panic!("Not wasm32 but {:?}", v),
        }
    }

    pub fn finish(mut self) -> CompilerResult {
        match self {
            Self::Wasm32(mut module) => CompilerResult::Wasm32(module.finish()),
            Self::X86_64(..) => CompilerResult::X86_64,
            Self::Bitbeat(module) => CompilerResult::Bitbeat(module),
        }
    }

    fn new(target: &Target) -> Self {
        match target {
            Target::Wasm32 => Self::new_wasm32(),
            Target::X86_64Linux => Self::new_x86_64(),
            Target::Bitbeat => Self::new_bitbeat(),
        }
    }

    fn new_wasm32() -> Self {
        Self::Wasm32(wasm32::passes::emit::Wasm32Module::default())
    }

    fn new_x86_64() -> Output {
        Self::X86_64(String::new())
    }

    fn new_bitbeat() -> Output {
        Self::Bitbeat(bitbeat::bitbeat::Module::new("main"))
    }
}

impl Default for Output {
    fn default() -> Self {
        Self::Wasm32(wasm32::passes::emit::Wasm32Module::default())
    }
}

#[derive(Debug)]
pub enum CompilerResult {
    Wasm32(Vec<u8>),
    X86_64,
    Bitbeat(bitbeat::bitbeat::Module),
}
impl Default for CompilerResult {
    fn default() -> Self {
        Self::Wasm32(Vec::new())
    }
}

impl CompilerResult {
    pub fn save_to_file(&self, path: &str) {
        match self {
            Self::Wasm32(bytes) => std::fs::write(path, bytes).unwrap(),
            Self::X86_64 => {
                eprintln!("save to file is not implemented for X86_64");
            }
            Self::Bitbeat(module) => module.save_to_file(path),
        }
    }
}

#[derive(Debug, Default)]
pub struct Context {
    pub cfg: crate::passes::control_flow_graph::ControlFlowGraph,
    pub liveness: crate::passes::liveness::LivenessAnalysisInfo,
    pub local_function_variables: crate::passes::local_function_variables::LocalFunctionVariables,
    pub output: Output,
}

impl Context {
    pub(crate) fn new(target: &Target) -> Self {
        Self {
            output: Output::new(target),
            ..Default::default()
        }
    }
}

pub trait Backend {
    fn passes(&self) -> Vec<Box<dyn crate::passes::Pass>>;
}

pub trait Lower<T> {
    type Output;
    fn lower(
        &self,
        ctx: &mut crate::backend::Context,
        t: &mut T,
    ) -> Result<Self::Output, crate::error::Error>;
}
