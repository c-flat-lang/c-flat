#![allow(unused)]
use std::collections::HashMap;

use crate::ir::Variable;

pub mod bitbeat;
pub mod wasm32;
pub mod x86_64;

#[derive(Debug)]
pub enum Output {
    Wasm32(wasm32::passes::emit_wasm32::Wasm32Module),
    Bitbeat(bitbeat::bitbeat::Module),
    X86_64(String),
}

impl Output {
    pub fn get_wasm32(&self) -> &wasm32::passes::emit_wasm32::Wasm32Module {
        match &self {
            Self::Wasm32(module) => module,
            v => panic!("Not wasm32 but {:?}", v),
        }
    }

    pub fn get_mut_wasm32(&mut self) -> &mut wasm32::passes::emit_wasm32::Wasm32Module {
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

    pub fn get_x86_64(&self) -> &String {
        match &self {
            Self::X86_64(module) => module,
            v => panic!("Not wasm32 but {:?}", v),
        }
    }

    pub fn finish(&mut self) -> CompilerResult {
        match self {
            Self::Wasm32(module) => CompilerResult::Wasm32(module.finish()),
            v => unimplemented!("{v:?}"),
        }
    }
}

impl Default for Output {
    fn default() -> Self {
        Self::Wasm32(wasm32::passes::emit_wasm32::Wasm32Module::default())
    }
}

#[derive(Debug)]
pub enum CompilerResult {
    Wasm32(Vec<u8>),
}

impl CompilerResult {
    pub fn save_to_file(&self, path: &str) {
        match self {
            Self::Wasm32(bytes) => std::fs::write(path, bytes).unwrap(),
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

pub trait Backend {
    fn passes(&self) -> Vec<Box<dyn crate::passes::Pass>>;
}

pub trait Lower {
    fn lower(&self, ctx: &mut crate::backend::Context) -> Result<(), crate::error::Error>;
}
