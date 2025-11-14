#![allow(unused)]
use std::{collections::HashMap, path::Path, process::Command};

use inkwell::{
    targets::{InitializationConfig, TargetMachine},
    OptimizationLevel,
};

use crate::{ir::Variable, Target};

pub mod bitbeat;
pub mod wasm32;
pub mod x86_64;

#[derive(Debug)]
pub enum TargetSpecificContext {
    Wasm32,
    X86_64Linux(inkwell::context::Context),
    Bitbeat,
}

#[derive(Debug)]
pub enum Output<'ctx> {
    Wasm32(wasm32::passes::emit::Wasm32Module),
    Bitbeat(bitbeat::bitbeat::Module),
    X86_64(x86_64::linux::passes::emit_x86_64_linux::LLVMContext<'ctx>),
}

impl<'ctx> Output<'ctx> {
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

    pub fn get_x86_64(&self) -> &x86_64::linux::passes::emit_x86_64_linux::LLVMContext<'ctx> {
        match &self {
            Self::X86_64(module) => module,
            v => panic!("Not wasm32 but {:?}", v),
        }
    }

    pub fn get_mut_x86_64(
        &mut self,
    ) -> &mut x86_64::linux::passes::emit_x86_64_linux::LLVMContext<'ctx> {
        match self {
            Self::X86_64(module) => module,
            v => panic!("Not wasm32 but {:?}", v),
        }
    }

    pub fn finish(mut self) -> CompilerResult {
        match self {
            Self::Wasm32(mut module) => CompilerResult::Wasm32(module.finish()),
            Self::X86_64(ctx) => {
                // Initialize native target
                inkwell::targets::Target::initialize_native(&InitializationConfig::default())
                    .expect("Failed to initialize target");
                // Create target machine for object file generation
                let triple = TargetMachine::get_default_triple();
                let target =
                    inkwell::targets::Target::from_triple(&triple).expect("Failed to get target");
                let target_machine = target
                    .create_target_machine(
                        &triple,
                        "generic",
                        "",
                        OptimizationLevel::Default,
                        inkwell::targets::RelocMode::Default,
                        inkwell::targets::CodeModel::Default,
                    )
                    .expect("Failed to create target machine");

                // Write object file
                target_machine
                    .write_to_file(
                        &ctx.llvm_module,
                        inkwell::targets::FileType::Object,
                        Path::new("output.o"),
                    )
                    .expect("Failed to write object file");

                println!("Generated object file: output.o");

                // Link the object file into an executable using GCC
                Command::new("gcc")
                    .args(["output.o", "-o", "output", "-static"])
                    .status()
                    .expect("Failed to link executable");

                println!("Executable created: output");

                CompilerResult::X86_64
            }
            Self::Bitbeat(module) => CompilerResult::Bitbeat(module),
        }
    }

    fn new(target: &Target, csc: &'ctx TargetSpecificContext) -> Self {
        match target {
            Target::Wasm32 => Self::new_wasm32(),
            Target::X86_64Linux => Self::new_x86_64(csc),
            Target::Bitbeat => Self::new_bitbeat(),
        }
    }

    fn new_wasm32() -> Self {
        Self::Wasm32(wasm32::passes::emit::Wasm32Module::default())
    }

    fn new_x86_64(scs: &'ctx TargetSpecificContext) -> Output<'ctx> {
        let TargetSpecificContext::X86_64Linux(llvm_context) = scs else {
            panic!("Expected X86_64 compiler specific context");
        };
        Self::X86_64(x86_64::linux::passes::emit_x86_64_linux::LLVMContext::new(
            llvm_context,
            "bitbox",
        ))
    }

    fn new_bitbeat() -> Output<'ctx> {
        Self::Bitbeat(bitbeat::bitbeat::Module::new("main"))
    }
}

impl Default for Output<'_> {
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
pub struct Context<'ctx> {
    pub cfg: crate::passes::control_flow_graph::ControlFlowGraph,
    pub liveness: crate::passes::liveness::LivenessAnalysisInfo,
    pub local_function_variables: crate::passes::local_function_variables::LocalFunctionVariables,
    pub output: Output<'ctx>,
}

impl<'ctx> Context<'ctx> {
    pub(crate) fn new(target: &Target, csc: &'ctx TargetSpecificContext) -> Self {
        Self {
            output: Output::new(target, csc),
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
