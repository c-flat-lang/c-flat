use crate::passes::{DebugPass, PassOutput};

pub mod backend;
pub mod error;
pub mod ir;
pub mod passes;
pub mod text;

#[cfg(test)]
mod test;

#[cfg(feature = "wasm")]
use wasm_bindgen::prelude::*;

// TODO: make default to be system dependent
#[cfg_attr(feature = "wasm", wasm_bindgen)]
#[repr(usize)]
#[derive(Debug, Clone, Copy, Default, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub enum Target {
    #[default]
    Wasm32,
    X86_64Linux,
    Bitbeat,
}

impl Target {
    pub fn backend(&self) -> Box<dyn backend::Backend> {
        match self {
            Target::Wasm32 => Box::new(backend::wasm32::Wasm32Backend),
            Target::X86_64Linux => Box::new(backend::x86_64::linux::X86_64LinuxBackend),
            Target::Bitbeat => Box::new(backend::bitbeat::BitbeatBackend),
        }
    }

    fn get_new_path(&self, src_path: &str) -> String {
        let extension = match self {
            Target::Wasm32 => ".wasm",
            Target::X86_64Linux => "",
            Target::Bitbeat => ".bb",
        };
        let path = src_path.rsplit_once('.').unwrap().0;
        format!("{}{}", path, extension)
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

pub struct Compiler {
    pub(crate) target: Target,
    pub(crate) src_path: String,
    pub(crate) backend: Box<dyn backend::Backend>,
    pub(crate) debug_mode: Option<DebugPass>,
    #[cfg(not(feature = "wasm"))]
    pub(crate) save_to_file: bool,
    pub results: Option<backend::CompilerResult>,
}

impl Compiler {
    pub fn new(src_path: impl Into<String>, target: Target, debug_mode: Option<DebugPass>) -> Self {
        Self {
            target,
            src_path: src_path.into(),
            backend: target.backend(),
            debug_mode,
            #[cfg(not(feature = "wasm"))]
            save_to_file: true,
            results: None,
        }
    }

    #[cfg(not(feature = "wasm"))]
    pub fn save(&mut self, save_to_file: bool) -> &mut Self {
        self.save_to_file = save_to_file;
        self
    }

    pub fn file_output_path(&self) -> String {
        self.target.get_new_path(&self.src_path)
    }

    pub fn run(&mut self, module: &mut ir::Module) -> Result<PassOutput, error::Error> {
        let mut ctx = backend::Context::new(&self.target);

        for mut pass in self.backend.passes() {
            let passoutput = pass.execute(module, &mut ctx, self.debug_mode)?;
            let PassOutput::String(_) = passoutput else {
                continue;
            };
            return Ok(passoutput);
        }

        let compiler_result = ctx.output.finish();
        self.results.replace(compiler_result.clone());
        #[cfg(not(feature = "wasm"))]
        {
            if self.save_to_file {
                let path = self.file_output_path();
                compiler_result.save_to_file(&path);
            }
        }
        Ok(PassOutput::Nothing)
    }
}
