use crate::passes::DebugPass;

pub mod backend;
pub mod error;
pub mod ir;
pub mod passes;
pub mod text;

#[cfg(test)]
mod test;

// TODO: make default to be system dependent
#[repr(usize)]
#[derive(Debug, Clone, Copy, Default, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub enum Target {
    #[default]
    Wasm32,
    X86_64Linux,
    Bitbeat,
}

impl Target {
    pub fn target_specific_context(&self) -> backend::TargetSpecificContext {
        match self {
            Target::Wasm32 => backend::TargetSpecificContext::Wasm32,
            Target::X86_64Linux => {
                backend::TargetSpecificContext::X86_64Linux(inkwell::context::Context::create())
            }
            Target::Bitbeat => backend::TargetSpecificContext::Bitbeat,
        }
    }
    pub fn backend(&self) -> Box<dyn backend::Backend> {
        eprintln!("Target: {}", self);
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
    target: Target,
    src_path: String,
    backend: Box<dyn backend::Backend>,
    debug_mode: Option<DebugPass>,
}

impl Compiler {
    pub fn new(src_path: impl Into<String>, target: Target, debug_mode: Option<DebugPass>) -> Self {
        Self {
            target,
            src_path: src_path.into(),
            backend: target.backend(),
            debug_mode,
        }
    }

    pub fn run(&self, module: &mut ir::Module) -> Result<(), error::Error> {
        let csc = self.target.target_specific_context();
        let mut ctx = backend::Context::new(&self.target, &csc);
        for mut pass in self.backend.passes() {
            pass.run(module, &mut ctx)?;
            if pass.debug(&module, &ctx, self.debug_mode) {
                return Ok(());
            }
        }

        let compiler_result = ctx.output.finish();
        let path = self.target.get_new_path(&self.src_path);
        compiler_result.save_to_file(&path);
        Ok(())
    }
}
