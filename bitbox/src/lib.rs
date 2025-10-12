pub mod backend;
pub mod error;
pub mod ir;
pub mod passes;

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
        src_path
            .chars()
            .rev()
            .collect::<String>()
            .replacen("bc.", &extension.chars().rev().collect::<String>(), 1)
            .chars()
            .rev()
            .collect()
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
}

impl Compiler {
    pub fn new(src_path: impl Into<String>, target: Target) -> Self {
        Self {
            target,
            src_path: src_path.into(),
            backend: target.backend(),
        }
    }

    pub fn run(&self, module: &mut ir::Module, ctx: &mut backend::Context) {
        for mut pass in self.backend.passes() {
            let Err(e) = pass.run(module, ctx) else {
                continue;
            };

            eprintln!("{}", e);
            std::process::exit(1);
        }

        let compiler_result = ctx.output.finish();
        let path = self.target.get_new_path(&self.src_path);
        compiler_result.save_to_file(&path);
    }
}

#[test]
fn test_compiler() {
    use crate::ir::{Instruction, Operand};
    let src_name = "test.cb";
    let compiler = Compiler::new(src_name, Target::Wasm32);

    let mut builder = crate::ir::builder::ModuleBuilder::default(); // {{{
    let mut main_function = crate::ir::builder::FunctionBuilder::new("main")
        .with_visibility(ir::Visibility::Public)
        .with_return_type(ir::Type::Unsigned(32));
    let mut assembler = main_function.assembler();

    assembler.create_block("entry");

    let condition = assembler.var(ir::Type::Unsigned(1));

    let x = assembler.var(ir::Type::Unsigned(32));
    assembler.assign(
        x.clone(),
        Operand::ConstantInt {
            value: "1".into(),
            ty: ir::Type::Unsigned(32),
        },
    );
    let y = assembler.var(ir::Type::Unsigned(32));
    assembler.assign(
        y.clone(),
        Operand::ConstantInt {
            value: "2".into(),
            ty: ir::Type::Unsigned(32),
        },
    );
    assembler.gt(condition.clone(), x.clone(), y.clone());
    let des = assembler.var(ir::Type::Unsigned(32));
    assembler.if_else(
        Some(des.clone()),
        condition,
        vec![Instruction::Assign(des.clone(), x.into())],
        vec![Instruction::Assign(des, y.into())],
    );

    builder.function(main_function.build());

    let mut module = builder.build(); // }}}

    let mut ctx = backend::Context::default();
    compiler.run(&mut module, &mut ctx);
    eprintln!("{:#?}", ctx);
    assert!(false);
}
