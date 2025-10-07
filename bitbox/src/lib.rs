pub mod backend;
pub mod error;
pub mod ir;
pub mod passes;

#[cfg(test)]
mod test;

#[repr(usize)]
#[derive(Debug, Clone, Copy)]
pub enum Target {
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
}

pub struct Compiler {
    backend: Box<dyn backend::Backend>,
}

impl Compiler {
    pub fn new(target: Target) -> Self {
        Self {
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
    }
}

#[test]
fn test_compiler() {
    use crate::ir::{Instruction, Operand};
    let compiler = Compiler::new(Target::Wasm32);

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
