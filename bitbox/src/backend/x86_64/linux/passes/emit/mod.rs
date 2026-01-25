mod assembler;
mod instruction;
use std::collections::HashMap;

use crate::backend::x86_64::linux::passes::emit::assembler::Assembler;
use crate::backend::{Context, Lower};
use crate::ir;
use crate::passes::DebugPass;

pub struct X86_64LinuxLowerContext<'ctx> {
    pub function_name: &'ctx str,
    pub assembler: Assembler,
    variable_to_reg_map: HashMap<String, assembler::Reg>,
}

impl<'ctx> X86_64LinuxLowerContext<'ctx> {
    pub fn new(function_name: &'ctx str) -> Self {
        Self {
            function_name,
            assembler: Assembler::default(),
            variable_to_reg_map: HashMap::new(),
        }
    }

    fn store_variable_to_reg(&mut self, name: impl Into<String>, des: impl Into<assembler::Reg>) {
        self.variable_to_reg_map.insert(name.into(), des.into());
    }

    fn get_reg_for_variable(&self, name: &str) -> Option<assembler::Reg> {
        self.variable_to_reg_map.get(name).copied()
    }

    fn free_variable(&mut self, name: &str) {
        let Some(reg) = self.variable_to_reg_map.remove(name) else {
            panic!(
                "COMPILER BUG: Variable {} is not in variable_to_reg_map",
                name
            );
        };
        self.assembler.free_reg(reg);
    }
}

#[derive(Debug)]
pub struct EmitX86_64LinuxPass;

impl crate::passes::Pass for EmitX86_64LinuxPass {
    fn debug(
        &self,
        _module: &crate::ir::Module,
        ctx: &crate::backend::Context,
        debug_mode: Option<DebugPass>,
    ) -> bool {
        let Some(DebugPass::EmitX86_64): Option<DebugPass> = debug_mode else {
            return false;
        };
        eprintln!("--- Dump x86_64 ---");
        eprintln!("{}", ctx.output.get_x86_64());
        true
    }

    fn run(
        &mut self,
        module: &mut ir::Module,
        ctx: &mut Context,
    ) -> Result<(), crate::error::Error> {
        eprintln!("EmitLLVMIRPass");

        if !module.functions.iter().any(|f| f.name == "main") {
            return Err(crate::error::Error::MissingMainFunction);
        }

        {
            let x86_64 = ctx.output.get_mut_x86_64();
            x86_64.push_str(".intel_syntax noprefix\n");
            x86_64.push_str(".globl main\n");
        }

        for function in module.functions.iter() {
            function.lower(ctx, &mut *self)?;
        }

        Ok(())
    }
}

impl Lower<EmitX86_64LinuxPass> for ir::Function {
    type Output = ();
    fn lower(
        &self,
        ctx: &mut crate::backend::Context,
        _backend: &mut EmitX86_64LinuxPass,
    ) -> Result<Self::Output, crate::error::Error> {
        use assembler::Reg64::*;
        let mut target = X86_64LinuxLowerContext::new(&self.name);

        // Prologue
        target.assembler.label(&self.name).push(Rbp).mov(Rbp, Rsp);

        for block in self.blocks.iter() {
            block.lower(ctx, &mut target)?;
        }

        // Epilogue
        target.assembler.mov(Rsp, Rbp).pop(Rbp).ret();

        let x86_64 = ctx.output.get_mut_x86_64();
        x86_64.push_str(target.assembler.to_string().as_str());
        Ok(())
    }
}

impl Lower<X86_64LinuxLowerContext<'_>> for ir::BasicBlock {
    type Output = ();
    fn lower(
        &self,
        ctx: &mut Context,
        target: &mut X86_64LinuxLowerContext<'_>,
    ) -> Result<Self::Output, crate::error::Error> {
        target.assembler.define_label(&self.label);

        for (instr_index, instruction) in self.instructions.iter().enumerate() {
            instruction.lower(ctx, target)?;
            let variables = instruction.get_variables();
            for var in variables {
                let is_alive =
                    ctx.liveness
                        .is_live(target.function_name, self.id, instr_index, &var);

                if !is_alive {
                    continue;
                }

                target.free_variable(&var.name);
            }
        }
        Ok(())
    }
}

impl Lower<X86_64LinuxLowerContext<'_>> for ir::Instruction {
    type Output = ();
    fn lower(
        &self,
        ctx: &mut Context,
        target: &mut X86_64LinuxLowerContext<'_>,
    ) -> Result<Self::Output, crate::error::Error> {
        match self {
            ir::Instruction::NoOp(..) => todo!(),
            ir::Instruction::Add(iadd) => iadd.lower(ctx, target)?,
            ir::Instruction::Assign(iassign) => iassign.lower(ctx, target)?,
            ir::Instruction::Alloc(..) => todo!("alloc"),
            ir::Instruction::Call(icall) => icall.lower(ctx, target)?,
            ir::Instruction::Cmp(..) => todo!("cmp"),
            ir::Instruction::Copy(..) => todo!("copy"),
            ir::Instruction::ElemGet(..) => todo!("elemget"),
            ir::Instruction::ElemSet(..) => todo!("elemset"),
            ir::Instruction::And(..) => todo!("and"),
            ir::Instruction::Or(..) => todo!("or"),
            ir::Instruction::XOr(..) => todo!("xor"),
            ir::Instruction::Gt(..) => todo!("gt"),
            ir::Instruction::Gte(..) => todo!("gte"),
            ir::Instruction::Lt(ilt) => ilt.lower(ctx, target)?,
            ir::Instruction::Jump(ijump) => ijump.lower(ctx, target)?,
            ir::Instruction::JumpIf(ijump) => ijump.lower(ctx, target)?,
            ir::Instruction::Load(..) => todo!("load"),
            ir::Instruction::Mul(..) => todo!("mul"),
            ir::Instruction::Return(ireturn) => ireturn.lower(ctx, target)?,
            ir::Instruction::Sub(..) => todo!("sub"),
            ir::Instruction::Div(..) => todo!("div"),
            ir::Instruction::Phi(..) | ir::Instruction::Loop(..) | ir::Instruction::IfElse(..) => {
                unreachable!(
                    "Lowering pass should be used before x86_64 emit pass {:?}",
                    self
                )
            }
        }
        Ok(())
    }
}
