mod assembler;
mod instruction;
use std::collections::HashMap;

use crate::backend::x86_64::linux::passes::emit::assembler::Assembler;
use crate::backend::{Context, Lower};
use crate::ir;
use crate::ir::instruction::{IAssign, IReturn};
use crate::passes::DebugPass;

pub struct X86_64LinuxLowerContext<'ctx> {
    pub function_name: &'ctx str,
    pub assembler: Assembler,
    variable_to_reg_map: HashMap<String, assembler::Reg64>,
}

impl<'ctx> X86_64LinuxLowerContext<'ctx> {
    pub fn new(function_name: &'ctx str) -> Self {
        Self {
            function_name,
            assembler: Assembler::default(),
            variable_to_reg_map: HashMap::new(),
        }
    }

    fn store_variable_to_reg(&mut self, name: impl Into<String>, des: assembler::Reg64) {
        self.variable_to_reg_map.insert(name.into(), des);
    }

    fn get_reg_for_variable(&self, name: &str) -> Option<assembler::Reg64> {
        self.variable_to_reg_map.get(name).copied()
    }
}

#[derive(Debug)]
pub struct EmitX86_64LinuxPass;

impl crate::passes::Pass for EmitX86_64LinuxPass {
    fn debug(
        &self,
        module: &crate::ir::Module,
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

        if module.functions.iter().find(|f| f.name == "main").is_none() {
            return Err(crate::error::Error::MissingMainFunction);
        }

        {
            let x86_64 = ctx.output.get_mut_x86_64();
            x86_64.push_str(&format!(".intel_syntax noprefix\n"));
            x86_64.push_str(&format!(".globl main\n"));
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
        backend: &mut EmitX86_64LinuxPass,
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
        for instruction in self.instructions.iter() {
            instruction.lower(ctx, target)?;
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
            ir::Instruction::Add(..) => todo!("add"),
            ir::Instruction::Assign(iassign) => iassign.lower(ctx, target)?,
            ir::Instruction::Alloc(..) => todo!("alloc"),
            ir::Instruction::Call(..) => todo!("call"),
            ir::Instruction::Cmp(..) => todo!("cmp"),
            ir::Instruction::Copy(..) => todo!("copy"),
            ir::Instruction::ElemGet(..) => todo!("elemget"),
            ir::Instruction::ElemSet(..) => todo!("elemset"),
            ir::Instruction::And(..) => todo!("and"),
            ir::Instruction::Or(..) => todo!("or"),
            ir::Instruction::XOr(..) => todo!("xor"),
            ir::Instruction::Gt(..) => todo!("gt"),
            ir::Instruction::Gte(..) => todo!("gte"),
            ir::Instruction::Lt(..) => todo!("lt"),
            ir::Instruction::Jump(ijump) => ijump.lower(ctx, target)?,
            ir::Instruction::JumpIf(ijump) => ijump.lower(ctx, target)?,
            ir::Instruction::Load(..) => todo!("load"),
            ir::Instruction::Mul(..) => todo!("mul"),
            ir::Instruction::Phi(iphi) => {
                unreachable!("Lowering pass should be used before emit x86_64 pass")
            }
            ir::Instruction::Return(ireturn) => ireturn.lower(ctx, target)?,
            ir::Instruction::Sub(..) => todo!("sub"),
            ir::Instruction::Div(..) => todo!("div"),
            ir::Instruction::Loop(..) | ir::Instruction::IfElse(..) => {
                unreachable!("Lowering pass should be used before llvm pass")
            }
        }
        Ok(())
    }
}
