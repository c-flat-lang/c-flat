use crate::backend::{Context, Lower};
use crate::ir;
use crate::ir::instruction::{IAssign, IReturn};

#[derive(Debug)]
pub struct EmitX86_64LinuxPass;

impl crate::passes::Pass for EmitX86_64LinuxPass {
    fn run(
        &mut self,
        module: &mut ir::Module,
        ctx: &mut Context,
    ) -> Result<(), crate::error::Error> {
        eprintln!("EmitLLVMIRPass");

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
        Ok(())
    }
}

pub struct X86_64LinuxLowerContext<'ctx> {
    pub function_name: &'ctx str,
}

impl Lower<X86_64LinuxLowerContext<'_>> for ir::BasicBlock {
    type Output = ();
    fn lower(
        &self,
        ctx: &mut Context,
        target: &mut X86_64LinuxLowerContext<'_>,
    ) -> Result<Self::Output, crate::error::Error> {
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
            ir::Instruction::Assign(..) => todo!("assign"),
            ir::Instruction::Alloc(..) => todo!("alloc"),
            ir::Instruction::Call(..) => todo!("call"),
            ir::Instruction::Cmp(..) => todo!("cmp"),
            ir::Instruction::Copy(..) => todo!("copy"),
            ir::Instruction::ElemGet(..) => todo!("elemget"),
            ir::Instruction::ElemSet(..) => todo!("elemset"),
            ir::Instruction::XOr(..) => todo!("xor"),
            ir::Instruction::Gt(..) => todo!("gt"),
            ir::Instruction::Lt(..) => todo!("lt"),
            ir::Instruction::Jump(_) => todo!("jump"),
            ir::Instruction::JumpIf(..) => todo!("jumpif"),
            ir::Instruction::Load(..) => todo!("load"),
            ir::Instruction::Mul(..) => todo!("mul"),
            ir::Instruction::Phi(..) => todo!("phi"),
            ir::Instruction::Return(..) => todo!("ret"),
            ir::Instruction::Sub(..) => todo!("sub"),
            ir::Instruction::Div(..) => todo!("div"),
            ir::Instruction::IfElse(..) => {
                unreachable!("Lowering pass should be used before llvm pass")
            }
        }
        Ok(())
    }
}

impl Lower<X86_64LinuxLowerContext<'_>> for ir::Operand {
    type Output = ();
    fn lower(
        &self,
        ctx: &mut Context,
        target: &mut X86_64LinuxLowerContext<'_>,
    ) -> Result<Self::Output, crate::error::Error> {
        Ok(())
    }
}
