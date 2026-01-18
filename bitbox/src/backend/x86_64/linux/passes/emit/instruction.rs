use super::X86_64LinuxLowerContext;
use super::assembler::Reg64;
use crate::backend::Lower;

use crate::ir::instruction::{IAssign, IReturn};
use crate::ir::{Operand, Type};

impl Lower<X86_64LinuxLowerContext<'_>> for Operand {
    type Output = Reg64;

    fn lower(
        &self,
        _ctx: &mut crate::backend::Context,
        target: &mut X86_64LinuxLowerContext<'_>,
    ) -> Result<Self::Output, crate::error::Error> {
        match self {
            Operand::ConstantInt { value, ty } => {
                let reg = target.assembler.alloc_reg();

                match ty {
                    Type::Signed(_) | Type::Unsigned(_) => {
                        target.assembler.mov(reg, value.parse::<i64>().unwrap());
                    }
                    _ => todo!("non-integer constants not implemented yet"),
                }

                Ok(reg)
            }

            _ => unreachable!(),
        }
    }
}

impl Lower<X86_64LinuxLowerContext<'_>> for IAssign {
    type Output = ();
    fn lower(
        &self,
        ctx: &mut crate::backend::Context,
        target: &mut X86_64LinuxLowerContext<'_>,
    ) -> Result<Self::Output, crate::error::Error> {
        let des = target.assembler.alloc_reg();
        target.store_variable_to_reg(&self.des.name, des);
        let src = self.src.lower(ctx, target)?;
        target.assembler.mov(des, src);
        Ok(())
    }
}

impl Lower<X86_64LinuxLowerContext<'_>> for IReturn {
    type Output = ();
    fn lower(
        &self,
        ctx: &mut crate::backend::Context,
        target: &mut X86_64LinuxLowerContext<'_>,
    ) -> Result<Self::Output, crate::error::Error> {
        match &self.src {
            Operand::ConstantInt { .. } => todo!("ret const"),
            Operand::Variable(var) => {
                let Some(reg) = target.get_reg_for_variable(&var.name) else {
                    panic!("Variable {:?} not found", var);
                };
                target.assembler.mov(Reg64::Rax, reg);
            }
            Operand::None => todo!("ret none"),
        }
        Ok(())
    }
}
