use super::X86_64LinuxLowerContext;
use super::assembler::Reg64;
use crate::backend::Lower;

use crate::ir::instruction::{IAssign, IJump, IJumpIf, IPhi, IReturn};
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

            Operand::Variable(var) => {
                let Some(reg) = target.get_reg_for_variable(&var.name) else {
                    panic!(
                        "Variable {:?} not found in {:#?}",
                        var, target.function_name
                    );
                };
                Ok(reg)
            }

            _ => unreachable!("{self:?}"),
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
        let des = if let Some(reg) = target.get_reg_for_variable(&self.des.name) {
            reg
        } else {
            target.assembler.alloc_reg()
        };

        let src = self.src.lower(ctx, target)?;
        // if matches!(self.src, Operand::ConstantInt { .. }) {
        //     target.free_reg(src, "src, assign");
        // }
        target.assembler.mov(des, src);
        target.store_variable_to_reg(&self.des.name, des);

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

impl Lower<X86_64LinuxLowerContext<'_>> for IJumpIf {
    type Output = ();
    fn lower(
        &self,
        ctx: &mut crate::backend::Context,
        target: &mut X86_64LinuxLowerContext<'_>,
    ) -> Result<Self::Output, crate::error::Error> {
        let cond = self.cond.lower(ctx, target)?;
        target.assembler.test(cond, cond).jnz(&self.label);
        // if matches!(self.cond, Operand::ConstantInt { .. }) {
        //     target.free_reg(cond, "cond, jumpif");
        // }
        Ok(())
    }
}

impl Lower<X86_64LinuxLowerContext<'_>> for IJump {
    type Output = ();
    fn lower(
        &self,
        ctx: &mut crate::backend::Context,
        target: &mut X86_64LinuxLowerContext<'_>,
    ) -> Result<Self::Output, crate::error::Error> {
        target.assembler.jmp(&self.label);
        Ok(())
    }
}
