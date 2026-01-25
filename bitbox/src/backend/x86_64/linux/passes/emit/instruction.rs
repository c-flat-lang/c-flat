use super::X86_64LinuxLowerContext;
use super::assembler::Reg;
use crate::backend::Lower;

use crate::backend::x86_64::linux::passes::emit::assembler::{Reg8, Reg64};
use crate::ir::instruction::{IAdd, IAssign, IJump, IJumpIf, ILt, IReturn};
use crate::ir::{Operand, Type};

impl Lower<X86_64LinuxLowerContext<'_>> for Operand {
    type Output = Reg;

    fn lower(
        &self,
        _ctx: &mut crate::backend::Context,
        target: &mut X86_64LinuxLowerContext<'_>,
    ) -> Result<Self::Output, crate::error::Error> {
        match self {
            Operand::ConstantInt(constant) => {
                let reg = target.assembler.alloc_reg::<Reg64>();

                match constant.ty {
                    Type::Signed(_) | Type::Unsigned(_) => {
                        target.assembler.mov(reg, constant.value);
                    }
                    _ => todo!("non-integer constants not implemented yet"),
                }

                Ok(reg.into())
            }

            Operand::Variable(var) => {
                let Some(reg) = target.get_reg_for_variable(&var.name) else {
                    panic!(
                        "Variable {:?} not found in {:#?}\n{:#?}",
                        var, target.function_name, target.variable_to_reg_map
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
            target.assembler.alloc_reg::<Reg64>().into()
        };

        let src = self.src.lower(ctx, target)?;
        target.assembler.mov(des, src);
        target.store_variable_to_reg(&self.des.name, des);

        Ok(())
    }
}

impl Lower<X86_64LinuxLowerContext<'_>> for IReturn {
    type Output = ();
    fn lower(
        &self,
        _ctx: &mut crate::backend::Context,
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
        Ok(())
    }
}

impl Lower<X86_64LinuxLowerContext<'_>> for IJump {
    type Output = ();
    fn lower(
        &self,
        _ctx: &mut crate::backend::Context,
        target: &mut X86_64LinuxLowerContext<'_>,
    ) -> Result<Self::Output, crate::error::Error> {
        target.assembler.jmp(&self.label);
        Ok(())
    }
}

impl Lower<X86_64LinuxLowerContext<'_>> for ILt {
    type Output = ();
    fn lower(
        &self,
        ctx: &mut crate::backend::Context,
        target: &mut X86_64LinuxLowerContext<'_>,
    ) -> Result<Self::Output, crate::error::Error> {
        let lhs = self.lhs.lower(ctx, target)?;
        let rhs = self.rhs.lower(ctx, target)?;
        target.assembler.cmp(lhs, rhs);
        let flag = target.assembler.alloc_reg::<Reg8>();
        target.assembler.setl(flag);
        let out = target.assembler.alloc_reg::<Reg64>();
        target.assembler.movezx(out, flag);
        target.store_variable_to_reg(&self.des.name, out);
        Ok(())
    }
}

impl Lower<X86_64LinuxLowerContext<'_>> for IAdd {
    type Output = ();
    fn lower(
        &self,
        ctx: &mut crate::backend::Context,
        target: &mut X86_64LinuxLowerContext<'_>,
    ) -> Result<Self::Output, crate::error::Error> {
        let lhs = self.lhs.lower(ctx, target)?;
        let rhs = self.rhs.lower(ctx, target)?;
        target.assembler.add(lhs, rhs);
        target.store_variable_to_reg(&self.des.name, lhs);
        Ok(())
    }
}
