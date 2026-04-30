use super::X86_64LinuxLowerContext;
use crate::backend::Lower;

use crate::backend::x86_64::linux::passes::emit::assembler::{
    Location, Reg, Reg8, Reg16, Reg32, Reg64, RegKind, Stack, XmmReg,
};
use crate::backend::x86_64::linux::passes::emit::error::Error;
use crate::ir::instruction::{
    IAdd, IAlloc, IAnd, IAssign, ICall, ICmp, IDiv, IElemGet, IElemSet, IGt, IGte, IJump, IJumpIf,
    ILt, IMul, INot, IRef, IRem, IReturn, ISub,
};
use crate::ir::{Operand, Type};

impl Lower<X86_64LinuxLowerContext<'_>> for Operand {
    type Output = Location;

    fn lower(
        &self,
        _ctx: &mut crate::backend::Context,
        target: &mut X86_64LinuxLowerContext<'_>,
    ) -> Result<Self::Output, crate::error::Error> {
        target.assembler.comment("lowering operand");
        match self {
            Operand::ConstantInt(constant) => match constant.ty {
                Type::Signed(_) | Type::Unsigned(_) => {
                    let reg = target.assembler.alloc.vreg::<Reg64>();
                    target.assembler.mov(reg, constant.parse::<i64>()?);
                    Ok(reg.into())
                }
                Type::Float(32) => {
                    let gp = target.assembler.alloc.vreg::<Reg32>();
                    target
                        .assembler
                        .mov(gp, constant.parse::<f32>()?.to_bits() as i64);
                    let xmm = target.assembler.alloc.vreg::<XmmReg>();
                    target.assembler.movd(xmm, gp);
                    Ok(xmm.into())
                }
                _ => todo!(
                    "non-integer {:?} constants not implemented yet",
                    constant.ty
                ),
            },

            Operand::Variable(variable) => {
                let Some(location) = target.assembler.alloc.get_variable_location(variable) else {
                    return Err(crate::error::Error::X86_64AssemblyError(
                        Error::UndefinedVariable {
                            variable: Box::new(variable.clone()),
                            function_name: Box::new(target.function_name.to_string()),
                            block_id: target.block_id,
                            instruction_index: target.instr_index,
                            note: Box::new(format!(
                                "{}:{}:{} operand variable not found\n{:#?}",
                                file!(),
                                line!(),
                                column!(),
                                target.assembler.alloc
                            )),
                        },
                    ));
                };
                Ok(location)
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
        target.assembler.comment("lowering assign");
        match &self.src {
            // TODO: Check if old source drops reference to variable location.
            // Arrays and pointers are aliased — no value copy needed.
            Operand::Variable(src_var)
                if src_var.ty.is_ptr() || matches!(self.des.ty, crate::ir::Type::Array(..)) =>
            {
                target.assembler.alloc.alias_variable(&self.des, src_var);
            }
            Operand::Variable(_) if self.des.temporary => {
                let src_loc = self.src.lower(ctx, target)?;
                let des = if let Some(des) = target.assembler.alloc.get_variable_location(&self.des)
                {
                    des
                } else {
                    match &self.des.ty {
                        Type::Float(_) => target.assembler.alloc.vreg::<XmmReg>().into(),
                        _ => target.assembler.alloc.vreg::<Reg64>().into(),
                    }
                };

                match &self.des.ty {
                    Type::Float(32) => {
                        if is_gp_loc(&src_loc) {
                            // des is an XmmReg vreg here (temporary), so cvtsi2ss is valid
                            target.assembler.cvtsi2ss(des.clone(), src_loc)
                        } else {
                            target.assembler.movss(des.clone(), src_loc)
                        }
                    }
                    Type::Float(64) => {
                        if is_gp_loc(&src_loc) {
                            target.assembler.cvtsi2sd(des.clone(), src_loc)
                        } else {
                            target.assembler.movsd(des.clone(), src_loc)
                        }
                    }
                    _ => target.assembler.mov(des.clone(), src_loc),
                };
                target.assembler.alloc.store_variable(&self.des, des);
            }
            _ => {
                let des_reg: Location =
                    if let Some(loc) = target.assembler.alloc.get_variable_location(&self.des) {
                        loc
                    } else if !self.des.temporary {
                        target.assembler.alloc.alloc_stack(&self.des.ty, 1).into()
                    } else {
                        match &self.des.ty {
                            Type::Float(_) => target.assembler.alloc.vreg::<XmmReg>().into(),
                            _ => target.assembler.alloc.vreg::<Reg64>().into(),
                        }
                    };
                let src_loc: Location = self.src.lower(ctx, target)?;

                match &self.des.ty {
                    Type::Float(32) => {
                        if is_gp_loc(&src_loc) {
                            // cvtsi2ss requires an XMM destination — use a temp vreg then store
                            let tmp = target.assembler.alloc.vreg::<XmmReg>();
                            target.assembler.cvtsi2ss(tmp, src_loc);
                            target.assembler.movss(des_reg.clone(), tmp);
                        } else {
                            target.assembler.movss(des_reg.clone(), src_loc);
                        }
                    }
                    Type::Float(64) => {
                        if is_gp_loc(&src_loc) {
                            let tmp = target.assembler.alloc.vreg::<XmmReg>();
                            target.assembler.cvtsi2sd(tmp, src_loc);
                            target.assembler.movsd(des_reg.clone(), tmp);
                        } else {
                            target.assembler.movsd(des_reg.clone(), src_loc);
                        }
                    }
                    _ => {
                        target.assembler.mov(des_reg.clone(), src_loc);
                    }
                };
                target.assembler.alloc.store_variable(&self.des, des_reg);
            }
        }

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
        target.assembler.comment("lowering return");

        match &self.src {
            Operand::ConstantInt { .. } => todo!("ret const"),
            Operand::Variable(var) => {
                let Some(reg) = target.assembler.alloc.get_variable_location(var) else {
                    panic!("Variable {:?} not found", var);
                };
                match &var.ty {
                    Type::Float(32) => {
                        target.assembler.movss(Reg::Xmm(XmmReg::Xmm0), reg);
                    }
                    Type::Float(64) => {
                        target.assembler.movsd(Reg::Xmm(XmmReg::Xmm0), reg);
                    }
                    _ => {
                        match Stack::access_size(&var.ty) {
                            1 => target.assembler.mov(Reg8::Al, reg),
                            2 => target.assembler.mov(Reg16::Ax, reg),
                            4 => target.assembler.mov(Reg32::Eax, reg),
                            8 => target.assembler.mov(Reg64::Rax, reg),
                            _ => unreachable!(),
                        };
                    }
                };
            }
            Operand::None => todo!("ret none"),
        }
        target
            .assembler
            .jmp(format!("exit_{}", target.function_name));
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
        target.assembler.comment("lowering jump if");
        let cond = self.cond.lower(ctx, target)?;
        target
            .assembler
            .test(cond.clone(), cond.clone())
            .jnz(format!("{}_{}", target.function_name, &self.label));

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
        target.assembler.comment("lowering jump if");
        target
            .assembler
            .jmp(format!("{}_{}", target.function_name, &self.label));

        Ok(())
    }
}

impl Lower<X86_64LinuxLowerContext<'_>> for IGt {
    type Output = ();
    fn lower(
        &self,
        ctx: &mut crate::backend::Context,
        target: &mut X86_64LinuxLowerContext<'_>,
    ) -> Result<Self::Output, crate::error::Error> {
        target.assembler.comment("lowering gt");
        let lhs = self.lhs.lower(ctx, target)?;
        let rhs = self.rhs.lower(ctx, target)?;

        target.assembler.cmp(lhs, rhs);
        let flag = target.assembler.alloc.vreg::<Reg8>();

        target.assembler.setg(flag);
        let out = target.assembler.alloc.vreg::<Reg64>();
        target.assembler.movezx(out, flag);
        target.assembler.alloc.store_variable(&self.des, out);

        Ok(())
    }
}

impl Lower<X86_64LinuxLowerContext<'_>> for IGte {
    type Output = ();
    fn lower(
        &self,
        ctx: &mut crate::backend::Context,
        target: &mut X86_64LinuxLowerContext<'_>,
    ) -> Result<Self::Output, crate::error::Error> {
        target.assembler.comment("lowering gt");
        let lhs = self.lhs.lower(ctx, target)?;
        let rhs = self.rhs.lower(ctx, target)?;

        target.assembler.cmp(lhs, rhs);
        let flag = target.assembler.alloc.vreg::<Reg8>();

        target.assembler.setge(flag);
        let out = target.assembler.alloc.vreg::<Reg64>();
        target.assembler.movezx(out, flag);
        target.assembler.alloc.store_variable(&self.des, out);

        Ok(())
    }
}

impl Lower<X86_64LinuxLowerContext<'_>> for IRem {
    type Output = ();

    fn lower(
        &self,
        ctx: &mut crate::backend::Context,
        target: &mut X86_64LinuxLowerContext<'_>,
    ) -> Result<Self::Output, crate::error::Error> {
        target.assembler.comment("lowering rem");

        let lhs = self.lhs.lower(ctx, target)?;
        let rhs = self.rhs.lower(ctx, target)?;

        match &self.des.ty {
            Type::Unsigned(bits) => todo!("@rem u{bits}"),
            Type::Signed(_) => {
                // cqo/idiv require physical rax and rdx
                target.assembler.mov(Reg64::Rax, lhs);
                target.assembler.cqo();
                // idiv divides rdx:rax by src; remainder → rdx
                target.assembler.idiv(rhs);
                // capture rdx (remainder) into a vreg for the destination
                let result = target.assembler.alloc.vreg::<Reg64>();
                target.assembler.mov(result, Reg64::Rdx);
                target.assembler.alloc.store_variable(&self.des, result);
            }
            Type::Float(bits) => todo!("@rem f{bits}"),
            ty => panic!("Remainder not supported for type {:?}", ty),
        }

        Ok(())
    }
}

impl Lower<X86_64LinuxLowerContext<'_>> for IDiv {
    type Output = ();

    fn lower(
        &self,
        ctx: &mut crate::backend::Context,
        target: &mut X86_64LinuxLowerContext<'_>,
    ) -> Result<Self::Output, crate::error::Error> {
        target.assembler.comment("lowering div");

        let lhs = self.lhs.lower(ctx, target)?;
        let rhs = self.rhs.lower(ctx, target)?;

        match &self.des.ty {
            Type::Signed(_) => {
                // cqo sign-extends rax into rdx:rax; idiv src: quotient → rax, remainder → rdx
                target.assembler.mov(Reg64::Rax, lhs);
                target.assembler.cqo();
                target.assembler.idiv(rhs);
                let result = target.assembler.alloc.vreg::<Reg64>();
                target.assembler.mov(result, Reg64::Rax);
                target.assembler.alloc.store_variable(&self.des, result);
            }
            Type::Float(32) => {
                let lhs_xmm = target.assembler.alloc.vreg::<XmmReg>();
                target.assembler.movss(lhs_xmm, lhs);
                target.assembler.divss(lhs_xmm, rhs);
                target
                    .assembler
                    .alloc
                    .store_variable(&self.des, Location::Reg(lhs_xmm));
            }
            Type::Float(64) => {
                let lhs_xmm = target.assembler.alloc.vreg::<XmmReg>();
                target.assembler.movsd(lhs_xmm, lhs);
                target.assembler.divsd(lhs_xmm, rhs);
                target
                    .assembler
                    .alloc
                    .store_variable(&self.des, Location::Reg(lhs_xmm));
            }
            Type::Unsigned(bits) => todo!("@div u{bits}"),
            ty => panic!("Division not supported for type {:?}", ty),
        }

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
        target.assembler.comment("lowering lt");
        let lhs = self.lhs.lower(ctx, target)?;
        let rhs = self.rhs.lower(ctx, target)?;

        target.assembler.cmp(lhs.clone(), rhs.clone());
        let flag = target.assembler.alloc.vreg::<Reg8>();

        target.assembler.setl(flag);
        let out = target.assembler.alloc.vreg::<Reg64>();
        target.assembler.movezx(out, flag);
        target.assembler.alloc.store_variable(&self.des, out);

        Ok(())
    }
}

impl Lower<X86_64LinuxLowerContext<'_>> for IMul {
    type Output = ();
    fn lower(
        &self,
        ctx: &mut crate::backend::Context,
        target: &mut X86_64LinuxLowerContext<'_>,
    ) -> Result<Self::Output, crate::error::Error> {
        target.assembler.comment("lowering mul");
        let lhs = self.lhs.lower(ctx, target)?;
        let rhs = self.rhs.lower(ctx, target)?;

        match &self.des.ty {
            Type::Float(32) => {
                let lhs_xmm = target.assembler.alloc.vreg::<XmmReg>();
                target.assembler.movss(lhs_xmm, lhs);
                target.assembler.mulss(lhs_xmm, rhs);
                target
                    .assembler
                    .alloc
                    .store_variable(&self.des, Location::Reg(lhs_xmm));
            }
            Type::Float(64) => {
                let lhs_xmm = target.assembler.alloc.vreg::<XmmReg>();
                target.assembler.movsd(lhs_xmm, lhs);
                target.assembler.mulsd(lhs_xmm, rhs);
                target
                    .assembler
                    .alloc
                    .store_variable(&self.des, Location::Reg(lhs_xmm));
            }
            _ => {
                let lhs_reg = target.assembler.materialize_value(&lhs);
                target.assembler.imul(lhs_reg, rhs);
                target
                    .assembler
                    .alloc
                    .store_variable(&self.des, Location::Reg(lhs_reg));
            }
        }

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
        target.assembler.comment("lowering add");
        let lhs = self.lhs.lower(ctx, target)?;
        let rhs = self.rhs.lower(ctx, target)?;

        match &self.des.ty {
            Type::Float(32) => {
                let lhs_xmm = target.assembler.alloc.vreg::<XmmReg>();
                target.assembler.movss(lhs_xmm, lhs);
                target.assembler.addss(lhs_xmm, rhs);
                target
                    .assembler
                    .alloc
                    .store_variable(&self.des, Location::Reg(lhs_xmm));
            }
            Type::Float(64) => {
                let lhs_xmm = target.assembler.alloc.vreg::<XmmReg>();
                target.assembler.movsd(lhs_xmm, lhs);
                target.assembler.addsd(lhs_xmm, rhs);
                target
                    .assembler
                    .alloc
                    .store_variable(&self.des, Location::Reg(lhs_xmm));
            }
            _ => {
                let lhs_reg = target.assembler.materialize_value(&lhs);
                target.assembler.add(lhs_reg, rhs);
                target
                    .assembler
                    .alloc
                    .store_variable(&self.des, Location::Reg(lhs_reg));
            }
        }

        Ok(())
    }
}

impl Lower<X86_64LinuxLowerContext<'_>> for ISub {
    type Output = ();
    fn lower(
        &self,
        ctx: &mut crate::backend::Context,
        target: &mut X86_64LinuxLowerContext<'_>,
    ) -> Result<Self::Output, crate::error::Error> {
        target.assembler.comment("lowering sub");
        let lhs = self.lhs.lower(ctx, target)?;
        let rhs = self.rhs.lower(ctx, target)?;

        match &self.des.ty {
            Type::Float(32) => {
                let lhs_xmm = target.assembler.alloc.vreg::<XmmReg>();
                target.assembler.movss(lhs_xmm, lhs);
                target.assembler.subss(lhs_xmm, rhs);
                target
                    .assembler
                    .alloc
                    .store_variable(&self.des, Location::Reg(lhs_xmm));
            }
            Type::Float(64) => {
                let lhs_xmm = target.assembler.alloc.vreg::<XmmReg>();
                target.assembler.movsd(lhs_xmm, lhs);
                target.assembler.subsd(lhs_xmm, rhs);
                target
                    .assembler
                    .alloc
                    .store_variable(&self.des, Location::Reg(lhs_xmm));
            }
            _ => {
                let lhs_reg = target.assembler.materialize_value(&lhs);
                target.assembler.sub(lhs_reg, rhs);
                target
                    .assembler
                    .alloc
                    .store_variable(&self.des, Location::Reg(lhs_reg));
            }
        }

        Ok(())
    }
}

impl Lower<X86_64LinuxLowerContext<'_>> for ICall {
    type Output = ();
    fn lower(
        &self,
        ctx: &mut crate::backend::Context,
        target: &mut X86_64LinuxLowerContext<'_>,
    ) -> Result<Self::Output, crate::error::Error> {
        target.assembler.comment("lowering call");
        let mut args = Vec::new();
        for arg in &self.args {
            let value = arg.lower(ctx, target)?;
            // Spill register-valued args to stack to prevent clobbering during arg setup.
            // Without this, a vreg holding arg[i] may be assigned the same physical register
            // that arg[j] (j > i) writes into, corrupting arg[i] before it reaches its arg reg.
            let spilled = if let Location::Reg(_) = &value {
                if let Some(ty) = arg.ty() {
                    let stack = target.assembler.alloc.alloc_stack(ty, 1);
                    target.assembler.mov(Location::Stack(stack), value);
                    Location::Stack(stack)
                } else {
                    value
                }
            } else {
                value
            };
            args.push(spilled);
        }
        let used_regs = target.assembler.caller_preserved_regs();
        for reg in used_regs.iter().rev() {
            target.assembler.push(*reg);
        }

        for (i, arg) in args.iter().enumerate().rev() {
            let reg = match arg {
                Location::Reg(_) => target
                    .assembler
                    .arg_regs::<Reg64>(i)
                    .expect("Too many arguments")
                    .into(),
                Location::Stack(stack) => match stack.access_size {
                    1 => target
                        .assembler
                        .arg_regs::<Reg8>(i)
                        .expect("Too many arguments")
                        .into(),
                    2 => target
                        .assembler
                        .arg_regs::<Reg16>(i)
                        .expect("Too many arguments")
                        .into(),
                    4 => target
                        .assembler
                        .arg_regs::<Reg32>(i)
                        .expect("Too many arguments")
                        .into(),
                    8 => target
                        .assembler
                        .arg_regs::<Reg64>(i)
                        .expect("Too many arguments")
                        .into(),
                    _ => unreachable!("invalid stack size"),
                },

                Location::MemIndexed(_) | Location::Address(_) => {
                    target.assembler.materialize_address(arg)
                }
                Location::Imm(_) => target
                    .assembler
                    .arg_regs::<Reg64>(i)
                    .expect("Too many arguments")
                    .into(),
            };

            target.assembler.mov(reg, arg.clone());
        }

        target.assembler.call(&self.callee);

        if let Some(variable) = &self.des {
            match &variable.ty {
                Type::Float(32) => {
                    // SysV ABI: f32 return value is in xmm0
                    let xmm = target.assembler.alloc.vreg::<XmmReg>();
                    target.assembler.movss(xmm, XmmReg::Xmm0);
                    target.assembler.alloc.store_variable(variable, xmm);
                }
                Type::Float(64) => {
                    // SysV ABI: f64 return value is in xmm0
                    let xmm = target.assembler.alloc.vreg::<XmmReg>();
                    target.assembler.movsd(xmm, XmmReg::Xmm0);
                    target.assembler.alloc.store_variable(variable, xmm);
                }
                _ => {
                    let reg = target.assembler.alloc.vreg::<Reg64>();
                    target.assembler.mov(reg, Reg64::Rax);
                    target.assembler.alloc.store_variable(variable, reg);
                }
            }
        }

        for reg in used_regs {
            target.assembler.pop(reg);
        }

        Ok(())
    }
}

impl Lower<X86_64LinuxLowerContext<'_>> for IAlloc {
    type Output = ();
    fn lower(
        &self,
        _ctx: &mut crate::backend::Context,
        target: &mut X86_64LinuxLowerContext<'_>,
    ) -> Result<Self::Output, crate::error::Error> {
        target.assembler.comment("lowering alloc");
        let Operand::ConstantInt(constant) = &self.size else {
            todo!("@alloc")
        };

        let mem_loc = target
            .assembler
            .alloc
            .alloc_stack(&self.ty, constant.parse::<i32>()?);

        target.assembler.alloc.store_variable(&self.des, mem_loc);

        Ok(())
    }
}

impl Lower<X86_64LinuxLowerContext<'_>> for IElemSet {
    type Output = ();

    fn lower(
        &self,
        ctx: &mut crate::backend::Context,
        target: &mut X86_64LinuxLowerContext<'_>,
    ) -> Result<Self::Output, crate::error::Error> {
        target.assembler.comment("lowering elemset");
        let base_ptr = target
            .assembler
            .alloc
            .get_variable_location(&self.addr)
            .unwrap_or_else(|| panic!("elemset base variable not found {}\n", self.addr.name));

        let index = self.index.lower(ctx, target)?;
        let value = self.value.lower(ctx, target)?;

        let base = target.assembler.materialize_address(&base_ptr);
        let index = target.assembler.materialize_value(&index);
        let value = target.assembler.materialize_value(&value);
        let element_size = self.addr.ty.element_size();

        // Cast value to the element size to avoid wide stores clobbering adjacent elements.
        let sized_value = match element_size {
            1 => value.cast_to::<Reg8>(),
            2 => value.cast_to::<Reg16>(),
            4 => value.cast_to::<Reg32>(),
            _ => value,
        };

        target
            .assembler
            .store_indexed(base, index, element_size, sized_value);

        Ok(())
    }
}

impl Lower<X86_64LinuxLowerContext<'_>> for IElemGet {
    type Output = ();

    fn lower(
        &self,
        ctx: &mut crate::backend::Context,
        target: &mut X86_64LinuxLowerContext<'_>,
    ) -> Result<Self::Output, crate::error::Error> {
        target.assembler.comment("lowering elemget");
        let base_ptr = &self.ptr.lower(ctx, target)?;

        let index = self.index.lower(ctx, target)?;
        let index_reg = target.assembler.materialize_value(&index);

        let base = target.assembler.materialize_address(base_ptr);

        let out = target.assembler.alloc.vreg::<Reg64>();

        // Use the element-sized register variant for the load to avoid
        // reading adjacent elements (e.g., use r11d for 4-byte loads).
        let load_reg = match self.des.ty.size() {
            1 => out.cast_to::<Reg8>(),
            2 => out.cast_to::<Reg16>(),
            4 => out.cast_to::<Reg32>(),
            _ => out,
        };
        target
            .assembler
            .load_indexed(base, index_reg, self.des.ty.size(), load_reg.into());

        target.assembler.alloc.store_variable(&self.des, out);

        Ok(())
    }
}

impl Lower<X86_64LinuxLowerContext<'_>> for ICmp {
    type Output = ();

    fn lower(
        &self,
        ctx: &mut crate::backend::Context,
        target: &mut X86_64LinuxLowerContext<'_>,
    ) -> Result<(), crate::error::Error> {
        target.assembler.comment("lowering cmp");
        let lhs = self.lhs.lower(ctx, target)?;
        let rhs = self.rhs.lower(ctx, target)?;

        target.assembler.cmp(lhs.clone(), rhs.clone());
        let flag_reg = target.assembler.alloc.vreg::<Reg8>();
        target.assembler.sete(flag_reg);
        let out = target.assembler.alloc.vreg::<Reg64>();
        target.assembler.movezx(out, flag_reg);
        target.assembler.alloc.store_variable(&self.des, out);

        Ok(())
    }
}

// impl Lower<X86_64LinuxLowerContext<'_>> for IAnd {
//     type Output = ();
//
//     fn lower(
//         &self,
//         ctx: &mut crate::backend::Context,
//         target: &mut X86_64LinuxLowerContext<'_>,
//     ) -> Result<(), crate::error::Error> {
//         let mut asm = target
//             .assembler
//             .emit(super::assembler::FunctionSection::Body);
//         asm.comment("lowering bitwise and");
//
//         let lhs = self.lhs.lower(ctx, target)?;
//         let rhs = self.rhs.lower(ctx, target)?;
//
//         let dst = asm.alloc.alloc_temp_reg::<Reg64>();
//
//         asm.mov(dst.clone(), lhs);
//         asm.and(dst.clone(), rhs);
//
//         asm.alloc.store_variable(&self.des, dst);
//         debug_assert!(
//             target
//                 .assembler
//                 .alloc
//                 .used_registers
//                 .iter()
//                 .all(|r| { target.assembler.alloc.reg_to_alloc_id.contains_key(r) }),
//             "IBitwiseAnd {}:{}:{} Temp register leaked across instruction boundary",
//             file!(),
//             line!(),
//             column!()
//         );
//         Ok(())
//     }
// }

impl Lower<X86_64LinuxLowerContext<'_>> for IAnd {
    type Output = ();

    fn lower(
        &self,
        ctx: &mut crate::backend::Context,
        target: &mut X86_64LinuxLowerContext<'_>,
    ) -> Result<(), crate::error::Error> {
        target.assembler.comment("lowering logical and");

        let lhs = self.lhs.lower(ctx, target)?;
        let rhs = self.rhs.lower(ctx, target)?;

        let out = target.assembler.alloc.vreg::<Reg64>();

        let false_lbl = &format!(
            "and_false_{}_{}_{}",
            target.function_name, target.block_id.0, target.instr_index
        );
        let done_lbl = &format!(
            "and_done_{}_{}_{}",
            target.function_name, target.block_id.0, target.instr_index
        );

        target.assembler.test(lhs.clone(), lhs);
        target.assembler.jz(false_lbl);

        target.assembler.test(rhs.clone(), rhs);
        target.assembler.jz(false_lbl);

        target.assembler.mov(out, 1);
        target.assembler.jmp(done_lbl);

        target.assembler.define_label(false_lbl);
        target.assembler.mov(out, 0);

        target.assembler.define_label(done_lbl);

        target.assembler.alloc.store_variable(&self.des, out);

        Ok(())
    }
}

impl Lower<X86_64LinuxLowerContext<'_>> for IRef {
    type Output = ();
    fn lower(
        &self,
        _ctx: &mut crate::backend::Context,
        target: &mut X86_64LinuxLowerContext<'_>,
    ) -> Result<Self::Output, crate::error::Error> {
        target.assembler.comment("lowering ref (address-of)");
        let src_loc = target
            .assembler
            .alloc
            .get_variable_location(&self.src)
            .unwrap_or_else(|| panic!("ref: source variable not found: {}", self.src.name));
        let addr_reg = target.assembler.materialize_address(&src_loc);
        target
            .assembler
            .alloc
            .store_variable(&self.des, Location::Reg(addr_reg));
        Ok(())
    }
}

impl Lower<X86_64LinuxLowerContext<'_>> for INot {
    type Output = ();
    fn lower(
        &self,
        _ctx: &mut crate::backend::Context,
        target: &mut X86_64LinuxLowerContext<'_>,
    ) -> Result<Self::Output, crate::error::Error> {
        target.assembler.comment("lowering not");
        let src_loc = target
            .assembler
            .alloc
            .get_variable_location(&self.src)
            .unwrap_or_else(|| panic!("not: source variable not found: {}", self.src.name));
        let src_reg = target.assembler.materialize_value(&src_loc);
        // test src, src → ZF=1 if src==0
        target.assembler.test(src_reg, src_reg);
        let flag_reg = target.assembler.alloc.vreg::<Reg8>();
        // sete al → al=1 if ZF set (i.e. src was 0 = false → NOT = true)
        target.assembler.sete(flag_reg);
        let out = target.assembler.alloc.vreg::<Reg64>();
        target.assembler.movezx(out, flag_reg);
        target.assembler.alloc.store_variable(&self.des, out);
        Ok(())
    }
}

/// Returns true if a location holds an integer/GP value (not an XMM float value).
fn is_gp_loc(loc: &Location) -> bool {
    match loc {
        Location::Reg(reg) => reg.kind() != RegKind::Xmm,
        Location::Stack(_) => true,
        _ => false,
    }
}
