use super::X86_64LinuxLowerContext;
use crate::backend::Lower;

use crate::backend::x86_64::linux::passes::emit::assembler::{
    Location, MemIndexed, Reg, Reg8, Reg16, Reg32, Reg64, RegKind, Stack, XmmReg,
};
use crate::backend::x86_64::linux::passes::emit::error::Error;
use crate::ir::instruction::{
    IAdd, IAlloc, IAnd, IAssign, ICall, ICmp, IDiv, IElemGet, IElemSet, IGt, IGte, IJump, IJumpIf,
    ILt, IMul, INot, IRef, IRem, IReturn, ISub,
};
use crate::ir::{AbiChunk, Operand, Type};

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
                if src_var.ty.is_ptr()
                    || matches!(
                        self.des.ty,
                        crate::ir::Type::Array(..) | crate::ir::Type::Struct(..)
                    ) =>
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
                    Type::Struct(s) => {
                        let base_offset = match &reg {
                            Location::Stack(s) => s.offset,
                            _ => panic!("struct return value must be on stack"),
                        };
                        match s.abi_chunks() {
                            Some(chunks) => {
                                for (i, chunk) in chunks.iter().enumerate() {
                                    let chunk_src = Stack {
                                        offset: base_offset - (i as i32 * 8),
                                        access_size: 8,
                                    };
                                    match chunk {
                                        AbiChunk::Sse => {
                                            let xmm = [XmmReg::Xmm0, XmmReg::Xmm1][i];
                                            target
                                                .assembler
                                                .movsd(Reg::Xmm(xmm), Location::Stack(chunk_src));
                                        }
                                        AbiChunk::Integer => {
                                            let gp = [Reg64::Rax, Reg64::Rdx][i];
                                            target.assembler.mov(gp, Location::Stack(chunk_src));
                                        }
                                    }
                                }
                            }
                            None => {
                                // MEMORY class: copy struct to the hidden return pointer saved in hidden_ret_ptr.
                                let hidden_slot = target
                                    .hidden_ret_ptr
                                    .expect("MEMORY-class return but no hidden_ret_ptr");
                                let ptr_reg = target.assembler.alloc.vreg::<Reg64>();
                                target.assembler.mov(ptr_reg, Location::Stack(hidden_slot));

                                let size = s.size() as usize;
                                let full_qwords = size / 8;
                                let remainder = size % 8;

                                for i in 0..full_qwords {
                                    let src = Stack {
                                        offset: base_offset - (i as i32 * 8),
                                        access_size: 8,
                                    };
                                    let chunk = target.assembler.alloc.vreg::<Reg64>();
                                    target.assembler.mov(chunk, Location::Stack(src));
                                    let off = target.assembler.alloc.vreg::<Reg64>();
                                    target.assembler.mov(off, (i * 8) as i64);
                                    target.assembler.store_indexed(
                                        ptr_reg.into(),
                                        off.into(),
                                        1,
                                        chunk.into(),
                                    );
                                }
                                if remainder > 0 {
                                    let src = Stack {
                                        offset: base_offset - (full_qwords as i32 * 8),
                                        access_size: remainder as i32,
                                    };
                                    let off = target.assembler.alloc.vreg::<Reg64>();
                                    target.assembler.mov(off, (full_qwords * 8) as i64);
                                    let tmp = target.assembler.alloc.vreg::<Reg64>();
                                    match remainder {
                                        1 => {
                                            target
                                                .assembler
                                                .mov(tmp.cast_to::<Reg8>(), Location::Stack(src));
                                            target.assembler.store_indexed(
                                                ptr_reg.into(),
                                                off.into(),
                                                1,
                                                tmp.cast_to::<Reg8>().into(),
                                            );
                                        }
                                        2 => {
                                            target
                                                .assembler
                                                .mov(tmp.cast_to::<Reg16>(), Location::Stack(src));
                                            target.assembler.store_indexed(
                                                ptr_reg.into(),
                                                off.into(),
                                                1,
                                                tmp.cast_to::<Reg16>().into(),
                                            );
                                        }
                                        3 | 4 => {
                                            target
                                                .assembler
                                                .mov(tmp.cast_to::<Reg32>(), Location::Stack(src));
                                            target.assembler.store_indexed(
                                                ptr_reg.into(),
                                                off.into(),
                                                1,
                                                tmp.cast_to::<Reg32>().into(),
                                            );
                                        }
                                        _ => {
                                            target.assembler.mov(tmp, Location::Stack(src));
                                            target.assembler.store_indexed(
                                                ptr_reg.into(),
                                                off.into(),
                                                1,
                                                tmp.into(),
                                            );
                                        }
                                    }
                                }
                                // Return the hidden pointer in rax (SysV requirement).
                                target.assembler.mov(Reg64::Rax, ptr_reg);
                            }
                        }
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
        let flag = target.assembler.alloc.vreg::<Reg8>();
        let out = target.assembler.alloc.vreg::<Reg64>();
        match self.lhs.ty() {
            Some(Type::Float(32)) => {
                target.assembler.ucomiss(lhs, rhs);
                target.assembler.seta(flag);
            }
            Some(Type::Float(64)) => {
                target.assembler.ucomisd(lhs, rhs);
                target.assembler.seta(flag);
            }
            _ => {
                target.assembler.cmp(lhs, rhs);
                target.assembler.setg(flag);
            }
        }
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
        target.assembler.comment("lowering gte");
        let lhs = self.lhs.lower(ctx, target)?;
        let rhs = self.rhs.lower(ctx, target)?;
        let flag = target.assembler.alloc.vreg::<Reg8>();
        let out = target.assembler.alloc.vreg::<Reg64>();
        match self.lhs.ty() {
            Some(Type::Float(32)) => {
                target.assembler.ucomiss(lhs, rhs);
                target.assembler.setae(flag);
            }
            Some(Type::Float(64)) => {
                target.assembler.ucomisd(lhs, rhs);
                target.assembler.setae(flag);
            }
            _ => {
                target.assembler.cmp(lhs, rhs);
                target.assembler.setge(flag);
            }
        }
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
        let flag = target.assembler.alloc.vreg::<Reg8>();
        let out = target.assembler.alloc.vreg::<Reg64>();
        match self.lhs.ty() {
            Some(Type::Float(32)) => {
                target.assembler.ucomiss(lhs, rhs);
                target.assembler.setb(flag);
            }
            Some(Type::Float(64)) => {
                target.assembler.ucomisd(lhs, rhs);
                target.assembler.setb(flag);
            }
            _ => {
                target.assembler.cmp(lhs.clone(), rhs.clone());
                target.assembler.setl(flag);
            }
        }
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
        let mut spilled_args: Vec<(Location, Option<Type>)> = Vec::new();
        for arg in &self.args {
            let value = arg.lower(ctx, target)?;
            // Spill register-valued args to stack to prevent clobbering during arg setup.
            let spilled = if let Location::Reg(_) = &value {
                if let Some(ty) = arg.ty() {
                    let stack = target.assembler.alloc.alloc_stack(ty, 1);
                    match ty {
                        Type::Float(32) => {
                            target
                                .assembler
                                .movss(Location::Stack(stack), value.clone());
                        }
                        Type::Float(64) => {
                            target
                                .assembler
                                .movsd(Location::Stack(stack), value.clone());
                        }
                        _ => {
                            target.assembler.mov(Location::Stack(stack), value.clone());
                        }
                    }
                    Location::Stack(stack)
                } else {
                    value
                }
            } else {
                value
            };
            spilled_args.push((spilled, arg.ty().cloned()));
        }

        let used_regs = target.assembler.caller_preserved_regs();
        for reg in used_regs.iter().rev() {
            target.assembler.push(*reg);
        }

        // Build list of (src, dst, move_kind) in argument order.
        enum MoveKind {
            Gp,
            Sse32,
            Sse64,
        }
        struct ArgMove {
            src: Location,
            dst: Location,
            kind: MoveKind,
        }
        let mut moves: Vec<ArgMove> = Vec::new();
        let mut gp_idx = 0usize;
        let mut xmm_idx = 0usize;

        // SysV MEMORY class: if the callee returns a struct > 16 bytes, pass a hidden return
        // pointer in RDI as the implicit first argument. Allocate space now so we can LEA it.
        let memory_class_ret_slot: Option<Stack> = if let Some(variable) = &self.des {
            if let Type::Struct(s) = &variable.ty {
                if s.abi_chunks().is_none() {
                    let slot = target.assembler.alloc.alloc_stack(&variable.ty, 1);
                    let addr_reg = target.assembler.alloc.vreg::<Reg64>();
                    target.assembler.lea(addr_reg, Location::Stack(slot));
                    let rdi: Reg64 = target.assembler.arg_regs(0).expect("no RDI");
                    moves.push(ArgMove {
                        src: Location::Reg(addr_reg.into()),
                        dst: Location::Reg(rdi.into()),
                        kind: MoveKind::Gp,
                    });
                    gp_idx = 1;
                    Some(slot)
                } else {
                    None
                }
            } else {
                None
            }
        } else {
            None
        };

        for (src, ty) in &spilled_args {
            match ty.as_ref() {
                Some(Type::Struct(s)) => match s.abi_chunks() {
                    Some(chunks) => {
                        let base_offset = match src {
                            Location::Stack(s) => s.offset,
                            _ => panic!("struct arg must be on stack"),
                        };
                        for (chunk_idx, chunk) in chunks.iter().enumerate() {
                            let chunk_src = Location::Stack(Stack {
                                offset: base_offset - (chunk_idx as i32 * 8),
                                access_size: 8,
                            });
                            match chunk {
                                AbiChunk::Sse => {
                                    let xmm = target
                                        .assembler
                                        .xmm_arg_reg(xmm_idx)
                                        .expect("Too many XMM arguments");
                                    moves.push(ArgMove {
                                        src: chunk_src,
                                        dst: Location::Reg(Reg::Xmm(xmm)),
                                        kind: MoveKind::Sse64,
                                    });
                                    xmm_idx += 1;
                                }
                                AbiChunk::Integer => {
                                    let gp: Reg64 = target
                                        .assembler
                                        .arg_regs(gp_idx)
                                        .expect("Too many GP arguments");
                                    moves.push(ArgMove {
                                        src: chunk_src,
                                        dst: Location::Reg(Reg::from(gp)),
                                        kind: MoveKind::Gp,
                                    });
                                    gp_idx += 1;
                                }
                            }
                        }
                    }
                    None => panic!("passing struct > 16 bytes not supported"),
                },
                Some(Type::Float(32)) => {
                    let xmm = target
                        .assembler
                        .xmm_arg_reg(xmm_idx)
                        .expect("Too many XMM arguments");
                    moves.push(ArgMove {
                        src: src.clone(),
                        dst: Location::Reg(Reg::Xmm(xmm)),
                        kind: MoveKind::Sse32,
                    });
                    xmm_idx += 1;
                }
                Some(Type::Float(64)) => {
                    let xmm = target
                        .assembler
                        .xmm_arg_reg(xmm_idx)
                        .expect("Too many XMM arguments");
                    moves.push(ArgMove {
                        src: src.clone(),
                        dst: Location::Reg(Reg::Xmm(xmm)),
                        kind: MoveKind::Sse64,
                    });
                    xmm_idx += 1;
                }
                _ => {
                    let dst: Location = match src {
                        Location::Reg(_) => {
                            let gp: Reg64 = target
                                .assembler
                                .arg_regs(gp_idx)
                                .expect("Too many arguments");
                            Location::Reg(Reg::from(gp))
                        }
                        Location::Stack(stack) => match stack.access_size {
                            1 => {
                                let r: Reg8 = target
                                    .assembler
                                    .arg_regs(gp_idx)
                                    .expect("Too many arguments");
                                Location::Reg(r.into())
                            }
                            2 => {
                                let r: Reg16 = target
                                    .assembler
                                    .arg_regs(gp_idx)
                                    .expect("Too many arguments");
                                Location::Reg(r.into())
                            }
                            4 => {
                                let r: Reg32 = target
                                    .assembler
                                    .arg_regs(gp_idx)
                                    .expect("Too many arguments");
                                Location::Reg(r.into())
                            }
                            8 => {
                                let r: Reg64 = target
                                    .assembler
                                    .arg_regs(gp_idx)
                                    .expect("Too many arguments");
                                Location::Reg(r.into())
                            }
                            _ => unreachable!("invalid stack size"),
                        },
                        Location::MemIndexed(_) | Location::Address(_) => {
                            let r = target.assembler.materialize_address(src);
                            Location::Reg(r)
                        }
                        Location::Imm(_) => {
                            let r: Reg64 = target
                                .assembler
                                .arg_regs(gp_idx)
                                .expect("Too many arguments");
                            Location::Reg(r.into())
                        }
                    };
                    moves.push(ArgMove {
                        src: src.clone(),
                        dst,
                        kind: MoveKind::Gp,
                    });
                    gp_idx += 1;
                }
            }
        }

        // Emit moves in forward order (all sources already on stack, no clobbering risk).
        for ArgMove { src, dst, kind } in moves {
            match kind {
                MoveKind::Gp => {
                    target.assembler.mov(dst, src);
                }
                MoveKind::Sse32 => {
                    target.assembler.movss(dst, src);
                }
                MoveKind::Sse64 => {
                    target.assembler.movsd(dst, src);
                }
            }
        }

        target.assembler.call(&self.callee);

        if let Some(variable) = &self.des {
            match &variable.ty {
                Type::Float(32) => {
                    let xmm = target.assembler.alloc.vreg::<XmmReg>();
                    target.assembler.movss(xmm, XmmReg::Xmm0);
                    target.assembler.alloc.store_variable(variable, xmm);
                }
                Type::Float(64) => {
                    let xmm = target.assembler.alloc.vreg::<XmmReg>();
                    target.assembler.movsd(xmm, XmmReg::Xmm0);
                    target.assembler.alloc.store_variable(variable, xmm);
                }
                Type::Struct(s) => {
                    match s.abi_chunks() {
                        Some(chunks) => {
                            let stack = target.assembler.alloc.alloc_stack(&variable.ty, 1);
                            for (i, chunk) in chunks.iter().enumerate() {
                                let chunk_dst = Stack {
                                    offset: stack.offset - (i as i32 * 8),
                                    access_size: 8,
                                };
                                match chunk {
                                    AbiChunk::Sse => {
                                        let xmm = [XmmReg::Xmm0, XmmReg::Xmm1][i];
                                        target.assembler.movsd(Location::Stack(chunk_dst), xmm);
                                    }
                                    AbiChunk::Integer => {
                                        let gp = [Reg64::Rax, Reg64::Rdx][i];
                                        target.assembler.mov(Location::Stack(chunk_dst), gp);
                                    }
                                }
                            }
                            target.assembler.alloc.store_variable(variable, stack);
                        }
                        None => {
                            // MEMORY class: callee wrote the struct to the slot we pre-allocated.
                            let slot =
                                memory_class_ret_slot.expect("MEMORY-class call but no ret slot");
                            target.assembler.alloc.store_variable(variable, slot);
                        }
                    }
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

        // Struct fields: use field_offset with scale=1. Handle float fields with movss/movsd.
        if let Type::Struct(s) = &self.addr.ty {
            let field_idx = match &self.index {
                Operand::ConstantInt(c) => c.parse::<usize>()?,
                _ => panic!("IElemSet: dynamic struct field index not supported"),
            };
            let byte_offset = s.field_offset(field_idx);
            let off_reg = target
                .assembler
                .materialize_value(&Location::Imm(byte_offset as i64));
            let mem = Location::MemIndexed(MemIndexed {
                base,
                index: off_reg,
                scale: 1,
            });

            match &s.fields[field_idx].1 {
                Type::Float(32) if matches!(self.value.ty(), Some(Type::Float(_))) => {
                    // Float value → float field: use movss.
                    let xmm_val = if matches!(&value, Location::Stack(_)) {
                        let xmm = target.assembler.alloc.vreg::<XmmReg>();
                        target.assembler.movss(Location::Reg(xmm), value);
                        Location::Reg(xmm)
                    } else {
                        value
                    };
                    target.assembler.movss(mem, xmm_val);
                }
                Type::Float(64) if matches!(self.value.ty(), Some(Type::Float(_))) => {
                    // Float value → float field: use movsd.
                    let xmm_val = if matches!(&value, Location::Stack(_)) {
                        let xmm = target.assembler.alloc.vreg::<XmmReg>();
                        target.assembler.movsd(Location::Reg(xmm), value);
                        Location::Reg(xmm)
                    } else {
                        value
                    };
                    target.assembler.movsd(mem, xmm_val);
                }
                field_ty => {
                    let store_size = field_ty.size().min(8);
                    let value_reg = if let Location::Stack(s) = &value {
                        let sized_stack = Stack {
                            offset: s.offset,
                            access_size: store_size,
                        };
                        target
                            .assembler
                            .materialize_value(&Location::Stack(sized_stack))
                    } else {
                        target.assembler.materialize_value(&value)
                    };
                    let sized_value = match store_size {
                        1 => value_reg.cast_to::<Reg8>(),
                        2 => value_reg.cast_to::<Reg16>(),
                        4 => value_reg.cast_to::<Reg32>(),
                        _ => value_reg,
                    };
                    target
                        .assembler
                        .store_indexed(base, off_reg, 1, sized_value);
                }
            }
            return Ok(());
        }

        // Array element store: index * element_size stride.
        let element_size = self.addr.ty.element_size();
        let index_reg = target.assembler.materialize_value(&index);

        let value_reg = if let Location::Stack(s) = &value {
            let sized_stack = Stack {
                offset: s.offset,
                access_size: element_size,
            };
            target
                .assembler
                .materialize_value(&Location::Stack(sized_stack))
        } else {
            target.assembler.materialize_value(&value)
        };
        let sized_value = match element_size {
            1 => value_reg.cast_to::<Reg8>(),
            2 => value_reg.cast_to::<Reg16>(),
            4 => value_reg.cast_to::<Reg32>(),
            _ => value_reg,
        };
        target
            .assembler
            .store_indexed(base, index_reg, element_size, sized_value);

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

        // For pointer-to-struct member access: load the pointer value first to get the struct address.
        let is_ptr_to_struct = matches!(self.ptr.ty(), Some(Type::Pointer(inner)) if matches!(inner.as_ref(), Type::Struct(_)));
        let effective_base: Location;
        let resolved_base = if is_ptr_to_struct {
            // base_ptr is the STORAGE location of the pointer variable; load its VALUE.
            let ptr_val = target.assembler.materialize_value(base_ptr);
            effective_base = Location::Reg(ptr_val);
            &effective_base
        } else {
            base_ptr
        };

        // For aggregate result types (array/struct fields), return the field's stack address
        // rather than loading a value into a register.
        // Read the constant index from self.index directly, since Operand::lower returns Reg not Imm.
        if matches!(self.des.ty, Type::Array(..) | Type::Struct(..)) {
            let byte_offset: i32 = match (&self.ptr.ty(), &self.index) {
                (Some(Type::Struct(s)), Operand::ConstantInt(c)) => {
                    s.field_offset(c.parse::<usize>()?)
                }
                (Some(Type::Pointer(inner)), Operand::ConstantInt(c)) => {
                    if let Type::Struct(s) = inner.as_ref() {
                        s.field_offset(c.parse::<usize>()?)
                    } else {
                        c.parse::<i32>()? * self.des.ty.size()
                    }
                }
                (_, Operand::ConstantInt(c)) => c.parse::<i32>()? * self.des.ty.size(),
                _ => panic!("IElemGet: dynamic index for aggregate result type not supported"),
            };
            match resolved_base {
                Location::Stack(s) => {
                    let field_stack = Stack {
                        offset: s.offset - byte_offset,
                        access_size: Stack::access_size(&self.des.ty),
                    };
                    target
                        .assembler
                        .alloc
                        .store_variable(&self.des, field_stack);
                }
                Location::Reg(r) => {
                    // Pointer value in register: r holds the struct address; compute field address.
                    let field_addr = if byte_offset == 0 {
                        *r
                    } else {
                        let new_reg = target.assembler.alloc.vreg::<Reg64>();
                        target.assembler.mov(new_reg, *r);
                        target.assembler.add(new_reg, byte_offset as i64);
                        new_reg
                    };
                    target
                        .assembler
                        .alloc
                        .store_variable(&self.des, Location::Reg(field_addr));
                }
                _ => panic!("IElemGet: unexpected location for aggregate base: {resolved_base:?}"),
            }
            return Ok(());
        }

        let index_reg = target.assembler.materialize_value(&index);

        let base = target.assembler.materialize_address(resolved_base);

        // For struct fields, use field_offset with scale=1 (not field_index * element_size).
        // For array elements, use element_index with scale=element_size.
        let ptr_ty = self.ptr.ty();
        let struct_ty_opt = match &ptr_ty {
            Some(Type::Struct(s)) => Some(s.clone()),
            Some(Type::Pointer(inner)) => match inner.as_ref() {
                Type::Struct(s) => Some(s.clone()),
                _ => None,
            },
            _ => None,
        };
        let (offset_reg, scale) = if let Some(s) = struct_ty_opt {
            let field_idx = match &self.index {
                Operand::ConstantInt(c) => c.parse::<usize>()?,
                _ => panic!("IElemGet: dynamic struct field index not supported for scalar"),
            };
            let byte_offset = s.field_offset(field_idx);
            let off = target
                .assembler
                .materialize_value(&Location::Imm(byte_offset as i64));
            (off, 1i32)
        } else {
            (index_reg, self.des.ty.size())
        };

        // Float element loads must use movss/movsd (not mov) since XMM regs can't use `mov`.
        match &self.des.ty {
            Type::Float(32) => {
                let xmm = target.assembler.alloc.vreg::<XmmReg>();
                let mem = Location::MemIndexed(MemIndexed::new(base, offset_reg, scale));
                target.assembler.movss(Location::Reg(xmm), mem);
                target
                    .assembler
                    .alloc
                    .store_variable(&self.des, Location::Reg(xmm));
            }
            Type::Float(64) => {
                let xmm = target.assembler.alloc.vreg::<XmmReg>();
                let mem = Location::MemIndexed(MemIndexed::new(base, offset_reg, scale));
                target.assembler.movsd(Location::Reg(xmm), mem);
                target
                    .assembler
                    .alloc
                    .store_variable(&self.des, Location::Reg(xmm));
            }
            _ => {
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
                    .load_indexed(base, offset_reg, scale, load_reg.into());

                target.assembler.alloc.store_variable(&self.des, out);
            }
        }

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
