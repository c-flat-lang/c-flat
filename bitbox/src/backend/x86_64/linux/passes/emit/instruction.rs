use super::X86_64LinuxLowerContext;
use crate::backend::Lower;

use crate::backend::x86_64::linux::passes::emit::assembler::{
    Location, Reg8, Reg16, Reg32, Reg64, Stack,
};
use crate::backend::x86_64::linux::passes::emit::error::Error;
use crate::ir::instruction::{
    IAdd, IAlloc, IAnd, IAssign, ICall, ICmp, IElemGet, IElemSet, IGt, IGte, IJump, IJumpIf, ILt,
    IReturn, ISub,
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
            Operand::ConstantInt(constant) => {
                let reg = target.assembler.alloc_temp_reg::<Reg64>();

                match constant.ty {
                    Type::Signed(_) | Type::Unsigned(_) => {
                        target.assembler.mov(reg.clone(), constant.value);
                    }
                    _ => todo!("non-integer constants not implemented yet"),
                }

                Ok(reg)
            }

            Operand::Variable(variable) => {
                let Some(location) = target.assembler.alloc.get_variable_location(&variable) else {
                    return Err(crate::error::Error::X86_64AssemblyError(
                        Error::UndefinedVariable {
                            variable: variable.clone(),
                            function_name: target.function_name.to_string(),
                            block_id: target.block_id,
                            instruction_index: target.instr_index,
                            note: format!(
                                "{}:{}:{} operand variable not found\n{:#?}",
                                file!(),
                                line!(),
                                column!(),
                                target.assembler
                            ),
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
            Operand::Variable(src_var) if self.des.temporary && src_var.ty.is_ptr() => {
                target.assembler.alloc.alias_variable(&self.des, &src_var);
            }
            Operand::Variable(_) if self.des.temporary => {
                let src_loc = self.src.lower(ctx, target)?;
                let des = if let Some(des) = target.assembler.alloc.get_variable_location(&self.des)
                {
                    des
                } else {
                    target.assembler.alloc_reg::<Reg64>().into()
                };

                target.assembler.mov(des.clone(), src_loc);
                target.assembler.alloc.store_variable(&self.des, des);
            }
            _ => {
                let des_reg: Location =
                    if let Some(loc) = target.assembler.alloc.get_variable_location(&self.des) {
                        loc
                    } else if !self.des.temporary {
                        target.assembler.alloc.alloc_stack(&self.des.ty, 1).into()
                    } else {
                        target.assembler.alloc_reg::<Reg64>().into()
                    };
                let src_loc: Location = self.src.lower(ctx, target)?;

                if let Location::Temp(temp) = src_loc {
                    target.assembler.alloc.free_reg(temp);
                }
                target.assembler.mov(des_reg.clone(), src_loc);
                target.assembler.alloc.store_variable(&self.des, des_reg);
            }
        }

        debug_assert!(
            target
                .assembler
                .alloc
                .used_registers
                .iter()
                .all(|r| { target.assembler.alloc.reg_to_alloc_id.contains_key(r) }),
            "IAssign {}:{}:{} Temp register leaked across instruction boundary",
            file!(),
            line!(),
            column!()
        );
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
                let Some(reg) = target.assembler.alloc.get_variable_location(&var) else {
                    panic!("Variable {:?} not found", var);
                };
                match Stack::access_size(&var.ty) {
                    1 => target.assembler.mov(Reg8::Al, reg),
                    2 => target.assembler.mov(Reg16::Ax, reg),
                    4 => target.assembler.mov(Reg32::Eax, reg),
                    8 => target.assembler.mov(Reg64::Rax, reg),
                    _ => unreachable!(),
                };
            }
            Operand::None => todo!("ret none"),
        }
        target
            .assembler
            .jmp(&format!("exit_{}", target.function_name));

        debug_assert!(
            target
                .assembler
                .alloc
                .used_registers
                .iter()
                .all(|r| { target.assembler.alloc.reg_to_alloc_id.contains_key(r) }),
            "IReturn {}:{}:{} Temp register leaked across instruction boundary",
            file!(),
            line!(),
            column!()
        );
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
            .jnz(&self.label);
        if let Location::Temp(temp) = cond {
            target.assembler.alloc.free_reg(temp);
        }

        debug_assert!(
            target
                .assembler
                .alloc
                .used_registers
                .iter()
                .all(|r| { target.assembler.alloc.reg_to_alloc_id.contains_key(r) }),
            "IJumpIf {}:{}:{} Temp register leaked across instruction boundary",
            file!(),
            line!(),
            column!()
        );
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
        target.assembler.jmp(&self.label);

        debug_assert!(
            target
                .assembler
                .alloc
                .used_registers
                .iter()
                .all(|r| { target.assembler.alloc.reg_to_alloc_id.contains_key(r) }),
            "IJump {}:{}:{} Temp register leaked across instruction boundary",
            file!(),
            line!(),
            column!()
        );
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

        target.assembler.cmp(lhs.clone(), rhs.clone());
        if let Location::Temp(temp) = rhs {
            target.assembler.alloc.free_reg(temp);
        }
        if let Location::Temp(temp) = lhs {
            target.assembler.alloc.free_reg(temp);
        }
        let flag = target.assembler.alloc_reg::<Reg8>();

        target.assembler.setg(flag);
        let out = target.assembler.alloc_reg::<Reg64>();
        target.assembler.movezx(out, flag);
        target.assembler.alloc.free_reg(flag);
        target.assembler.alloc.store_variable(&self.des, out);

        debug_assert!(
            target
                .assembler
                .alloc
                .used_registers
                .iter()
                .all(|r| { target.assembler.alloc.reg_to_alloc_id.contains_key(r) }),
            "IGt {}:{}:{} Temp register leaked across instruction boundary",
            file!(),
            line!(),
            column!()
        );
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

        target.assembler.cmp(lhs.clone(), rhs.clone());
        if let Location::Temp(temp) = rhs {
            target.assembler.alloc.free_reg(temp);
        }
        if let Location::Temp(temp) = lhs {
            target.assembler.alloc.free_reg(temp);
        }
        let flag = target.assembler.alloc_reg::<Reg8>();

        target.assembler.setge(flag);
        let out = target.assembler.alloc_reg::<Reg64>();
        target.assembler.movezx(out, flag);
        target.assembler.alloc.free_reg(flag);
        target.assembler.alloc.store_variable(&self.des, out);

        debug_assert!(
            target
                .assembler
                .alloc
                .used_registers
                .iter()
                .all(|r| { target.assembler.alloc.reg_to_alloc_id.contains_key(r) }),
            "IGte {}:{}:{} Temp register leaked across instruction boundary",
            file!(),
            line!(),
            column!()
        );
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
        if let Location::Temp(temp) = rhs {
            target.assembler.alloc.free_reg(temp);
        }
        if let Location::Temp(temp) = lhs {
            target.assembler.alloc.free_reg(temp);
        }
        let flag = target.assembler.alloc_reg::<Reg8>();

        target.assembler.setl(flag);
        let out = target.assembler.alloc_reg::<Reg64>();
        target.assembler.movezx(out, flag);
        target.assembler.alloc.free_reg(flag);
        target.assembler.alloc.store_variable(&self.des, out);

        debug_assert!(
            target
                .assembler
                .alloc
                .used_registers
                .iter()
                .all(|r| { target.assembler.alloc.reg_to_alloc_id.contains_key(r) }),
            "ILt {}:{}:{} Temp register leaked across instruction boundary",
            file!(),
            line!(),
            column!()
        );
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

        target.assembler.add(lhs.clone(), rhs.clone());
        if let Location::Temp(temp) = rhs {
            target.assembler.alloc.free_reg(temp);
        }
        target.assembler.alloc.store_variable(&self.des, lhs);

        debug_assert!(
            target
                .assembler
                .alloc
                .used_registers
                .iter()
                .all(|r| { target.assembler.alloc.reg_to_alloc_id.contains_key(r) }),
            "IAdd {}:{}:{} Temp register leaked across instruction boundary",
            file!(),
            line!(),
            column!()
        );
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

        target.assembler.sub(lhs.clone(), rhs.clone());
        if let Location::Temp(temp) = rhs {
            target.assembler.alloc.free_reg(temp);
        }
        target.assembler.alloc.store_variable(&self.des, lhs);

        debug_assert!(
            target
                .assembler
                .alloc
                .used_registers
                .iter()
                .all(|r| { target.assembler.alloc.reg_to_alloc_id.contains_key(r) }),
            "ISub {}:{}:{} Temp register leaked across instruction boundary",
            file!(),
            line!(),
            column!()
        );
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
            args.push(value);
        }
        let used_regs = target.assembler.caller_preserved_regs();
        for reg in used_regs.iter().rev() {
            target.assembler.push(*reg);
        }

        for (i, arg) in args.iter().enumerate().rev() {
            let reg = match arg {
                Location::Temp(_) | Location::Reg(_) => target
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
            let reg = target.assembler.alloc_reg::<Reg64>();
            target.assembler.mov(reg, Reg64::Rax);
            target.assembler.alloc.store_variable(&variable, reg);
        }

        for reg in used_regs {
            target.assembler.pop(reg);
        }

        debug_assert!(
            target
                .assembler
                .alloc
                .used_registers
                .iter()
                .all(|r| { target.assembler.alloc.reg_to_alloc_id.contains_key(r) }),
            "ICall {}:{}:{} Temp register leaked across instruction boundary",
            file!(),
            line!(),
            column!()
        );

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
            .alloc_stack(&self.ty, constant.value as i32);

        target.assembler.alloc.store_variable(&self.des, mem_loc);

        debug_assert!(
            target
                .assembler
                .alloc
                .used_registers
                .iter()
                .all(|r| { target.assembler.alloc.reg_to_alloc_id.contains_key(r) }),
            "IAlloc {}:{}:{} Temp register leaked across instruction boundary",
            file!(),
            line!(),
            column!()
        );
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
            .expect(&format!(
                "elemset base variable not found {}\n",
                self.addr.name
            ));

        let index = self.index.lower(ctx, target)?;
        let value = self.value.lower(ctx, target)?;

        let base = target.assembler.materialize_address(&base_ptr);
        let index = target.assembler.materialize_value(&index);
        let value = target.assembler.materialize_value(&value);
        let element_size = self.addr.ty.element_size();

        target
            .assembler
            .store_indexed(base, index, element_size, value);
        target.assembler.alloc.free_reg(base);
        target.assembler.alloc.free_reg(index);

        debug_assert!(
            target
                .assembler
                .alloc
                .used_registers
                .iter()
                .all(|r| { target.assembler.alloc.reg_to_alloc_id.contains_key(r) }),
            "IElemSet {}:{}:{} Temp register leaked across instruction boundary",
            file!(),
            line!(),
            column!()
        );
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

        let out = target.assembler.alloc_reg::<Reg64>();

        target
            .assembler
            .load_indexed(base, index_reg, self.des.ty.size(), out.clone().into());

        target.assembler.alloc.free_reg(base);
        target.assembler.alloc.free_reg(index_reg);

        if let Location::Temp(temp) = index {
            target.assembler.alloc.free_reg(temp);
        }

        target.assembler.alloc.store_variable(&self.des, out);

        debug_assert!(
            target
                .assembler
                .alloc
                .used_registers
                .iter()
                .all(|r| { target.assembler.alloc.reg_to_alloc_id.contains_key(r) }),
            "IElemGet {}:{}:{} Temp register leaked across instruction boundary",
            file!(),
            line!(),
            column!()
        );
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
        let flag_reg = target.assembler.alloc_reg::<Reg8>();
        target.assembler.sete(flag_reg.into());
        let out = target.assembler.alloc_reg::<Reg64>();
        target.assembler.movezx(out.clone(), flag_reg.clone());
        target.assembler.alloc.free_reg(flag_reg);
        target.assembler.alloc.store_variable(&self.des, out);

        debug_assert!(
            target
                .assembler
                .alloc
                .used_registers
                .iter()
                .all(|r| { target.assembler.alloc.reg_to_alloc_id.contains_key(r) }),
            "ICmp {}:{}:{} Temp register leaked across instruction boundary",
            file!(),
            line!(),
            column!()
        );
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

        let out = target.assembler.alloc_reg::<Reg64>();

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

        target.assembler.mov(out.clone(), 1);
        target.assembler.jmp(done_lbl);

        target.assembler.define_label(false_lbl);
        target.assembler.mov(out.clone(), 0);

        target.assembler.define_label(done_lbl);

        target.assembler.alloc.store_variable(&self.des, out);

        debug_assert!(
            target
                .assembler
                .alloc
                .used_registers
                .iter()
                .all(|r| { target.assembler.alloc.reg_to_alloc_id.contains_key(r) }),
            "IAnd {}:{}:{} Temp register leaked across instruction boundary",
            file!(),
            line!(),
            column!()
        );
        Ok(())
    }
}
