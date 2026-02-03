use super::X86_64LinuxLowerContext;
use crate::backend::Lower;

use crate::backend::x86_64::linux::passes::emit::assembler::{
    Location, Reg8, Reg16, Reg32, Reg64, Stack,
};
use crate::backend::x86_64::linux::passes::emit::error::Error;
use crate::ir::instruction::{
    IAdd, IAlloc, IAssign, ICall, IElemGet, IElemSet, IGt, IJump, IJumpIf, ILt, IReturn,
};
use crate::ir::{Operand, Type};

impl Lower<X86_64LinuxLowerContext<'_>> for Operand {
    type Output = Location;

    fn lower(
        &self,
        _ctx: &mut crate::backend::Context,
        target: &mut X86_64LinuxLowerContext<'_>,
    ) -> Result<Self::Output, crate::error::Error> {
        let mut asm = target
            .assembler
            .emit(super::assembler::FunctionSection::Body);
        asm.comment("lowering operand");
        match self {
            Operand::ConstantInt(constant) => {
                let reg = asm.alloc().alloc_temp_reg::<Reg64>();

                match constant.ty {
                    Type::Signed(_) | Type::Unsigned(_) => {
                        asm.mov(reg.clone(), constant.value);
                    }
                    _ => todo!("non-integer constants not implemented yet"),
                }

                Ok(reg)
            }

            Operand::Variable(variable) => {
                let Some(location) = asm.alloc().get_variable_location(&variable) else {
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
        let mut asm = target
            .assembler
            .emit(super::assembler::FunctionSection::Body);
        asm.comment("lowering assign");
        match &self.src {
            // TODO: Check if old source drops reference to variable location.
            Operand::Variable(src_var) if self.des.temporary && src_var.ty.is_ptr() => {
                asm.alloc().alias_variable(&self.des, &src_var);
            }
            Operand::Variable(src_var) if self.des.temporary => {
                let src_loc = self.src.lower(ctx, target)?;
                let mut asm = target
                    .assembler
                    .emit(super::assembler::FunctionSection::Body);
                let des = if let Some(des) = asm.alloc().get_variable_location(&self.des) {
                    des
                } else {
                    asm.alloc().alloc_reg::<Reg64>().into()
                };

                asm.mov(des.clone(), src_loc);
                asm.alloc().store_variable(&self.des, des);
            }
            _ => {
                let des_reg: Location =
                    if let Some(loc) = asm.alloc().get_variable_location(&self.des) {
                        loc
                    } else if !self.des.temporary {
                        asm.alloc().alloc_stack(&self.des.ty, 1).into()
                    } else {
                        asm.alloc().alloc_reg::<Reg64>().into()
                    };
                let src_loc: Location = self.src.lower(ctx, target)?;

                let mut asm = target
                    .assembler
                    .emit(super::assembler::FunctionSection::Body);
                if let Location::Temp(temp) = src_loc {
                    asm.alloc().free_reg(temp);
                }
                let mut asm = target
                    .assembler
                    .emit(super::assembler::FunctionSection::Body);
                asm.mov(des_reg.clone(), src_loc);
                asm.alloc().store_variable(&self.des, des_reg);
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
        let mut asm = target
            .assembler
            .emit(super::assembler::FunctionSection::Body);
        asm.comment("lowering return");

        match &self.src {
            Operand::ConstantInt { .. } => todo!("ret const"),
            Operand::Variable(var) => {
                let Some(reg) = asm.alloc().get_variable_location(&var) else {
                    panic!("Variable {:?} not found", var);
                };
                let mut asm = target
                    .assembler
                    .emit(super::assembler::FunctionSection::Body);
                match Stack::access_size(&var.ty) {
                    1 => asm.mov(Reg8::Al, reg),
                    2 => asm.mov(Reg16::Ax, reg),
                    4 => asm.mov(Reg32::Eax, reg),
                    8 => asm.mov(Reg64::Rax, reg),
                    _ => unreachable!(),
                };
            }
            Operand::None => todo!("ret none"),
        }
        let mut asm = target
            .assembler
            .emit(super::assembler::FunctionSection::Body);
        asm.jmp(&format!("exit_{}", target.function_name));
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
        let mut asm = target
            .assembler
            .emit(super::assembler::FunctionSection::Body);
        asm.comment("lowering jump if");
        let cond = self.cond.lower(ctx, target)?;
        let mut asm = target
            .assembler
            .emit(super::assembler::FunctionSection::Body);
        asm.test(cond.clone(), cond.clone()).jnz(&self.label);
        if let Location::Temp(temp) = cond {
            asm.alloc().free_reg(temp);
        }
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
        let mut asm = target
            .assembler
            .emit(super::assembler::FunctionSection::Body);
        asm.comment("lowering jump if");
        asm.jmp(&self.label);
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
        let mut asm = target
            .assembler
            .emit(super::assembler::FunctionSection::Body);
        asm.comment("lowering gt");
        let lhs = self.lhs.lower(ctx, target)?;
        let rhs = self.rhs.lower(ctx, target)?;

        let mut asm = target
            .assembler
            .emit(super::assembler::FunctionSection::Body);
        asm.cmp(lhs.clone(), rhs.clone());
        if let Location::Temp(temp) = rhs {
            asm.alloc().free_reg(temp);
        }
        if let Location::Temp(temp) = lhs {
            asm.alloc().free_reg(temp);
        }
        let flag = asm.alloc().alloc_reg::<Reg8>();

        let mut asm = target
            .assembler
            .emit(super::assembler::FunctionSection::Body);
        asm.setg(flag);
        let out = asm.alloc().alloc_reg::<Reg64>();
        asm.movezx(out, flag);
        asm.alloc().free_reg(flag);
        asm.alloc().store_variable(&self.des, out);
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
        let mut asm = target
            .assembler
            .emit(super::assembler::FunctionSection::Body);
        asm.comment("lowering lt");
        let lhs = self.lhs.lower(ctx, target)?;
        let rhs = self.rhs.lower(ctx, target)?;

        let mut asm = target
            .assembler
            .emit(super::assembler::FunctionSection::Body);
        asm.cmp(lhs.clone(), rhs.clone());
        if let Location::Temp(temp) = rhs {
            asm.alloc().free_reg(temp);
        }
        if let Location::Temp(temp) = lhs {
            asm.alloc().free_reg(temp);
        }
        let flag = asm.alloc().alloc_reg::<Reg8>();

        let mut asm = target
            .assembler
            .emit(super::assembler::FunctionSection::Body);
        asm.setl(flag);
        let out = asm.alloc().alloc_reg::<Reg64>();
        asm.movezx(out, flag);
        asm.alloc().free_reg(flag);
        asm.alloc().store_variable(&self.des, out);
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
        let mut asm = target
            .assembler
            .emit(super::assembler::FunctionSection::Body);
        asm.comment("lowering add");
        let lhs = self.lhs.lower(ctx, target)?;
        let rhs = self.rhs.lower(ctx, target)?;

        let mut asm = target
            .assembler
            .emit(super::assembler::FunctionSection::Body);
        asm.add(lhs.clone(), rhs);
        if let Location::Temp(temp) = lhs {
            asm.alloc().free_reg(temp);
        }
        asm.alloc().store_variable(&self.des, lhs);
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
        let mut asm = target
            .assembler
            .emit(super::assembler::FunctionSection::Body);
        asm.comment("lowering call");
        let mut args = Vec::new();
        for arg in &self.args {
            let value = arg.lower(ctx, target)?;
            args.push(value);
        }
        let mut asm = target
            .assembler
            .emit(super::assembler::FunctionSection::Body);
        let used_regs = asm.caller_preserved_regs();
        for reg in used_regs.iter().rev() {
            asm.push(*reg);
        }

        for (i, arg) in args.iter().enumerate().rev() {
            // pick the appropriate size of register for the arg
            let reg = match arg {
                Location::Temp(_) | Location::Reg(_) => {
                    // just use Reg64 for temps and registers
                    asm.arg_regs::<Reg64>(i).expect("Too many arguments").into()
                }
                Location::Stack(stack) => match stack.access_size {
                    1 => asm.arg_regs::<Reg8>(i).expect("Too many arguments").into(),
                    2 => asm.arg_regs::<Reg16>(i).expect("Too many arguments").into(),
                    4 => asm.arg_regs::<Reg32>(i).expect("Too many arguments").into(),
                    8 => asm.arg_regs::<Reg64>(i).expect("Too many arguments").into(),
                    _ => unreachable!("invalid stack size"),
                },
                Location::MemIndexed(_) => {
                    // materialize the address into a temp reg first
                    let temp = asm.materialize_address(arg);
                    temp
                }
                Location::Imm(_) => {
                    // immediate value, just use Reg64 as destination
                    asm.arg_regs::<Reg64>(i).expect("Too many arguments").into()
                }
            };

            // Move value into the argument register
            asm.mov(reg, arg.clone());
        }

        asm.call(&self.callee);

        if let Some(variable) = &self.des {
            let reg = asm.alloc().alloc_reg::<Reg64>();
            asm.mov(reg, Reg64::Rax);
            asm.alloc().store_variable(&variable, reg);
        }

        for reg in used_regs {
            asm.pop(reg);
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
        let mut asm = target
            .assembler
            .emit(super::assembler::FunctionSection::Body);
        asm.comment("lowering alloc");
        let Operand::ConstantInt(constant) = &self.size else {
            todo!("@alloc")
        };

        let mem_loc = asm.alloc().alloc_stack(&self.ty, constant.value as i32);

        asm.alloc().store_variable(&self.des, mem_loc);

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
        let mut asm = target
            .assembler
            .emit(super::assembler::FunctionSection::Body);
        asm.comment("lowering elemset");
        let base_ptr = asm
            .alloc()
            .get_variable_location(&self.addr)
            .expect(&format!(
                "elemset base variable not found {}\n",
                self.addr.name
            ));

        // Lower index/value FIRST so they can't clobber the base address
        let index = self.index.lower(ctx, target)?;
        let value = self.value.lower(ctx, target)?;
        let mut asm = target
            .assembler
            .emit(super::assembler::FunctionSection::Body);

        let base = asm.materialize_address(&base_ptr);

        let index = asm.materialize_address(&index);

        asm.store_indexed(base, index, self.addr.ty.size(), value);

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
        let mut asm = target
            .assembler
            .emit(super::assembler::FunctionSection::Body);
        asm.comment("lowering elemget");
        // Get the base pointer of the array
        let base_ptr = &self.ptr.lower(ctx, target)?;

        // Lower index first to avoid clobbering base_ptr
        let index = self.index.lower(ctx, target)?;
        let mut asm = target
            .assembler
            .emit(super::assembler::FunctionSection::Body);
        let index_reg = asm.materialize_address(&index);

        // Materialize base address
        let base = asm.materialize_address(base_ptr);

        // Allocate a register to hold the loaded value
        let out = asm.alloc().alloc_reg::<Reg64>();

        // Load the value from memory: out = *(base + index * element_size)
        asm.load_indexed(base, index_reg, self.des.ty.size(), out.clone().into());

        // Free temps if needed
        if let Location::Temp(temp) = index {
            asm.alloc().free_reg(temp);
        }

        // Store the loaded value in the SSA variable
        asm.alloc().store_variable(&self.des, out);

        Ok(())
    }
}
