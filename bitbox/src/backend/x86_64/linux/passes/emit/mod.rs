mod allocator;
mod assembler;
pub mod error;
mod instruction;

use crate::backend::x86_64::linux::passes::emit::assembler::{
    Assembler, Reg, Reg8, Reg16, Reg32, Reg64, Stack,
};
use crate::backend::{Context, Lower};
use crate::ir;
use crate::passes::DebugPass;
use assembler::Location;

#[derive(Debug)]
pub struct Allocation {
    pub location: Location,
    pub references: usize,
}

#[derive(Debug)]
pub struct X86_64LinuxLowerContext<'ctx> {
    pub function_name: &'ctx str,
    pub block_id: ir::BlockId,
    pub instr_index: usize,
    pub assembler: Assembler,
}

impl<'ctx> X86_64LinuxLowerContext<'ctx> {
    fn new(function_name: &'ctx str) -> Self {
        Self {
            function_name,
            block_id: ir::BlockId(0),
            instr_index: 0,
            assembler: Assembler::default(),
        }
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
            // TODO: It would be nice to not have to use strings here
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

        let mut pro = target.assembler.emit(assembler::FunctionSection::Prolog);
        pro.comment(format!("Defining Function {}", self.name))
            .define_label(&self.name)
            .push(Rbp)
            .mov(Rbp, Rsp);
        let mut asm = target.assembler.emit(assembler::FunctionSection::Body);
        // Args
        asm.comment("-- args --");
        for (index, arg) in self.params.iter().enumerate() {
            let arg_size = Stack::access_size(&arg.ty);
            let Some(reg): Option<Reg> = (match arg_size {
                1 => asm.arg_regs::<Reg8>(index).map(Reg8::into),
                2 => asm.arg_regs::<Reg16>(index).map(Reg16::into),
                4 => asm.arg_regs::<Reg32>(index).map(Reg32::into),
                8 => asm.arg_regs::<Reg64>(index).map(Reg64::into),

                _ => unreachable!(),
            }) else {
                panic!("Too many arguments in function {}", self.name);
            };
            let arg_stack_memory = asm.alloc().alloc_stack(&arg.ty, 1);
            asm.comment(format!("arg {}: {}", index, arg.name));
            asm.mov(arg_stack_memory.clone(), reg);
            asm.alloc().store_variable(&arg, arg_stack_memory);
        }

        for block in self.blocks.iter() {
            target.block_id = block.id;
            block.lower(ctx, &mut target)?;
        }

        let mut pro = target.assembler.emit(assembler::FunctionSection::Prolog);
        let mut offset = pro.alloc().stack_memory_offset as i64;
        offset = (offset + 15) & !15;
        pro.sub(Rsp, offset);

        let mut epi = target.assembler.emit(assembler::FunctionSection::Epilog);
        epi.define_label(format!("exit_{}", self.name))
            .add(Rsp, offset)
            .mov(Rsp, Rbp)
            .pop(Rbp)
            .ret()
            .comment(format!("--- End of Function {} ---", self.name));

        let x86_64 = ctx.output.get_mut_x86_64();
        x86_64.push_str(target.assembler.to_string().as_str());
        Ok(())
    }
}

fn strip_ansi(s: &str) -> String {
    let mut out = String::with_capacity(s.len());
    let mut chars = s.chars().peekable();

    while let Some(c) = chars.next() {
        if c == '\x1b' && chars.peek() == Some(&'[') {
            // skip ESC [
            chars.next();
            // skip until 'm'
            while let Some(c) = chars.next() {
                if c == 'm' {
                    break;
                }
            }
        } else {
            out.push(c);
        }
    }

    out
}

impl Lower<X86_64LinuxLowerContext<'_>> for ir::BasicBlock {
    type Output = ();
    fn lower(
        &self,
        ctx: &mut Context,
        target: &mut X86_64LinuxLowerContext<'_>,
    ) -> Result<Self::Output, crate::error::Error> {
        let mut asm = target.assembler.emit(assembler::FunctionSection::Body);
        asm.define_label(&self.label);

        for (instr_index, instruction) in self.instructions.iter().enumerate() {
            let mut asm = target.assembler.emit(assembler::FunctionSection::Body);
            asm.comment(strip_ansi(&format!("{instruction}")));
            target.instr_index = instr_index;
            instruction.lower(ctx, target)?;

            let mut asm = target.assembler.emit(assembler::FunctionSection::Body);
            let variables = asm.alloc().allocated_variables();

            let instr_index = instr_index + 1;
            for var in variables {
                let is_live_after =
                    ctx.liveness
                        .is_live_after(target.function_name, self.id, instr_index, &var);

                if is_live_after {
                    continue;
                }

                asm.alloc().free_variable(&var);
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
            ir::Instruction::Add(iadd) => iadd.lower(ctx, target)?,
            ir::Instruction::Alloc(ialloc) => ialloc.lower(ctx, target)?,
            ir::Instruction::And(..) => todo!("and"),
            ir::Instruction::Assign(iassign) => iassign.lower(ctx, target)?,
            ir::Instruction::Call(icall) => icall.lower(ctx, target)?,
            ir::Instruction::Cmp(..) => todo!("cmp"),
            ir::Instruction::Copy(..) => todo!("copy"),
            ir::Instruction::Div(..) => todo!("div"),
            ir::Instruction::ElemGet(ielemget) => ielemget.lower(ctx, target)?,
            ir::Instruction::ElemSet(ielemset) => ielemset.lower(ctx, target)?,
            ir::Instruction::Gt(igt) => igt.lower(ctx, target)?,
            ir::Instruction::Gte(..) => todo!("gte"),
            ir::Instruction::Jump(ijump) => ijump.lower(ctx, target)?,
            ir::Instruction::JumpIf(ijump) => ijump.lower(ctx, target)?,
            ir::Instruction::Load(..) => todo!("load"),
            ir::Instruction::Lt(ilt) => ilt.lower(ctx, target)?,
            ir::Instruction::Mul(..) => todo!("mul"),
            ir::Instruction::NoOp(..) => todo!(),
            ir::Instruction::Or(..) => todo!("or"),
            ir::Instruction::Phi(..) | ir::Instruction::Loop(..) | ir::Instruction::IfElse(..) => {
                unreachable!(
                    "Lowering pass should be used before x86_64 emit pass {:?}",
                    self
                )
            }
            ir::Instruction::Return(ireturn) => ireturn.lower(ctx, target)?,
            ir::Instruction::Sub(..) => todo!("sub"),
            ir::Instruction::XOr(..) => todo!("xor"),
        }
        Ok(())
    }
}
