pub mod allocator;
pub mod assembler;
pub mod error;
mod instruction;

use crate::backend::x86_64::linux;
use crate::backend::x86_64::linux::passes::emit::assembler::{
    Assembler, Reg, Reg8, Reg16, Reg32, Reg64, Stack,
};
use crate::backend::{Context, Lower};
use crate::ir::{self, AbiChunk, Type};
use crate::passes::{DebugPass, PassOutput};
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
    fn debug_pass(&self) -> DebugPass {
        DebugPass::Emit
    }

    fn debug(&self, _module: &crate::ir::Module, ctx: &crate::backend::Context) -> PassOutput {
        PassOutput::String(format!("{}", ctx.output.get_x86_64()))
    }

    fn run(
        &mut self,
        module: &mut ir::Module,
        ctx: &mut Context,
    ) -> Result<(), crate::error::Error> {
        if !module.functions.iter().any(|f| f.name == "main") {
            return Err(crate::error::Error::MissingMainFunction);
        }

        {
            // TODO: It would be nice to not have to use strings here
            let x86_64 = ctx.output.get_mut_x86_64();
            x86_64.push_directive(".intel_syntax noprefix");
            x86_64.push_directive(".globl main");
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

        target
            .assembler
            .set_section(assembler::FunctionSection::Prolog);
        target
            .assembler
            .comment(format!("Defining Function {}", self.name))
            .define_label(&self.name)
            .push(Rbp)
            .mov(Rbp, Rsp);
        target
            .assembler
            .set_section(assembler::FunctionSection::Body);
        // Args
        target.assembler.comment("-- args --");
        let mut gp_idx = 0usize;
        let mut xmm_idx = 0usize;
        for arg in self.params.iter() {
            let arg_stack = target.assembler.alloc.alloc_stack(&arg.ty, 1);
            target.assembler.comment(format!("arg: {}", arg.name));
            match &arg.ty {
                Type::Struct(s) => {
                    match s.abi_chunks() {
                        Some(chunks) => {
                            for (chunk_idx, chunk) in chunks.iter().enumerate() {
                                let chunk_dst = Stack {
                                    offset: arg_stack.offset - (chunk_idx as i32 * 8),
                                    access_size: 8,
                                };
                                match chunk {
                                    AbiChunk::Integer => {
                                        let gp: Reg64 = target
                                            .assembler
                                            .arg_regs(gp_idx)
                                            .expect("too many GP params");
                                        target.assembler.mov(Location::Stack(chunk_dst), gp);
                                        gp_idx += 1;
                                    }
                                    AbiChunk::Sse => {
                                        let xmm = target
                                            .assembler
                                            .xmm_arg_reg(xmm_idx)
                                            .expect("too many XMM params");
                                        target.assembler.movsd(Location::Stack(chunk_dst), xmm);
                                        xmm_idx += 1;
                                    }
                                }
                            }
                        }
                        None => {
                            // MEMORY class: passed as pointer in GP reg
                            let ptr: Reg64 =
                                target.assembler.arg_regs(gp_idx).expect("too many params");
                            target.assembler.mov(Location::Stack(arg_stack), ptr);
                            gp_idx += 1;
                        }
                    }
                }
                Type::Float(32) => {
                    let xmm = target
                        .assembler
                        .xmm_arg_reg(xmm_idx)
                        .expect("too many XMM params");
                    target.assembler.movss(Location::Stack(arg_stack), xmm);
                    xmm_idx += 1;
                }
                Type::Float(64) => {
                    let xmm = target
                        .assembler
                        .xmm_arg_reg(xmm_idx)
                        .expect("too many XMM params");
                    target.assembler.movsd(Location::Stack(arg_stack), xmm);
                    xmm_idx += 1;
                }
                _ => {
                    let arg_size = Stack::access_size(&arg.ty);
                    let reg: Reg = (match arg_size {
                        1 => target.assembler.arg_regs::<Reg8>(gp_idx).map(Reg::from),
                        2 => target.assembler.arg_regs::<Reg16>(gp_idx).map(Reg::from),
                        4 => target.assembler.arg_regs::<Reg32>(gp_idx).map(Reg::from),
                        8 => target.assembler.arg_regs::<Reg64>(gp_idx).map(Reg::from),
                        _ => unreachable!(),
                    })
                    .expect("too many args");
                    target.assembler.mov(Location::Stack(arg_stack), reg);
                    gp_idx += 1;
                }
            }
            target.assembler.alloc.store_variable(arg, arg_stack);
        }

        target
            .assembler
            .set_section(assembler::FunctionSection::Body);

        for block in self.blocks.iter() {
            target.block_id = block.id;
            block.lower(ctx, &mut target)?;
        }

        target
            .assembler
            .set_section(assembler::FunctionSection::Prolog);
        let mut offset = target.assembler.alloc.stack_memory_offset as i64;
        offset = (offset + 15) & !15;
        target.assembler.sub(Rsp, offset);

        target
            .assembler
            .set_section(assembler::FunctionSection::Epilog);
        target
            .assembler
            .define_label(format!("exit_{}", self.name))
            .add(Rsp, offset)
            .mov(Rsp, Rbp)
            .pop(Rbp)
            .ret()
            .comment(format!("--- End of Function {} ---", self.name));

        let module = ctx.output.get_mut_x86_64();

        let func = linux::Function {
            name: self.name.clone(),
            prolog: target.assembler.prolog,
            instructions: target.assembler.instructions,
            epilog: target.assembler.epilog,
        };
        module.push_function(func);

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
            for c in chars.by_ref() {
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
        target
            .assembler
            .define_label(format!("{}_{}", target.function_name, &self.label));

        for (instr_index, instruction) in self.instructions.iter().enumerate() {
            target
                .assembler
                .comment(strip_ansi(&format!("{instruction}")));
            target.instr_index = instr_index;
            instruction.lower(ctx, target)?;

            let variables = target.assembler.alloc.allocated_variables();

            let instr_index = instr_index + 1;
            for var in variables {
                let is_live_after =
                    ctx.liveness
                        .is_live_after(target.function_name, self.id, instr_index, &var);

                if is_live_after {
                    continue;
                }

                target.assembler.alloc.free_variable(&var);
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
            ir::Instruction::And(iand) => iand.lower(ctx, target)?,
            ir::Instruction::Assign(iassign) => iassign.lower(ctx, target)?,
            ir::Instruction::Call(icall) => icall.lower(ctx, target)?,
            ir::Instruction::Cmp(icmp) => icmp.lower(ctx, target)?,
            ir::Instruction::Copy(..) => todo!("copy"),
            ir::Instruction::Div(idiv) => idiv.lower(ctx, target)?,
            ir::Instruction::ElemGet(ielemget) => ielemget.lower(ctx, target)?,
            ir::Instruction::ElemSet(ielemset) => ielemset.lower(ctx, target)?,
            ir::Instruction::Gt(igt) => igt.lower(ctx, target)?,
            ir::Instruction::Gte(igte) => igte.lower(ctx, target)?,
            ir::Instruction::Rem(irem) => irem.lower(ctx, target)?,
            ir::Instruction::Jump(ijump) => ijump.lower(ctx, target)?,
            ir::Instruction::JumpIf(ijump) => ijump.lower(ctx, target)?,
            ir::Instruction::Load(..) => todo!("load"),
            ir::Instruction::Lt(ilt) => ilt.lower(ctx, target)?,
            ir::Instruction::Mul(imul) => imul.lower(ctx, target)?,
            ir::Instruction::NoOp(..) => todo!(),
            ir::Instruction::Or(..) => todo!("or"),
            ir::Instruction::Phi(..) | ir::Instruction::Loop(..) | ir::Instruction::IfElse(..) => {
                unreachable!(
                    "Lowering pass should be used before x86_64 emit pass {:?}",
                    self
                )
            }
            ir::Instruction::Return(ireturn) => ireturn.lower(ctx, target)?,
            ir::Instruction::Sub(isub) => isub.lower(ctx, target)?,
            ir::Instruction::XOr(..) => todo!("xor"),
            ir::Instruction::Ref(iref) => iref.lower(ctx, target)?,
            ir::Instruction::Not(inot) => inot.lower(ctx, target)?,
        }
        Ok(())
    }
}
