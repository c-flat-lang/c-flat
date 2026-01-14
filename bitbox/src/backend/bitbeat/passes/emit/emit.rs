use super::{BitbeatLowerContext, OperandResult};
use crate::backend::Lower;
use crate::ir::instruction::{IAdd, ICmp, IJumpIf, ILoad, IReturn};
use crate::passes::{DebugPass, Pass};

#[derive(Debug)]
pub struct EmitBitbeatPass;

impl Pass for EmitBitbeatPass {
    fn debug(
        &self,
        module: &crate::ir::Module,
        ctx: &crate::backend::Context,
        debug_mode: Option<DebugPass>,
    ) -> bool {
        let Some(DebugPass::EmitBitbeat) = debug_mode else {
            return false;
        };
        eprintln!("--- Dump Bitbeat ---");
        let module = ctx.output.get_bitbeat();
        let data = ron::ser::to_string_pretty(module, ron::ser::PrettyConfig::default())
            .expect("Failed to serialize module");
        eprintln!("{}", data);
        true
    }

    fn run(
        &mut self,
        module: &mut crate::ir::Module,
        ctx: &mut crate::backend::Context,
    ) -> Result<(), crate::error::Error> {
        eprintln!("EmitBitbeatPass");
        for function in module.functions.iter() {
            function.lower(ctx, &mut *self)?;
        }
        Ok(())
    }
}

impl Lower<EmitBitbeatPass> for crate::ir::Function {
    type Output = ();
    fn lower(
        &self,
        ctx: &mut crate::backend::Context,
        _: &mut EmitBitbeatPass,
    ) -> Result<Self::Output, crate::error::Error> {
        let mut function = bitbeat::Function::new(&self.name).arity(self.params.len());
        let is_returning = self.return_type != crate::ir::Type::Void;
        function = function.returns(is_returning);

        {
            let mut module = ctx.output.get_mut_bitbeat();
            let mut target = BitbeatLowerContext::new(self.name.clone(), function.instructions());
            if is_returning {
                target.returns();
            }

            for param in self.params.iter() {
                let reg = target.alloc();
                target.variables.insert(param.to_string(), reg);
            }

            for block in self.blocks.iter() {
                target.assembler.label(&block.label);
                target.current_block_id = block.id;
                block.lower(ctx, &mut target)?;
            }
        }

        let mut module = ctx.output.get_mut_bitbeat();
        module.add_function(function);

        Ok(())
    }
}

impl Lower<BitbeatLowerContext<'_>> for crate::ir::BasicBlock {
    type Output = ();
    fn lower(
        &self,
        ctx: &mut crate::backend::Context,
        target: &mut BitbeatLowerContext<'_>,
    ) -> Result<Self::Output, crate::error::Error> {
        for (ip, instruction) in self.instructions.iter().enumerate() {
            target.ip = ip;
            instruction.lower(ctx, target)?;
        }
        Ok(())
    }
}

impl Lower<BitbeatLowerContext<'_>> for crate::ir::Instruction {
    type Output = ();
    fn lower(
        &self,
        ctx: &mut crate::backend::Context,
        target: &mut BitbeatLowerContext<'_>,
    ) -> Result<Self::Output, crate::error::Error> {
        match self {
            crate::ir::Instruction::NoOp(..) => todo!(),
            crate::ir::Instruction::Add(iadd) => iadd.lower(ctx, target)?,
            crate::ir::Instruction::Assign(iassign) => iassign.lower(ctx, target)?,
            crate::ir::Instruction::Alloc(..) => {
                todo!("You need memory to do this instruction")
            }
            crate::ir::Instruction::Call(icall) => icall.lower(ctx, target)?,
            crate::ir::Instruction::Cmp(icmp) => icmp.lower(ctx, target)?,
            crate::ir::Instruction::Copy(..) => todo!("copy"),
            crate::ir::Instruction::ElemGet(..) => todo!("elemget"),
            crate::ir::Instruction::ElemSet(..) => todo!("elemset"),
            crate::ir::Instruction::XOr(ixor) => ixor.lower(ctx, target)?,
            crate::ir::Instruction::Gt(..) => todo!("gt"),
            crate::ir::Instruction::Lt(ilt) => ilt.lower(ctx, target)?,
            crate::ir::Instruction::Jump(ijump) => ijump.lower(ctx, target)?,
            crate::ir::Instruction::JumpIf(ijumpif) => ijumpif.lower(ctx, target)?,
            crate::ir::Instruction::Load(iload) => iload.lower(ctx, target)?,
            crate::ir::Instruction::Mul(imul) => imul.lower(ctx, target)?,
            crate::ir::Instruction::Phi(..) => todo!("@phi"),
            crate::ir::Instruction::Return(ireturn) => ireturn.lower(ctx, target)?,
            crate::ir::Instruction::Sub(isub) => isub.lower(ctx, target)?,
            crate::ir::Instruction::Div(..) => todo!("div"),
            crate::ir::Instruction::IfElse(..) => todo!("ifelse"),
        }
        Ok(())
    }
}

impl Lower<BitbeatLowerContext<'_>> for OperandResult {
    type Output = bitbeat::Reg;
    fn lower(
        &self,
        ctx: &mut crate::backend::Context,
        target: &mut BitbeatLowerContext<'_>,
    ) -> Result<Self::Output, crate::error::Error> {
        match self {
            OperandResult::Register(reg) => Ok(*reg),
            OperandResult::Value(value) => {
                let des = target.alloc();
                target.assembler.load_imm(des, *value);
                Ok(des)
            }
        }
    }
}

impl Lower<BitbeatLowerContext<'_>> for crate::ir::Operand {
    type Output = OperandResult;
    fn lower(
        &self,
        ctx: &mut crate::backend::Context,
        target: &mut BitbeatLowerContext<'_>,
    ) -> Result<Self::Output, crate::error::Error> {
        match self {
            crate::ir::Operand::Variable(variable) => {
                let reg = variable.lower(ctx, target)?;
                Ok(OperandResult::Register(reg))
            }
            crate::ir::Operand::ConstantInt { value, .. } => {
                Ok(OperandResult::Value(value.parse::<i64>().unwrap()))
            }
            crate::ir::Operand::None => unimplemented!("Missing Operand None"),
        }
    }
}

impl Lower<BitbeatLowerContext<'_>> for crate::ir::Variable {
    type Output = bitbeat::Reg;
    fn lower(
        &self,
        ctx: &mut crate::backend::Context,
        target: &mut BitbeatLowerContext<'_>,
    ) -> Result<Self::Output, crate::error::Error> {
        let Some(reg) = target.variables.get(&self.to_string()) else {
            let reg = target.alloc();
            target.variables.insert(self.to_string(), reg);
            return Ok(reg);
        };
        Ok(*reg)
    }
}
