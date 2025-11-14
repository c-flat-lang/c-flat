use crate::backend::Lower;
use crate::ir::instruction::{IAdd, ICmp, IJumpIf, ILoad, IReturn};
use crate::passes::Pass;

#[derive(Debug)]
pub struct EmitBitbeatPass;

impl Pass for EmitBitbeatPass {
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

#[derive(Debug)]
struct BitbeatLowerContext<'ctx> {
    function_name: String,
    assembler: bitbeat::InstructionBuilder<'ctx>,
    registery: [bool; bitbeat::REG_COUNT],
    current_block_id: crate::ir::BlockId,
    ip: usize,
    // HACK: key should be a Variable not String
    variables: std::collections::BTreeMap<String, bitbeat::Reg>,
}

impl<'ctx> BitbeatLowerContext<'ctx> {
    fn new(function_name: String, assembler: bitbeat::InstructionBuilder<'ctx>) -> Self {
        Self {
            function_name,
            assembler,
            registery: [false; bitbeat::REG_COUNT],
            current_block_id: crate::ir::BlockId(0),
            ip: 0,
            variables: std::collections::BTreeMap::new(),
        }
    }

    fn returns(&mut self) -> &mut Self {
        self.registery[0] = true;
        self
    }

    fn alloc(&mut self) -> bitbeat::Reg {
        let id = self
            .registery
            .iter()
            .position(|r| !r)
            .expect("Out of registers");
        self.registery[id] = true;
        bitbeat::Reg(id)
    }

    fn free(&mut self, reg: &bitbeat::Reg) {
        self.registery[reg.0] = false
    }
}

impl Lower<EmitBitbeatPass> for crate::ir::Function {
    type Output = ();
    fn lower(
        &self,
        ctx: &mut crate::backend::Context,
        _: &mut EmitBitbeatPass,
    ) -> Result<Self::Output, crate::error::Error> {
        let mut function = bitbeat::Function::new(&self.name);
        let is_returning = self.return_type != crate::ir::Type::Void;
        function = function.returns(is_returning);

        {
            let mut module = ctx.output.get_mut_bitbeat();
            let mut target = BitbeatLowerContext::new(self.name.clone(), function.instructions());
            if is_returning {
                target.returns();
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
            crate::ir::Instruction::Add(IAdd { des, lhs, rhs }) => {
                let lhs = lhs.lower(ctx, target)?.lower(ctx, target)?;
                let rhs = rhs.lower(ctx, target)?.lower(ctx, target)?;
                let des = des.lower(ctx, target)?;
                target.assembler.add(des, lhs, rhs);
            }
            crate::ir::Instruction::Assign(..) => todo!("assign"),
            crate::ir::Instruction::Alloc(..) => {
                todo!("You need memory to do this instruction")
            }
            crate::ir::Instruction::Call(..) => todo!("call"),
            crate::ir::Instruction::Cmp(ICmp { des, lhs, rhs }) => {
                let lhs = lhs.lower(ctx, target)?.lower(ctx, target)?;
                let rhs = rhs.lower(ctx, target)?.lower(ctx, target)?;
                let des = des.lower(ctx, target)?;
                target.assembler.cmp_eq(des, lhs, rhs);
            }
            crate::ir::Instruction::ElemGet(..) => todo!("elemget"),
            crate::ir::Instruction::ElemSet(..) => todo!("elemset"),
            crate::ir::Instruction::Gt(..) => todo!("gt"),
            crate::ir::Instruction::Lt(..) => todo!("lt"),
            crate::ir::Instruction::Jump(ijump) => {
                target.assembler.jump(&ijump.label);
            }
            crate::ir::Instruction::JumpIf(IJumpIf { cond, label }) => {
                let operand_result = cond.lower(ctx, target)?;
                let reg = operand_result.lower(ctx, target)?;
                target.assembler.jump_if(reg, label);
            }
            crate::ir::Instruction::Load(ILoad { des, src }) => {
                let OperandResult::Value(value) = src.lower(ctx, target)? else {
                    panic!("Load Imm expected value found register");
                };
                let reg = des.lower(ctx, target)?;
                target.assembler.load_imm(reg, value);
            }
            crate::ir::Instruction::Mul(..) => todo!("mul"),
            crate::ir::Instruction::Phi(..) => todo!("@phi"),
            crate::ir::Instruction::Return(IReturn { ty, src }) => {
                let operand_result = src.lower(ctx, target)?;
                let reg = operand_result.lower(ctx, target)?;
                target.assembler.print(reg);
                // target.assembler.send(bitbeat::Reg(0), reg);
            }
            crate::ir::Instruction::Sub(..) => todo!("sub"),
            crate::ir::Instruction::Div(..) => todo!("div"),
            crate::ir::Instruction::IfElse(..) => todo!("ifelse"),
        }
        Ok(())
    }
}

#[derive(Debug, Clone)]
enum OperandResult {
    Register(bitbeat::Reg),
    Value(i64),
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
                // HACK: register registery
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
