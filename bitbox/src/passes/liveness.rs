use crate::ir::{
    instruction::{IAdd, IAssign, ICall, ICmp, IDiv, IGt, ILt, IMul, ISub},
    BlockId, Variable,
};
use std::collections::HashMap;

use super::Pass;

impl crate::ir::Instruction {
    pub fn uses(&self) -> Vec<Variable> {
        match self {
            Self::Add(IAdd { lhs, rhs, .. })
            | Self::Sub(ISub { lhs, rhs, .. })
            | Self::Mul(IMul { lhs, rhs, .. })
            | Self::Div(IDiv { lhs, rhs, .. })
            | Self::Cmp(ICmp { lhs, rhs, .. })
            | Self::Gt(IGt { lhs, rhs, .. })
            | Self::Lt(ILt { lhs, rhs, .. }) => {
                let mut vars = Vec::new();
                if let crate::ir::Operand::Variable(v) = lhs {
                    vars.push(v.clone());
                }
                if let crate::ir::Operand::Variable(v) = rhs {
                    vars.push(v.clone());
                }
                vars
            }
            Self::Assign(IAssign { src: op, .. }) | Self::Load(_, op) | Self::JumpIf(op, _) => {
                if let crate::ir::Operand::Variable(v) = op {
                    vec![v.clone()]
                } else {
                    vec![]
                }
            }
            Self::Return(_, op) => {
                if let crate::ir::Operand::Variable(v) = op {
                    vec![v.clone()]
                } else {
                    vec![]
                }
            }
            _ => vec![],
        }
    }

    pub fn defines(&self) -> Vec<Variable> {
        match self {
            Self::Add(IAdd { des, .. })
            | Self::Sub(ISub { des, .. })
            | Self::Mul(IMul { des, .. })
            | Self::Div(IDiv { des, .. })
            | Self::Cmp(ICmp { des, .. })
            | Self::Gt(IGt { des, .. })
            | Self::Lt(ILt { des, .. })
            | Self::Assign(IAssign { des, .. })
            | Self::Load(des, _)
            | Self::Phi(des, _)
            | Self::Call(ICall { des: Some(des), .. }) => vec![des.clone()],
            _ => vec![],
        }
    }
}

pub type InstructionIndex = usize;

#[derive(Debug, Default)]
pub struct LivenessAnalysisInfo {
    table: HashMap<String, HashMap<(BlockId, InstructionIndex), Vec<Variable>>>,
}

impl LivenessAnalysisInfo {
    pub fn add(
        &mut self,
        function_name: &str,
        block_id: BlockId,
        index: InstructionIndex,
        variable: Variable,
    ) {
        self.table
            .entry(function_name.to_string())
            .or_default()
            .entry((block_id, index))
            .or_default()
            .push(variable);
    }

    pub fn is_live(
        &self,
        function_name: &str,
        block: BlockId,
        instr_index: usize,
        var: &Variable,
    ) -> bool {
        self.table
            .get(function_name)
            .and_then(|block_table| block_table.get(&(block, instr_index)))
            .is_some_and(|vars| vars.contains(var))
    }
}

#[derive(Debug)]
pub struct LivenessAnalysisPass;

impl Pass for LivenessAnalysisPass {
    fn run(
        &mut self,
        module: &mut crate::ir::Module,
        ctx: &mut crate::backend::Context,
    ) -> Result<(), crate::error::Error> {
        eprintln!("LivenessAnalysisPass");

        for function in &module.functions {
            for block in &function.blocks {
                let mut live_after = std::collections::HashSet::new();

                for (index, instr) in block.instructions.iter().enumerate().rev() {
                    let uses = instr.uses();
                    let defines = instr.defines();

                    // live_before = (live_after - defines) ∪ uses
                    let mut live_before = live_after
                        .difference(&defines.iter().cloned().collect())
                        .cloned()
                        .collect::<std::collections::HashSet<_>>();

                    live_before.extend(uses.iter().cloned());

                    // Record which vars are live at this instruction
                    for var in &live_before {
                        ctx.liveness
                            .add(function.name.as_str(), block.id, index, var.clone());
                    }

                    live_after = live_before;
                }
            }
        }

        Ok(())
    }
}
