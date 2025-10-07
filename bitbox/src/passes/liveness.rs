use crate::ir::{BlockId, Variable};

use super::Pass;

impl crate::ir::Instruction {
    pub fn uses(&self) -> Vec<Variable> {
        match self {
            Self::Add(_, a, b)
            | Self::Sub(_, a, b)
            | Self::Mul(_, a, b)
            | Self::Div(_, a, b)
            | Self::Cmp(_, a, b)
            | Self::Gt(_, a, b) => {
                let mut vars = Vec::new();
                if let crate::ir::Operand::Variable(v) = a {
                    vars.push(v.clone());
                }
                if let crate::ir::Operand::Variable(v) = b {
                    vars.push(v.clone());
                }
                vars
            }
            Self::Assign(_, op) | Self::Load(_, op) | Self::JumpIf(op, _) => {
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
            Self::Add(var, _, _)
            | Self::Sub(var, _, _)
            | Self::Mul(var, _, _)
            | Self::Div(var, _, _)
            | Self::Cmp(var, _, _)
            | Self::Gt(var, _, _)
            | Self::Assign(var, _)
            | Self::Load(var, _)
            | Self::Phi(var, _)
            | Self::Call(var, _, _) => vec![var.clone()],
            _ => vec![],
        }
    }
}

pub type InstructionIndex = usize;

#[derive(Debug, Default)]
pub struct LivenessAnalysisInfo {
    table: std::collections::HashMap<(BlockId, InstructionIndex), Vec<Variable>>,
}

impl LivenessAnalysisInfo {
    pub fn add(&mut self, block_id: BlockId, index: InstructionIndex, variable: Variable) {
        self.table
            .entry((block_id, index))
            .or_default()
            .push(variable);
    }

    pub fn is_live(&self, block: BlockId, instr_index: usize, var: &Variable) -> bool {
        self.table
            .get(&(block, instr_index))
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
        eprintln!("Running LivenessAnalysisPass");

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
                        ctx.liveness.add(block.id, index, var.clone());
                    }

                    live_after = live_before;
                }
            }
        }

        Ok(())
    }
}
