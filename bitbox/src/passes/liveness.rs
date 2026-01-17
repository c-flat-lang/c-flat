use crate::{
    ir::{
        BlockId, Variable,
        instruction::{
            IAdd, IAssign, ICall, ICmp, IDiv, IGt, IJumpIf, ILoad, ILt, IMul, IPhi, ISub,
        },
    },
    passes::DebugPass,
};

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
            Self::Assign(IAssign { src: op, .. })
            | Self::Load(ILoad { src: op, .. })
            | Self::JumpIf(IJumpIf { cond: op, .. }) => {
                if let crate::ir::Operand::Variable(v) = op {
                    vec![v.clone()]
                } else {
                    vec![]
                }
            }
            Self::Return(ireturn) => {
                if let crate::ir::Operand::Variable(v) = &ireturn.src {
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
            | Self::Load(ILoad { des, .. })
            | Self::Phi(IPhi { des, .. })
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

use std::collections::{HashMap, HashSet};

impl Pass for LivenessAnalysisPass {
    fn debug(
        &self,
        _module: &crate::ir::Module,
        ctx: &crate::backend::Context,
        debug_mode: Option<DebugPass>,
    ) -> bool {
        if !matches!(debug_mode, Some(DebugPass::LivenessAnalysis)) {
            return false;
        }

        eprintln!("{:?}", DebugPass::LivenessAnalysis);

        for (function_name, block_table) in &ctx.liveness.table {
            eprintln!("Function: {}", function_name);
            let mut blocks: Vec<(&crate::ir::BlockId, &usize, &[crate::ir::Variable])> =
                block_table
                    .iter()
                    .map(|((b, i), v)| (b, i, v.as_slice()))
                    .collect();
            blocks.sort_by(|a, b| a.0.0.cmp(&b.0.0).then(a.1.cmp(b.1)));

            for (block, index, vars) in blocks.iter() {
                eprintln!("{:?}", block);
                eprintln!("Instruction: {}", index);
                eprintln!("Live variables: {:#?}", vars);
            }
        }

        true
    }

    fn run(
        &mut self,
        module: &mut crate::ir::Module,
        ctx: &mut crate::backend::Context,
    ) -> Result<(), crate::error::Error> {
        eprintln!("LivenessAnalysisPass");

        for function in &module.functions {
            let mut label_to_block_id: HashMap<String, crate::ir::BlockId> = HashMap::new();
            let mut id_to_block: HashMap<crate::ir::BlockId, &crate::ir::BasicBlock> =
                HashMap::new();
            for block in &function.blocks {
                label_to_block_id.insert(block.label.clone(), block.id);
                id_to_block.insert(block.id, block);
            }

            let mut r#gen: HashMap<crate::ir::BlockId, HashSet<crate::ir::Variable>> =
                HashMap::new();
            let mut kill: HashMap<crate::ir::BlockId, HashSet<crate::ir::Variable>> =
                HashMap::new();

            for block in &function.blocks {
                let mut g: HashSet<crate::ir::Variable> = HashSet::new();
                let mut k: HashSet<crate::ir::Variable> = HashSet::new();

                for instr in &block.instructions {
                    match instr {
                        crate::ir::Instruction::Phi(iphi) => {
                            k.insert(iphi.des.clone());
                        }
                        _ => {
                            for u in instr.uses() {
                                if !k.contains(&u) {
                                    g.insert(u);
                                }
                            }
                            for d in instr.defines() {
                                k.insert(d);
                            }
                        }
                    }
                }

                r#gen.insert(block.id, g);
                kill.insert(block.id, k);
            }

            let mut live_in: HashMap<crate::ir::BlockId, HashSet<crate::ir::Variable>> =
                HashMap::new();
            let mut live_out: HashMap<crate::ir::BlockId, HashSet<crate::ir::Variable>> =
                HashMap::new();

            for block in &function.blocks {
                live_in.insert(block.id, HashSet::new());
                live_out.insert(block.id, HashSet::new());
            }

            let out_bound = ctx
                .cfg
                .out_bound
                .get(&function.name)
                .cloned()
                .unwrap_or_default();

            let mut changed = true;
            while changed {
                changed = false;

                for block in &function.blocks {
                    let b_id = block.id;

                    let mut new_live_out: HashSet<crate::ir::Variable> = HashSet::new();

                    let succs = out_bound.get(&b_id).cloned().unwrap_or_else(Vec::new);

                    for succ in succs {
                        let succ_live_in = live_in.get(&succ).cloned().unwrap_or_default();
                        let mut temp: HashSet<crate::ir::Variable> = succ_live_in;

                        if let Some(succ_block) = id_to_block.get(&succ) {
                            for instr in &succ_block.instructions {
                                if let crate::ir::Instruction::Phi(iphi) = instr {
                                    temp.remove(&iphi.des);
                                }
                            }

                            for instr in &succ_block.instructions {
                                if let crate::ir::Instruction::Phi(iphi) = instr {
                                    for (label, v) in &iphi.branches {
                                        // compare label strings
                                        if let Some(current_label) =
                                            id_to_block.get(&b_id).map(|b| b.label.as_str())
                                            && label == current_label
                                        {
                                            temp.insert(v.clone());
                                        }
                                    }
                                }
                            }
                        }

                        new_live_out.extend(temp);
                    }

                    // live_in = r#gen ∪ (live_out - kill)
                    let block_gen = r#gen.get(&b_id).cloned().unwrap_or_default();
                    let block_kill = kill.get(&b_id).cloned().unwrap_or_default();

                    let mut new_live_in: HashSet<crate::ir::Variable> = HashSet::new();

                    // live_out - kill
                    for v in new_live_out.iter().cloned() {
                        if !block_kill.contains(&v) {
                            new_live_in.insert(v);
                        }
                    }

                    new_live_in.extend(block_gen.iter().cloned());

                    // check changes
                    let in_changed = match live_in.get(&b_id) {
                        Some(old) => &new_live_in != old,
                        None => true,
                    };
                    let out_changed = match live_out.get(&b_id) {
                        Some(old) => &new_live_out != old,
                        None => true,
                    };

                    if in_changed || out_changed {
                        changed = true;
                        live_in.insert(b_id, new_live_in);
                        live_out.insert(b_id, new_live_out);
                    }
                }
            }

            for block in function.blocks.iter() {
                let mut live_after = live_out.get(&block.id).cloned().unwrap_or_default();

                for (index, instr) in block.instructions.iter().enumerate().rev() {
                    let uses = instr.uses();
                    let defines = instr.defines();
                    // eprintln!(
                    //     "{bi} {index} {defines:?} {:?}",
                    //     ctx.cfg.out_bound.get(&BlockId(block.id.0 + 1))
                    // );

                    // live_before = (live_after - defines) ∪ uses
                    let mut live_before: HashSet<crate::ir::Variable> = HashSet::new();

                    for v in live_after.iter().cloned() {
                        if !defines.contains(&v) {
                            live_before.insert(v);
                        }
                    }

                    for u in uses {
                        live_before.insert(u);
                    }

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
