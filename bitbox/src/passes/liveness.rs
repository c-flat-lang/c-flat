use crate::{
    ir::{
        BasicBlock, BlockId, Instruction, Variable,
        instruction::{
            IAdd, IAlloc, IAnd, IAssign, ICall, ICmp, ICopy, IDiv, IElemGet, IElemSet, IGt, IGte,
            IIfElse, IJump, IJumpIf, ILoad, ILoop, ILt, IMul, INoOp, INot, IOr, IPhi, IRef, IRem,
            IReturn, ISub, IXOr,
        },
    },
    passes::{DebugPass, PassOutput},
};

use super::Pass;

trait LivenessAnalysis {
    fn uses(&self) -> Vec<Variable>;
    fn defines(&self) -> Vec<Variable>;
}

macro_rules! liveness_ops {
    ($struct_name:ident) => {
        impl LivenessAnalysis for $struct_name {
            fn uses(&self) -> Vec<Variable> {
                let mut vars = Vec::new();
                if let crate::ir::Operand::Variable(v) = &self.lhs {
                    vars.push(v.clone());
                }
                if let crate::ir::Operand::Variable(v) = &self.rhs {
                    vars.push(v.clone());
                }
                vars
            }

            fn defines(&self) -> Vec<Variable> {
                vec![self.des.clone()]
            }
        }
    };
    ($($struct_name:ident),* $(,)?) => {
        $(liveness_ops!($struct_name);)*
    };
}

liveness_ops!(
    IAdd, ISub, IMul, IDiv, ICmp, IGt, IGte, IRem, ILt, IAnd, IOr, IXOr
);

impl LivenessAnalysis for IAssign {
    fn uses(&self) -> Vec<Variable> {
        if let crate::ir::Operand::Variable(v) = &self.src {
            vec![v.clone()]
        } else {
            vec![]
        }
    }
    fn defines(&self) -> Vec<Variable> {
        vec![self.des.clone()]
    }
}

impl LivenessAnalysis for ILoad {
    fn uses(&self) -> Vec<Variable> {
        if let crate::ir::Operand::Variable(v) = &self.src {
            vec![v.clone()]
        } else {
            vec![]
        }
    }
    fn defines(&self) -> Vec<Variable> {
        vec![self.des.clone()]
    }
}

impl LivenessAnalysis for IJumpIf {
    fn uses(&self) -> Vec<Variable> {
        if let crate::ir::Operand::Variable(v) = &self.cond {
            vec![v.clone()]
        } else {
            vec![]
        }
    }
    fn defines(&self) -> Vec<Variable> {
        vec![]
    }
}

impl LivenessAnalysis for IJump {
    fn uses(&self) -> Vec<Variable> {
        vec![]
    }
    fn defines(&self) -> Vec<Variable> {
        vec![]
    }
}

impl LivenessAnalysis for ICopy {
    fn uses(&self) -> Vec<Variable> {
        if let crate::ir::Operand::Variable(v) = &self.src {
            vec![v.clone()]
        } else {
            vec![]
        }
    }
    fn defines(&self) -> Vec<Variable> {
        vec![self.des.clone()]
    }
}

impl LivenessAnalysis for IIfElse {
    fn uses(&self) -> Vec<Variable> {
        let mut vars = vec![];
        for block in self.cond.iter() {
            for instruction in block.instructions.iter() {
                vars.extend(instruction.uses());
            }
        }

        for block in self.then_branch.iter() {
            for instruction in block.instructions.iter() {
                vars.extend(instruction.uses());
            }
        }
        for block in self.else_branch.iter() {
            for instruction in block.instructions.iter() {
                vars.extend(instruction.uses());
            }
        }
        vars
    }
    fn defines(&self) -> Vec<Variable> {
        let mut vars = vec![];
        for block in self.cond.iter() {
            for instruction in block.instructions.iter() {
                vars.extend(instruction.defines());
            }
        }

        vars.push(self.cond_result.clone());

        for block in self.then_branch.iter() {
            for instruction in block.instructions.iter() {
                vars.extend(instruction.defines());
            }
        }
        for block in self.else_branch.iter() {
            for instruction in block.instructions.iter() {
                vars.extend(instruction.defines());
            }
        }
        if let Some(v) = self.result.as_ref() {
            vars.push(v.clone());
        }
        vars
    }
}

impl LivenessAnalysis for IReturn {
    fn uses(&self) -> Vec<Variable> {
        if let crate::ir::Operand::Variable(v) = &self.src {
            vec![v.clone()]
        } else {
            vec![]
        }
    }
    fn defines(&self) -> Vec<Variable> {
        vec![]
    }
}

impl LivenessAnalysis for IAlloc {
    fn uses(&self) -> Vec<Variable> {
        if let crate::ir::Operand::Variable(v) = &self.size {
            vec![v.clone()]
        } else {
            vec![]
        }
    }
    fn defines(&self) -> Vec<Variable> {
        vec![self.des.clone()]
    }
}

impl LivenessAnalysis for IElemGet {
    fn uses(&self) -> Vec<Variable> {
        let mut vars = vec![];
        if let crate::ir::Operand::Variable(v) = &self.ptr {
            vars.push(v.clone());
        }
        if let crate::ir::Operand::Variable(v) = &self.index {
            vars.push(v.clone());
        }
        vars
    }
    fn defines(&self) -> Vec<Variable> {
        vec![self.des.clone()]
    }
}

impl LivenessAnalysis for IElemSet {
    fn uses(&self) -> Vec<Variable> {
        let mut vars = vec![];
        vars.push(self.addr.clone());
        if let crate::ir::Operand::Variable(v) = &self.index {
            vars.push(v.clone());
        }
        if let crate::ir::Operand::Variable(v) = &self.value {
            vars.push(v.clone());
        }
        vars
    }
    fn defines(&self) -> Vec<Variable> {
        vec![]
    }
}

impl LivenessAnalysis for IPhi {
    fn uses(&self) -> Vec<Variable> {
        let mut vars = vec![];
        for (_, v) in self.branches.iter() {
            vars.push(v.clone());
        }
        vars
    }
    fn defines(&self) -> Vec<Variable> {
        vec![self.des.clone()]
    }
}

impl LivenessAnalysis for ILoop {
    fn uses(&self) -> Vec<Variable> {
        let mut vars = vec![];
        for block in self.cond.iter() {
            for instruction in block.instructions.iter() {
                vars.extend(instruction.uses());
            }
        }

        for block in self.body.iter() {
            for instruction in block.instructions.iter() {
                vars.extend(instruction.uses());
            }
        }
        vars
    }
    fn defines(&self) -> Vec<Variable> {
        let mut vars = vec![];
        for block in self.cond.iter() {
            for instruction in block.instructions.iter() {
                vars.extend(instruction.defines());
            }
        }

        vars.push(self.cond_result.clone());

        for block in self.body.iter() {
            for instruction in block.instructions.iter() {
                vars.extend(instruction.defines());
            }
        }
        vars
    }
}

impl LivenessAnalysis for ICall {
    fn uses(&self) -> Vec<Variable> {
        let mut vars = vec![];
        for arg in self.args.iter() {
            if let crate::ir::Operand::Variable(v) = arg {
                vars.push(v.clone());
            }
        }
        vars
    }

    fn defines(&self) -> Vec<Variable> {
        let Some(v) = self.des.as_ref() else {
            return vec![];
        };
        vec![v.clone()]
    }
}

impl LivenessAnalysis for INoOp {
    fn uses(&self) -> Vec<Variable> {
        vec![]
    }
    fn defines(&self) -> Vec<Variable> {
        vec![]
    }
}

impl LivenessAnalysis for IRef {
    fn uses(&self) -> Vec<Variable> {
        vec![self.src.clone()]
    }
    fn defines(&self) -> Vec<Variable> {
        vec![self.des.clone()]
    }
}

impl LivenessAnalysis for INot {
    fn uses(&self) -> Vec<Variable> {
        vec![self.src.clone()]
    }
    fn defines(&self) -> Vec<Variable> {
        vec![self.des.clone()]
    }
}

impl crate::ir::Instruction {
    pub fn uses(&self) -> Vec<Variable> {
        match self {
            Self::NoOp(i) => i.uses(),
            Self::Add(i) => i.uses(),
            Self::Assign(i) => i.uses(),
            Self::Alloc(i) => i.uses(),
            Self::Call(i) => i.uses(),
            Self::Cmp(i) => i.uses(),
            Self::Copy(i) => i.uses(),
            Self::ElemGet(i) => i.uses(),
            Self::ElemSet(i) => i.uses(),
            Self::And(i) => i.uses(),
            Self::Or(i) => i.uses(),
            Self::XOr(i) => i.uses(),
            Self::Gt(i) => i.uses(),
            Self::Gte(i) => i.uses(),
            Self::Rem(i) => i.uses(),
            Self::Lt(i) => i.uses(),
            Self::Jump(i) => i.uses(),
            Self::JumpIf(i) => i.uses(),
            Self::Load(i) => i.uses(),
            Self::Mul(i) => i.uses(),
            Self::Phi(i) => i.uses(),
            Self::Return(i) => i.uses(),
            Self::Sub(i) => i.uses(),
            Self::Div(i) => i.uses(),
            Self::IfElse(i) => i.uses(),
            Self::Loop(i) => i.uses(),
            Self::Ref(i) => i.uses(),
            Self::Not(i) => i.uses(),
        }
    }

    pub fn defines(&self) -> Vec<Variable> {
        match self {
            Self::NoOp(i) => i.defines(),
            Self::Add(i) => i.defines(),
            Self::Assign(i) => i.defines(),
            Self::Alloc(i) => i.defines(),
            Self::Call(i) => i.defines(),
            Self::Cmp(i) => i.defines(),
            Self::Copy(i) => i.defines(),
            Self::ElemGet(i) => i.defines(),
            Self::ElemSet(i) => i.defines(),
            Self::And(i) => i.defines(),
            Self::Or(i) => i.defines(),
            Self::XOr(i) => i.defines(),
            Self::Gt(i) => i.defines(),
            Self::Gte(i) => i.defines(),
            Self::Rem(i) => i.defines(),
            Self::Lt(i) => i.defines(),
            Self::Jump(i) => i.defines(),
            Self::JumpIf(i) => i.defines(),
            Self::Load(i) => i.defines(),
            Self::Mul(i) => i.defines(),
            Self::Phi(i) => i.defines(),
            Self::Return(i) => i.defines(),
            Self::Sub(i) => i.defines(),
            Self::Div(i) => i.defines(),
            Self::IfElse(i) => i.defines(),
            Self::Loop(i) => i.defines(),
            Self::Ref(i) => i.defines(),
            Self::Not(i) => i.defines(),
        }
    }
}

pub type InstructionIndex = usize;

#[derive(Debug, Default)]
pub struct LivenessAnalysisInfo {
    pub(crate) table: HashMap<String, HashMap<(BlockId, InstructionIndex), Vec<Variable>>>,
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

    pub fn is_live_after(
        &self,
        function_name: &str,
        block: BlockId,
        instr_index: usize,
        var: &Variable,
    ) -> bool {
        let Some(block_table) = self.table.get(function_name) else {
            return false;
        };

        // If the variable appears at any recorded point that is:
        //  - in the same block and at an instruction index >= instr_index, OR
        //  - in some other block (we conservatively assume it could be reachable),
        // then treat it as live-after.
        for ((b, i), vars) in block_table.iter() {
            if b == &block {
                if i >= &instr_index && vars.contains(var) {
                    return true;
                }
            } else {
                // Conservative: variable appears later in another block.
                if vars.contains(var) {
                    return true;
                }
            }
        }

        false
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

    pub fn format(&self, module: &crate::ir::Module) -> String {
        let mut output = String::new();
        let mut function_names: Vec<&String> = self.table.keys().collect();
        function_names.sort();
        for function_name in function_names {
            let block_table = &self.table[function_name];
            output += &format!("Function: {}\n", function_name);
            let mut blocks: Vec<(&crate::ir::BlockId, &usize, &[crate::ir::Variable])> =
                block_table
                    .iter()
                    .map(|((b, i), v)| (b, i, v.as_slice()))
                    .collect();
            blocks.sort_by(|a, b| a.0.0.cmp(&b.0.0).then(a.1.cmp(b.1)));

            let Some(function) = module.functions.iter().find(|f| &f.name == function_name) else {
                panic!("Function {} not found", function_name);
            };

            let ir_block = &function.blocks;

            let mut last_block: Option<&&BlockId> = None;
            for (block, index, vars) in blocks.iter() {
                if last_block.is_none() || last_block.is_some_and(|b| b != block) {
                    output += &format!("{:?}\n", block);
                }
                output += &format!(
                    "Instruction: {}, {}\n",
                    index, ir_block[block.0].instructions[**index]
                );
                output += &format!("Live variables: {}\n", {
                    let mut sorted: Vec<_> = vars.iter().map(|v| v.name.clone()).collect();
                    sorted.sort();
                    sorted
                        .iter()
                        .map(|n| format!("{}, ", n))
                        .collect::<String>()
                });
                last_block = Some(block);
            }
        }
        output
    }
}

#[derive(Debug)]
pub struct LivenessAnalysisPass;

use std::collections::{HashMap, HashSet};

impl Pass for LivenessAnalysisPass {
    fn debug_pass(&self) -> DebugPass {
        DebugPass::LivenessAnalysis
    }

    fn debug(&self, module: &crate::ir::Module, ctx: &crate::backend::Context) -> PassOutput {
        PassOutput::String(ctx.liveness.format(module))
    }

    fn run(
        &mut self,
        module: &mut crate::ir::Module,
        ctx: &mut crate::backend::Context,
    ) -> Result<(), crate::error::Error> {
        for function in &module.functions {
            let mut label_to_block_id: HashMap<String, BlockId> = HashMap::new();
            let mut id_to_block: HashMap<BlockId, &BasicBlock> = HashMap::new();
            for block in &function.blocks {
                label_to_block_id.insert(block.label.clone(), block.id);
                id_to_block.insert(block.id, block);
            }

            let mut r#gen: HashMap<BlockId, HashSet<Variable>> = HashMap::new();
            let mut kill: HashMap<BlockId, HashSet<Variable>> = HashMap::new();

            for block in &function.blocks {
                let mut g: HashSet<Variable> = HashSet::new();
                let mut k: HashSet<Variable> = HashSet::new();

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

            let mut live_in: HashMap<BlockId, HashSet<Variable>> = HashMap::new();
            let mut live_out: HashMap<BlockId, HashSet<Variable>> = HashMap::new();

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

                    let mut new_live_out: HashSet<Variable> = HashSet::new();
                    let succs = out_bound.get(&b_id).cloned().unwrap_or_else(Vec::new);

                    for succ in succs {
                        let succ_live_in = live_in.get(&succ).cloned().unwrap_or_default();
                        let mut temp = succ_live_in;

                        if let Some(succ_block) = id_to_block.get(&succ) {
                            // remove phi defs
                            for instr in &succ_block.instructions {
                                if let Instruction::Phi(iphi) = instr {
                                    temp.remove(&iphi.des);
                                }
                            }

                            // add phi uses for this edge
                            for instr in &succ_block.instructions {
                                if let Instruction::Phi(iphi) = instr {
                                    for (label, v) in &iphi.branches {
                                        if let Some(cur_label) =
                                            id_to_block.get(&b_id).map(|b| b.label.as_str())
                                            && label == cur_label
                                        {
                                            temp.insert(v.clone());
                                        }
                                    }
                                }
                            }
                        }

                        new_live_out.extend(temp);
                    }

                    // ✅ instruction-level computation ONLY
                    let mut new_live_in = new_live_out.clone();
                    for instr in block.instructions.iter().rev() {
                        for d in instr.defines() {
                            new_live_in.remove(&d);
                        }
                        for u in instr.uses() {
                            new_live_in.insert(u);
                        }
                    }

                    let in_changed = live_in.get(&b_id).is_none_or(|old| *old != new_live_in);
                    let out_changed = live_out.get(&b_id).is_none_or(|old| *old != new_live_out);

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

                    // live_before = (live_after - defines) ∪ uses
                    let mut live_before: HashSet<Variable> = HashSet::new();

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
