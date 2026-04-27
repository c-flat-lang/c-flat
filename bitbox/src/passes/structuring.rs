use std::collections::HashMap;

use super::Pass;
use crate::{
    ir::{
        BasicBlock, BlockId, Function, Instruction, Operand, Type, Variable, instruction::IIfElse,
    },
    passes::{DebugPass, PassOutput},
};
#[cfg(feature = "uuids")]
use uuid::Uuid;

#[derive(Default)]
pub struct StructuringPass;

impl Pass for StructuringPass {
    fn debug_pass(&self) -> DebugPass {
        DebugPass::StructuringIr
    }

    fn debug(&self, module: &crate::ir::Module, _ctx: &crate::backend::Context) -> PassOutput {
        PassOutput::String(format!("{}", module))
    }

    fn run(
        &mut self,
        module: &mut crate::ir::Module,
        ctx: &mut crate::backend::Context,
    ) -> Result<(), crate::error::Error> {
        for func in &mut module.functions {
            loop {
                let mut changed = false;
                if try_raise_if_else(func, ctx) {
                    changed = true;
                }
                if !changed {
                    break;
                }
            }
            // Raise if-else patterns inside nested ILoop/IIfElse block lists.
            raise_nested_in_blocks(&mut func.blocks);
        }
        Ok(())
    }
}

/// Recursively raises if-else patterns inside nested `ILoop`/`IIfElse` block lists.
fn raise_nested_in_blocks(blocks: &mut [BasicBlock]) {
    for block in blocks.iter_mut() {
        for inst in block.instructions.iter_mut() {
            match inst {
                Instruction::Loop(iloop) => {
                    for inner_blocks in [&mut iloop.cond, &mut iloop.body] {
                        loop {
                            if !try_raise_if_else_in_blocks(inner_blocks) {
                                break;
                            }
                        }
                        raise_nested_in_blocks(inner_blocks);
                    }
                }
                Instruction::IfElse(iifelse) => {
                    for inner_blocks in [
                        &mut iifelse.cond,
                        &mut iifelse.then_branch,
                        &mut iifelse.else_branch,
                    ] {
                        loop {
                            if !try_raise_if_else_in_blocks(inner_blocks) {
                                break;
                            }
                        }
                        raise_nested_in_blocks(inner_blocks);
                    }
                }
                _ => {}
            }
        }
    }
}

/// Compute out-bound edges for a local set of blocks by scanning jump/jumpif instructions.
fn compute_local_out_bound(blocks: &[BasicBlock]) -> HashMap<BlockId, Vec<BlockId>> {
    let label_to_id: HashMap<String, BlockId> =
        blocks.iter().map(|b| (b.label.clone(), b.id)).collect();

    let mut out_bound: HashMap<BlockId, Vec<BlockId>> = HashMap::new();
    for block in blocks {
        let mut successors = vec![];
        for inst in &block.instructions {
            match inst {
                Instruction::Jump(ij) => {
                    if let Some(&id) = label_to_id.get(&ij.label) {
                        successors.push(id);
                    }
                }
                Instruction::JumpIf(jif) => {
                    if let Some(&id) = label_to_id.get(&jif.label) {
                        successors.push(id);
                    }
                }
                _ => {}
            }
        }
        if !successors.is_empty() {
            out_bound.insert(block.id, successors);
        }
    }
    out_bound
}

/// Attempts to raise the first if-else pattern found in a local `Vec<BasicBlock>`.
/// Returns `true` if an if-else was successfully raised.
fn try_raise_if_else_in_blocks(blocks: &mut Vec<BasicBlock>) -> bool {
    let out_bound = compute_local_out_bound(blocks);

    for m_idx in (0..blocks.len()).rev() {
        let merge_id = blocks[m_idx].id;

        let mut branch_ids: Vec<BlockId> = vec![];
        for (&from_id, tos) in out_bound.iter() {
            if tos.contains(&merge_id) {
                branch_ids.push(from_id);
            }
        }
        if branch_ids.is_empty() {
            continue;
        }

        let mut then_ids: Vec<BlockId> = vec![];
        let mut else_ids: Vec<BlockId> = vec![];

        for &bid in &branch_ids {
            let block = blocks.iter().find(|b| b.id == bid).unwrap();
            let label = &block.label;
            if label.starts_with("then.") || label.starts_with("then_") || label == "then" {
                then_ids.push(bid);
            } else if label.starts_with("else.") || label.starts_with("else_") || label == "else" {
                else_ids.push(bid);
            }
        }

        let has_else = !else_ids.is_empty();
        if then_ids.is_empty() {
            continue;
        }

        let mut cond_id = None;
        let mut cond_result = Variable::new("undefined", Type::Void);

        for (&from_id, tos) in out_bound.iter() {
            if tos.len() != 2 {
                continue;
            }

            let target1 = tos[0];
            let target2 = tos[1];

            let then_target = if has_else
                && then_ids.contains(&target1)
                && (else_ids.contains(&target2)
                    || (then_ids.contains(&target2) && else_ids.contains(&target1)))
            {
                true
            } else {
                (then_ids.contains(&target1) && target2 == merge_id)
                    || (then_ids.contains(&target2) && target1 == merge_id)
            };

            if then_target
                && let Some(cond_block) = blocks.iter().find(|b| b.id == from_id)
                && let Some((Instruction::JumpIf(jif), Instruction::Jump(_))) = cond_block
                    .instructions
                    .last_chunk::<2>()
                    .map(|i| (i[0].clone(), i[1].clone()))
            {
                if let Operand::Variable(v) = &jif.cond {
                    cond_result = v.clone();
                }
                cond_id = Some(from_id);
                break;
            }
        }

        let Some(cond_id) = cond_id else {
            continue;
        };
        let cond_idx = blocks.iter().position(|b| b.id == cond_id).unwrap();
        let merge_block = blocks[m_idx].clone();

        let mut cond_block = blocks[cond_idx].clone();
        while let Some(last) = cond_block.instructions.last() {
            if matches!(last, Instruction::JumpIf(_) | Instruction::Jump(_)) {
                cond_block.instructions.pop();
            } else {
                break;
            }
        }

        let then_branch: Vec<BasicBlock> = then_ids
            .iter()
            .filter_map(|&tid| {
                let idx = blocks.iter().position(|b| b.id == tid)?;
                let mut bb = blocks[idx].clone();
                if let Some(Instruction::Jump(ij)) = bb.instructions.last()
                    && ij.label == merge_block.label
                {
                    bb.instructions.pop();
                }
                Some(bb)
            })
            .collect();

        let else_branch: Vec<BasicBlock> = if has_else {
            else_ids
                .iter()
                .filter_map(|&eid| {
                    let idx = blocks.iter().position(|b| b.id == eid)?;
                    let mut bb = blocks[idx].clone();
                    if let Some(Instruction::Jump(ij)) = bb.instructions.last()
                        && ij.label == merge_block.label
                    {
                        bb.instructions.pop();
                    }
                    Some(bb)
                })
                .collect()
        } else {
            vec![]
        };

        let result = if let Some(Instruction::Phi(iphi)) = merge_block.instructions.first() {
            Some(iphi.des.clone())
        } else {
            None
        };

        let iifelse = IIfElse {
            cond: vec![BasicBlock {
                id: BlockId(0),
                label: String::new(),
                instructions: cond_block.instructions.clone(),
            }],
            cond_result,
            then_branch,
            else_branch,
            result,
        };

        let mut new_insts = vec![Instruction::IfElse(iifelse)];
        let mut after = merge_block.instructions.clone();
        if !after.is_empty() && matches!(after[0], Instruction::Phi(_)) {
            after.remove(0);
        }
        new_insts.extend(after);

        cond_block.instructions = new_insts;
        blocks[cond_idx] = cond_block;

        let mut to_remove: Vec<usize> = then_ids
            .iter()
            .chain(else_ids.iter())
            .filter_map(|&id| blocks.iter().position(|b| b.id == id))
            .collect();
        if let Some(merge_pos) = blocks.iter().position(|b| b.id == merge_id) {
            to_remove.push(merge_pos);
        }
        to_remove.sort_unstable_by(|a, b| b.cmp(a));
        for &r in &to_remove {
            if r != cond_idx {
                blocks.remove(r);
            }
        }

        renumber_local_blocks(blocks);
        return true;
    }
    false
}

fn renumber_local_blocks(blocks: &mut [BasicBlock]) {
    for (i, b) in blocks.iter_mut().enumerate() {
        b.id = BlockId(i);
    }
}

/// Attempts to find and raise the first (innermost) if-else pattern using CFG info.
/// Returns `true` if an if-else was successfully raised.
fn try_raise_if_else(func: &mut Function, ctx: &crate::backend::Context) -> bool {
    let func_name = &func.name;

    let out_bound = match ctx.cfg.out_bound.get(func_name) {
        Some(table) => table,
        None => return false,
    };

    for m_idx in (0..func.blocks.len()).rev() {
        let merge_block = &func.blocks[m_idx];
        let merge_id = merge_block.id;

        let mut branch_ids: Vec<BlockId> = vec![];
        for (&from_id, tos) in out_bound.iter() {
            if tos.contains(&merge_id) {
                branch_ids.push(from_id);
            }
        }
        if branch_ids.is_empty() {
            continue;
        }

        let mut then_ids: Vec<BlockId> = vec![];
        let mut else_ids: Vec<BlockId> = vec![];

        for &bid in &branch_ids {
            let block = func.blocks.iter().find(|b| b.id == bid).unwrap();
            let label = &block.label;

            if label.starts_with("then.") || label.starts_with("then_") || label == "then" {
                then_ids.push(bid);
            } else if label.starts_with("else.") || label.starts_with("else_") || label == "else" {
                else_ids.push(bid);
            }
        }

        let has_else = !else_ids.is_empty();
        if then_ids.is_empty() {
            continue;
        }

        let mut cond_id = None;
        let mut cond_result = Variable::new("undefined", Type::Void);

        for (&from_id, tos) in out_bound.iter() {
            if tos.len() != 2 {
                continue;
            }

            let targets: Vec<BlockId> = tos.clone();
            let target1 = targets[0];
            let target2 = targets[1];

            let then_target = if has_else
                && then_ids.contains(&target1)
                && (else_ids.contains(&target2)
                    || (then_ids.contains(&target2) && else_ids.contains(&target1)))
            {
                true
            } else {
                (then_ids.contains(&target1) && target2 == merge_id)
                    || (then_ids.contains(&target2) && target1 == merge_id)
            };

            if then_target
                && let Some(cond_block) = func.blocks.iter().find(|b| b.id == from_id)
                && let Some((Instruction::JumpIf(jif), Instruction::Jump(_))) = cond_block
                    .instructions
                    .last_chunk::<2>()
                    .map(|i| (i[0].clone(), i[1].clone()))
            {
                if let Operand::Variable(v) = &jif.cond {
                    cond_result = v.clone();
                }
                cond_id = Some(from_id);
                break;
            }
        }

        let Some(cond_id) = cond_id else {
            continue;
        };
        let cond_idx = func.blocks.iter().position(|b| b.id == cond_id).unwrap();

        let mut cond_block = func.blocks[cond_idx].clone();

        while let Some(last) = cond_block.instructions.last() {
            if matches!(last, Instruction::JumpIf(_) | Instruction::Jump(_)) {
                cond_block.instructions.pop();
            } else {
                break;
            }
        }

        let then_branch: Vec<BasicBlock> = then_ids
            .iter()
            .filter_map(|&tid| {
                let idx = func.blocks.iter().position(|b| b.id == tid)?;
                let mut bb = func.blocks[idx].clone();
                if let Some(Instruction::Jump(ij)) = bb.instructions.last()
                    && ij.label == merge_block.label
                {
                    bb.instructions.pop();
                }
                Some(bb)
            })
            .collect();

        let else_branch: Vec<BasicBlock> = if has_else {
            else_ids
                .iter()
                .filter_map(|&eid| {
                    let idx = func.blocks.iter().position(|b| b.id == eid)?;
                    let mut bb = func.blocks[idx].clone();
                    if let Some(Instruction::Jump(ij)) = bb.instructions.last()
                        && ij.label == merge_block.label
                    {
                        bb.instructions.pop();
                    }
                    Some(bb)
                })
                .collect()
        } else {
            vec![]
        };

        let result = if let Some(Instruction::Phi(iphi)) = merge_block.instructions.first() {
            Some(iphi.des.clone())
        } else {
            None
        };

        let iifelse = IIfElse {
            cond: vec![BasicBlock {
                id: BlockId(0),
                label: String::new(),
                instructions: cond_block.instructions.clone(),
            }],
            cond_result,
            then_branch,
            else_branch,
            result,
        };

        let mut new_insts = vec![Instruction::IfElse(iifelse)];

        let mut after = merge_block.instructions.clone();
        if !after.is_empty() && matches!(after[0], Instruction::Phi(_)) {
            after.remove(0);
        }
        new_insts.extend(after);

        cond_block.instructions = new_insts;
        func.blocks[cond_idx] = cond_block;

        let mut to_remove: Vec<usize> = then_ids
            .iter()
            .chain(else_ids.iter())
            .filter_map(|&id| func.blocks.iter().position(|b| b.id == id))
            .collect();
        if let Some(merge_pos) = func.blocks.iter().position(|b| b.id == merge_id) {
            to_remove.push(merge_pos);
        }
        to_remove.sort_unstable_by(|a, b| b.cmp(a));

        for &r in &to_remove {
            if r != cond_idx {
                func.blocks.remove(r);
            }
        }

        renumber_blocks(func);
        return true;
    }
    false
}

fn renumber_blocks(func: &mut Function) {
    for (i, b) in func.blocks.iter_mut().enumerate() {
        b.id = BlockId(i);
    }
}
