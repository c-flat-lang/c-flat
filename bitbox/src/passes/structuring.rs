use std::collections::HashMap;

use super::Pass;
use crate::{
    ir::{BasicBlock, BlockId, Instruction, Operand, Type, Variable, instruction::IIfElse},
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
        _ctx: &mut crate::backend::Context,
    ) -> Result<(), crate::error::Error> {
        for func in &mut module.functions {
            // Recompute edges from the current blocks each pass (via
            // try_raise_if_else_in_blocks) so nested / else-if chains raise fully.
            // Using the global CFG here would go stale after the first raise
            // (blocks are removed + renumbered), leaving outer ifs unstructured
            // and producing raw jumps the wasm backend can't encode.
            while try_raise_if_else_in_blocks(&mut func.blocks) {}
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

/// Handles the case where the then-branch spans multiple consecutive blocks and the
/// then-block is therefore NOT a direct predecessor of the merge block.
///
/// Pattern: `cond` ends with `@jumpif cond_var, %then.*` + `@jump %merge_label`.
/// All blocks between `then.*` and `merge_label` (exclusive) become the then-branch.
fn try_raise_if_else_multi_block_in_blocks(blocks: &mut Vec<BasicBlock>) -> bool {
    let label_to_idx: HashMap<&str, usize> = blocks
        .iter()
        .enumerate()
        .map(|(i, b)| (b.label.as_str(), i))
        .collect();

    for cond_idx in 0..blocks.len() {
        let Some((Instruction::JumpIf(jif), Instruction::Jump(ij))) = blocks[cond_idx]
            .instructions
            .last_chunk::<2>()
            .map(|chunk| (chunk[0].clone(), chunk[1].clone()))
        else {
            continue;
        };

        let then_label = jif.label.clone();
        let merge_label = ij.label.clone();

        let is_then = then_label.starts_with("then.")
            || then_label.starts_with("then_")
            || then_label == "then";
        if !is_then {
            continue;
        }

        let Some(&then_idx) = label_to_idx.get(then_label.as_str()) else {
            continue;
        };
        let Some(&merge_idx) = label_to_idx.get(merge_label.as_str()) else {
            continue;
        };

        // Require strict ordering: cond < then < merge
        if then_idx <= cond_idx || merge_idx <= then_idx {
            continue;
        }

        let cond_result = if let Operand::Variable(v) = &jif.cond {
            v.clone()
        } else {
            continue;
        };

        // Build cond instructions (strip trailing JumpIf + Jump)
        let mut cond_insts = blocks[cond_idx].instructions.clone();
        cond_insts.pop(); // Jump
        cond_insts.pop(); // JumpIf

        // Build then_branch: all blocks from then_idx up to (but not including) merge_idx
        let then_branch: Vec<BasicBlock> = blocks[then_idx..merge_idx]
            .iter()
            .cloned()
            .map(|mut bb| {
                // Strip trailing jump to the merge block
                if let Some(Instruction::Jump(j)) = bb.instructions.last()
                    && j.label == merge_label
                {
                    bb.instructions.pop();
                }
                bb
            })
            .collect();

        let merge_block_insts = blocks[merge_idx].instructions.clone();
        let result = if let Some(Instruction::Phi(iphi)) = merge_block_insts.first() {
            Some(iphi.des.clone())
        } else {
            None
        };
        let mut after_merge = merge_block_insts;
        if result.is_some() && !after_merge.is_empty() {
            after_merge.remove(0);
        }

        let iifelse = IIfElse {
            cond: vec![BasicBlock {
                id: BlockId(0),
                label: String::new(),
                instructions: cond_insts,
            }],
            cond_result,
            then_branch,
            else_branch: vec![],
            result,
        };

        let mut new_insts = vec![Instruction::IfElse(iifelse)];
        new_insts.extend(after_merge);

        blocks[cond_idx].instructions = new_insts;

        // Remove then-branch blocks and merge block (highest index first)
        let mut to_remove: Vec<usize> = (then_idx..=merge_idx).collect();
        to_remove.sort_unstable_by(|a, b| b.cmp(a));
        for &r in &to_remove {
            blocks.remove(r);
        }

        renumber_local_blocks(blocks);
        return true;
    }
    false
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

        // An elif-chain block (labeled "else.*") can serve dual purpose: it IS the else-branch
        // of an outer if AND the cond of the inner if. Exclude it from else_branch here.
        else_ids.retain(|&id| id != cond_id);
        let has_else = !else_ids.is_empty();

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
    // Fall back to the multi-block then-branch strategy.
    try_raise_if_else_multi_block_in_blocks(blocks)
}

fn renumber_local_blocks(blocks: &mut [BasicBlock]) {
    for (i, b) in blocks.iter_mut().enumerate() {
        b.id = BlockId(i);
    }
}
