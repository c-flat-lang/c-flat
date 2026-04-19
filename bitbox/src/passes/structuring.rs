use super::Pass;
use crate::{
    ir::{
        BasicBlock, BlockId, Function, Instruction, Operand, Type, Variable,
        instruction::{IIfElse, ILoop},
    },
    passes::DebugPass,
};
#[cfg(feature = "uuids")]
use uuid::Uuid;

#[derive(Default)]
pub struct StructuringPass;

impl Pass for StructuringPass {
    fn debug_pass(&self) -> DebugPass {
        DebugPass::StructuredIr
    }

    fn debug(&self, module: &crate::ir::Module, _ctx: &crate::backend::Context) {
        eprintln!("{}", module);
    }

    fn run(
        &mut self,
        module: &mut crate::ir::Module,
        _ctx: &mut crate::backend::Context,
    ) -> Result<(), crate::error::Error> {
        for func in &mut module.functions {
            loop {
                let mut changed = false;
                // Prefer innermost first by scanning from the end of the block list
                // (lowering inserted sub-blocks toward the end).
                if try_raise_loop(func) || try_raise_if_else(func) {
                    changed = true;
                }
                if !changed {
                    break;
                }
            }
        }
        Ok(())
    }
}

/// Attempts to find and raise the first (innermost) loop pattern.
/// Returns `true` if a loop was successfully raised.
fn try_raise_loop(func: &mut Function) -> bool {
    for s_idx in (0..func.blocks.len()).rev() {
        let block = &func.blocks[s_idx];
        if !block.label.starts_with("start_loop_") {
            continue;
        }

        let len = block.instructions.len();
        if len < 2 {
            continue;
        }

        let (jif, ij) = match (&block.instructions[len - 2], &block.instructions[len - 1]) {
            (Instruction::JumpIf(jif), Instruction::Jump(ij)) => (jif, ij),
            _ => continue,
        };

        let body_label = &jif.label;
        let exit_label = &ij.label;

        if !body_label.starts_with("body_loop_") || !exit_label.starts_with("exit_loop_") {
            continue;
        }

        let cond_result = match &jif.cond {
            Operand::Variable(v) => v.clone(),
            _ => continue,
        };

        let start_label = &block.label;

        // Collect all body blocks (may be >1 if the original body had multiple blocks)
        let body_idxs: Vec<usize> = func
            .blocks
            .iter()
            .enumerate()
            .filter(|(_, b)| b.label == *body_label)
            .map(|(k, _)| k)
            .collect();

        if body_idxs.is_empty() {
            continue;
        }

        // Find exit block
        let Some(exit_idx) = func.blocks.iter().position(|b| b.label == *exit_label) else {
            continue;
        };

        // Find preheader: the unique block (outside the body) that jumps to start_label
        let pre_idxs: Vec<usize> = func
            .blocks
            .iter()
            .enumerate()
            .filter_map(|(k, b)| {
                let blen = b.instructions.len();
                if blen > 0
                    && let Instruction::Jump(_) = &b.instructions[blen - 1]
                {
                    return Some(k);
                }
                None
            })
            .collect();

        if pre_idxs.len() != 1 {
            continue; // not a clean preheader (or multiple entry points)
        }
        let pre_idx = pre_idxs[0];

        // === Perform the raise ===
        let mut pre_block = func.blocks[pre_idx].clone();
        // Remove the synthetic Jump(start_label) that was added by lowering
        pre_block.instructions.pop();

        let mut start_block = func.blocks[s_idx].clone();
        // Extract cond instructions (everything before the two terminators)
        let cond_insts: Vec<Instruction> = start_block
            .instructions
            .drain(..start_block.instructions.len() - 2)
            .collect();

        let cond = vec![BasicBlock {
            id: BlockId(0),
            label: String::new(),
            instructions: cond_insts,
        }];

        // Clone and clean body blocks (remove synthetic back-edge Jump if present)
        let mut body_blocks: Vec<BasicBlock> = body_idxs
            .iter()
            .map(|&idx| func.blocks[idx].clone())
            .collect();
        for bb in &mut body_blocks {
            if let Some(Instruction::Jump(ij)) = bb.instructions.last()
                && ij.label == *start_label
            {
                bb.instructions.pop();
            }
        }

        let iloop = ILoop {
            cond,
            cond_result,
            body: body_blocks,
        };

        // Put the structured Loop back where the original Loop was
        pre_block.instructions.push(Instruction::Loop(iloop));

        // Append the original "after" code that was moved to the exit block
        let after_insts = func.blocks[exit_idx].instructions.clone();
        pre_block.instructions.extend(after_insts);

        func.blocks[pre_idx] = pre_block;

        // Remove the synthetic blocks (start, body*, exit)
        let mut to_remove = vec![s_idx];
        to_remove.extend(body_idxs);
        to_remove.push(exit_idx);
        to_remove.sort_by(|a, b| b.cmp(a)); // descending so indices stay valid
        for &r in &to_remove {
            func.blocks.remove(r);
        }

        // Renumber block IDs
        for (idx, b) in func.blocks.iter_mut().enumerate() {
            b.id = BlockId(idx);
        }

        return true;
    }
    false
}

/// Attempts to find and raise the first (innermost) if-else pattern.
/// Returns `true` if an if-else was successfully raised.
///
/// Note: This assumes the original IfElse was the *first* instruction in its block
/// (i.e. no prefix code before the IfElse). The lowering inlines the condition
/// instructions into the same block as any prefix, making a perfect reverse
/// impossible without data-flow analysis when prefix code exists. If your frontend
/// always emits IfElse as the first instruction of its block, this will be exact.
fn try_raise_if_else(func: &mut Function) -> bool {
    for m_idx in (0..func.blocks.len()).rev() {
        let block = &func.blocks[m_idx];
        if !block.label.starts_with("merge_") {
            continue;
        }
        let merge_label = block.label.clone();

        // Find branches that jump to merge
        let branch_idxs: Vec<usize> = func
            .blocks
            .iter()
            .enumerate()
            .filter_map(|(k, b)| {
                if let Some(Instruction::Jump(ij)) = b.instructions.last() {
                    if ij.label == merge_label {
                        Some(k)
                    } else {
                        None
                    }
                } else {
                    None
                }
            })
            .collect();

        if branch_idxs.is_empty() {
            continue;
        }

        let mut then_idxs = vec![];
        let mut else_idxs = vec![];
        let mut then_label = String::new();
        let mut else_label = String::new();

        for &k in &branch_idxs {
            let lbl = &func.blocks[k].label;
            if lbl.starts_with("then_") {
                then_idxs.push(k);
                then_label = lbl.clone();
            } else if lbl.starts_with("else_") {
                else_idxs.push(k);
                else_label = lbl.clone();
            }
        }

        let has_else = !else_idxs.is_empty();
        if then_idxs.is_empty() {
            continue;
        }

        let expected_next_label = if has_else {
            else_label.clone()
        } else {
            merge_label.clone()
        };

        // Find cond block
        let mut cond_idx = None;
        let mut cond_result = Variable::new("undefined", Type::Void);

        for (k, b) in func.blocks.iter().enumerate() {
            let len = b.instructions.len();
            if len < 2 {
                continue;
            }

            match (&b.instructions[len - 2], &b.instructions[len - 1]) {
                (Instruction::JumpIf(jif), Instruction::Jump(ij))
                    if jif.label == then_label && ij.label == expected_next_label =>
                {
                    cond_idx = Some(k);
                    if let Operand::Variable(v) = &jif.cond {
                        cond_result = v.clone();
                    }
                    break;
                }
                _ => {}
            }
        }

        let Some(cond_idx) = cond_idx else {
            continue;
        };

        // === Raise ===
        let mut cond_block = func.blocks[cond_idx].clone();
        cond_block.instructions.pop(); // Jump
        cond_block.instructions.pop(); // JumpIf

        let cond = vec![BasicBlock {
            id: BlockId(0),
            label: String::new(),
            instructions: cond_block.instructions,
        }];

        let then_branch: Vec<BasicBlock> = then_idxs
            .iter()
            .map(|&idx| {
                let mut bb = func.blocks[idx].clone();
                if let Some(Instruction::Jump(ij)) = bb.instructions.last()
                    && ij.label == merge_label
                {
                    bb.instructions.pop();
                }
                bb
            })
            .collect();

        let else_branch: Vec<BasicBlock> = if has_else {
            else_idxs
                .iter()
                .map(|&idx| {
                    let mut bb = func.blocks[idx].clone();
                    if let Some(Instruction::Jump(ij)) = bb.instructions.last()
                        && ij.label == merge_label
                    {
                        bb.instructions.pop();
                    }
                    bb
                })
                .collect()
        } else {
            vec![]
        };

        let result = if let Some(Instruction::Phi(iphi)) = func.blocks[m_idx].instructions.first() {
            Some(iphi.des.clone())
        } else {
            None
        };

        let iifelse = IIfElse {
            cond,
            cond_result,
            then_branch,
            else_branch,
            result,
        };

        cond_block.instructions = vec![Instruction::IfElse(iifelse)];

        // keep the code after the phi
        let mut after = func.blocks[m_idx].instructions.clone();
        if !after.is_empty() && matches!(after[0], Instruction::Phi(_)) {
            after.remove(0);
        }
        cond_block.instructions.extend(after);

        func.blocks[cond_idx] = cond_block;

        // remove synthetic blocks
        let mut to_remove = then_idxs;
        to_remove.extend(else_idxs);
        to_remove.push(m_idx);
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
