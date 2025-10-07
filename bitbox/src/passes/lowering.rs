use crate::ir::{BasicBlock, BlockId, Instruction};

use super::Pass;

#[derive(Debug)]
pub struct LoweringPass;

impl Pass for LoweringPass {
    fn run(
        &mut self,
        module: &mut crate::ir::Module,
        _ctx: &mut crate::backend::Context,
    ) -> Result<(), crate::error::Error> {
        eprintln!("LoweringPass");
        for function in module.functions.iter_mut() {
            let mut inserted_new_block: Option<usize> = None;
            let mut new_blocks = Vec::new();
            for (block_index, block) in function.blocks.iter_mut().enumerate() {
                for (inst_index, instruction) in block.clone().instructions.iter().enumerate() {
                    let Instruction::IfElse {
                        cond,
                        then_branch,
                        else_branch,
                        result,
                    } = instruction
                    else {
                        continue;
                    };
                    inserted_new_block = Some(block_index);

                    let then_label = format!("{}_then", block.label);
                    let else_label = format!("{}_else", block.label);
                    let merge_label = format!("{}_merge", block.label);

                    let mut then_block = BasicBlock {
                        id: BlockId(usize::MAX),
                        label: then_label.clone(),
                        instructions: then_branch.clone(),
                    };

                    let mut else_block = BasicBlock {
                        id: BlockId(usize::MAX),
                        label: else_label.clone(),
                        instructions: else_branch.clone(),
                    };

                    let mut merge_block = BasicBlock {
                        id: BlockId(usize::MAX),
                        label: merge_label.clone(),
                        instructions: vec![],
                    };

                    let jump_if = Instruction::JumpIf(cond.clone(), then_label.clone());
                    let jump = Instruction::Jump(else_label.clone());

                    block.instructions[inst_index] = jump_if;
                    block.instructions.insert(inst_index + 1, jump);

                    if let Some(res) = result.clone() {
                        let val_then = match then_block.instructions.last().unwrap() {
                            Instruction::Assign(v, _) => v.clone(),
                            _ => {
                                panic!("missing assignment in then block {:#?}", then_block)
                            }
                        };
                        let val_else = match else_block.instructions.last().unwrap() {
                            Instruction::Assign(v, _) => v.clone(),
                            _ => panic!("missing assignment in else block"),
                        };
                        merge_block.instructions.push(Instruction::Phi(
                            res.clone(),
                            vec![
                                (val_then, then_label.clone()),
                                (val_else, else_label.clone()),
                            ],
                        ));
                    }

                    // After both then/else, jump to merge
                    then_block
                        .instructions
                        .push(Instruction::Jump(merge_label.clone()));
                    else_block
                        .instructions
                        .push(Instruction::Jump(merge_label.clone()));

                    new_blocks.push(then_block);
                    new_blocks.push(else_block);
                    new_blocks.push(merge_block);
                }
            }
            if let Some(index) = inserted_new_block {
                for new_block in new_blocks.iter().rev() {
                    function.blocks.insert(index + 1, new_block.clone());
                }

                function
                    .blocks
                    .iter_mut()
                    .enumerate()
                    .for_each(|(index, block)| {
                        block.id = BlockId(index);
                    })
            }
        }
        Ok(())
    }
}
