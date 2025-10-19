use uuid::Uuid;

use crate::{
    ir::{BasicBlock, BlockId, Instruction, Operand},
    passes::DebugPass,
};

use super::Pass;

#[derive(Debug)]
pub struct LoweringPass;

impl Pass for LoweringPass {
    fn debug(
        &self,
        module: &crate::ir::Module,
        _ctx: &crate::backend::Context,
        debug_mode: Option<DebugPass>,
    ) -> bool {
        if !matches!(debug_mode, Some(DebugPass::LoweredIr)) {
            return false;
        }

        eprintln!("--- Dumping LoweringPass ---");
        eprintln!("{}", module);
        true
    }

    fn run(
        &mut self,
        module: &mut crate::ir::Module,
        _ctx: &mut crate::backend::Context,
    ) -> Result<(), crate::error::Error> {
        for func in &mut module.functions {
            let mut i = 0;
            while i < func.blocks.len() {
                let mut block = func.blocks[i].clone();

                let mut j = 0;
                while j < block.instructions.len() {
                    if let Instruction::IfElse {
                        cond,
                        cond_result,
                        then_branch,
                        else_branch,
                        result,
                    } = block.instructions[j].clone()
                    {
                        let after_if = block.instructions.split_off(j + 1);
                        block.instructions.pop(); // remove the IfElse itself

                        let cond_label = format!("cond_{}", Uuid::new_v4());
                        let then_label = format!("then_{}", Uuid::new_v4());
                        let else_label = format!("else_{}", Uuid::new_v4());
                        let merge_label = format!("merge_{}", Uuid::new_v4());

                        for mut cb in cond.clone() {
                            cb.label = cond_label.clone();
                            for inst in cb.instructions {
                                block.instructions.push(inst);
                            }
                        }

                        block.instructions.push(Instruction::JumpIf(
                            Operand::Variable(cond_result.clone()),
                            then_label.clone(),
                        ));
                        let jump_to_next_block = if else_branch.is_empty() {
                            merge_label.clone()
                        } else {
                            else_label.clone()
                        };
                        block
                            .instructions
                            .push(Instruction::Jump(jump_to_next_block));

                        let mut then_blocks = then_branch.clone();
                        for tb in &mut then_blocks {
                            tb.label = then_label.clone();
                            let ends_in_ret_or_jump = tb.instructions.last().map(|inst| {
                                matches!(
                                    inst,
                                    Instruction::Return(_, _)
                                        | Instruction::Jump(_)
                                        | Instruction::JumpIf(_, _)
                                )
                            });
                            if ends_in_ret_or_jump != Some(true) {
                                tb.instructions.push(Instruction::Jump(merge_label.clone()));
                            }
                        }

                        let mut else_blocks = else_branch.clone();
                        for eb in &mut else_blocks {
                            eb.label = else_label.clone();
                            let ends_in_ret_or_jump = eb.instructions.last().map(|inst| {
                                matches!(
                                    inst,
                                    Instruction::Return(_, _)
                                        | Instruction::Jump(_)
                                        | Instruction::JumpIf(_, _)
                                )
                            });
                            if ends_in_ret_or_jump != Some(true) {
                                eb.instructions.push(Instruction::Jump(merge_label.clone()));
                            }
                        }

                        let mut merge_block = BasicBlock {
                            id: BlockId(0),
                            label: merge_label.clone(),
                            instructions: after_if,
                        };

                        if let Some(res_var) = result {
                            merge_block.instructions.insert(
                                0,
                                Instruction::Phi(
                                    res_var.clone(),
                                    vec![
                                        (res_var.clone(), then_label.clone()),
                                        (res_var.clone(), else_label.clone()),
                                    ],
                                ),
                            );
                        }

                        func.blocks[i] = block;
                        let mut tail = func.blocks.split_off(i + 1);
                        func.blocks.extend(then_blocks);
                        func.blocks.extend(else_blocks);
                        func.blocks.push(merge_block);
                        func.blocks.append(&mut tail);

                        for (idx, b) in func.blocks.iter_mut().enumerate() {
                            b.id = BlockId(idx);
                        }

                        break;
                    }

                    j += 1;
                }

                i += 1;
            }
        }

        Ok(())
    }
}
