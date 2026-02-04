#[cfg(feature = "uuids")]
use uuid::Uuid;

use crate::{
    ir::{
        BasicBlock, BlockId, Function, Instruction, Operand,
        instruction::{IIfElse, IJump, IJumpIf, ILoop, IPhi},
    },
    passes::DebugPass,
};

use super::Pass;

struct LabelCreator {
    #[cfg(not(feature = "uuids"))]
    counter: u32,
}

impl Default for LabelCreator {
    fn default() -> Self {
        Self {
            #[cfg(not(feature = "uuids"))]
            counter: 0,
        }
    }
}

impl LabelCreator {
    #[cfg(not(feature = "uuids"))]
    fn gen_name(&mut self) -> String {
        let counter = self.counter;
        self.counter += 1;
        return format!("{}", counter);
    }

    #[cfg(feature = "uuids")]
    fn gen_name(&mut self) -> String {
        return format!("{}", Uuid::new_v4());
    }
}

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
        eprintln!("{:?}Pass", DebugPass::LoweredIr);
        let mut var_creator = LabelCreator::default();
        for func in &mut module.functions {
            loop {
                let mut changed = false;

                for i in 0..func.blocks.len() {
                    for j in 0..func.blocks[i].instructions.len() {
                        match func.blocks[i].instructions[j].clone() {
                            Instruction::IfElse(iifelse) => {
                                lower_if_else(&mut var_creator, func, i, j, iifelse);
                                changed = true;
                                break;
                            }
                            Instruction::Loop(iloop) => {
                                lower_loop(&mut var_creator, func, i, j, iloop);
                                changed = true;
                                break;
                            }
                            _ => {}
                        }
                    }
                    if changed {
                        break;
                    }
                }

                if !changed {
                    break;
                }
            }
        }

        Ok(())
    }
}

fn lower_loop(
    vc: &mut LabelCreator,
    func: &mut Function,
    block_index: usize,
    instr_index: usize,
    iloop: ILoop,
) {
    let ILoop {
        cond,
        cond_result,
        body,
    } = iloop;

    let mut block = func.blocks[block_index].clone();
    let after_loop = block.instructions.split_off(instr_index + 1);

    debug_assert!(matches!(
        block.instructions.pop(),
        Some(Instruction::Loop(_))
    ));

    let start_label = format!("start_loop_{}", vc.gen_name());
    let body_label = format!("body_loop_{}", vc.gen_name());
    let exit_label = format!("exit_loop_{}", vc.gen_name());

    block
        .instructions
        .push(IJump::new(start_label.clone()).into());

    let mut start_block = BasicBlock {
        id: BlockId(0),
        label: start_label.clone(),
        instructions: vec![],
    };

    for cb in cond.clone() {
        for inst in cb.instructions {
            start_block.instructions.push(inst);
        }
    }

    start_block
        .instructions
        .push(IJumpIf::new(Operand::Variable(cond_result.clone()), body_label.clone()).into());
    start_block
        .instructions
        .push(IJump::new(exit_label.clone()).into());

    let mut body_blocks = body.clone();
    for bb in &mut body_blocks {
        bb.label = body_label.clone();

        let ends_in_ret_or_jump = bb.instructions.last().map(|inst| {
            matches!(
                inst,
                Instruction::Return(..) | Instruction::Jump(..) | Instruction::JumpIf(..)
            )
        });

        if ends_in_ret_or_jump != Some(true) {
            bb.instructions.push(IJump::new(start_label.clone()).into());
        }
    }

    let exit_block = BasicBlock {
        id: BlockId(0),
        label: exit_label.clone(),
        instructions: after_loop,
    };

    func.blocks[block_index] = block;
    let mut tail = func.blocks.split_off(block_index + 1);

    func.blocks.push(start_block);
    func.blocks.extend(body_blocks);
    func.blocks.push(exit_block);
    func.blocks.append(&mut tail);

    for (idx, b) in func.blocks.iter_mut().enumerate() {
        b.id = BlockId(idx);
    }
}

fn lower_if_else(
    vc: &mut LabelCreator,
    func: &mut Function,
    block_index: usize,
    instr_index: usize,
    iifelse: IIfElse,
) {
    let IIfElse {
        cond,
        cond_result,
        then_branch,
        else_branch,
        result,
    } = iifelse;

    let mut block = func.blocks[block_index].clone();
    let after_if = block.instructions.split_off(instr_index + 1);
    debug_assert!(matches!(
        block.instructions.pop(),
        Some(Instruction::IfElse(_))
    ));

    let cond_label = format!("cond_{}", vc.gen_name());
    let then_label = format!("then_{}", vc.gen_name());
    let else_label = format!("else_{}", vc.gen_name());
    let merge_label = format!("merge_{}", vc.gen_name());

    for mut cb in cond.clone() {
        cb.label = cond_label.clone();
        for inst in cb.instructions {
            block.instructions.push(inst);
        }
    }

    block
        .instructions
        .push(IJumpIf::new(Operand::Variable(cond_result.clone()), then_label.clone()).into());
    let jump_to_next_block = if else_branch.is_empty() {
        merge_label.clone()
    } else {
        else_label.clone()
    };
    block
        .instructions
        .push(IJump::new(jump_to_next_block).into());

    let mut then_blocks = then_branch.clone();
    for tb in &mut then_blocks {
        tb.label = then_label.clone();
        let ends_in_ret_or_jump = tb.instructions.last().map(|inst| {
            matches!(
                inst,
                Instruction::Return(..) | Instruction::Jump(..) | Instruction::JumpIf(..)
            )
        });
        if ends_in_ret_or_jump != Some(true) {
            tb.instructions.push(IJump::new(merge_label.clone()).into());
        }
    }

    let mut else_blocks = else_branch.clone();
    for eb in &mut else_blocks {
        eb.label = else_label.clone();
        let ends_in_ret_or_jump = eb.instructions.last().map(|inst| {
            matches!(
                inst,
                Instruction::Return(..) | Instruction::Jump(..) | Instruction::JumpIf(..)
            )
        });
        if ends_in_ret_or_jump != Some(true) {
            eb.instructions.push(IJump::new(merge_label.clone()).into());
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
            IPhi::new(
                res_var.clone(),
                vec![
                    (then_label.clone(), res_var.clone()),
                    (else_label.clone(), res_var.clone()),
                ],
            )
            .into(),
        );
    }

    func.blocks[block_index] = block;
    let mut tail = func.blocks.split_off(block_index + 1);
    func.blocks.extend(then_blocks);
    func.blocks.extend(else_blocks);
    func.blocks.push(merge_block);
    func.blocks.append(&mut tail);

    for (idx, b) in func.blocks.iter_mut().enumerate() {
        b.id = BlockId(idx);
    }
}
