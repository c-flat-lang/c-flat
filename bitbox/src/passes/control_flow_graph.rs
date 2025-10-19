use super::Pass;

type Table = std::collections::HashMap<crate::ir::BlockId, Vec<crate::ir::BlockId>>;

#[derive(Debug, Default)]
pub struct ControlFlowGraph {
    in_bound: Table,
    out_bound: Table,
}

impl ControlFlowGraph {
    pub fn push_in(&mut self, from: crate::ir::BlockId, to: crate::ir::BlockId) {
        self.in_bound.entry(from).or_default().push(to);
    }
    pub fn push_out(&mut self, from: crate::ir::BlockId, to: crate::ir::BlockId) {
        self.out_bound.entry(from).or_default().push(to);
    }
}

#[derive(Debug)]
pub struct ControlFlowGraphPass;

impl Pass for ControlFlowGraphPass {
    fn run(
        &mut self,
        module: &mut crate::ir::Module,
        ctx: &mut crate::backend::Context,
    ) -> Result<(), crate::error::Error> {
        eprintln!("ControlFlowGraphPass");
        for function in module.functions.iter() {
            let mut name_to_block_id = std::collections::HashMap::new();

            for block in function.blocks.iter() {
                name_to_block_id.insert(block.label.clone(), block.id);
            }

            for block in function.blocks.iter() {
                for instruction in block.instructions.iter() {
                    match instruction {
                        crate::ir::Instruction::JumpIf(_, target) => {
                            let Some(target_block_id) = name_to_block_id.get(target) else {
                                panic!("block not found");
                            };
                            ctx.cfg.push_out(block.id, *target_block_id);
                            ctx.cfg.push_in(*target_block_id, block.id);
                        }
                        crate::ir::Instruction::Jump(target) => {
                            let Some(target_block_id) = name_to_block_id.get(target) else {
                                panic!("block {target} not found");
                            };
                            ctx.cfg.push_out(block.id, *target_block_id);
                            ctx.cfg.push_in(*target_block_id, block.id);
                        }
                        _ => {}
                    }
                }
            }
        }
        Ok(())
    }
}
