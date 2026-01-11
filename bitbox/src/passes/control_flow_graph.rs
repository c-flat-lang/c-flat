use crate::{ir::instruction::IJumpIf, passes::DebugPass};

use super::Pass;

type Table = std::collections::HashMap<crate::ir::BlockId, Vec<crate::ir::BlockId>>;

#[derive(Debug, Default)]
pub struct ControlFlowGraph {
    pub in_bound: Table,
    pub out_bound: Table,
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
    fn debug(
        &self,
        _module: &crate::ir::Module,
        ctx: &crate::backend::Context,
        debug_mode: Option<DebugPass>,
    ) -> bool {
        if !matches!(debug_mode, Some(DebugPass::ControlFlowGraph)) {
            return false;
        }

        eprintln!("{:?}", DebugPass::ControlFlowGraph);

        let mut in_bound = ctx
            .cfg
            .in_bound
            .iter()
            .map(|(k, v)| (k, v))
            .collect::<Vec<_>>();
        in_bound.sort_by(|a, b| a.0 .0.cmp(&b.0 .0));
        eprintln!("in_bound: {in_bound:#?}",);
        let mut out_bound = ctx
            .cfg
            .out_bound
            .iter()
            .map(|(k, v)| (k, v))
            .collect::<Vec<_>>();
        out_bound.sort_by(|a, b| a.0 .0.cmp(&b.0 .0));
        eprintln!("out_bound: {out_bound:#?}",);

        true
    }

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
                        crate::ir::Instruction::JumpIf(IJumpIf { label, .. }) => {
                            let Some(target_block_id) = name_to_block_id.get(label) else {
                                panic!("block not found");
                            };
                            ctx.cfg.push_out(block.id, *target_block_id);
                            ctx.cfg.push_in(*target_block_id, block.id);
                        }
                        crate::ir::Instruction::Jump(ijump) => {
                            let Some(target_block_id) = name_to_block_id.get(&ijump.label) else {
                                panic!("block {} not found", ijump.label);
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
