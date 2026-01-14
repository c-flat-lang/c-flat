use crate::{ir::instruction::IJumpIf, passes::DebugPass};
use std::collections::HashMap;

use super::Pass;

type Table = HashMap<crate::ir::BlockId, Vec<crate::ir::BlockId>>;

#[derive(Debug, Default)]
pub struct ControlFlowGraph {
    pub in_bound: HashMap<String, Table>,
    pub out_bound: HashMap<String, Table>,
}

impl ControlFlowGraph {
    pub fn push_in(
        &mut self,
        function: impl Into<String>,
        from: crate::ir::BlockId,
        to: crate::ir::BlockId,
    ) {
        self.in_bound
            .entry(function.into())
            .or_default()
            .entry(from)
            .or_default()
            .push(to);
    }
    pub fn push_out(
        &mut self,
        function: impl Into<String>,
        from: crate::ir::BlockId,
        to: crate::ir::BlockId,
    ) {
        self.out_bound
            .entry(function.into())
            .or_default()
            .entry(from)
            .or_default()
            .push(to);
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

        let formater = |data: &HashMap<String, Table>, kind: &str| {
            let mut data = data
                .iter()
                .map(|(k, v)| {
                    (
                        k.clone(),
                        v.iter().map(|(k, v)| (*k, v.clone())).collect::<Vec<_>>(),
                    )
                })
                .collect::<Vec<_>>();
            eprintln!("{kind}:");
            for (name, data) in data.iter_mut() {
                data.sort_by(|a, b| a.0.0.cmp(&b.0.0));
                eprintln!("{name}:");
                for (block_id, in_bounds) in data.iter() {
                    eprintln!("  {block_id:?}:");
                    for in_bound in in_bounds.iter() {
                        eprintln!("    {in_bound:?}");
                    }
                }
            }
        };

        formater(&ctx.cfg.in_bound, "IN BOUND");
        formater(&ctx.cfg.out_bound, "OUT BOUND");

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
                            ctx.cfg.push_out(&function.name, block.id, *target_block_id);
                            ctx.cfg.push_in(&function.name, *target_block_id, block.id);
                        }
                        crate::ir::Instruction::Jump(ijump) => {
                            let Some(target_block_id) = name_to_block_id.get(&ijump.label) else {
                                panic!("block {} not found", ijump.label);
                            };
                            ctx.cfg.push_out(&function.name, block.id, *target_block_id);
                            ctx.cfg.push_in(&function.name, *target_block_id, block.id);
                        }
                        _ => {}
                    }
                }
            }
        }
        Ok(())
    }
}
