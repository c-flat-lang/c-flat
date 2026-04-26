use std::collections::HashMap;

use crate::passes::{DebugPass, PassOutput};

use super::Pass;

#[derive(Debug)]
pub struct PhiNodeEliminationPass;

impl Pass for PhiNodeEliminationPass {
    fn debug_pass(&self) -> DebugPass {
        DebugPass::PhiNodeElimination
    }

    fn debug(&self, _module: &crate::ir::Module, _ctx: &crate::backend::Context) -> PassOutput {
        PassOutput::Nothing
    }

    fn run(
        &mut self,
        module: &mut crate::ir::Module,
        _ctx: &mut crate::backend::Context,
    ) -> Result<(), crate::error::Error> {
        for function in module.functions.iter_mut() {
            let mut nodes = Vec::new();
            let mut label_to_block_id: HashMap<String, usize> = HashMap::new();

            for (block_id, block) in function.blocks.iter_mut().enumerate() {
                label_to_block_id.insert(block.label.clone(), block_id);
                for (ip, instruction) in block.instructions.iter_mut().enumerate() {
                    let crate::ir::Instruction::Phi(phi) = instruction else {
                        continue;
                    };
                    nodes.push((block_id, ip, phi.clone()));
                }
            }
            for (block_id, ip, _) in nodes {
                function.blocks[block_id].instructions.remove(ip);
            }
        }
        Ok(())
    }
}
