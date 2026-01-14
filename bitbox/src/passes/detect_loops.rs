use std::collections::HashMap;

use crate::ir::{BlockId, Module};
use crate::passes::{DebugPass, Pass};

#[derive(Debug, Default)]
pub struct LoopInfo {
    pub loops: HashMap<String, Loop>,
}

impl LoopInfo {
    pub fn get(&self, function_name: &str) -> Option<&Loop> {
        self.loops.get(function_name)
    }
}

#[derive(Debug)]
pub struct Loop {
    pub header: BlockId,
    pub body: Vec<BlockId>,
    pub exits: Vec<BlockId>,
}

#[derive(Debug)]
pub struct DetectLoopsPass;

impl Pass for DetectLoopsPass {
    fn debug(
        &self,
        module: &crate::ir::Module,
        ctx: &crate::backend::Context,
        debug_mode: Option<DebugPass>,
    ) -> bool {
        let Some(DebugPass::DetectLoops) = debug_mode else {
            return false;
        };
        eprintln!("--- Dump Loop Detection ---");

        for loop_ in &ctx.loops.loops {
            eprintln!("Loop: {:#?}", loop_);
            // module.functions.
        }

        true
    }

    fn run(
        &mut self,
        _: &mut Module,
        ctx: &mut crate::backend::Context,
    ) -> Result<(), crate::error::Error> {
        eprintln!("DetectLoopsPass");

        for (function_name, data) in ctx.cfg.in_bound.iter() {
            for (block, to) in data.iter() {
                if to.contains(&block) {
                    ctx.loops.loops.insert(
                        function_name.clone(),
                        Loop {
                            header: block.clone(),
                            body: vec![block.clone()],
                            exits: vec![BlockId(block.0 + 1)],
                        },
                    );
                }
            }
        }

        Ok(())
    }
}
