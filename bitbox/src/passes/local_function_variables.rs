use crate::ir::{
    Variable,
    instruction::{ICall, IIfElse},
};

use super::Pass;
use std::collections::HashMap;

#[derive(Debug)]
pub struct LocalFunctionVariables {
    table: HashMap<String, Vec<Variable>>,
    functions: Vec<String>,
}

impl Default for LocalFunctionVariables {
    fn default() -> Self {
        Self {
            table: HashMap::new(),
            functions: vec![
                "write_int".to_string(),
                "writeln".to_string(),
                "write_char".to_string(),
            ],
        }
    }
}

impl LocalFunctionVariables {
    pub fn add(&mut self, function_name: impl Into<String>, variable: Variable) {
        let function_name = function_name.into();
        if !self.functions.contains(&function_name) {
            self.functions.push(function_name.clone());
        }
        let values = self.table.entry(function_name).or_default();

        if values.contains(&variable) {
            return;
        }

        values.push(variable);
    }

    pub fn get(&self, function_name: &str) -> Vec<Variable> {
        self.table
            .get(function_name)
            .map(|set| set.to_vec())
            .unwrap_or_default()
    }

    pub fn get_function_id(&self, as_str: &str) -> Option<usize> {
        self.functions.iter().position(|f| f == as_str)
    }
}

#[derive(Debug)]
pub struct LocalFunctionVariablesPass;

impl Pass for LocalFunctionVariablesPass {
    fn run(
        &mut self,
        module: &mut crate::ir::Module,
        ctx: &mut crate::backend::Context,
    ) -> Result<(), crate::error::Error> {
        eprintln!("LocalFunctionVariablesPass");
        for function in module.functions.iter() {
            for parameter in function.params.iter() {
                ctx.local_function_variables
                    .add(function.name.as_str(), parameter.clone());
            }
            for block in function.blocks.iter() {
                block_pass(function.name.as_str(), block, ctx);
            }
        }
        Ok(())
    }
}

fn block_pass(
    function_name: &str,
    block: &crate::ir::BasicBlock,
    ctx: &mut crate::backend::Context,
) {
    for instruction in block.instructions.iter() {
        match instruction {
            crate::ir::Instruction::Add(iadd) => ctx
                .local_function_variables
                .add(function_name, iadd.des.clone()),
            crate::ir::Instruction::Assign(iassign) => ctx
                .local_function_variables
                .add(function_name, iassign.des.clone()),
            crate::ir::Instruction::Alloc(ialloc) => ctx
                .local_function_variables
                .add(function_name, ialloc.des.clone()),
            crate::ir::Instruction::Call(ICall {
                des: Some(variable),
                ..
            }) => ctx
                .local_function_variables
                .add(function_name, variable.clone()),
            crate::ir::Instruction::Cmp(icmp) => ctx
                .local_function_variables
                .add(function_name, icmp.des.clone()),
            crate::ir::Instruction::ElemGet(ielemget) => ctx
                .local_function_variables
                .add(function_name, ielemget.des.clone()),
            crate::ir::Instruction::And(iand) => ctx
                .local_function_variables
                .add(function_name, iand.des.clone()),
            crate::ir::Instruction::Or(ior) => ctx
                .local_function_variables
                .add(function_name, ior.des.clone()),
            crate::ir::Instruction::XOr(ixor) => ctx
                .local_function_variables
                .add(function_name, ixor.des.clone()),
            crate::ir::Instruction::Gt(igt) => ctx
                .local_function_variables
                .add(function_name, igt.des.clone()),
            crate::ir::Instruction::Gte(igte) => ctx
                .local_function_variables
                .add(function_name, igte.des.clone()),
            crate::ir::Instruction::Lt(ilt) => ctx
                .local_function_variables
                .add(function_name, ilt.des.clone()),
            crate::ir::Instruction::Load(iload) => ctx
                .local_function_variables
                .add(function_name, iload.des.clone()),
            crate::ir::Instruction::Mul(imul) => ctx
                .local_function_variables
                .add(function_name, imul.des.clone()),
            crate::ir::Instruction::Phi(iphi) => ctx
                .local_function_variables
                .add(function_name, iphi.des.clone()),
            crate::ir::Instruction::Sub(isub) => ctx
                .local_function_variables
                .add(function_name, isub.des.clone()),
            crate::ir::Instruction::Div(idiv) => ctx
                .local_function_variables
                .add(function_name, idiv.des.clone()),
            crate::ir::Instruction::Loop(iloop) => {
                for block in iloop.cond.iter() {
                    block_pass(function_name, block, ctx);
                }
                for block in iloop.body.iter() {
                    block_pass(function_name, block, ctx);
                }
            }
            crate::ir::Instruction::IfElse(IIfElse {
                cond,
                cond_result,
                then_branch,
                else_branch,
                result,
            }) => {
                // NOTE: Due to the way the blocks are independent of each other
                // the names of variables could be the same as the outer block.
                // This is a problem for future me.
                for block in cond.iter() {
                    block_pass(function_name, block, ctx);
                }
                ctx.local_function_variables
                    .add(function_name, cond_result.clone());
                for block in then_branch.iter() {
                    block_pass(function_name, block, ctx);
                }
                for block in else_branch.iter() {
                    block_pass(function_name, block, ctx);
                }
                if let Some(result) = result {
                    ctx.local_function_variables
                        .add(function_name, result.clone());
                }
            }
            crate::ir::Instruction::Call(..)
            | crate::ir::Instruction::NoOp(..)
            | crate::ir::Instruction::Copy(..)
            | crate::ir::Instruction::ElemSet(..)
            | crate::ir::Instruction::Jump(..)
            | crate::ir::Instruction::JumpIf(..)
            | crate::ir::Instruction::Return(..) => (),
        }
    }
}
