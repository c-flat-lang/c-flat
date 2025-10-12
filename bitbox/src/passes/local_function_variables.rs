use crate::ir::Variable;

use super::Pass;
use std::collections::{HashMap, HashSet};

#[derive(Debug, Default)]
pub struct LocalFunctionVariables {
    table: HashMap<String, HashSet<Variable>>,
}

impl LocalFunctionVariables {
    pub fn add(&mut self, function_name: impl Into<String>, variable: Variable) {
        self.table
            .entry(function_name.into())
            .or_default()
            .insert(variable);
    }

    pub fn get(&self, function_name: &str) -> Vec<Variable> {
        self.table
            .get(function_name)
            .map(|set| set.iter().cloned().collect())
            .unwrap_or_default()
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
            crate::ir::Instruction::Add(variable, _, _) => ctx
                .local_function_variables
                .add(function_name, variable.clone()),
            crate::ir::Instruction::Assign(variable, _) => ctx
                .local_function_variables
                .add(function_name, variable.clone()),
            crate::ir::Instruction::Alloc(_, variable) => ctx
                .local_function_variables
                .add(function_name, variable.clone()),
            crate::ir::Instruction::Call(variable, _, _) => ctx
                .local_function_variables
                .add(function_name, variable.clone()),
            crate::ir::Instruction::Cmp(variable, _, _) => ctx
                .local_function_variables
                .add(function_name, variable.clone()),
            crate::ir::Instruction::Gt(variable, _, _) => ctx
                .local_function_variables
                .add(function_name, variable.clone()),
            crate::ir::Instruction::Load(variable, _) => ctx
                .local_function_variables
                .add(function_name, variable.clone()),
            crate::ir::Instruction::Mul(variable, _, _) => ctx
                .local_function_variables
                .add(function_name, variable.clone()),
            crate::ir::Instruction::Phi(variable, _) => ctx
                .local_function_variables
                .add(function_name, variable.clone()),
            crate::ir::Instruction::Sub(variable, _, _) => ctx
                .local_function_variables
                .add(function_name, variable.clone()),
            crate::ir::Instruction::Div(variable, _, _) => ctx
                .local_function_variables
                .add(function_name, variable.clone()),
            crate::ir::Instruction::IfElse {
                ..
                // cond,
                // then_branch,
                // else_branch,
                // result,
            } => todo!("IfElse"),
            crate::ir::Instruction::IfElse_ {
                cond,
                then_branch,
                else_branch,
                result,
            } => {
                // NOTE: Due to the way the blcosk are independent of each other
                // the names of variables could be the same as the outer block.
                // This is a problem for future me.
                for block in cond.iter() {
                    block_pass(function_name, block, ctx);
                }
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
            _ => (),
        }
    }
}
