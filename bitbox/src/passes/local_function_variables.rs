use crate::ir::Variable;

use super::Pass;
use std::collections::{HashMap, HashSet};

#[derive(Debug, Default)]
pub struct LocalFunctionVariables {
    table: HashMap<String, Vec<Variable>>,
    functions: Vec<String>,
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
            .map(|set| set.iter().cloned().collect())
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
