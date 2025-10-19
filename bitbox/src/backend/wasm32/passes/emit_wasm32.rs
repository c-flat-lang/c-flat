#![allow(unused)]
use std::collections::HashMap;

use crate::backend::Lower;
use crate::ir::{self, Module, Type, Visibility};
use crate::passes::Pass;
use wasm_encoder::{
    BlockType, CodeSection, EntityType, ExportKind, ExportSection, Function as WasmFunction,
    FunctionSection, ImportSection, InstructionSink, Module as WasmModule, TypeSection, ValType,
};

#[derive(Debug)]
pub struct EmitWasm32Pass;

impl Pass for EmitWasm32Pass {
    fn run(
        &mut self,
        module: &mut Module,
        ctx: &mut crate::backend::Context,
    ) -> Result<(), crate::error::Error> {
        eprintln!("EmitWasm32Pass");
        // eprintln!("{:#?}", module);
        // eprintln!("{}", module);
        // eprintln!("{:#?}", ctx.local_function_variables.get("main"));

        {
            let mut module = ctx.output.get_mut_wasm32();

            module
                .import_section
                .import("core", "write_i32", EntityType::Function(0));
            module
                .type_section
                .ty()
                .function(vec![ValType::I32], vec![]);

            module.funciton_count += 1;
        }

        for function in module.functions.iter() {
            function.lower(ctx, &*self)?;
        }
        Ok(())
    }
}

trait LowerToWasm32 {
    fn lower_to_wasm32(
        &self,
        function_name: &str,
        assembler: &mut InstructionSink<'_>,
        ctx: &mut crate::backend::Context,
    ) -> Result<(), crate::error::Error>;
}

/// ID (Decimal)    ID (Hex)  Section
/// 0       0x00    Custom
/// 1       0x01    Type
/// 2       0x02    Import
/// 3       0x03    Function
/// 4       0x04    Table
/// 5       0x05    Memory
/// 6       0x06    Global
/// 7       0x07    Export
/// 8       0x08    Start
/// 9       0x09    Element
/// 10      0x0a    Code
/// 11      0x0b    Data
/// 12      0x0c    Data Count
#[derive(Debug, Default)]
pub struct Wasm32Module {
    module: WasmModule,
    type_section: TypeSection,
    import_section: ImportSection,
    code_section: CodeSection,
    export_section: ExportSection,
    function_section: FunctionSection,

    /// Counter for function ids for the function section
    funciton_count: u32,
}

impl Wasm32Module {
    pub fn finish(&mut self) -> Vec<u8> {
        self.module.section(&self.type_section);
        self.module.section(&self.import_section);
        self.module.section(&self.function_section);
        self.module.section(&self.export_section);
        self.module.section(&self.code_section);
        self.module.clone().finish()
    }
}

impl Into<ValType> for ir::Type {
    fn into(self) -> ValType {
        match self {
            ir::Type::Unsigned(1..=32) => ValType::I32,
            ir::Type::Signed(1..=32) => ValType::I32,
            ir::Type::Float(1..=32) => ValType::F32,
            ir::Type::Float(33..=64) => ValType::F64,
            ir::Type::Pointer(_) => todo!(),
            ir::Type::Array(_, _) => todo!(),
            ir::Type::Void => todo!(),
            _ => panic!("Unsupported type: {}", self),
        }
    }
}

impl Into<BlockType> for ir::Type {
    fn into(self) -> BlockType {
        match self {
            ir::Type::Unsigned(1..=32) => BlockType::Result(ValType::I32),
            ir::Type::Signed(1..=32) => BlockType::Result(ValType::I32),
            ir::Type::Float(1..=32) => BlockType::Result(ValType::F32),
            ir::Type::Float(33..=64) => BlockType::Result(ValType::F64),
            ir::Type::Pointer(_) => todo!(),
            ir::Type::Array(_, _) => todo!(),
            ir::Type::Void => todo!(),
            _ => panic!("Unsupported type: {}", self),
        }
    }
}

impl Lower<EmitWasm32Pass> for ir::Function {
    fn lower(
        &self,
        ctx: &mut crate::backend::Context,
        _: &EmitWasm32Pass,
    ) -> Result<(), crate::error::Error> {
        let f = {
            // Type Section
            let param_types: Vec<ValType> =
                self.params.iter().map(|p| p.ty.clone().into()).collect();

            let result_type: Vec<ValType> = if self.return_type == Type::Void {
                vec![]
            } else {
                vec![self.return_type.clone().into()]
            };
            let module = ctx.output.get_mut_wasm32();
            module.type_section.ty().function(param_types, result_type);

            // Function Section
            let function_count_id = module.funciton_count;
            module.function_section.function(function_count_id);

            // Export Section
            if let Visibility::Public = self.visibility {
                module.export_section.export(
                    self.name.as_str(),
                    ExportKind::Func,
                    function_count_id,
                );
            }

            let params_length = self.params.len();
            let local_variables = ctx
                .local_function_variables
                .get(self.name.as_str())
                .iter()
                .skip(params_length)
                .cloned()
                .collect::<Vec<_>>();
            let mut locals: HashMap<ValType, u32> = HashMap::new();
            for var in local_variables {
                if var.ty == Type::Void {
                    continue;
                }
                let ty = var.ty.clone().into();
                locals
                    .entry(ty)
                    .and_modify(|count| *count += 1)
                    .or_insert(1);
            }

            let locals = locals.iter().fold(Vec::new(), |mut acc, (ty, count)| {
                acc.push((*count, *ty));
                acc
            });
            let mut f = WasmFunction::new(locals);
            let mut instructions = f.instructions();
            // let function_return_type = match self.return_type {
            //     Type::Void => BlockType::Empty,
            //     _ => BlockType::Result(self.return_type.clone().into()),
            // };

            for block in self.blocks.iter() {
                block.lower_to_wasm32(self.name.as_str(), &mut instructions, ctx)?;
            }

            instructions.end();
            f
        };
        let module = ctx.output.get_mut_wasm32();
        module.code_section.function(&f);
        module.funciton_count += 1;
        Ok(())
    }
}

impl LowerToWasm32 for ir::BasicBlock {
    fn lower_to_wasm32(
        &self,
        function_name: &str,
        assembler: &mut InstructionSink<'_>,
        ctx: &mut crate::backend::Context,
    ) -> Result<(), crate::error::Error> {
        for instruction in self.instructions.iter() {
            instruction.lower_to_wasm32(function_name, assembler, ctx)?;
        }
        Ok(())
    }
}

impl LowerToWasm32 for ir::Instruction {
    fn lower_to_wasm32(
        &self,
        function_name: &str,
        assembler: &mut InstructionSink<'_>,
        ctx: &mut crate::backend::Context,
    ) -> Result<(), crate::error::Error> {
        match self {
            ir::Instruction::NoOp => todo!("@noop"),
            ir::Instruction::Add(variable, lhs, rhs) => {
                lhs.lower_to_wasm32(function_name, assembler, ctx);
                rhs.lower_to_wasm32(function_name, assembler, ctx);
                match variable.ty.clone().into() {
                    ValType::I32 => assembler.i32_add(),
                    ValType::I64 => todo!("@gt i64"),
                    ValType::F32 => todo!("@gt f32"),
                    ValType::F64 => todo!("@gt f64"),
                    ValType::V128 => todo!("@gt v128"),
                    ValType::Ref(_) => todo!("@gt ref"),
                };
                let Some(idx) = ctx
                    .local_function_variables
                    .get(function_name)
                    .iter()
                    .position(|v| v.name == variable.name)
                else {
                    panic!("Variable {:?} not found", variable);
                };
                assembler.local_set(idx as u32);
            }
            ir::Instruction::Assign(variable, operand) => match variable.ty.clone().into() {
                ValType::I32 => {
                    let variables = ctx.local_function_variables.get(function_name);
                    let Some(idx) = variables.iter().position(|v| v.name == variable.name) else {
                        panic!("Variable {:?} not found", variable);
                    };
                    operand.lower_to_wasm32(function_name, assembler, ctx);
                    assembler.local_set(idx as u32);
                }
                ValType::I64 => todo!("@assign i64"),
                ValType::F32 => todo!("@assign f32"),
                ValType::F64 => todo!("@assign f64"),
                ValType::V128 => todo!("@assign v128"),
                ValType::Ref(_) => todo!("@assign ref"),
            },
            ir::Instruction::Alloc(_, variable) => {
                // TODO: not sure what to output here.
                eprintln!("@alloc {}", variable);
            }
            ir::Instruction::Call(variable, name, operands) => {
                for operand in operands.iter() {
                    operand.lower_to_wasm32(function_name, assembler, ctx);
                }
                let Some(function_id) = ctx.local_function_variables.get_function_id(name.as_str())
                else {
                    panic!("Function {:?} not found", name);
                };
                assembler.call(function_id as u32);
                if let Some(variable) = variable {
                    let variables = ctx.local_function_variables.get(function_name);
                    let Some(idx) = variables.iter().position(|v| v.name == variable.name) else {
                        panic!("Variable {:?} not found", variable);
                    };
                    assembler.local_set(idx as u32);
                }
            }
            ir::Instruction::Cmp(variable, lhs, rhs) => {
                lhs.lower_to_wasm32(function_name, assembler, ctx);
                rhs.lower_to_wasm32(function_name, assembler, ctx);
                match variable.ty.clone().into() {
                    ValType::I32 => assembler.i32_eq(),
                    ValType::I64 => todo!("@gt i64"),
                    ValType::F32 => todo!("@gt f32"),
                    ValType::F64 => todo!("@gt f64"),
                    ValType::V128 => todo!("@gt v128"),
                    ValType::Ref(_) => todo!("@gt ref"),
                };
                let Some(idx) = ctx
                    .local_function_variables
                    .get(function_name)
                    .iter()
                    .position(|v| v.name == variable.name)
                else {
                    panic!("Variable {:?} not found", variable);
                };
                assembler.local_set(idx as u32);
            }
            ir::Instruction::Gt(variable, lhs, rhs) => {
                lhs.lower_to_wasm32(function_name, assembler, ctx);
                rhs.lower_to_wasm32(function_name, assembler, ctx);
                match variable.ty.clone().into() {
                    ValType::I32 => assembler.i32_gt_s(),
                    ValType::I64 => todo!("@gt i64"),
                    ValType::F32 => todo!("@gt f32"),
                    ValType::F64 => todo!("@gt f64"),
                    ValType::V128 => todo!("@gt v128"),
                    ValType::Ref(_) => todo!("@gt ref"),
                };
                let Some(idx) = ctx
                    .local_function_variables
                    .get(function_name)
                    .iter()
                    .position(|v| v.name == variable.name)
                else {
                    panic!("Variable {:?} not found", variable);
                };
                assembler.local_set(idx as u32);
            }
            ir::Instruction::Lt(variable, lhs, rhs) => {
                lhs.lower_to_wasm32(function_name, assembler, ctx);
                rhs.lower_to_wasm32(function_name, assembler, ctx);
                match variable.ty.clone().into() {
                    ValType::I32 => assembler.i32_lt_s(),
                    ValType::I64 => todo!("@gt i64"),
                    ValType::F32 => todo!("@gt f32"),
                    ValType::F64 => todo!("@gt f64"),
                    ValType::V128 => todo!("@gt v128"),
                    ValType::Ref(_) => todo!("@gt ref"),
                };
                let Some(idx) = ctx
                    .local_function_variables
                    .get(function_name)
                    .iter()
                    .position(|v| v.name == variable.name)
                else {
                    panic!("Variable {:?} not found", variable);
                };
                assembler.local_set(idx as u32);
            }
            ir::Instruction::Jump(_) => todo!("@jump"),
            ir::Instruction::JumpIf(operand, _) => todo!("@jumpif"),
            ir::Instruction::Load(variable, operand) => todo!("@load"),
            ir::Instruction::Mul(variable, operand, operand1) => todo!("@mul"),
            ir::Instruction::Phi(variable, items) => todo!("@phi"),
            ir::Instruction::Return(ty, operand) => match ty.clone().into() {
                Type::Void => todo!("@return void"),
                _ => {
                    if operand.is_variable() {
                        let local_variables = ctx.local_function_variables.get(function_name);
                        let ir::Operand::Variable(variable) = operand else {
                            panic!("Operand is not a variable");
                        };
                        let Some(idx) =
                            local_variables.iter().position(|v| v.name == variable.name)
                        else {
                            panic!("Variable {:?} not found", variable);
                        };
                        assembler.local_get(idx as u32);
                    }
                    assembler.return_();
                }
            },
            ir::Instruction::Sub(variable, lhs, rhs) => {
                lhs.lower_to_wasm32(function_name, assembler, ctx);
                rhs.lower_to_wasm32(function_name, assembler, ctx);
                match variable.ty.clone().into() {
                    ValType::I32 => assembler.i32_sub(),
                    ValType::I64 => todo!("@gt i64"),
                    ValType::F32 => todo!("@gt f32"),
                    ValType::F64 => todo!("@gt f64"),
                    ValType::V128 => todo!("@gt v128"),
                    ValType::Ref(_) => todo!("@gt ref"),
                };
                let Some(idx) = ctx
                    .local_function_variables
                    .get(function_name)
                    .iter()
                    .position(|v| v.name == variable.name)
                else {
                    panic!("Variable {:?} not found", variable);
                };
                assembler.local_set(idx as u32);
            }
            ir::Instruction::Div(variable, operand, operand1) => todo!("@div"),
            ir::Instruction::IfElse {
                cond,
                cond_result,
                then_branch,
                else_branch,
                result,
            } => {
                for block in cond.iter() {
                    block.lower_to_wasm32(function_name, assembler, ctx)?;
                }

                // let ty: BlockType = result
                //     .as_ref()
                //     .map(|r| r.ty.clone().into())
                //     .unwrap_or(BlockType::Empty);

                let Some(idx) = ctx
                    .local_function_variables
                    .get(function_name)
                    .iter()
                    .position(|v| v.name == cond_result.name)
                else {
                    panic!("Variable {:?} not found", cond_result);
                };

                assembler.local_get(idx as u32);
                assembler.if_(BlockType::Empty);

                for block in then_branch.iter() {
                    block.lower_to_wasm32(function_name, assembler, ctx)?;
                }

                if !else_branch.is_empty() {
                    assembler.else_();
                    for block in else_branch.iter() {
                        block.lower_to_wasm32(function_name, assembler, ctx)?;
                    }
                }

                assembler.end();
            }
        }
        Ok(())
    }
}

impl LowerToWasm32 for ir::Operand {
    fn lower_to_wasm32(
        &self,
        function_name: &str,
        assembler: &mut InstructionSink<'_>,
        ctx: &mut crate::backend::Context,
    ) -> Result<(), crate::error::Error> {
        match self {
            ir::Operand::ConstantInt { value, ty } => match ty.clone().into() {
                ValType::I32 => {
                    assembler.i32_const(value.parse().unwrap());
                }
                ValType::I64 => todo!("@const_i64"),
                ValType::F32 => todo!("@const_f32"),
                ValType::F64 => todo!("@const_f64"),
                ValType::V128 => todo!("@const_v128"),
                ValType::Ref(_) => todo!("@const_ref"),
            },
            ir::Operand::Variable(variable) => {
                let variables = ctx.local_function_variables.get(function_name);
                let Some(idx) = variables.iter().position(|v| v.name == variable.name) else {
                    panic!("Variable {:?} not found in {:#?}", variable, variables);
                };
                assembler.local_get(idx as u32);
            }
            ir::Operand::None => todo!("@none"),
        }
        Ok(())
    }
}
