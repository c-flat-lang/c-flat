#![allow(unused)]
use std::collections::HashMap;

use crate::backend::Lower;
use crate::ir::instruction::{
    IAdd, IAlloc, IAssign, ICall, ICmp, IElemGet, IElemSet, IGt, ILt, ISub,
};
use crate::ir::{self, Module, Type, Visibility};
use crate::passes::Pass;
use wasm_encoder::{
    BlockType, CodeSection, ConstExpr, EntityType, ExportKind, ExportSection,
    Function as WasmFunction, FunctionSection, GlobalSection, GlobalType, ImportSection,
    InstructionSink, MemorySection, MemoryType, Module as WasmModule, TypeSection, ValType,
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

            module.memory_section.memory(MemoryType {
                minimum: 1,
                maximum: None,
                memory64: false,
                shared: false,
                page_size_log2: None,
            });
            module.global_section.global(
                GlobalType {
                    val_type: ValType::I32,
                    mutable: true,
                    shared: false,
                },
                &ConstExpr::i32_const(0),
            );
        }

        for function in module.functions.iter() {
            function.lower(ctx, &mut *self)?;
        }
        Ok(())
    }
}

#[derive(Debug)]
struct Wasm32LowerContext<'ctx> {
    function_name: String,
    assembler: &'ctx mut InstructionSink<'ctx>,
}
impl<'ctx> Wasm32LowerContext<'ctx> {
    fn new(function_name: String, assembler: &'ctx mut InstructionSink<'ctx>) -> Self {
        Self {
            function_name,
            assembler,
        }
    }
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
    function_section: FunctionSection,
    memory_section: MemorySection,
    global_section: GlobalSection,
    export_section: ExportSection,
    code_section: CodeSection,

    /// Counter for function ids for the function section
    funciton_count: u32,
}

impl Wasm32Module {
    pub fn finish(&mut self) -> Vec<u8> {
        self.module.section(&self.type_section);
        self.module.section(&self.import_section);
        self.module.section(&self.function_section);
        self.module.section(&self.memory_section);
        self.module.section(&self.global_section);
        self.module.section(&self.export_section);
        self.module.section(&self.code_section);
        self.module.clone().finish()
    }
}

impl From<ir::Type> for ValType {
    fn from(value: ir::Type) -> Self {
        match value {
            ir::Type::Unsigned(1..=32) => ValType::I32,
            ir::Type::Signed(1..=32) => ValType::I32,
            ir::Type::Float(1..=32) => ValType::F32,
            ir::Type::Float(33..=64) => ValType::F64,
            ir::Type::Pointer(_) => ValType::I32,
            ir::Type::Array(_, _) => ValType::I32,
            ir::Type::Void => unreachable!("Void is not a valid type"),
            _ => panic!("Unsupported type: {}", value),
        }
    }
}

impl From<ir::Type> for BlockType {
    fn from(value: ir::Type) -> BlockType {
        match value {
            ir::Type::Unsigned(1..=32) => BlockType::Result(ValType::I32),
            ir::Type::Signed(1..=32) => BlockType::Result(ValType::I32),
            ir::Type::Float(1..=32) => BlockType::Result(ValType::F32),
            ir::Type::Float(33..=64) => BlockType::Result(ValType::F64),
            ir::Type::Pointer(_) => todo!(),
            ir::Type::Array(_, _) => todo!(),
            ir::Type::Void => todo!(),
            _ => panic!("Unsupported type: {}", value),
        }
    }
}

impl Lower<EmitWasm32Pass> for ir::Function {
    type Output = ();
    fn lower(
        &self,
        ctx: &mut crate::backend::Context,
        _: &mut EmitWasm32Pass,
    ) -> Result<Self::Output, crate::error::Error> {
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

            let mut target = Wasm32LowerContext::new(self.name.to_string(), &mut instructions);
            for block in self.blocks.iter() {
                block.lower(ctx, &mut target)?;
            }

            target.assembler.end();
            f
        };
        let module = ctx.output.get_mut_wasm32();
        module.code_section.function(&f);
        module.funciton_count += 1;
        Ok(())
    }
}

impl Lower<Wasm32LowerContext<'_>> for ir::BasicBlock {
    type Output = ();
    fn lower(
        &self,
        ctx: &mut crate::backend::Context,
        target: &mut Wasm32LowerContext<'_>,
    ) -> Result<Self::Output, crate::error::Error> {
        for instruction in self.instructions.iter() {
            instruction.lower(ctx, target)?;
        }
        Ok(())
    }
}

impl Lower<Wasm32LowerContext<'_>> for ir::Instruction {
    type Output = ();
    fn lower(
        &self,
        ctx: &mut crate::backend::Context,
        target: &mut Wasm32LowerContext<'_>,
    ) -> Result<Self::Output, crate::error::Error> {
        match self {
            ir::Instruction::NoOp(..) => todo!("@noop"),
            ir::Instruction::Add(IAdd { des, lhs, rhs }) => {
                lhs.lower(ctx, target);
                rhs.lower(ctx, target);
                match des.ty.clone().into() {
                    ValType::I32 => target.assembler.i32_add(),
                    ValType::I64 => todo!("@gt i64"),
                    ValType::F32 => todo!("@gt f32"),
                    ValType::F64 => todo!("@gt f64"),
                    ValType::V128 => todo!("@gt v128"),
                    ValType::Ref(_) => todo!("@gt ref"),
                };
                let Some(idx) = ctx
                    .local_function_variables
                    .get(&target.function_name)
                    .iter()
                    .position(|v| v.name == des.name)
                else {
                    panic!("Variable {:?} not found", des);
                };
                target.assembler.local_set(idx as u32);
            }
            ir::Instruction::Assign(IAssign { des, src }) => match des.ty.clone().into() {
                ValType::I32 => {
                    let variables = ctx.local_function_variables.get(&target.function_name);
                    let Some(idx) = variables.iter().position(|v| v.name == des.name) else {
                        panic!("Variable {:?} not found", des);
                    };
                    src.lower(ctx, target);
                    target.assembler.local_set(idx as u32);
                }
                ValType::I64 => todo!("@assign i64"),
                ValType::F32 => todo!("@assign f32"),
                ValType::F64 => todo!("@assign f64"),
                ValType::V128 => todo!("@assign v128"),
                ValType::Ref(_) => todo!("@assign ref"),
            },
            ir::Instruction::Alloc(IAlloc { ty, des, size }) => {
                des.lower(ctx, target);
                size.lower(ctx, target)?;
                target.assembler.i32_mul();

                target.assembler.global_get(0); // HACK: probably need to make a HEAP index.

                let Some(ptr_idx) = ctx
                    .local_function_variables
                    .get(&target.function_name)
                    .iter()
                    .position(|v| v.name == des.name)
                else {
                    panic!("Variable {:?} not found", des);
                };

                target.assembler.local_tee(ptr_idx as u32);
                target.assembler.i32_add();
                target.assembler.global_set(0); // HACK: probably need to make a HEAP index.
            }
            ir::Instruction::Call(ICall {
                des: variable,
                callee: name,
                args: operands,
            }) => {
                for operand in operands.iter() {
                    operand.lower(ctx, target);
                }
                let Some(function_id) = ctx.local_function_variables.get_function_id(name.as_str())
                else {
                    panic!("Function {:?} not found", name);
                };
                target.assembler.call(function_id as u32);
                if let Some(variable) = variable {
                    let variables = ctx.local_function_variables.get(&target.function_name);
                    let Some(idx) = variables.iter().position(|v| v.name == variable.name) else {
                        panic!("Variable {:?} not found", variable);
                    };
                    target.assembler.local_set(idx as u32);
                }
            }
            ir::Instruction::Cmp(ICmp { des, lhs, rhs }) => {
                lhs.lower(ctx, target);
                rhs.lower(ctx, target);
                match des.ty.clone().into() {
                    ValType::I32 => target.assembler.i32_eq(),
                    ValType::I64 => todo!("@gt i64"),
                    ValType::F32 => todo!("@gt f32"),
                    ValType::F64 => todo!("@gt f64"),
                    ValType::V128 => todo!("@gt v128"),
                    ValType::Ref(_) => todo!("@gt ref"),
                };
                let Some(idx) = ctx
                    .local_function_variables
                    .get(&target.function_name)
                    .iter()
                    .position(|v| v.name == des.name)
                else {
                    panic!("Variable {:?} not found", des);
                };
                target.assembler.local_set(idx as u32);
            }
            ir::Instruction::ElemGet(IElemGet { des, ptr, index }) => {
                index.lower(ctx, target);
                target.assembler.i32_const(des.ty.size());
                target.assembler.i32_mul();

                ptr.lower(ctx, target);
                target.assembler.i32_add();

                match des.ty.clone().into() {
                    ValType::I32 => target.assembler.i32_load(wasm_encoder::MemArg {
                        offset: 0,
                        align: 2,
                        memory_index: 0,
                    }),
                    ValType::I64 => todo!("@gt i64"),
                    ValType::F32 => todo!("@gt f32"),
                    ValType::F64 => todo!("@gt f64"),
                    ValType::V128 => todo!("@gt v128"),
                    ValType::Ref(_) => todo!("@gt ref"),
                };

                let variables = ctx.local_function_variables.get(&target.function_name);
                let Some(idx) = variables.iter().position(|v| v.name == des.name) else {
                    panic!("Variable {:?} not found", self);
                };
                target.assembler.local_set(idx as u32);
            }
            ir::Instruction::ElemSet(IElemSet { addr, index, value }) => {
                let ty = addr.ty.clone();
                index.lower(ctx, target);
                target.assembler.i32_const(ty.size());
                target.assembler.i32_mul();
                value.lower(ctx, target);
                match ty.clone().into() {
                    ValType::I32 => target.assembler.i32_store(wasm_encoder::MemArg {
                        offset: 0,
                        align: 2,
                        memory_index: 0,
                    }),
                    ValType::I64 => todo!("@gt i64"),
                    ValType::F32 => todo!("@gt f32"),
                    ValType::F64 => todo!("@gt f64"),
                    ValType::V128 => todo!("@gt v128"),
                    ValType::Ref(_) => todo!("@gt ref"),
                };
            }
            ir::Instruction::Gt(IGt { des, lhs, rhs }) => {
                lhs.lower(ctx, target);
                rhs.lower(ctx, target);
                match des.ty.clone().into() {
                    ValType::I32 => target.assembler.i32_gt_s(),
                    ValType::I64 => todo!("@gt i64"),
                    ValType::F32 => todo!("@gt f32"),
                    ValType::F64 => todo!("@gt f64"),
                    ValType::V128 => todo!("@gt v128"),
                    ValType::Ref(_) => todo!("@gt ref"),
                };
                let Some(idx) = ctx
                    .local_function_variables
                    .get(&target.function_name)
                    .iter()
                    .position(|v| v.name == des.name)
                else {
                    panic!("Variable {:?} not found", des);
                };
                target.assembler.local_set(idx as u32);
            }
            ir::Instruction::Lt(ILt { des, lhs, rhs }) => {
                lhs.lower(ctx, target);
                rhs.lower(ctx, target);
                match des.ty.clone().into() {
                    ValType::I32 => target.assembler.i32_lt_s(),
                    ValType::I64 => todo!("@gt i64"),
                    ValType::F32 => todo!("@gt f32"),
                    ValType::F64 => todo!("@gt f64"),
                    ValType::V128 => todo!("@gt v128"),
                    ValType::Ref(_) => todo!("@gt ref"),
                };
                let Some(idx) = ctx
                    .local_function_variables
                    .get(&target.function_name)
                    .iter()
                    .position(|v| v.name == des.name)
                else {
                    panic!("Variable {:?} not found", des);
                };
                target.assembler.local_set(idx as u32);
            }
            ir::Instruction::Jump(_) => todo!("@jump"),
            ir::Instruction::JumpIf(operand, label) => {}
            ir::Instruction::Load(variable, operand) => {
                operand.lower(ctx, target)?;
                let Some(idx) = ctx
                    .local_function_variables
                    .get(&target.function_name)
                    .iter()
                    .position(|v| v.name == variable.name)
                else {
                    panic!("Variable {:?} not found", variable);
                };
                target.assembler.local_set(idx as u32);
            }
            ir::Instruction::Mul(imul) => todo!("@mul"),
            ir::Instruction::Phi(variable, items) => todo!("@phi"),
            ir::Instruction::Return(ty, operand) => match ty.clone() {
                Type::Void => todo!("@return void"),
                _ => {
                    operand.lower(ctx, target);
                    // if operand.is_variable() {
                    //     let local_variables = ctx.local_function_variables.get(function_name);
                    //     let ir::Operand::Variable(variable) = operand else {
                    //         panic!("Operand is not a variable");
                    //     };
                    //     let Some(idx) =
                    //         local_variables.iter().position(|v| v.name == variable.name)
                    //     else {
                    //         panic!("Variable {:?} not found", variable);
                    //     };
                    //     assembler.local_get(idx as u32);
                    // }
                    target.assembler.return_();
                }
            },
            ir::Instruction::Sub(ISub { des, lhs, rhs }) => {
                lhs.lower(ctx, target);
                rhs.lower(ctx, target);
                match des.ty.clone().into() {
                    ValType::I32 => target.assembler.i32_sub(),
                    ValType::I64 => todo!("@gt i64"),
                    ValType::F32 => todo!("@gt f32"),
                    ValType::F64 => todo!("@gt f64"),
                    ValType::V128 => todo!("@gt v128"),
                    ValType::Ref(_) => todo!("@gt ref"),
                };
                let Some(idx) = ctx
                    .local_function_variables
                    .get(&target.function_name)
                    .iter()
                    .position(|v| v.name == des.name)
                else {
                    panic!("Variable {:?} not found", des);
                };
                target.assembler.local_set(idx as u32);
            }
            ir::Instruction::Div(idiv) => todo!("@div"),
            ir::Instruction::IfElse {
                cond,
                cond_result,
                then_branch,
                else_branch,
                result,
            } => {
                for block in cond.iter() {
                    block.lower(ctx, target)?;
                }

                // let ty: BlockType = result
                //     .as_ref()
                //     .map(|r| r.ty.clone().into())
                //     .unwrap_or(BlockType::Empty);

                let Some(idx) = ctx
                    .local_function_variables
                    .get(&target.function_name)
                    .iter()
                    .position(|v| v.name == cond_result.name)
                else {
                    panic!("Variable {:?} not found", cond_result);
                };

                target.assembler.local_get(idx as u32);
                target.assembler.if_(BlockType::Empty);

                for block in then_branch.iter() {
                    block.lower(ctx, target)?;
                }

                if !else_branch.is_empty() {
                    target.assembler.else_();
                    for block in else_branch.iter() {
                        block.lower(ctx, target)?;
                    }
                }

                target.assembler.end();
            }
        }
        Ok(())
    }
}

impl Lower<Wasm32LowerContext<'_>> for ir::Operand {
    type Output = ();
    fn lower(
        &self,
        ctx: &mut crate::backend::Context,
        target: &mut Wasm32LowerContext<'_>,
    ) -> Result<(), crate::error::Error> {
        match self {
            ir::Operand::ConstantInt { value, ty } => match ty.clone().into() {
                ValType::I32 => {
                    target.assembler.i32_const(value.parse().unwrap());
                }
                ValType::I64 => todo!("@const_i64"),
                ValType::F32 => todo!("@const_f32"),
                ValType::F64 => todo!("@const_f64"),
                ValType::V128 => todo!("@const_v128"),
                ValType::Ref(_) => todo!("@const_ref"),
            },
            ir::Operand::Variable(variable) => {
                let variables = ctx.local_function_variables.get(&target.function_name);
                let Some(idx) = variables.iter().position(|v| v.name == variable.name) else {
                    panic!("Variable {:?} not found in {:#?}", variable, variables);
                };
                target.assembler.local_get(idx as u32);
            }
            ir::Operand::None => todo!("@none"),
        }
        Ok(())
    }
}

impl Lower<Wasm32LowerContext<'_>> for ir::Variable {
    type Output = ();
    fn lower(
        &self,
        ctx: &mut crate::backend::Context,
        target: &mut Wasm32LowerContext<'_>,
    ) -> Result<(), crate::error::Error> {
        let variables = ctx.local_function_variables.get(&target.function_name);
        let Some(idx) = variables.iter().position(|v| v.name == self.name) else {
            panic!("Variable {:?} not found", self);
        };
        target.assembler.local_get(idx as u32);

        Ok(())
    }
}
