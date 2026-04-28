mod instruction;
use std::collections::HashMap;

use crate::backend::Lower;
use crate::ir::{self, Module, Type, Visibility};
use crate::passes::{DebugPass, Pass, PassOutput};
use wasm_encoder::{
    BlockType, CodeSection, ConstExpr, EntityType, ExportKind, ExportSection,
    Function as WasmFunction, FunctionSection, GlobalSection, GlobalType, ImportSection,
    InstructionSink, MemorySection, MemoryType, Module as WasmModule, TypeSection, ValType,
};

#[derive(Debug)]
pub struct EmitWasm32Pass;

impl Pass for EmitWasm32Pass {
    fn debug_pass(&self) -> DebugPass {
        DebugPass::Emit
    }

    fn debug(&self, _module: &crate::ir::Module, ctx: &crate::backend::Context) -> PassOutput {
        let mut wasm_module = ctx.output.get_wasm32().clone();
        let bytes = wasm_module.finish();
        let output = match wasmprinter::print_bytes(&bytes) {
            Ok(wat) => wat,
            Err(err) => err.to_string(),
        };
        PassOutput::String(output)
    }

    fn run(
        &mut self,
        module: &mut Module,
        ctx: &mut crate::backend::Context,
    ) -> Result<(), crate::error::Error> {
        {
            let module = ctx.output.get_mut_wasm32();

            module.import_section.import(
                "core",
                "write_i32",
                EntityType::Function(module.funciton_count),
            );
            module
                .type_section
                .ty()
                .function(vec![ValType::I32], vec![]);

            module.funciton_count += 1;

            module.import_section.import(
                "core",
                "writenl",
                EntityType::Function(module.funciton_count),
            );
            module.type_section.ty().function(vec![], vec![]);

            module.funciton_count += 1;

            module.import_section.import(
                "core",
                "write_char",
                EntityType::Function(module.funciton_count),
            );

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

        // Emit extern C functions as wasm imports (skip built-ins already added above).
        const BUILTIN_IMPORTS: &[&str] = &["write_i32", "writenl", "write_char"];
        for ext in module.externs.iter() {
            if BUILTIN_IMPORTS.contains(&ext.name.as_str()) {
                continue;
            }
            let param_types: Vec<ValType> = ext.params.iter().map(|t| t.clone().into()).collect();
            let result_types: Vec<ValType> = if ext.return_type == Type::Void {
                vec![]
            } else {
                vec![ext.return_type.clone().into()]
            };
            let wasm_out = ctx.output.get_mut_wasm32();
            wasm_out
                .type_section
                .ty()
                .function(param_types, result_types);
            wasm_out.import_section.import(
                "env",
                ext.name.as_str(),
                EntityType::Function(wasm_out.funciton_count),
            );
            wasm_out.funciton_count += 1;
        }

        // Rebuild function index list so get_function_id() returns correct wasm indices:
        // [built-in imports (0..2), extern imports (3..), defined functions (..)].
        ctx.local_function_variables.clear_functions();
        for name in BUILTIN_IMPORTS {
            ctx.local_function_variables.register_function(*name);
        }
        for ext in module.externs.iter() {
            if !BUILTIN_IMPORTS.contains(&ext.name.as_str()) {
                ctx.local_function_variables
                    .register_function(ext.name.as_str());
            }
        }
        for func in module.functions.iter() {
            ctx.local_function_variables
                .register_function(func.name.as_str());
        }

        for function in module.functions.iter() {
            function.lower(ctx, &mut *self)?;
        }
        Ok(())
    }
}

#[derive(Debug)]
pub(crate) struct Wasm32LowerContext<'ctx> {
    pub function_name: String,
    pub assembler: &'ctx mut InstructionSink<'ctx>,
    pub blocks: HashMap<String, u32>,
}

impl<'ctx> Wasm32LowerContext<'ctx> {
    fn new(function_name: String, assembler: &'ctx mut InstructionSink<'ctx>) -> Self {
        Self {
            function_name,
            assembler,
            blocks: HashMap::new(),
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
#[derive(Debug, Default, Clone)]
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
            ir::Type::Struct(_) => ValType::I32,
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

            let mut target = Wasm32LowerContext::new(self.name.to_string(), &mut instructions);
            for (index, block) in self.blocks.iter().enumerate() {
                target.blocks.insert(block.label.clone(), index as u32);
            }
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
            ir::Instruction::Add(iadd) => iadd.lower(ctx, target)?,
            ir::Instruction::Assign(iassign) => iassign.lower(ctx, target)?,
            ir::Instruction::Alloc(ialloc) => ialloc.lower(ctx, target)?,
            ir::Instruction::Call(icall) => icall.lower(ctx, target)?,
            ir::Instruction::Cmp(icmp) => icmp.lower(ctx, target)?,
            ir::Instruction::Copy(icopy) => icopy.lower(ctx, target)?,
            ir::Instruction::ElemGet(ielemget) => ielemget.lower(ctx, target)?,
            ir::Instruction::ElemSet(ielemset) => ielemset.lower(ctx, target)?,
            ir::Instruction::And(iand) => iand.lower(ctx, target)?,
            ir::Instruction::Or(ior) => ior.lower(ctx, target)?,
            ir::Instruction::XOr(ixor) => ixor.lower(ctx, target)?,
            ir::Instruction::Gt(igt) => igt.lower(ctx, target)?,
            ir::Instruction::Gte(igte) => igte.lower(ctx, target)?,
            ir::Instruction::Lt(ilt) => ilt.lower(ctx, target)?,
            ir::Instruction::Jump(ijump) => ijump.lower(ctx, target)?,
            ir::Instruction::JumpIf(ijumpif) => ijumpif.lower(ctx, target)?,
            ir::Instruction::Load(iload) => iload.lower(ctx, target)?,
            ir::Instruction::Mul(imul) => imul.lower(ctx, target)?,
            ir::Instruction::Phi(..) => todo!("@phi"),
            ir::Instruction::Return(ireturn) => ireturn.lower(ctx, target)?,
            ir::Instruction::Sub(isub) => isub.lower(ctx, target)?,
            ir::Instruction::Div(..) => todo!("@div"),
            ir::Instruction::IfElse(iifelse) => iifelse.lower(ctx, target)?,
            ir::Instruction::Loop(iloop) => iloop.lower(ctx, target)?,
            ir::Instruction::Ref(..) => todo!("@ref"),
            ir::Instruction::Not(inot) => inot.lower(ctx, target)?,
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
            ir::Operand::ConstantInt(constant) => match constant.ty.clone().into() {
                ValType::I32 => {
                    target.assembler.i32_const(constant.value as i32);
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
