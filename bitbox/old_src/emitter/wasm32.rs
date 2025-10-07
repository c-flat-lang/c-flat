use super::Emitter;
use crate::ir::{
    BasicBlock, Constant, Function, Import, Instruction, Operand, Type, Variable, Visibility,
};
use std::collections::HashMap;
use wasm_encoder::{
    BlockType, CodeSection, ExportKind, ExportSection, Function as WasmFunction, FunctionSection,
    Ieee32, Ieee64, InstructionSink, Module as WasmModule, TypeSection, ValType,
};

fn ir_type_to_wasm_ty(ty: &Type) -> Option<ValType> {
    match ty {
        Type::Unsigned(1..=32) => Some(ValType::I32),
        Type::Signed(1..=32) => Some(ValType::I32),
        Type::Float(1..=32) => Some(ValType::F32),
        Type::Float(33..=64) => Some(ValType::F64),
        Type::Pointer(_) => todo!(),
        Type::Array(_, _) => todo!(),
        Type::Void => None,
        _ => panic!("Unsupported type: {}", ty),
    }
}

#[derive(Debug, Default, Clone)]
struct LocalScope {
    variables: HashMap<Variable, Operand>,
    variables_ids: HashMap<Variable, u32>,
    function_ids: HashMap<String, u32>,
    label_ids: HashMap<String, u32>,
}

impl LocalScope {
    fn get_variable_id(&mut self, var: &Variable) -> u32 {
        let Some(id) = self.variables_ids.get(var) else {
            panic!("Variable not found: {}", var);
        };

        *id
    }

    fn set_variable(&mut self, var: Variable, value: Operand) {
        self.variables.insert(var, value);
    }

    fn get_label_id(&self, label: &str) -> Option<u32> {
        self.label_ids.get(label).copied()
    }

    fn set_label_id(&mut self, label: &str) {
        let id = self.label_ids.len() as u32;
        self.label_ids.insert(label.to_string(), id);
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
#[derive(Debug)]
pub struct Wasm32 {
    module: WasmModule,
    type_section: TypeSection,
    code_section: CodeSection,
    export_section: ExportSection,
    function_section: FunctionSection,

    funciton_count: u32,
    local_scope: LocalScope,
}

impl Wasm32 {
    fn emit_block(
        &mut self,
        blocks: &[BasicBlock],
        sink: &mut InstructionSink,
        scope: &mut LocalScope,
    ) {
        for block in blocks.iter() {
            for instruction in block.instructions.iter() {
                self.emit_instruction(instruction, sink, scope);
            }
        }
        // for instruction in block {
        //     let Some(label) = &instruction.label else {
        //         continue;
        //     };
        //     scope.set_label_id(label);
        // }
        // let mut label_depths = HashMap::new();
        // let mut block_stack = vec![];
        //
        // for instruction in block.iter() {
        //     if let Some(label) = &instruction.label {
        //         sink.block(BlockType::Empty);
        //         scope.set_label_id(label);
        //         label_depths.insert(label.clone(), block_stack.len());
        //         block_stack.push(label.clone());
        //     }
        //
        //     self.emit_instruction(instruction, sink, scope);
        //
        //     if instruction.label.is_some() {
        //         sink.end();
        //         block_stack.pop();
        //     }
        // }
    }

    fn emit_instruction(
        &mut self,
        instruction: &Instruction,
        sink: &mut InstructionSink,
        scope: &mut LocalScope,
    ) {
        //         let label = labeled_instruction.label.as_ref();
        match &instruction {
            // Instruction::NoOp => {
            //     // sink.nop();
            // }
            Instruction::Add(var, lhs, rhs) => self.emit_add(var, lhs, rhs, sink, scope),
            Instruction::Assign(var, value) => self.emit_assign(var, value, sink, scope),
            // Instruction::Call(var, caller, args) => {
            //     self.emit_call(label, var, caller, args, sink, scope)
            // }
            // Instruction::Cmp(var, lhs, rhs) => self.emit_cmp(label, var, lhs, rhs, sink, scope),
            // Instruction::Gt(var, lhs, rhs) => self.emit_gt(label, var, lhs, rhs, sink, scope),
            // Instruction::Jump(jump_label) => self.emit_jump(jump_label, sink, scope),
            // Instruction::JumpIf(condition, jump_label) => {
            //     self.emit_jump_if(jump_label, condition, sink, scope)
            // }
            // Instruction::Load(..) => todo!("Load"),
            Instruction::Mul(var, lhs, rhs) => self.emit_mul(var, lhs, rhs, sink, scope),
            // Instruction::Phi(..) => {}
            Instruction::Return(ty, operand) => self.emit_return(ty, operand, sink, scope),
            Instruction::Sub(var, lhs, rhs) => self.emit_sub(var, lhs, rhs, sink, scope),
            // Instruction::Div(..) => todo!("Div"),
            i => todo!("{i:#?}"),
        };
    }

    fn emit_add(
        &mut self,
        var: &Variable,
        lhs: &Operand,
        rhs: &Operand,
        sink: &mut InstructionSink,
        scope: &mut LocalScope,
    ) {
        self.emit_operand(lhs, sink, scope);
        self.emit_operand(rhs, sink, scope);
        let ty = ir_type_to_wasm_ty(&var.ty).unwrap();
        match ty {
            ValType::I32 => sink.i32_add(),
            ValType::I64 => sink.i64_add(),
            ValType::F32 => sink.f32_add(),
            ValType::F64 => sink.f64_add(),
            ValType::V128 => todo!(),
            ValType::Ref(ref_type) => todo!("{ref_type:?}"),
        };
        let id = scope.get_variable_id(var);
        sink.local_tee(id);
    }

    fn emit_sub(
        &mut self,
        var: &Variable,
        lhs: &Operand,
        rhs: &Operand,
        sink: &mut InstructionSink,
        scope: &mut LocalScope,
    ) {
        self.emit_operand(lhs, sink, scope);
        self.emit_operand(rhs, sink, scope);
        let ty = ir_type_to_wasm_ty(&var.ty).unwrap();
        match ty {
            ValType::I32 => sink.i32_sub(),
            ValType::I64 => sink.i64_sub(),
            ValType::F32 => sink.f32_sub(),
            ValType::F64 => sink.f64_sub(),
            ValType::V128 => todo!(),
            ValType::Ref(ref_type) => todo!("{ref_type:?}"),
        };
        let id = scope.get_variable_id(var);
        sink.local_tee(id);
    }

    fn emit_operand(
        &mut self,
        operand: &Operand,
        sink: &mut InstructionSink,
        scope: &mut LocalScope,
    ) {
        match operand {
            Operand::ConstantInt { value, ty } => match ir_type_to_wasm_ty(ty) {
                Some(ValType::I32) => sink.i32_const(value.parse().unwrap()),
                Some(ValType::I64) => sink.i64_const(value.parse().unwrap()),
                Some(ValType::F32) => sink.f32_const(Ieee32::from(value.parse::<f32>().unwrap())),
                Some(ValType::F64) => sink.f64_const(Ieee64::from(value.parse::<f64>().unwrap())),
                Some(ValType::V128) => todo!(),
                Some(ValType::Ref(ref_type)) => todo!("{ref_type:?}"),
                None => todo!(),
            },
            Operand::Variable(variable) => {
                let var_id = scope.get_variable_id(variable);
                sink.local_get(var_id)
            }
            Operand::None => todo!("NONE"),
        };
    }

    fn emit_mul(
        &mut self,
        var: &Variable,
        lhs: &Operand,
        rhs: &Operand,
        sink: &mut InstructionSink,
        scope: &mut LocalScope,
    ) {
        self.emit_operand(lhs, sink, scope);
        self.emit_operand(rhs, sink, scope);
        let ty = ir_type_to_wasm_ty(&var.ty).unwrap();
        match ty {
            ValType::I32 => sink.i32_mul(),
            ValType::I64 => sink.i64_mul(),
            ValType::F32 => sink.f32_mul(),
            ValType::F64 => sink.f64_mul(),
            ValType::V128 => todo!(),
            ValType::Ref(ref_type) => todo!("{ref_type:?}"),
        };
        let id = scope.get_variable_id(var);
        sink.local_set(id);
    }

    fn emit_assign(
        &mut self,
        var: &Variable,
        value: &Operand,
        sink: &mut InstructionSink,
        scope: &mut LocalScope,
    ) {
        scope.set_variable(var.clone(), value.clone());
        let id = scope.get_variable_id(var);
        self.emit_operand(value, sink, scope);
        sink.local_set(id);
    }

    fn emit_return(
        &mut self,
        _ty: &Type,
        operand: &Operand,
        sink: &mut InstructionSink,
        scope: &mut LocalScope,
    ) {
        self.emit_operand(operand, sink, scope);
        sink.return_();
    }

    //     fn emit_call(
    //         &mut self,
    //         _label: Option<&String>,
    //         var: &Variable,
    //         caller: &str,
    //         args: &[Operand],
    //         sink: &mut InstructionSink<'_>,
    //         scope: &mut LocalScope,
    //     ) {
    //         for arg in args.iter() {
    //             self.emit_operand(arg, sink, scope);
    //         }
    //         let Some(id) = scope.function_ids.get(caller) else {
    //             panic!("Function not found: {caller}");
    //         };
    //         sink.call(*id);
    //         if var.ty == Type::Void {
    //             return;
    //         }
    //         let var_id = scope.get_variable_id(var);
    //         sink.local_set(var_id);
    //     }
    //
    //     fn emit_cmp(
    //         &mut self,
    //         _label: Option<&String>,
    //         var: &Variable,
    //         lhs: &Operand,
    //         rhs: &Operand,
    //         sink: &mut InstructionSink<'_>,
    //         scope: &mut LocalScope,
    //     ) {
    //         self.emit_operand(lhs, sink, scope);
    //         self.emit_operand(rhs, sink, scope);
    //         let ty = ir_type_to_wasm_ty(&var.ty).unwrap();
    //         match ty {
    //             ValType::I32 => sink.i32_eq(),
    //             ValType::I64 => sink.i64_eq(),
    //             ValType::F32 => sink.f32_eq(),
    //             ValType::F64 => sink.f64_eq(),
    //             ValType::V128 => todo!("V128"),
    //             ValType::Ref(ref_type) => todo!("{ref_type:?}"),
    //         };
    //
    //         let var_id = scope.get_variable_id(var);
    //         sink.local_set(var_id);
    //     }
    //
    //     fn emit_gt(
    //         &mut self,
    //         _label: Option<&String>,
    //         var: &Variable,
    //         lhs: &Operand,
    //         rhs: &Operand,
    //         sink: &mut InstructionSink<'_>,
    //         scope: &mut LocalScope,
    //     ) {
    //         self.emit_operand(lhs, sink, scope);
    //         self.emit_operand(rhs, sink, scope);
    //         let ty = ir_type_to_wasm_ty(&var.ty).unwrap();
    //         match ty {
    //             ValType::I32 => sink.i32_gt_s(),
    //             ValType::I64 => sink.i64_gt_s(),
    //             ValType::F32 => sink.f32_gt(),
    //             ValType::F64 => sink.f64_gt(),
    //             ValType::V128 => todo!("V128"),
    //             ValType::Ref(ref_type) => todo!("{ref_type:?}"),
    //         };
    //
    //         let var_id = scope.get_variable_id(var);
    //         sink.local_set(var_id);
    //     }
    //
    //     fn emit_jump_if(
    //         &mut self,
    //         label: &str,
    //         condition: &Operand,
    //         sink: &mut InstructionSink<'_>,
    //         scope: &mut LocalScope,
    //     ) {
    //         eprintln!("JUMP_IF: {label} {condition:?}");
    //         self.emit_operand(condition, sink, scope);
    //         let Some(label_id) = scope.get_label_id(label) else {
    //             panic!("Label not found: {label}");
    //         };
    //         sink.i32_eqz();
    //         sink.br_if(label_id + 1);
    //     }
    //
    //     fn emit_jump(
    //         &mut self,
    //         jump_label: &str,
    //         sink: &mut InstructionSink<'_>,
    //         scope: &mut LocalScope,
    //     ) {
    //         let Some(label_id) = scope.get_label_id(jump_label) else {
    //             panic!("Label not found: {jump_label}");
    //         };
    //         sink.br(label_id);
    //     }
}

impl Default for Wasm32 {
    fn default() -> Self {
        Self {
            module: WasmModule::new(),
            type_section: TypeSection::new(),
            code_section: CodeSection::new(),
            export_section: ExportSection::new(),
            function_section: FunctionSection::new(),
            funciton_count: 0,
            local_scope: LocalScope::default(),
        }
    }
}

impl Emitter for Wasm32 {
    fn startup(&mut self, module: &crate::ir::Module) {
        let mut function_ids = HashMap::new();
        let mut funciton_count = 0;
        for function in &module.functions {
            function_ids.insert(function.name.clone(), funciton_count);
            funciton_count += 1;
        }
        self.local_scope.function_ids = function_ids
    }

    fn to_bytes(&mut self) -> Vec<u8> {
        self.module.section(&self.type_section);
        self.module.section(&self.function_section);
        self.module.section(&self.export_section);
        self.module.section(&self.code_section);
        self.module.clone().finish()
    }

    fn emit_function(&mut self, function: &Function) {
        eprintln!("{}", function);

        // Type Section
        let param_types: Vec<ValType> = function
            .params
            .iter()
            .map(|p| ir_type_to_wasm_ty(&p.ty).unwrap())
            .collect();

        let result_type = match ir_type_to_wasm_ty(&function.return_type) {
            Some(t) => vec![t],
            None => vec![],
        };
        self.type_section.ty().function(param_types, result_type);

        // Function Section
        self.function_section.function(self.funciton_count);

        // Export Section
        if let Visibility::Public = function.visibility {
            self.export_section.export(
                function.name.as_str(),
                ExportKind::Func,
                self.funciton_count,
            );
        }

        let mut local_scope = self.local_scope.clone();
        let mut locals: HashMap<ValType, u32> = HashMap::new();
        let mut offset_i = 0;
        for (i, p) in function.params.iter().enumerate() {
            let ty = ir_type_to_wasm_ty(&p.ty).unwrap();
            locals
                .entry(ty)
                .and_modify(|counter| *counter += 1)
                .or_insert(1);
            local_scope.variables_ids.insert(p.clone(), i as u32);
            offset_i += 1;
        }
        for (i, v) in function.locals.iter().enumerate() {
            let ty = ir_type_to_wasm_ty(&v.ty).unwrap();
            locals
                .entry(ty)
                .and_modify(|counter| *counter += 1)
                .or_insert(1);
            local_scope
                .variables_ids
                .insert(v.clone(), i as u32 + offset_i);
        }

        let locals = locals.iter().fold(Vec::new(), |mut acc, (ty, count)| {
            acc.push((*count, *ty));
            acc
        });
        let mut f = WasmFunction::new(locals);
        let mut instructions = f.instructions();
        // let function_return_type = match function.return_type {
        //     Type::Void => BlockType::Empty,
        //     _ => BlockType::Result(ir_type_to_wasm_ty(&function.return_type).unwrap()),
        // };

        self.emit_block(&function.blocks, &mut instructions, &mut local_scope);

        instructions.end();
        self.code_section.function(&f);
        self.funciton_count += 1;
    }

    fn emit_import(&mut self, _: &Import) {
        todo!("emit_import")
    }
    fn emit_constant(&mut self, _: &Constant) {
        todo!("emit_constant")
    }
}
