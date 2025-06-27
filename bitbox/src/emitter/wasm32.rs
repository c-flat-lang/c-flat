use super::Emitter;
use crate::ir::{
    Constant, Function, Import, Instruction, LabeledInstruction, Operand, Type, Variable,
    Visibility,
};
use std::collections::HashMap;
use wasm_encoder::{
    CodeSection, ExportKind, ExportSection, Function as WasmFunction, FunctionSection, Ieee32,
    Ieee64, InstructionSink, Module as WasmModule, TypeSection, ValType,
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

#[derive(Debug)]
pub struct Wasm32 {
    module: WasmModule,
    funciton_count: u32,
    variables: HashMap<Variable, Operand>,
}

impl Wasm32 {
    fn emit_block(&mut self, block: &[LabeledInstruction], instructions: &mut InstructionSink) {
        for instruction in block {
            self.emit_instruction(&instruction, instructions);
        }
    }

    fn emit_instruction(
        &mut self,
        labeled_instruction: &LabeledInstruction,
        sink: &mut InstructionSink,
    ) {
        let label = labeled_instruction.label.as_ref();
        match &labeled_instruction.instruction {
            Instruction::NoOp => {
                sink.nop();
            }
            Instruction::Add(var, lhs, rhs) => self.emit_add(label, var, lhs, rhs, sink),
            Instruction::Assign(var, value) => self.emit_assign(label, var, value, sink),
            Instruction::Call(var, caller, args) => todo!("Call\n{var:?}\n{caller:?}\n{args:?}"),
            Instruction::Cmp(..) => todo!("Cmp"),
            Instruction::Jump(..) => todo!("Jump"),
            Instruction::JumpIf(..) => todo!("JumpIf"),
            Instruction::Load(..) => todo!("Load"),
            Instruction::Mul(var, lhs, rhs) => self.emit_mul(label, var, lhs, rhs, sink),
            Instruction::Phi(..) => todo!("Phi"),
            Instruction::Return(ty, operand) => self.emit_return(label, ty, operand, sink),
            Instruction::Sub(..) => todo!("Sub"),
            Instruction::Div(..) => todo!("Div"),
        };
    }

    fn emit_add(
        &self,
        _label: Option<&String>,
        var: &Variable,
        lhs: &Operand,
        rhs: &Operand,
        sink: &mut InstructionSink,
    ) {
        self.emit_operand(lhs, sink);
        self.emit_operand(rhs, sink);
        let ty = ir_type_to_wasm_ty(&var.ty).unwrap();
        match ty {
            ValType::I32 => sink.i32_add(),
            ValType::I64 => sink.i64_add(),
            ValType::F32 => sink.f32_add(),
            ValType::F64 => sink.f64_add(),
            ValType::V128 => todo!(),
            ValType::Ref(ref_type) => todo!("{ref_type:?}"),
        };
    }

    fn emit_operand(&self, operand: &Operand, sink: &mut InstructionSink) {
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
                let Some(operand) = self.variables.get(variable) else {
                    panic!("Variable not found: {variable:?}");
                };
                self.emit_operand(operand, sink);
                sink
            }
            Operand::None => todo!("NONE"),
        };
    }

    fn emit_mul(
        &self,
        _label: Option<&String>,
        var: &Variable,
        lhs: &Operand,
        rhs: &Operand,
        sink: &mut InstructionSink,
    ) {
        self.emit_operand(lhs, sink);
        self.emit_operand(rhs, sink);
        let ty = ir_type_to_wasm_ty(&var.ty).unwrap();
        match ty {
            ValType::I32 => sink.i32_mul(),
            ValType::I64 => sink.i64_mul(),
            ValType::F32 => sink.f32_mul(),
            ValType::F64 => sink.f64_mul(),
            ValType::V128 => todo!(),
            ValType::Ref(ref_type) => todo!("{ref_type:?}"),
        };
    }

    fn emit_assign(
        &mut self,
        _label: Option<&String>,
        var: &Variable,
        value: &Operand,
        _sink: &mut InstructionSink,
    ) {
        self.variables.insert(var.clone(), value.clone());
    }

    fn emit_return(
        &self,
        _label: Option<&String>,
        _ty: &Type,
        operand: &Operand,
        sink: &mut InstructionSink,
    ) {
        self.emit_operand(operand, sink);
        sink.return_();
    }
}

impl Default for Wasm32 {
    fn default() -> Self {
        Self {
            module: WasmModule::new(),
            funciton_count: 0,
            variables: HashMap::new(),
        }
    }
}

impl Emitter for Wasm32 {
    fn emit_function(&mut self, function: &Function) {
        eprintln!("{}", function);

        // Type Section
        let mut type_section = TypeSection::new();
        let param_types: Vec<ValType> = function
            .params
            .iter()
            .map(|p| ir_type_to_wasm_ty(&p.ty).unwrap())
            .collect();

        let result_type = vec![ir_type_to_wasm_ty(&function.return_type).unwrap()];
        type_section.ty().function(param_types, result_type);
        self.module.section(&type_section);

        // Function Section
        let mut function_section = FunctionSection::new();
        function_section.function(self.funciton_count);
        self.module.section(&function_section);

        // Export Section
        let mut export_section = ExportSection::new();
        if let Visibility::Public = function.visibility {
            export_section.export(
                function.name.as_str(),
                ExportKind::Func,
                self.funciton_count,
            );
            self.module.section(&export_section);
        }

        let mut code_section = CodeSection::new();
        let mut locals: Vec<(u32, ValType)> = function
            .params
            .iter()
            .enumerate()
            .map(|(i, p)| (i as u32, ir_type_to_wasm_ty(&p.ty).unwrap()))
            .collect();
        let mut f = WasmFunction::new(locals);
        let mut instructions = f.instructions();
        instructions.end();
        self.emit_block(&function.blocks, &mut instructions);
        instructions.end();
        self.funciton_count += 1;
    }

    fn emit_import(&mut self, _: &Import) {}
    fn emit_constant(&mut self, _: &Constant) {}
}
