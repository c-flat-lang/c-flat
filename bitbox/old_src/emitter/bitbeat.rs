#![allow(unused)]
use super::Emitter;
use crate::ir::{self, Constant, Function, Import, Module, Operand, SymbolTable, Variable};
use std::collections::HashMap;

// fn ir_type_to_bitbeat_ty(ty: &Type) -> Option<bitbeat::Type> {
//     match ty {
//         Type::Unsigned(1..=32) => Some(ValType::I32),
//         Type::Signed(1..=32) => Some(ValType::I32),
//         Type::Float(1..=32) => Some(ValType::F32),
//         Type::Float(33..=64) => Some(ValType::F64),
//         Type::Pointer(_) => todo!(),
//         Type::Array(_, _) => todo!(),
//         Type::Void => None,
//         _ => panic!("Unsupported type: {}", ty),
//     }
// }

#[derive(Debug, Default)]
struct LocalScope {
    variables: HashMap<Variable, bitbeat::Reg>,
}

impl LocalScope {
    fn set_var(&mut self, var: Variable, reg: bitbeat::Reg) {
        self.variables.insert(var, reg);
    }

    fn var(&self, var: &Variable) -> Option<bitbeat::Reg> {
        self.variables.get(var).copied()
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, Default, Hash)]
struct RegisterAllocator {
    used: u32,
}

impl RegisterAllocator {
    pub fn alloc(&mut self) -> Option<bitbeat::Reg> {
        for i in 0..32 {
            if (self.used & (1 << i)) == 0 {
                self.used |= 1 << i;
                return Some(bitbeat::Reg(i));
            }
        }
        None
    }

    pub fn free(&mut self, reg: bitbeat::Reg) {
        self.used &= !(1 << reg.0);
    }

    pub fn is_used(&self, reg: bitbeat::Reg) -> bool {
        (self.used & (1 << reg.0)) != 0
    }
}

#[derive(Debug)]
pub struct BitBeat {
    reg_allocator: RegisterAllocator,
    module: bitbeat::Module,
    // symbol_table: &'table SymbolTable,
}

impl Default for BitBeat {
    fn default() -> Self {
        Self {
            reg_allocator: RegisterAllocator::default(),
            module: bitbeat::Module::new("program"),
        }
    }
}

impl BitBeat {
    fn emit_block(
        &mut self,
        blocks: &[ir::BasicBlock],
        assmbler: &mut bitbeat::InstructionBuilder,
        local_scope: &mut LocalScope,
    ) {
        for block in blocks {
            for instruction in block.instructions.iter() {
                if let Some(label) = &instruction.label {
                    assmbler.label(label);
                }
                self.emit_instruction(&instruction, assmbler, local_scope);
            }
        }
    }

    fn emit_instruction(
        &mut self,
        labeled_instruction: &ir::LabeledInstruction,
        assmbler: &mut bitbeat::InstructionBuilder,
        local_scope: &mut LocalScope,
    ) {
        let label = labeled_instruction.label.as_ref();
        match &labeled_instruction.instruction {
            ir::Instruction::NoOp => {
                assmbler.noop();
            }
            ir::Instruction::Add(des, lhs, rhs) => {
                self.emit_add(label, des, lhs, rhs, assmbler, local_scope)
            }
            ir::Instruction::Assign(var, operand) => {
                self.emit_assign(label, var, operand, assmbler, local_scope)
            }
            ir::Instruction::Call(return_var, caller_name, args) => {
                self.emit_call(label, return_var, caller_name, args, assmbler, local_scope)
            }
            ir::Instruction::Cmp(var, lhs, rhs) => {
                self.emit_cmp(label, var, lhs, rhs, assmbler, local_scope)
            }
            ir::Instruction::Gt(var, lhs, rhs) => {
                self.emit_gt(label, var, lhs, rhs, assmbler, local_scope)
            }
            ir::Instruction::Jump(jump_label) => {
                self.emit_jump(label, jump_label, assmbler, local_scope)
            }
            ir::Instruction::JumpIf(operand, jump_label) => {
                self.emit_jump_if(label, jump_label, operand, assmbler, local_scope)
            }
            ir::Instruction::Load(..) => todo!("Load"),
            ir::Instruction::Mul(..) => todo!("Mul"),
            ir::Instruction::Phi(..) => todo!("Phi"),
            ir::Instruction::Return(ty, operand) => {
                self.emit_return(label, ty, operand, assmbler, local_scope)
            }
            ir::Instruction::Sub(des, lhs, rhs) => {
                self.emit_sub(label, des, lhs, rhs, assmbler, local_scope)
            }
            ir::Instruction::Div(..) => todo!("Div"),
        };
    }

    fn emit_assign(
        &mut self,
        label: Option<&String>,
        var: &ir::Variable,
        operand: &ir::Operand,
        assmbler: &mut bitbeat::InstructionBuilder<'_>,
        local_scope: &mut LocalScope,
    ) {
        let reg = self.emit_operand(operand, assmbler, local_scope);
        let var = local_scope.set_var(var.clone(), reg);
    }

    fn emit_operand(
        &mut self,
        operand: &ir::Operand,
        assmbler: &mut bitbeat::InstructionBuilder<'_>,
        local_scope: &mut LocalScope,
    ) -> bitbeat::Reg {
        match operand {
            ir::Operand::ConstantInt { value, ty } => {
                // let ty = ir_type_to_bitbeat_ty(ty).unwrap();
                let reg = self.reg_allocator.alloc().unwrap();
                let number: i64 = value.parse().unwrap();
                assmbler.load_imm(reg, number);
                reg
            }
            ir::Operand::Variable(variable) => {
                let var_id = if let Some(reg) = local_scope.var(variable) {
                    reg
                } else {
                    let reg = self.reg_allocator.alloc().unwrap();
                    local_scope.set_var(variable.clone(), reg);
                    reg
                };
                var_id
            }
            ir::Operand::None => todo!("emit_operand, None"),
        }
    }

    fn emit_cmp(
        &mut self,
        label: Option<&String>,
        var: &Variable,
        lhs: &ir::Operand,
        rhs: &ir::Operand,
        assmbler: &mut bitbeat::InstructionBuilder<'_>,
        local_scope: &mut LocalScope,
    ) {
        let reg = if let Some(reg) = local_scope.var(var) {
            reg
        } else {
            let reg = self.reg_allocator.alloc().unwrap();
            local_scope.set_var(var.clone(), reg);
            reg
        };

        let lhs = self.emit_operand(lhs, assmbler, local_scope);
        let rhs = self.emit_operand(rhs, assmbler, local_scope);
        assmbler.cmp_eq(reg, lhs, rhs);
    }

    fn emit_gt(
        &mut self,
        label: Option<&String>,
        var: &Variable,
        lhs: &ir::Operand,
        rhs: &ir::Operand,
        assmbler: &mut bitbeat::InstructionBuilder<'_>,
        local_scope: &mut LocalScope,
    ) {
        todo!("emit_gt")
    }

    fn emit_jump_if(
        &mut self,
        label: Option<&String>,
        jump_label: &str,
        operand: &ir::Operand,
        assmbler: &mut bitbeat::InstructionBuilder<'_>,
        local_scope: &mut LocalScope,
    ) {
        let reg = self.emit_operand(operand, assmbler, local_scope);
        assmbler.jump_if(reg, jump_label);
    }

    fn emit_jump(
        &mut self,
        label: Option<&String>,
        jump_label: &str,
        assmbler: &mut bitbeat::InstructionBuilder<'_>,
        local_scope: &mut LocalScope,
    ) {
        assmbler.jump(jump_label);
    }

    fn emit_return(
        &mut self,
        label: Option<&String>,
        ty: &ir::Type,
        operand: &ir::Operand,
        assmbler: &mut bitbeat::InstructionBuilder<'_>,
        local_scope: &mut LocalScope,
    ) {
        let reg = self.emit_operand(operand, assmbler, local_scope);
        assmbler.send(bitbeat::Reg(0), reg);
    }

    fn emit_sub(
        &mut self,
        label: Option<&String>,
        des: &Variable,
        lhs: &ir::Operand,
        rhs: &ir::Operand,
        assmbler: &mut bitbeat::InstructionBuilder<'_>,
        local_scope: &mut LocalScope,
    ) {
        let lhs = self.emit_operand(lhs, assmbler, local_scope);
        let rhs = self.emit_operand(rhs, assmbler, local_scope);
        let reg = self.reg_allocator.alloc().unwrap();
        assmbler.sub(reg, lhs, rhs);
        local_scope.set_var(des.clone(), reg);
    }

    fn emit_add(
        &mut self,
        label: Option<&String>,
        des: &Variable,
        lhs: &ir::Operand,
        rhs: &ir::Operand,
        assmbler: &mut bitbeat::InstructionBuilder<'_>,
        local_scope: &mut LocalScope,
    ) {
        let lhs = self.emit_operand(lhs, assmbler, local_scope);
        let rhs = self.emit_operand(rhs, assmbler, local_scope);
        let reg = self.reg_allocator.alloc().unwrap();
        assmbler.add(reg, lhs, rhs);
        local_scope.set_var(des.clone(), reg);
    }

    fn emit_call(
        &mut self,
        label: Option<&String>,
        return_var: &Variable,
        caller_name: &str,
        args: &[Operand],
        assmbler: &mut bitbeat::InstructionBuilder<'_>,
        local_scope: &mut LocalScope,
    ) {
        let arg_reg = args
            .iter()
            .map(|arg| self.emit_operand(arg, assmbler, local_scope))
            .collect::<Vec<bitbeat::Reg>>();
        let return_reg = if let Some(reg) = local_scope.var(return_var) {
            reg
        } else {
            let r = self.reg_allocator.alloc().unwrap();
            local_scope.set_var(return_var.clone(), r);
            r
        };
        if caller_name == "print" {
            for reg in arg_reg {
                assmbler.print(reg);
            }
            return;
        }

        let pid_reg = self.reg_allocator.alloc().unwrap();
        // HACK: just hardcoding the module name for now
        assmbler.spawn("program", caller_name, arg_reg, pid_reg);
        if caller_name == "fib" {
            assmbler.recv(return_reg);
        }
    }
}

impl Emitter for BitBeat {
    fn to_bytes(&mut self) -> Vec<u8> {
        vec![]
    }
    // fn emit(&mut self, module: &Module) -> Option<Vec<u8>> {
    //     self.emit_imports(&module.imports);
    //     self.emit_constants(&module.constants);
    //     self.emit_functions(&module.functions);
    //     eprintln!("{:#?}", self.module);
    //     let mut vm = bitbeat::Machine::default();
    //     vm.register_module(self.module.clone());
    //     vm.spawn("program", "main", &[]);
    //     // vm.run();
    //     None
    // }

    fn emit_import(&mut self, _import: &Import) {
        todo!()
    }

    fn emit_constant(&mut self, _constant: &Constant) {
        todo!()
    }

    fn emit_function(&mut self, function: &Function) {
        let reg_allocator_used = self.reg_allocator.used;
        self.reg_allocator.used = 0;
        let mut func = bitbeat::Function::new(&function.name).arity(function.params.len());
        if function.return_type != crate::ir::Type::Void {
            func = func.returns();
            // Should be the first register
            self.reg_allocator.alloc().unwrap();
        }

        // TODO: move this to struct def and clone when passed to function
        let mut local_scope = LocalScope::default();
        {
            let mut assmbler = func.instructions();
            self.emit_block(&function.blocks, &mut assmbler, &mut local_scope);
        }

        self.module.add_function(func);
        self.reg_allocator.used = reg_allocator_used;
    }
}
