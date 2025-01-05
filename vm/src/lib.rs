#![allow(unused, dead_code)]
use std::collections::HashMap;

#[derive(Debug, Default, Clone)]
pub struct Process {
    module: String,
    function: String,
    pc: usize,
    memory: Vec<Value>,
    stack: Vec<Value>,
    args: Vec<Value>,
    version: u8,
}

#[derive(Debug)]
pub struct Module {
    name: String,
    functions: HashMap<String, Function>,
    version: u8,
}

impl Module {
    pub fn new(name: &str, version: u8) -> Self {
        Self {
            name: name.to_string(),
            functions: HashMap::new(),
            version,
        }
    }

    pub fn add_function(&mut self, function: Function) {
        let name = function.name.clone();
        self.functions.insert(name, function);
    }
}

#[derive(Debug)]
pub struct Function {
    name: String,
    locals: usize,
    instructions: Vec<Instruction>,
}

impl Function {
    pub fn new(name: impl Into<String>, instructions: Vec<Instruction>, locals: usize) -> Self {
        Function {
            name: name.into(),
            locals,
            instructions,
        }
    }

    pub fn execute(
        &self,
        process: &mut Process,
        modules: &HashMap<String, Vec<Module>>,
    ) -> Option<Value> {
        while process.pc < self.instructions.len() {
            let instruction = &self.instructions[process.pc];
            // eprintln!(
            //     "{:>2}: {:?} {:?} {:?}",
            //     process.pc, instruction.opcode, instruction.operand, process.stack
            // );
            process.pc += 1;
            match instruction.opcode {
                Opcode::Load => match &instruction.operand {
                    Operand::Immediate(value) => {
                        process.stack.push(value.clone());
                    }
                    Operand::Address(address) => {
                        todo!(
                            "grabbing from memory at address {} {:#?}",
                            address,
                            process.stack
                        );
                    }
                    Operand::Range(_, _) | Operand::None => {}
                },
                Opcode::LoadLocal => match &instruction.operand {
                    Operand::Address(address) => {
                        let Some(value) = process.args.get(*address) else {
                            panic!("no value at address {}", address);
                        };
                        process.stack.push(value.clone());
                    }
                    Operand::Range(..) | Operand::Immediate(..) | Operand::None => todo!(),
                },
                Opcode::Store => match &instruction.operand {
                    Operand::Address(address) => {
                        if let Some(value) = process.stack.pop() {
                            process.memory[*address] = value;
                        }
                    }
                    _ => {}
                },
                Opcode::Add => {
                    let Some(rhs) = process.stack.pop() else {
                        panic!("no value on stack");
                    };
                    let Some(lhs) = process.stack.pop() else {
                        panic!("no value on stack");
                    };
                    match (&lhs, &rhs) {
                        (Value::Integer(lhs), Value::Integer(rhs)) => {
                            process.stack.push(Value::Integer(lhs + rhs));
                        }
                        (Value::Float(lhs), Value::Float(rhs)) => {
                            process.stack.push(Value::Float(lhs + rhs));
                        }
                        _ => panic!("cannot add values '{:?}' and '{:?}'", lhs, rhs),
                    }
                }
                Opcode::Sub => {
                    let Some(rhs) = process.stack.pop() else {
                        panic!("no value on stack");
                    };
                    let Some(lhs) = process.stack.pop() else {
                        panic!("no value on stack");
                    };
                    match (&lhs, &rhs) {
                        (Value::Integer(lhs), Value::Integer(rhs)) => {
                            process.stack.push(Value::Integer(lhs - rhs));
                        }
                        (Value::Float(lhs), Value::Float(rhs)) => {
                            process.stack.push(Value::Float(lhs - rhs));
                        }
                        _ => panic!("cannot sub values '{:?}' and '{:?}'", lhs, rhs),
                    }
                }
                Opcode::Call => {
                    let Operand::Immediate(Value::Identifier(function_name)) = &instruction.operand
                    else {
                        panic!("not an identifier");
                    };

                    let Some((module, func_name)) = function_name.split_once(':') else {
                        panic!("malformed function name {}", function_name);
                    };

                    let Some(current_module) = modules.get(module) else {
                        panic!("Module {} not found", process.module);
                    };
                    let Some(func) = current_module
                        .iter()
                        .find_map(|module| module.functions.get(func_name))
                    else {
                        panic!(
                            "Function {} not found in module {}",
                            function_name, process.module
                        );
                    };

                    let arg_count = func.locals;
                    let end = process.stack.len();

                    let mut p = Process {
                        module: process.module.clone(),
                        function: func.name.clone(),
                        args: process.stack.drain(end - arg_count..).collect(),
                        ..Default::default()
                    };

                    let v = func.execute(&mut p, modules)?;
                    process.stack.push(v);
                }
                Opcode::Return => {
                    let Some(value) = process.stack.pop() else {
                        panic!("no value on stack");
                    };
                    return Some(value);
                }
                Opcode::BranchIf => {
                    let Some(value) = process.stack.pop() else {
                        panic!("no value on stack");
                    };
                    let Value::Boolean(value) = value else {
                        panic!("not a boolean value found '{:?}'", value);
                    };
                    if value {
                        let Operand::Address(address) = &instruction.operand else {
                            panic!("not an address");
                        };
                        process.pc = *address;
                    }
                }
                Opcode::Branch => {
                    let Operand::Address(address) = &instruction.operand else {
                        panic!("not an address");
                    };
                    process.pc = *address;
                }
                Opcode::Eq => {
                    let Some(right) = process.stack.pop() else {
                        panic!("no value on stack");
                    };
                    let Some(left) = process.stack.pop() else {
                        panic!("no value on stack");
                    };
                    process.stack.push(Value::Boolean(left == right));
                }
                Opcode::Or => {
                    let Some(right) = process.stack.pop() else {
                        panic!("no value on stack");
                    };
                    let Value::Boolean(right) = right else {
                        panic!("not a boolean value found '{:?}'", right);
                    };
                    let Some(left) = process.stack.pop() else {
                        panic!("no value on stack");
                    };
                    let Value::Boolean(left) = left else {
                        panic!("not a boolean value found '{:?}'", left);
                    };
                    process.stack.push(Value::Boolean(left || right));
                }
            }
        }

        None
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Opcode {
    Load,
    LoadLocal,
    Store,
    Add,
    Sub,
    Call,
    Return,
    BranchIf,
    Branch,
    Eq,
    Or,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    Integer(i32),
    Float(f64),
    String(String),
    Boolean(bool),
    Identifier(String),
}

impl Value {
    fn as_integer(&self) -> Option<i32> {
        match self {
            Value::Integer(i) => Some(*i),
            _ => None,
        }
    }
}

#[derive(Debug)]
pub enum Operand {
    Immediate(Value),
    Address(usize),
    Range(usize, usize),
    None,
}

impl Operand {
    fn as_immediate(&self) -> Option<Value> {
        match self {
            Operand::Immediate(value) => Some(value.clone()),
            _ => None,
        }
    }
}

#[derive(Debug)]
pub struct Instruction {
    pub opcode: Opcode,
    pub operand: Operand,
}

#[derive(Debug, Default)]
pub struct VM {
    modules: HashMap<String, Vec<Module>>,
    processes: Vec<Process>,
}

impl VM {
    pub fn load_module(&mut self, module: Module) {
        self.modules
            .entry(module.name.clone())
            .or_default()
            .push(module);
    }

    pub fn start_process(&mut self, module: impl Into<String>, function: impl Into<String>) {
        self.processes.push(Process {
            module: module.into(),
            function: function.into(),
            ..Default::default()
        });
    }

    pub fn run(&mut self) {
        let current_modules: HashMap<_, _> = self
            .modules
            .iter()
            .map(|(name, versions)| (name.clone(), versions.last().unwrap()))
            .collect();

        for process in &mut self.processes {
            let Some(current_module) = current_modules.get(&process.module) else {
                panic!("Module {} not found", process.module);
            };
            if process.version != current_module.version {
                println!(
                    "Process using old version {} of module {}; updating to version {}.",
                    process.version, process.module, current_module.version
                );
                process.version = current_module.version;
                process.pc = 0;
            }

            let Some(function) = current_module.functions.get(&process.function) else {
                panic!(
                    "Function {} not found in module {}",
                    process.function, process.module
                );
            };

            let v = function.execute(process, &self.modules);
            println!("Result: {:?}", v);
        }
    }
}

#[macro_export]
macro_rules! instruction {
    ($opcode:expr) => {
        Instruction {
            opcode: $opcode,
            operand: Operand::None,
        }
    };
    ($opcode:expr, $operand:expr) => {
        Instruction {
            opcode: $opcode,
            operand: $operand,
        }
    };
}
