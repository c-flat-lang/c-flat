#![allow(unused, dead_code)]
use std::collections::HashMap;

pub type Result<T> = std::result::Result<T, Box<Error>>;
macro_rules! bail {
    ($e:expr) => {
        return Err(Box::new($e));
    };
}

#[derive(Debug, Clone)]
pub enum Error {
    NoSuchFunction {
        function: String,
        module: String,
    },
    NoSuchModule {
        module: String,
    },
    MalformedFunctionName {
        name: String,
    },
    ExpectedOperand {
        expected: Operand,
        found: Operand,
    },
    StackUnderflow {
        module: String,
        function: String,
        pc: usize,
    },
    TypeMismatch {
        expected: String,
        found: Value,
    },
    InvalidProgramAddress {
        address: usize,
        module: String,
        function: String,
    },
    InvalidOperationArgument {
        module: String,
        function: String,
        pc: usize,
    },
    InvalidOperationResult {
        module: String,
        function: String,
        pc: usize,
        args: Vec<Value>,
        expected: String,
    },
}

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
    ) -> Result<Option<Value>> {
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
                            bail!(Error::InvalidProgramAddress {
                                address: *address,
                                module: process.module.clone(),
                                function: process.function.clone(),
                            });
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
                        bail!(Error::StackUnderflow {
                            module: process.module.clone(),
                            function: process.function.clone(),
                            pc: process.pc,
                        });
                    };
                    let Some(lhs) = process.stack.pop() else {
                        bail!(Error::StackUnderflow {
                            module: process.module.clone(),
                            function: process.function.clone(),
                            pc: process.pc,
                        });
                    };
                    match (&lhs, &rhs) {
                        (Value::Integer(lhs), Value::Integer(rhs)) => {
                            process.stack.push(Value::Integer(lhs + rhs));
                        }
                        (Value::Float(lhs), Value::Float(rhs)) => {
                            process.stack.push(Value::Float(lhs + rhs));
                        }
                        (l, r) => {
                            bail!(Error::InvalidOperationResult {
                                module: process.module.clone(),
                                function: process.function.clone(),
                                pc: process.pc,
                                args: vec![l.clone(), r.clone()],
                                expected: "Integer or Float".to_string(),
                            });
                        }
                    }
                }
                Opcode::Sub => {
                    let Some(rhs) = process.stack.pop() else {
                        bail!(Error::StackUnderflow {
                            module: process.module.clone(),
                            function: process.function.clone(),
                            pc: process.pc,
                        });
                    };
                    let Some(lhs) = process.stack.pop() else {
                        bail!(Error::StackUnderflow {
                            module: process.module.clone(),
                            function: process.function.clone(),
                            pc: process.pc,
                        });
                    };
                    match (&lhs, &rhs) {
                        (Value::Integer(lhs), Value::Integer(rhs)) => {
                            process.stack.push(Value::Integer(lhs - rhs));
                        }
                        (Value::Float(lhs), Value::Float(rhs)) => {
                            process.stack.push(Value::Float(lhs - rhs));
                        }
                        (l, r) => {
                            bail!(Error::InvalidOperationResult {
                                module: process.module.clone(),
                                function: process.function.clone(),
                                pc: process.pc,
                                args: vec![l.clone(), r.clone()],
                                expected: "Integer or Float".to_string(),
                            });
                        }
                    }
                }
                Opcode::Call => {
                    let function_name = match &instruction.operand {
                        Operand::Immediate(Value::Identifier(function_name)) => function_name,
                        operand => {
                            bail!(Error::ExpectedOperand {
                                expected: Operand::Immediate(Value::Identifier(
                                    "identifier".to_string()
                                )),
                                found: operand.clone(),
                            });
                        }
                    };

                    let Some((module, func_name)) = function_name.split_once(':') else {
                        bail!(Error::MalformedFunctionName {
                            name: function_name.to_string(),
                        });
                    };

                    let Some(current_module) = modules.get(module) else {
                        bail!(Error::NoSuchModule {
                            module: module.to_string(),
                        });
                    };
                    let Some(func) = current_module
                        .iter()
                        .find_map(|module| module.functions.get(func_name))
                    else {
                        bail!(Error::NoSuchFunction {
                            module: module.to_string(),
                            function: func_name.to_string(),
                        });
                    };

                    let arg_count = func.locals;
                    let end = process.stack.len();

                    let mut p = Process {
                        module: process.module.clone(),
                        function: func.name.clone(),
                        args: process.stack.drain(end - arg_count..).collect(),
                        ..Default::default()
                    };

                    let Some(v) = func.execute(&mut p, modules)? else {
                        continue;
                    };
                    process.stack.push(v);
                }
                Opcode::Return => {
                    let Some(value) = process.stack.pop() else {
                        bail!(Error::StackUnderflow {
                            module: process.module.clone(),
                            function: process.function.clone(),
                            pc: process.pc,
                        });
                    };
                    return Ok(Some(value));
                }
                Opcode::BranchIf => {
                    let Some(value) = process.stack.pop() else {
                        bail!(Error::StackUnderflow {
                            module: process.module.clone(),
                            function: process.function.clone(),
                            pc: process.pc,
                        });
                    };
                    let Value::Boolean(value) = value else {
                        bail!(Error::TypeMismatch {
                            expected: "Boolean".to_string(),
                            found: value,
                        });
                    };
                    if value {
                        let Operand::Address(address) = &instruction.operand else {
                            bail!(Error::ExpectedOperand {
                                expected: Operand::Address(0),
                                found: instruction.operand.clone(),
                            });
                        };
                        process.pc = *address;
                    }
                }
                Opcode::Branch => {
                    let Operand::Address(address) = &instruction.operand else {
                        bail!(Error::ExpectedOperand {
                            expected: Operand::Address(0),
                            found: instruction.operand.clone(),
                        });
                    };
                    process.pc = *address;
                }
                Opcode::Eq => {
                    let Some(right) = process.stack.pop() else {
                        bail!(Error::StackUnderflow {
                            module: process.module.clone(),
                            function: process.function.clone(),
                            pc: process.pc,
                        });
                    };
                    let Some(left) = process.stack.pop() else {
                        bail!(Error::StackUnderflow {
                            module: process.module.clone(),
                            function: process.function.clone(),
                            pc: process.pc,
                        });
                    };
                    process.stack.push(Value::Boolean(left == right));
                }
                Opcode::Or => {
                    let Some(right) = process.stack.pop() else {
                        bail!(Error::StackUnderflow {
                            module: process.module.clone(),
                            function: process.function.clone(),
                            pc: process.pc,
                        });
                    };
                    let Value::Boolean(right) = right else {
                        bail!(Error::TypeMismatch {
                            expected: "Boolean".to_string(),
                            found: right,
                        });
                    };
                    let Some(left) = process.stack.pop() else {
                        bail!(Error::StackUnderflow {
                            module: process.module.clone(),
                            function: process.function.clone(),
                            pc: process.pc,
                        });
                    };
                    let Value::Boolean(left) = left else {
                        bail!(Error::TypeMismatch {
                            expected: "Boolean".to_string(),
                            found: left,
                        });
                    };
                    process.stack.push(Value::Boolean(left || right));
                }
            }
        }

        Ok(None)
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

#[derive(Debug, Clone)]
pub enum Operand {
    Immediate(Value),
    Address(usize),
    Range(usize, usize),
    None,
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

    pub fn run(&mut self) -> Result<()> {
        let current_modules: HashMap<_, _> = self
            .modules
            .iter()
            .map(|(name, versions)| (name.clone(), versions.last().unwrap()))
            .collect();

        for process in &mut self.processes {
            let Some(current_module) = current_modules.get(&process.module) else {
                bail!(Error::NoSuchModule {
                    module: process.module.clone(),
                });
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
                bail!(Error::NoSuchFunction {
                    module: process.module.clone(),
                    function: process.function.clone(),
                });
            };

            let v = function.execute(process, &self.modules)?;
            println!("Result: {:?}", v);
        }
        Ok(())
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
