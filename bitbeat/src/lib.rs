#![allow(unused, dead_code)]
mod instruction;

use std::collections::HashMap;

pub type Result<T> = std::result::Result<T, Box<Error>>;

#[macro_export]
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

type InstructionFn =
    fn(&Operand, &mut Process, &HashMap<String, Vec<Module>>) -> Result<Option<Value>>;

const EXE_INSTRUCTIONS: [InstructionFn; 12] = [
    instruction::opcode_load,
    instruction::opcode_load_local,
    instruction::opcode_store,
    instruction::opcode_store_local,
    instruction::opcode_add,
    instruction::opcode_sub,
    instruction::opcode_call,
    instruction::opcode_return,
    instruction::opcode_branch_if,
    instruction::opcode_branch,
    instruction::opcode_eq,
    instruction::opcode_or,
];

#[derive(Debug, Default)]
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
            let Some(v) = EXE_INSTRUCTIONS[instruction.opcode as usize](
                &instruction.operand,
                process,
                modules,
            )?
            else {
                continue;
            };

            return Ok(Some(v));
        }

        Ok(None)
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Opcode {
    Load,
    LoadLocal,
    Store,
    StoreLocal,
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
    Identifier(Box<String>),
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
