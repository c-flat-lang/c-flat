pub mod decoder;
use std::cell::RefCell;
use std::collections::{BTreeMap, VecDeque};
use std::rc::Rc;
use std::sync::Arc;

pub type Word = i64;
pub type Pid = usize;

pub const REG_COUNT: usize = 32;

#[derive(Debug, Copy, Clone)]
pub struct Reg(pub usize);

#[derive(Debug, Clone, Copy)]
pub struct Address(pub usize);

#[derive(Clone, Debug)]
pub enum Instruction {
    Noop,
    Halt,
    LoadImm {
        dst: Reg,
        value: Word,
    },
    Add {
        dst: Reg,
        lhs: Reg,
        rhs: Reg,
    },
    Sub {
        dst: Reg,
        lhs: Reg,
        rhs: Reg,
    },
    Mul {
        dst: Reg,
        lhs: Reg,
        rhs: Reg,
    },
    CmpEq {
        dst: Reg,
        lhs: Reg,
        rhs: Reg,
    },
    CmpLE {
        dst: Reg,
        lhs: Reg,
        rhs: Reg,
    },
    Mov {
        dst: Reg,
        src: Reg,
    },
    JumpIf {
        cmp: Reg,
        target: Address,
    },
    Jump {
        target: Address,
    },
    Spawn {
        module: Box<String>,
        function: Box<String>,
        args: Vec<Reg>,
        dst: Reg,
    },
    Print {
        reg: Reg,
    },
    Send {
        dst_pid: Reg,
        src_reg: Reg,
    },
    Recv {
        dst_reg: Reg,
    },
}

pub struct InstructionBuilder<'a> {
    instructions: &'a mut Vec<Instruction>,
    labels: std::collections::BTreeMap<String, usize>,
    backpatch: Vec<(usize, String)>,
}

impl Drop for InstructionBuilder<'_> {
    fn drop(&mut self) {
        for (i, target) in self.backpatch.iter() {
            let Some(index) = self.labels.get(target) else {
                panic!("label {target} not found");
            };
            self.instructions[*i] = match self.instructions[*i] {
                Instruction::JumpIf { cmp, .. } => Instruction::JumpIf {
                    cmp,
                    target: Address(*index),
                },
                Instruction::Jump { .. } => Instruction::Jump {
                    target: Address(*index),
                },
                _ => unreachable!(),
            };
        }
    }
}

impl<'a> InstructionBuilder<'a> {
    pub fn new(instructions: &'a mut Vec<Instruction>) -> Self {
        Self {
            instructions,
            labels: BTreeMap::new(),
            backpatch: Vec::new(),
        }
    }

    pub fn noop(&mut self) -> &mut Self {
        self.instructions.push(Instruction::Noop);
        self
    }

    pub fn load_imm(&mut self, dst: Reg, value: Word) -> &mut Self {
        self.instructions.push(Instruction::LoadImm { dst, value });
        self
    }

    pub fn add(&mut self, dst: Reg, lhs: Reg, rhs: Reg) -> &mut Self {
        self.instructions.push(Instruction::Add { dst, lhs, rhs });
        self
    }

    pub fn sub(&mut self, dst: Reg, lhs: Reg, rhs: Reg) -> &mut Self {
        self.instructions.push(Instruction::Sub { dst, lhs, rhs });
        self
    }

    pub fn mul(&mut self, dst: Reg, lhs: Reg, rhs: Reg) -> &mut Self {
        self.instructions.push(Instruction::Mul { dst, lhs, rhs });
        self
    }

    pub fn cmp_eq(&mut self, dst: Reg, lhs: Reg, rhs: Reg) -> &mut Self {
        self.instructions.push(Instruction::CmpEq { dst, lhs, rhs });
        self
    }

    pub fn cmp_le(&mut self, dst: Reg, lhs: Reg, rhs: Reg) -> &mut Self {
        self.instructions.push(Instruction::CmpLE { dst, lhs, rhs });
        self
    }

    pub fn mov(&mut self, dst: Reg, src: Reg) -> &mut Self {
        self.instructions.push(Instruction::Mov { dst, src });
        self
    }

    pub fn jump_if(&mut self, cmp: Reg, target: impl Into<String>) -> &mut Self {
        let target = target.into();
        self.backpatch.push((self.instructions.len(), target));
        self.instructions.push(Instruction::JumpIf {
            cmp,
            target: Address(usize::MAX),
        });
        self
    }

    pub fn jump(&mut self, target: impl Into<String>) -> &mut Self {
        let target = target.into();
        self.backpatch.push((self.instructions.len(), target));
        self.instructions.push(Instruction::Jump {
            target: Address(usize::MAX),
        });
        self
    }

    pub fn spawn(
        &mut self,
        module: impl Into<String>,
        function: impl Into<String>,
        args: Vec<Reg>,
        dst: Reg,
    ) -> &mut Self {
        self.instructions.push(Instruction::Spawn {
            module: Box::new(module.into()),
            function: Box::new(function.into()),
            args,
            dst,
        });
        self
    }

    pub fn print(&mut self, reg: Reg) -> &mut Self {
        self.instructions.push(Instruction::Print { reg });
        self
    }

    pub fn send(&mut self, dst_pid: Reg, src_reg: Reg) -> &mut Self {
        self.instructions
            .push(Instruction::Send { dst_pid, src_reg });
        self
    }

    pub fn recv(&mut self, dst_reg: Reg) -> &mut Self {
        self.instructions.push(Instruction::Recv { dst_reg });
        self
    }

    pub fn halt(&mut self) -> &mut Self {
        self.instructions.push(Instruction::Halt);
        self
    }

    pub fn label(&mut self, name: impl Into<String>) -> &mut Self {
        self.labels.insert(name.into(), self.instructions.len());
        self
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum ProcessState {
    Running,
    Halted,
    Waiting,
    Crashed(&'static str),
}

#[derive(Debug, Clone, Default)]
pub struct Module {
    name: String,
    functions: BTreeMap<String, Arc<Function>>,
}

impl Module {
    pub fn new(name: &str) -> Self {
        Self {
            name: name.to_string(),
            functions: BTreeMap::new(),
        }
    }

    #[inline(always)]
    pub fn add_function(&mut self, function: Function) {
        let name = function.name.clone();
        self.functions.insert(name, Arc::new(function));
    }

    #[inline(always)]
    pub fn get_function(&self, name: &str) -> Option<Arc<Function>> {
        self.functions.get(name).cloned()
    }
}

#[derive(Debug, Clone)]
pub struct Function {
    name: String,
    arity: usize,
    code: Vec<Instruction>,
    returns: bool,
}

impl Function {
    pub fn new(name: impl Into<String>) -> Self {
        Self {
            name: name.into(),
            arity: 0,
            code: Vec::new(),
            returns: false,
        }
    }

    /// Marks the function as returning
    /// Return PID is stored in the first register
    pub fn returns(mut self) -> Self {
        self.returns = true;
        self
    }

    pub fn arity(mut self, arity: usize) -> Self {
        self.arity = arity;
        self
    }

    pub fn instructions(&mut self) -> InstructionBuilder<'_> {
        InstructionBuilder::new(&mut self.code)
    }
}

#[derive(Debug)]
struct Process {
    pid: Pid,
    regs: [Word; REG_COUNT],
    ip: usize,
    function: Arc<Function>,
    mailbox: VecDeque<Word>,
    state: ProcessState,
}

impl Process {
    pub fn new(pid: Pid, function: Arc<Function>, args: &[Word]) -> Self {
        let mut regs = [0; REG_COUNT];
        for (i, arg) in args.iter().enumerate() {
            regs[i] = *arg;
        }

        Self {
            pid,
            regs,
            ip: 0,
            function,
            mailbox: VecDeque::new(),
            state: ProcessState::Running,
        }
    }

    pub fn reset(&mut self, pid: Pid, function: Arc<Function>, args: &[Word]) {
        self.regs[..].fill(0);
        for (i, arg) in args.iter().enumerate() {
            self.regs[i] = *arg;
        }

        self.pid = pid;
        self.ip = 0;
        self.function = function;
        self.mailbox.clear();
        self.state = ProcessState::Running;
    }

    fn step(
        &mut self,
        process_table: &mut BTreeMap<Pid, Rc<RefCell<Process>>>,
        halted: &mut Vec<Rc<RefCell<Process>>>,
        run_queue: &mut VecDeque<Rc<RefCell<Process>>>,
        modules: &BTreeMap<String, Module>,
        next_pid: &mut usize,
    ) {
        if self.ip >= self.function.code.len()
            || matches!(self.state, ProcessState::Halted | ProcessState::Crashed(..))
        {
            self.state = ProcessState::Halted;
            return;
        }

        let ip = self.ip;
        self.ip += 1;

        match &self.function.code[ip] {
            Instruction::Noop => {}
            Instruction::Halt => self.state = ProcessState::Halted,
            Instruction::LoadImm { dst, value } => self.regs[dst.0] = *value,
            Instruction::Add { dst, lhs, rhs } => self.handle_add(*dst, *lhs, *rhs),
            Instruction::Sub { dst, lhs, rhs } => self.handle_sub(*dst, *lhs, *rhs),
            Instruction::Mul { dst, lhs, rhs } => self.handle_mul(*dst, *lhs, *rhs),
            Instruction::CmpEq { dst, lhs, rhs } => self.handle_cmp_eq(*dst, *lhs, *rhs),
            Instruction::CmpLE { dst, lhs, rhs } => self.handle_cmp_le(*dst, *lhs, *rhs),
            Instruction::Mov { dst, src } => self.handle_mov(*dst, *src),
            Instruction::JumpIf { cmp, target } => self.handle_jump_if(*cmp, *target),
            Instruction::Jump { target } => self.handle_jump(*target),
            Instruction::Spawn {
                module,
                function,
                args,
                dst,
            } => {
                let Some(module) = modules.get(module.as_str()) else {
                    self.state =
                        ProcessState::Crashed("failed to spawn process due to missing module");
                    return;
                };
                let Some(function) = module.get_function(function) else {
                    self.state =
                        ProcessState::Crashed("failed to spawn process due to missing function");
                    return;
                };

                let new_pid = *next_pid;
                *next_pid += 1;
                let mut args: Vec<Word> = args.iter().map(|&arg| self.regs[arg.0]).collect();
                if function.returns {
                    args.insert(0, self.pid as Word);
                }
                let new_proc = if let Some(proc_rc) = halted.pop() {
                    proc_rc
                        .borrow_mut()
                        .reset(new_pid, Arc::clone(&function), &args);
                    proc_rc
                } else {
                    Rc::new(RefCell::new(Process::new(
                        new_pid,
                        Arc::clone(&function),
                        &args,
                    )))
                };
                self.regs[dst.0] = new_pid as i64;

                process_table.insert(new_pid, Rc::clone(&new_proc));
                run_queue.push_back(new_proc);
            }
            Instruction::Print { reg } => self.handle_print(*reg),
            Instruction::Send { dst_pid, src_reg } => {
                self.handle_send(process_table, run_queue, *dst_pid, *src_reg)
            }
            Instruction::Recv { dst_reg } => self.handle_recv(*dst_reg),
        }
    }

    fn handle_add(&mut self, dst: Reg, lhs: Reg, rhs: Reg) {
        self.regs[dst.0] = self.regs[lhs.0] + self.regs[rhs.0];
    }

    fn handle_sub(&mut self, dst: Reg, lhs: Reg, rhs: Reg) {
        self.regs[dst.0] = self.regs[lhs.0] - self.regs[rhs.0];
    }

    fn handle_mul(&mut self, dst: Reg, lhs: Reg, rhs: Reg) {
        self.regs[dst.0] = self.regs[lhs.0] * self.regs[rhs.0];
    }

    fn handle_cmp_eq(&mut self, dst: Reg, lhs: Reg, rhs: Reg) {
        self.regs[dst.0] = (self.regs[lhs.0] == self.regs[rhs.0]) as i64;
    }

    fn handle_cmp_le(&mut self, dst: Reg, lhs: Reg, rhs: Reg) {
        self.regs[dst.0] = (self.regs[lhs.0] <= self.regs[rhs.0]) as i64;
    }

    fn handle_mov(&mut self, dst: Reg, src: Reg) {
        self.regs[dst.0] = self.regs[src.0];
    }

    fn handle_jump_if(&mut self, cmp: Reg, target: Address) {
        if self.regs[cmp.0] == 0 {
            return;
        }
        self.ip = target.0;
    }

    fn handle_jump(&mut self, target: Address) {
        self.ip = target.0;
    }

    fn handle_print(&mut self, reg: Reg) {
        println!("{}", self.regs[reg.0]);
    }

    fn handle_send(
        &mut self,
        process_table: &mut BTreeMap<Pid, Rc<RefCell<Process>>>,
        run_queue: &mut VecDeque<Rc<RefCell<Process>>>,
        dst_pid: Reg,
        src_reg: Reg,
    ) {
        let pid = self.regs[dst_pid.0] as Pid;
        let Some(dst_proc) = process_table.get(&pid) else {
            self.state =
                ProcessState::Crashed("failed to SEND data to PID due to failed to find it");
            return;
        };
        let mut p = dst_proc.borrow_mut();
        p.mailbox.push_back(self.regs[src_reg.0]);
        run_queue.push_back(Rc::clone(dst_proc));
    }

    fn handle_recv(&mut self, dst_reg: Reg) {
        let Some(msg) = self.mailbox.pop_front() else {
            self.state = ProcessState::Waiting;
            self.ip -= 1;
            return;
        };

        self.regs[dst_reg.0] = msg;
        self.state = ProcessState::Running;
    }
}

#[derive(Debug, Default)]
pub struct Machine {
    next_pid: usize,
    modules: BTreeMap<String, Module>,
    processes: BTreeMap<Pid, Rc<RefCell<Process>>>,
    halted: Vec<Rc<RefCell<Process>>>,
    run_queue: VecDeque<Rc<RefCell<Process>>>,
}

impl Machine {
    pub fn register_module(&mut self, module: Module) -> &mut Self {
        self.modules.insert(module.name.clone(), module);
        self
    }

    pub fn spawn(&mut self, module_name: &str, function_name: &str, args: &[Word]) -> Option<Pid> {
        let module = self.modules.get(module_name)?;
        let function = module.get_function(function_name)?;
        if function.arity != args.len() {
            eprintln!("Incorrect arity for function `{function_name}`");
            return None;
        }

        let pid = self.next_pid;
        self.next_pid += 1;

        let process = Rc::new(RefCell::new(Process::new(pid, function, args)));

        self.processes.insert(pid, Rc::clone(&process));
        self.run_queue.push_back(process);
        Some(pid)
    }

    pub fn step(&mut self) {
        let Some(proc_rc) = self.run_queue.pop_front() else {
            return;
        };

        let state = {
            let mut proc = proc_rc.borrow_mut();

            proc.step(
                &mut self.processes,
                &mut self.halted,
                &mut self.run_queue,
                &self.modules,
                &mut self.next_pid,
            );

            proc.state
        };

        if state == ProcessState::Running {
            self.run_queue.push_back(proc_rc);
        }
    }

    pub fn send(&mut self, pid: Pid, msg: Word) {
        if let Some(proc_rc) = self.processes.get_mut(&pid) {
            let mut proc = proc_rc.borrow_mut();
            proc.mailbox.push_back(msg);

            if proc.state == ProcessState::Waiting {
                proc.state = ProcessState::Running;
                self.run_queue.push_back(Rc::clone(proc_rc));
            }
        }
    }

    pub fn run(&mut self) {
        while let Some(proc_rc) = self.run_queue.pop_front() {
            let mut proc = proc_rc.borrow_mut();

            loop {
                proc.step(
                    &mut self.processes,
                    &mut self.halted,
                    &mut self.run_queue,
                    &self.modules,
                    &mut self.next_pid,
                );

                if proc.state != ProcessState::Running {
                    break;
                }
            }

            match proc.state {
                ProcessState::Running => {
                    self.run_queue.push_back(Rc::clone(&proc_rc));
                }
                ProcessState::Halted => {
                    if let Some(proc) = self.processes.remove(&proc.pid) {
                        if self.halted.len() < 10 {
                            self.halted.push(proc);
                        }
                    }
                }
                ProcessState::Crashed(msg) => println!("[PID {}] Crashed:\n{msg}", proc.pid),
                _ => {}
            }
        }
    }
}
