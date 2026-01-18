#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Reg64 {
    Rax,
    Rbx,
    Rcx,
    Rdx,
    Rsi,
    Rdi,
    Rbp,
    Rsp,
    R8,
    R9,
    R10,
    R11,
    R12,
    R13,
    R14,
    R15,
}

impl std::fmt::Display for Reg64 {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let reg = match self {
            Reg64::Rax => "rax",
            Reg64::Rbx => "rbx",
            Reg64::Rcx => "rcx",
            Reg64::Rdx => "rdx",
            Reg64::Rsi => "rsi",
            Reg64::Rdi => "rdi",
            Reg64::Rbp => "rbp",
            Reg64::Rsp => "rsp",
            Reg64::R8 => "r8",
            Reg64::R9 => "r9",
            Reg64::R10 => "r10",
            Reg64::R11 => "r11",
            Reg64::R12 => "r12",
            Reg64::R13 => "r13",
            Reg64::R14 => "r14",
            Reg64::R15 => "r15",
        };
        write!(f, "{}", reg)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Location {
    Reg(Reg64),
    Mem(i32),
    Stack(i32),
    Imm(i64),
}

impl From<Reg64> for Location {
    fn from(value: Reg64) -> Self {
        Location::Reg(value)
    }
}

impl From<i64> for Location {
    fn from(value: i64) -> Self {
        Location::Imm(value)
    }
}

impl std::fmt::Display for Location {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Location::Reg(reg) => write!(f, "{}", reg),
            Location::Mem(off) => write!(f, "[rbp{}]", off),
            Location::Stack(off) => write!(f, "[rbp{}]", off),
            Location::Imm(val) => write!(f, "{}", val),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Instruction {
    Mov(Location, Location),
    Pop(Location),
    Push(Location),
    Label(String),
    Ret,
}

impl std::fmt::Display for Instruction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Mov(dst, src) => write!(f, "  mov {}, {}", dst, src),
            Self::Pop(dst) => write!(f, "  pop {}", dst),
            Self::Push(src) => write!(f, "  push {}", src),
            Self::Label(label) => write!(f, "{}:", label),
            Self::Ret => write!(f, "  ret"),
        }
    }
}

pub struct Assembler {
    instructions: Vec<Instruction>,
    free_registers: Vec<Reg64>,
    used_registers: Vec<Reg64>,
}

impl std::fmt::Display for Assembler {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for i in &self.instructions {
            writeln!(f, "{}", i)?;
        }
        Ok(())
    }
}

impl Default for Assembler {
    fn default() -> Self {
        Self {
            instructions: Vec::new(),
            free_registers: vec![
                Reg64::Rax,
                Reg64::Rbx,
                Reg64::Rcx,
                Reg64::Rdx,
                Reg64::Rsi,
                Reg64::Rdi,
                Reg64::R8,
                Reg64::R9,
                Reg64::R10,
                Reg64::R11,
            ],
            used_registers: Vec::new(),
        }
    }
}

impl Assembler {
    pub fn alloc_reg(&mut self) -> Reg64 {
        let reg = self.free_registers.pop().expect("Out of registers");
        self.used_registers.push(reg);
        reg
    }

    pub fn free_reg(&mut self, reg: Reg64) {
        debug_assert!(
            self.used_registers.contains(&reg),
            "Double free or freeing unused register: {:?}",
            reg
        );

        self.used_registers.retain(|&r| r != reg);
        self.free_registers.push(reg);
    }

    pub fn label(&mut self, name: &str) -> &mut Self {
        self.instructions.push(Instruction::Label(name.to_string()));
        self
    }

    pub fn ret(&mut self) -> &mut Self {
        self.instructions.push(Instruction::Ret);
        self
    }
}

// Instruction API
impl Assembler {
    pub fn mov(&mut self, lhs: impl Into<Location>, rhs: impl Into<Location>) -> &mut Self {
        let lhs = lhs.into();
        debug_assert!(!matches!(lhs, Location::Imm(_)));
        self.instructions.push(Instruction::Mov(lhs, rhs.into()));
        self
    }

    pub fn push(&mut self, src: impl Into<Location>) -> &mut Self {
        let src = src.into();
        debug_assert!(!matches!(src, Location::Imm(_)));
        self.instructions.push(Instruction::Push(src));
        self
    }

    pub fn pop(&mut self, dst: impl Into<Location>) -> &mut Self {
        let dst = dst.into();
        debug_assert!(!matches!(dst, Location::Imm(_)));
        self.instructions.push(Instruction::Pop(dst));
        self
    }
}
