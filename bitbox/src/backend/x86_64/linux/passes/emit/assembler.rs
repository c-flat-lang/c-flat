#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum PhysReg {
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

impl From<PhysReg> for Reg64 {
    fn from(value: PhysReg) -> Self {
        match value {
            PhysReg::Rax => Self::Rax,
            PhysReg::Rbx => Self::Rbx,
            PhysReg::Rcx => Self::Rcx,
            PhysReg::Rdx => Self::Rdx,
            PhysReg::Rsi => Self::Rsi,
            PhysReg::Rdi => Self::Rdi,
            PhysReg::Rbp => Self::Rbp,
            PhysReg::Rsp => Self::Rsp,
            PhysReg::R8 => Self::R8,
            PhysReg::R9 => Self::R9,
            PhysReg::R10 => Self::R10,
            PhysReg::R11 => Self::R11,
            PhysReg::R12 => Self::R12,
            PhysReg::R13 => Self::R13,
            PhysReg::R14 => Self::R14,
            PhysReg::R15 => Self::R15,
        }
    }
}

impl std::fmt::Display for Reg64 {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let reg = match self {
            Self::Rax => "rax",
            Self::Rbx => "rbx",
            Self::Rcx => "rcx",
            Self::Rdx => "rdx",
            Self::Rsi => "rsi",
            Self::Rdi => "rdi",
            Self::Rbp => "rbp",
            Self::Rsp => "rsp",
            Self::R8 => "r8",
            Self::R9 => "r9",
            Self::R10 => "r10",
            Self::R11 => "r11",
            Self::R12 => "r12",
            Self::R13 => "r13",
            Self::R14 => "r14",
            Self::R15 => "r15",
        };
        write!(f, "{}", reg)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Reg32 {
    Eax,
    Ebx,
    Ecx,
    Edx,
    Esi,
    Edi,
    Ebp,
    Esp,
    R8d,
    R9d,
    R10d,
    R11d,
    R12d,
    R13d,
    R14d,
    R15d,
}

impl From<PhysReg> for Reg32 {
    fn from(value: PhysReg) -> Self {
        match value {
            PhysReg::Rax => Self::Eax,
            PhysReg::Rbx => Self::Ebx,
            PhysReg::Rcx => Self::Ecx,
            PhysReg::Rdx => Self::Edx,
            PhysReg::Rsi => Self::Esi,
            PhysReg::Rdi => Self::Edi,
            PhysReg::Rbp => Self::Ebp,
            PhysReg::Rsp => Self::Esp,
            PhysReg::R8 => Self::R8d,
            PhysReg::R9 => Self::R9d,
            PhysReg::R10 => Self::R10d,
            PhysReg::R11 => Self::R11d,
            PhysReg::R12 => Self::R12d,
            PhysReg::R13 => Self::R13d,
            PhysReg::R14 => Self::R14d,
            PhysReg::R15 => Self::R15d,
        }
    }
}

impl std::fmt::Display for Reg32 {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let reg = match self {
            Self::Eax => "eax",
            Self::Ebx => "ebx",
            Self::Ecx => "ecx",
            Self::Edx => "edx",
            Self::Esi => "esi",
            Self::Edi => "edi",
            Self::Ebp => "ebp",
            Self::Esp => "esp",
            Self::R8d => "r8d",
            Self::R9d => "r9d",
            Self::R10d => "r10d",
            Self::R11d => "r11d",
            Self::R12d => "r12d",
            Self::R13d => "r13d",
            Self::R14d => "r14d",
            Self::R15d => "r15d",
        };
        write!(f, "{}", reg)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Reg16 {
    Ax,
    Bx,
    Cx,
    Dx,
    Si,
    Di,
    Bp,
    Sp,
    R8w,
    R9w,
    R10w,
    R11w,
    R12w,
    R13w,
    R14w,
    R15w,
}

impl From<PhysReg> for Reg16 {
    fn from(value: PhysReg) -> Self {
        match value {
            PhysReg::Rax => Self::Ax,
            PhysReg::Rbx => Self::Bx,
            PhysReg::Rcx => Self::Cx,
            PhysReg::Rdx => Self::Dx,
            PhysReg::Rsi => Self::Si,
            PhysReg::Rdi => Self::Di,
            PhysReg::Rbp => Self::Bp,
            PhysReg::Rsp => Self::Sp,
            PhysReg::R8 => Self::R8w,
            PhysReg::R9 => Self::R9w,
            PhysReg::R10 => Self::R10w,
            PhysReg::R11 => Self::R11w,
            PhysReg::R12 => Self::R12w,
            PhysReg::R13 => Self::R13w,
            PhysReg::R14 => Self::R14w,
            PhysReg::R15 => Self::R15w,
        }
    }
}

impl std::fmt::Display for Reg16 {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let reg = match self {
            Self::Ax => "ax",
            Self::Bx => "bx",
            Self::Cx => "cx",
            Self::Dx => "dx",
            Self::Si => "si",
            Self::Di => "di",
            Self::Bp => "bp",
            Self::Sp => "sp",
            Self::R8w => "r8w",
            Self::R9w => "r9w",
            Self::R10w => "r10w",
            Self::R11w => "r11w",
            Self::R12w => "r12w",
            Self::R13w => "r13w",
            Self::R14w => "r14w",
            Self::R15w => "r15w",
        };
        write!(f, "{}", reg)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Reg8 {
    Al,
    Bl,
    Cl,
    Dl,
    Sil,
    Dil,
    Bpl,
    Spl,
    R8b,
    R9b,
    R10b,
    R11b,
    R12b,
    R13b,
    R14b,
    R15b,
}

impl From<PhysReg> for Reg8 {
    fn from(value: PhysReg) -> Self {
        match value {
            PhysReg::Rax => Self::Al,
            PhysReg::Rbx => Self::Bl,
            PhysReg::Rcx => Self::Bl,
            PhysReg::Rdx => Self::Dl,
            PhysReg::Rsi => Self::Sil,
            PhysReg::Rdi => Self::Dil,
            PhysReg::Rbp => Self::Bpl,
            PhysReg::Rsp => Self::Spl,
            PhysReg::R8 => Self::R8b,
            PhysReg::R9 => Self::R9b,
            PhysReg::R10 => Self::R10b,
            PhysReg::R11 => Self::R11b,
            PhysReg::R12 => Self::R12b,
            PhysReg::R13 => Self::R13b,
            PhysReg::R14 => Self::R14b,
            PhysReg::R15 => Self::R15b,
        }
    }
}

impl std::fmt::Display for Reg8 {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let reg = match self {
            Self::Al => "al",
            Self::Bl => "bl",
            Self::Cl => "cl",
            Self::Dl => "dl",
            Self::Sil => "sil",
            Self::Dil => "dil",
            Self::Bpl => "bpl",
            Self::Spl => "spl",
            Self::R8b => "r8b",
            Self::R9b => "r9b",
            Self::R10b => "r10b",
            Self::R11b => "r11b",
            Self::R12b => "r12b",
            Self::R13b => "r13b",
            Self::R14b => "r14b",
            Self::R15b => "r15b",
        };
        write!(f, "{}", reg)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Reg {
    Reg64(Reg64),
    Reg32(Reg32),
    Reg16(Reg16),
    Reg8(Reg8),
}

impl From<PhysReg> for Reg {
    fn from(value: PhysReg) -> Self {
        match value {
            PhysReg::Rax => Reg::Reg64(Reg64::Rax),
            PhysReg::Rbx => Reg::Reg64(Reg64::Rbx),
            PhysReg::Rcx => Reg::Reg64(Reg64::Rcx),
            PhysReg::Rdx => Reg::Reg64(Reg64::Rdx),
            PhysReg::Rsi => Reg::Reg64(Reg64::Rsi),
            PhysReg::Rdi => Reg::Reg64(Reg64::Rdi),
            PhysReg::Rbp => Reg::Reg64(Reg64::Rbp),
            PhysReg::Rsp => Reg::Reg64(Reg64::Rsp),
            PhysReg::R8 => Reg::Reg64(Reg64::R8),
            PhysReg::R9 => Reg::Reg64(Reg64::R9),
            PhysReg::R10 => Reg::Reg64(Reg64::R10),
            PhysReg::R11 => Reg::Reg64(Reg64::R11),
            PhysReg::R12 => Reg::Reg64(Reg64::R12),
            PhysReg::R13 => Reg::Reg64(Reg64::R13),
            PhysReg::R14 => Reg::Reg64(Reg64::R14),
            PhysReg::R15 => Reg::Reg64(Reg64::R15),
        }
    }
}

impl Reg {
    pub fn phys(self) -> PhysReg {
        match self {
            Reg::Reg64(r) => match r {
                Reg64::Rax => PhysReg::Rax,
                Reg64::Rbx => PhysReg::Rbx,
                Reg64::Rcx => PhysReg::Rcx,
                Reg64::Rdx => PhysReg::Rdx,
                Reg64::Rsi => PhysReg::Rsi,
                Reg64::Rdi => PhysReg::Rdi,
                Reg64::Rbp => PhysReg::Rbp,
                Reg64::Rsp => PhysReg::Rsp,
                Reg64::R8 => PhysReg::R8,
                Reg64::R9 => PhysReg::R9,
                Reg64::R10 => PhysReg::R10,
                Reg64::R11 => PhysReg::R11,
                Reg64::R12 => PhysReg::R12,
                Reg64::R13 => PhysReg::R13,
                Reg64::R14 => PhysReg::R14,
                Reg64::R15 => PhysReg::R15,
            },
            Reg::Reg32(r) => match r {
                Reg32::Eax => PhysReg::Rax,
                Reg32::Ebx => PhysReg::Rbx,
                Reg32::Ecx => PhysReg::Rcx,
                Reg32::Edx => PhysReg::Rdx,
                Reg32::Esi => PhysReg::Rsi,
                Reg32::Edi => PhysReg::Rdi,
                Reg32::Ebp => PhysReg::Rbp,
                Reg32::Esp => PhysReg::Rsp,
                Reg32::R8d => PhysReg::R8,
                Reg32::R9d => PhysReg::R9,
                Reg32::R10d => PhysReg::R10,
                Reg32::R11d => PhysReg::R11,
                Reg32::R12d => PhysReg::R12,
                Reg32::R13d => PhysReg::R13,
                Reg32::R14d => PhysReg::R14,
                Reg32::R15d => PhysReg::R15,
            },
            Reg::Reg16(Reg16::Ax) => PhysReg::Rax,
            Reg::Reg16(Reg16::Bx) => PhysReg::Rbx,
            Reg::Reg16(Reg16::Cx) => PhysReg::Rcx,
            Reg::Reg16(Reg16::Dx) => PhysReg::Rdx,
            Reg::Reg16(Reg16::Si) => PhysReg::Rsi,
            Reg::Reg16(Reg16::Di) => PhysReg::Rdi,
            Reg::Reg16(Reg16::Bp) => PhysReg::Rbp,
            Reg::Reg16(Reg16::Sp) => PhysReg::Rsp,
            Reg::Reg16(Reg16::R8w) => PhysReg::R8,
            Reg::Reg16(Reg16::R9w) => PhysReg::R9,
            Reg::Reg16(Reg16::R10w) => PhysReg::R10,
            Reg::Reg16(Reg16::R11w) => PhysReg::R11,
            Reg::Reg16(Reg16::R12w) => PhysReg::R12,
            Reg::Reg16(Reg16::R13w) => PhysReg::R13,
            Reg::Reg16(Reg16::R14w) => PhysReg::R14,
            Reg::Reg16(Reg16::R15w) => PhysReg::R15,
            Reg::Reg8(Reg8::Al) => PhysReg::Rax,
            Reg::Reg8(Reg8::Bl) => PhysReg::Rbx,
            Reg::Reg8(Reg8::Cl) => PhysReg::Rcx,
            Reg::Reg8(Reg8::Dl) => PhysReg::Rdx,
            Reg::Reg8(Reg8::Sil) => PhysReg::Rsi,
            Reg::Reg8(Reg8::Dil) => PhysReg::Rdi,
            Reg::Reg8(Reg8::Bpl) => PhysReg::Rbp,
            Reg::Reg8(Reg8::Spl) => PhysReg::Rsp,
            Reg::Reg8(Reg8::R8b) => PhysReg::R8,
            Reg::Reg8(Reg8::R9b) => PhysReg::R9,
            Reg::Reg8(Reg8::R10b) => PhysReg::R10,
            Reg::Reg8(Reg8::R11b) => PhysReg::R11,
            Reg::Reg8(Reg8::R12b) => PhysReg::R12,
            Reg::Reg8(Reg8::R13b) => PhysReg::R13,
            Reg::Reg8(Reg8::R14b) => PhysReg::R14,
            Reg::Reg8(Reg8::R15b) => PhysReg::R15,
        }
    }
}

impl std::fmt::Display for Reg {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Reg::Reg64(reg) => write!(f, "{}", reg),
            Reg::Reg32(reg) => write!(f, "{}", reg),
            Reg::Reg16(reg) => write!(f, "{}", reg),
            Reg::Reg8(reg) => write!(f, "{}", reg),
        }
    }
}

impl From<Reg64> for Reg {
    fn from(reg: Reg64) -> Self {
        Reg::Reg64(reg)
    }
}

impl From<Reg32> for Reg {
    fn from(reg: Reg32) -> Self {
        Reg::Reg32(reg)
    }
}

impl From<Reg16> for Reg {
    fn from(reg: Reg16) -> Self {
        Reg::Reg16(reg)
    }
}

impl From<Reg8> for Reg {
    fn from(reg: Reg8) -> Self {
        Reg::Reg8(reg)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Location {
    Reg(Reg),
    Mem(i32),
    Stack(i32),
    Imm(i64),
}

impl From<Reg> for Location {
    fn from(reg: Reg) -> Self {
        Location::Reg(reg)
    }
}

impl From<Reg64> for Location {
    fn from(reg: Reg64) -> Self {
        Location::Reg(Reg::from(reg))
    }
}

impl From<Reg32> for Location {
    fn from(reg: Reg32) -> Self {
        Location::Reg(Reg::from(reg))
    }
}

impl From<Reg16> for Location {
    fn from(reg: Reg16) -> Self {
        Location::Reg(Reg::from(reg))
    }
}

impl From<Reg8> for Location {
    fn from(reg: Reg8) -> Self {
        Location::Reg(Reg::from(reg))
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
pub struct Label(pub String);

impl Label {
    pub fn prepare(&self) -> String {
        self.0.replace("-", "_")
    }
}

impl std::fmt::Display for Label {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.prepare())
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Instruction {
    Mov(Location, Location),
    Pop(Location),
    Push(Location),
    Label(Label),
    Ret,
    Jnz(Label),
    Test(Location, Location),
    Jmp(Label),
    DefineLabel(Label),
    Cmp(Location, Location),
    Setl(Location),
    Movezx(Location, Location),
}

impl std::fmt::Display for Instruction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Mov(dst, src) => write!(f, "  mov {}, {}", dst, src),
            Self::Pop(dst) => write!(f, "  pop {}", dst),
            Self::Push(src) => write!(f, "  push {}", src),
            Self::Label(label) => write!(f, "{}:", label),
            Self::Ret => write!(f, "  ret"),
            Self::Jnz(label) => write!(f, "  jnz {}", label),
            Self::Test(dst, src) => write!(f, "  test {}, {}", dst, src),
            Self::Jmp(label) => write!(f, "  jmp {}", label),
            Self::DefineLabel(label) => write!(f, "{}:", label),
            Self::Cmp(dst, src) => write!(f, "  cmp {}, {}", dst, src),
            Self::Setl(dst) => write!(f, "  setl {}", dst),
            Self::Movezx(dst, src) => write!(f, "  movzx {}, {}", dst, src),
        }
    }
}

pub struct Assembler {
    instructions: Vec<Instruction>,
    free_registers: Vec<PhysReg>,
    used_registers: Vec<PhysReg>,
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
                PhysReg::Rax,
                PhysReg::Rbx,
                PhysReg::Rcx,
                PhysReg::Rdx,
                PhysReg::Rsi,
                PhysReg::Rdi,
                PhysReg::R8,
                PhysReg::R9,
                PhysReg::R10,
                PhysReg::R11,
            ],
            used_registers: Vec::new(),
        }
    }
}

impl Assembler {
    pub fn alloc_reg<T>(&mut self) -> T
    where
        T: From<PhysReg>,
    {
        let reg = self.free_registers.pop().expect("Out of registers");
        self.used_registers.push(reg);
        reg.into()
    }

    pub fn free_reg(&mut self, reg: impl Into<Reg>) {
        let phys = reg.into().phys();
        debug_assert!(
            self.used_registers.contains(&phys),
            "Double free or freeing unused register: {:?}",
            phys,
        );

        self.used_registers.retain(|&r| r != phys);
        self.free_registers.push(phys);
    }

    pub fn label(&mut self, name: &str) -> &mut Self {
        self.instructions
            .push(Instruction::Label(Label(name.to_string())));
        self
    }

    pub fn ret(&mut self) -> &mut Self {
        self.instructions.push(Instruction::Ret);
        self
    }

    pub fn jnz(&mut self, label: impl Into<String>) -> &mut Self {
        self.instructions
            .push(Instruction::Jnz(Label(label.into())));
        self
    }

    pub fn test(&mut self, lhs: impl Into<Location>, rhs: impl Into<Location>) -> &mut Self {
        self.instructions
            .push(Instruction::Test(rhs.into(), lhs.into()));
        self
    }

    pub fn jmp(&mut self, label: impl Into<String>) -> &mut Self {
        self.instructions
            .push(Instruction::Jmp(Label(label.into())));
        self
    }

    pub fn define_label(&mut self, label: impl Into<String>) -> &mut Self {
        self.instructions
            .push(Instruction::DefineLabel(Label(label.into())));
        self
    }

    pub fn mov(&mut self, lhs: impl Into<Location>, rhs: impl Into<Location>) -> &mut Self {
        let lhs = lhs.into();
        debug_assert!(!matches!(lhs, Location::Imm(_)));
        self.instructions.push(Instruction::Mov(lhs, rhs.into()));
        self
    }

    pub fn movezx(&mut self, lhs: impl Into<Location>, rhs: impl Into<Location>) -> &mut Self {
        let lhs = lhs.into();
        debug_assert!(!matches!(lhs, Location::Imm(_)));
        self.instructions.push(Instruction::Movezx(lhs, rhs.into()));
        self
    }

    pub fn cmp(&mut self, lhs: impl Into<Location>, rhs: impl Into<Location>) -> &mut Self {
        let lhs = lhs.into();
        debug_assert!(!matches!(lhs, Location::Imm(_)));
        self.instructions.push(Instruction::Cmp(lhs, rhs.into()));
        self
    }

    pub fn setl(&mut self, src: impl Into<Location>) -> &mut Self {
        let src = src.into();
        debug_assert!(!matches!(src, Location::Imm(_)));
        self.instructions.push(Instruction::Setl(src));
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
