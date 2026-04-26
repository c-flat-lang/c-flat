pub trait CastableReg {
    const KIND: RegKind;
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct VReg {
    pub id: usize,
    pub kind: RegKind,
}

impl VReg {
    pub fn as_reg64(self) -> Self {
        Self {
            id: self.id,
            kind: RegKind::Reg64,
        }
    }

    pub fn as_reg32(self) -> Self {
        Self {
            id: self.id,
            kind: RegKind::Reg32,
        }
    }

    pub fn as_reg16(self) -> Self {
        Self {
            id: self.id,
            kind: RegKind::Reg16,
        }
    }

    pub fn as_reg8(self) -> Self {
        Self {
            id: self.id,
            kind: RegKind::Reg8,
        }
    }
}

impl std::fmt::Display for VReg {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "vreg({} : {:?})", self.id, self.kind)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum RegKind {
    Reg8,
    Reg16,
    Reg32,
    Reg64,
}

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

impl From<Reg64> for PhysReg {
    fn from(value: Reg64) -> Self {
        match value {
            Reg64::Rax => Self::Rax,
            Reg64::Rbx => Self::Rbx,
            Reg64::Rcx => Self::Rcx,
            Reg64::Rdx => Self::Rdx,
            Reg64::Rsi => Self::Rsi,
            Reg64::Rdi => Self::Rdi,
            Reg64::Rbp => Self::Rbp,
            Reg64::Rsp => Self::Rsp,
            Reg64::R8 => Self::R8,
            Reg64::R9 => Self::R9,
            Reg64::R10 => Self::R10,
            Reg64::R11 => Self::R11,
            Reg64::R12 => Self::R12,
            Reg64::R13 => Self::R13,
            Reg64::R14 => Self::R14,
            Reg64::R15 => Self::R15,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
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

impl CastableReg for Reg64 {
    const KIND: RegKind = RegKind::Reg64;
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

impl From<Reg32> for Reg64 {
    fn from(value: Reg32) -> Self {
        match value {
            Reg32::Eax => Self::Rax,
            Reg32::Ebx => Self::Rbx,
            Reg32::Ecx => Self::Rcx,
            Reg32::Edx => Self::Rdx,
            Reg32::Esi => Self::Rsi,
            Reg32::Edi => Self::Rdi,
            Reg32::Ebp => Self::Rbp,
            Reg32::Esp => Self::Rsp,
            Reg32::R8d => Self::R8,
            Reg32::R9d => Self::R9,
            Reg32::R10d => Self::R10,
            Reg32::R11d => Self::R11,
            Reg32::R12d => Self::R12,
            Reg32::R13d => Self::R13,
            Reg32::R14d => Self::R14,
            Reg32::R15d => Self::R15,
        }
    }
}

impl From<Reg16> for Reg64 {
    fn from(value: Reg16) -> Self {
        match value {
            Reg16::Ax => Self::Rax,
            Reg16::Bx => Self::Rbx,
            Reg16::Cx => Self::Rcx,
            Reg16::Dx => Self::Rdx,
            Reg16::Si => Self::Rsi,
            Reg16::Di => Self::Rdi,
            Reg16::Bp => Self::Rbp,
            Reg16::Sp => Self::Rsp,
            Reg16::R8w => Self::R8,
            Reg16::R9w => Self::R9,
            Reg16::R10w => Self::R10,
            Reg16::R11w => Self::R11,
            Reg16::R12w => Self::R12,
            Reg16::R13w => Self::R13,
            Reg16::R14w => Self::R14,
            Reg16::R15w => Self::R15,
        }
    }
}

impl From<Reg8> for Reg64 {
    fn from(value: Reg8) -> Self {
        match value {
            Reg8::Al => Self::Rax,
            Reg8::Bl => Self::Rbx,
            Reg8::Cl => Self::Rcx,
            Reg8::Dl => Self::Rdx,
            Reg8::Sil => Self::Rsi,
            Reg8::Dil => Self::Rdi,
            Reg8::Bpl => Self::Rbp,
            Reg8::Spl => Self::Rsp,
            Reg8::R8b => Self::R8,
            Reg8::R9b => Self::R9,
            Reg8::R10b => Self::R10,
            Reg8::R11b => Self::R11,
            Reg8::R12b => Self::R12,
            Reg8::R13b => Self::R13,
            Reg8::R14b => Self::R14,
            Reg8::R15b => Self::R15,
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

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
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

impl CastableReg for Reg32 {
    const KIND: RegKind = RegKind::Reg32;
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

impl From<Reg64> for Reg32 {
    fn from(value: Reg64) -> Self {
        match value {
            Reg64::Rax => Self::Eax,
            Reg64::Rbx => Self::Ebx,
            Reg64::Rcx => Self::Ecx,
            Reg64::Rdx => Self::Edx,
            Reg64::Rsi => Self::Esi,
            Reg64::Rdi => Self::Edi,
            Reg64::Rbp => Self::Ebp,
            Reg64::Rsp => Self::Esp,
            Reg64::R8 => Self::R8d,
            Reg64::R9 => Self::R9d,
            Reg64::R10 => Self::R10d,
            Reg64::R11 => Self::R11d,
            Reg64::R12 => Self::R12d,
            Reg64::R13 => Self::R13d,
            Reg64::R14 => Self::R14d,
            Reg64::R15 => Self::R15d,
        }
    }
}

impl From<Reg16> for Reg32 {
    fn from(value: Reg16) -> Self {
        match value {
            Reg16::Ax => Self::Eax,
            Reg16::Bx => Self::Ebx,
            Reg16::Cx => Self::Ecx,
            Reg16::Dx => Self::Edx,
            Reg16::Si => Self::Esi,
            Reg16::Di => Self::Edi,
            Reg16::Bp => Self::Ebp,
            Reg16::Sp => Self::Esp,
            Reg16::R8w => Self::R8d,
            Reg16::R9w => Self::R9d,
            Reg16::R10w => Self::R10d,
            Reg16::R11w => Self::R11d,
            Reg16::R12w => Self::R12d,
            Reg16::R13w => Self::R13d,
            Reg16::R14w => Self::R14d,
            Reg16::R15w => Self::R15d,
        }
    }
}

impl From<Reg8> for Reg32 {
    fn from(value: Reg8) -> Self {
        match value {
            Reg8::Al => Self::Eax,
            Reg8::Bl => Self::Ebx,
            Reg8::Cl => Self::Ecx,
            Reg8::Dl => Self::Edx,
            Reg8::Sil => Self::Esi,
            Reg8::Dil => Self::Edi,
            Reg8::Bpl => Self::Ebp,
            Reg8::Spl => Self::Esp,
            Reg8::R8b => Self::R8d,
            Reg8::R9b => Self::R9d,
            Reg8::R10b => Self::R10d,
            Reg8::R11b => Self::R11d,
            Reg8::R12b => Self::R12d,
            Reg8::R13b => Self::R13d,
            Reg8::R14b => Self::R14d,
            Reg8::R15b => Self::R15d,
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

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
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

impl CastableReg for Reg16 {
    const KIND: RegKind = RegKind::Reg16;
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

impl From<Reg64> for Reg16 {
    fn from(value: Reg64) -> Self {
        match value {
            Reg64::Rax => Self::Ax,
            Reg64::Rbx => Self::Bx,
            Reg64::Rcx => Self::Cx,
            Reg64::Rdx => Self::Dx,
            Reg64::Rsi => Self::Si,
            Reg64::Rdi => Self::Di,
            Reg64::Rbp => Self::Bp,
            Reg64::Rsp => Self::Sp,
            Reg64::R8 => Self::R8w,
            Reg64::R9 => Self::R9w,
            Reg64::R10 => Self::R10w,
            Reg64::R11 => Self::R11w,
            Reg64::R12 => Self::R12w,
            Reg64::R13 => Self::R13w,
            Reg64::R14 => Self::R14w,
            Reg64::R15 => Self::R15w,
        }
    }
}

impl From<Reg32> for Reg16 {
    fn from(value: Reg32) -> Self {
        match value {
            Reg32::Eax => Self::Ax,
            Reg32::Ebx => Self::Bx,
            Reg32::Ecx => Self::Cx,
            Reg32::Edx => Self::Dx,
            Reg32::Esi => Self::Si,
            Reg32::Edi => Self::Di,
            Reg32::Ebp => Self::Bp,
            Reg32::Esp => Self::Sp,
            Reg32::R8d => Self::R8w,
            Reg32::R9d => Self::R9w,
            Reg32::R10d => Self::R10w,
            Reg32::R11d => Self::R11w,
            Reg32::R12d => Self::R12w,
            Reg32::R13d => Self::R13w,
            Reg32::R14d => Self::R14w,
            Reg32::R15d => Self::R15w,
        }
    }
}

impl From<Reg8> for Reg16 {
    fn from(value: Reg8) -> Self {
        match value {
            Reg8::Al => Self::Ax,
            Reg8::Bl => Self::Bx,
            Reg8::Cl => Self::Cx,
            Reg8::Dl => Self::Dx,
            Reg8::Sil => Self::Si,
            Reg8::Dil => Self::Di,
            Reg8::Bpl => Self::Bp,
            Reg8::Spl => Self::Sp,
            Reg8::R8b => Self::R8w,
            Reg8::R9b => Self::R9w,
            Reg8::R10b => Self::R10w,
            Reg8::R11b => Self::R11w,
            Reg8::R12b => Self::R12w,
            Reg8::R13b => Self::R13w,
            Reg8::R14b => Self::R14w,
            Reg8::R15b => Self::R15w,
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

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
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

impl CastableReg for Reg8 {
    const KIND: RegKind = RegKind::Reg8;
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

impl From<Reg64> for Reg8 {
    fn from(value: Reg64) -> Self {
        match value {
            Reg64::Rax => Self::Al,
            Reg64::Rbx => Self::Bl,
            Reg64::Rcx => Self::Cl,
            Reg64::Rdx => Self::Dl,
            Reg64::Rsi => Self::Sil,
            Reg64::Rdi => Self::Dil,
            Reg64::Rbp => Self::Bpl,
            Reg64::Rsp => Self::Spl,
            Reg64::R8 => Self::R8b,
            Reg64::R9 => Self::R9b,
            Reg64::R10 => Self::R10b,
            Reg64::R11 => Self::R11b,
            Reg64::R12 => Self::R12b,
            Reg64::R13 => Self::R13b,
            Reg64::R14 => Self::R14b,
            Reg64::R15 => Self::R15b,
        }
    }
}

impl From<Reg32> for Reg8 {
    fn from(value: Reg32) -> Self {
        match value {
            Reg32::Eax => Self::Al,
            Reg32::Ebx => Self::Bl,
            Reg32::Ecx => Self::Cl,
            Reg32::Edx => Self::Dl,
            Reg32::Esi => Self::Sil,
            Reg32::Edi => Self::Dil,
            Reg32::Ebp => Self::Bpl,
            Reg32::Esp => Self::Spl,
            Reg32::R8d => Self::R8b,
            Reg32::R9d => Self::R9b,
            Reg32::R10d => Self::R10b,
            Reg32::R11d => Self::R11b,
            Reg32::R12d => Self::R12b,
            Reg32::R13d => Self::R13b,
            Reg32::R14d => Self::R14b,
            Reg32::R15d => Self::R15b,
        }
    }
}

impl From<Reg16> for Reg8 {
    fn from(value: Reg16) -> Self {
        match value {
            Reg16::Ax => Self::Al,
            Reg16::Bx => Self::Bl,
            Reg16::Cx => Self::Cl,
            Reg16::Dx => Self::Dl,
            Reg16::Si => Self::Sil,
            Reg16::Di => Self::Dil,
            Reg16::Bp => Self::Bpl,
            Reg16::Sp => Self::Spl,
            Reg16::R8w => Self::R8b,
            Reg16::R9w => Self::R9b,
            Reg16::R10w => Self::R10b,
            Reg16::R11w => Self::R11b,
            Reg16::R12w => Self::R12b,
            Reg16::R13w => Self::R13b,
            Reg16::R14w => Self::R14b,
            Reg16::R15w => Self::R15b,
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

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Reg {
    Reg64(Reg64),
    Reg32(Reg32),
    Reg16(Reg16),
    Reg8(Reg8),
    VReg(VReg),
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
    pub fn cast_to<T>(self) -> Self
    where
        T: CastableReg,
    {
        match T::KIND {
            RegKind::Reg64 => match self {
                Reg::VReg(reg) => reg.as_reg64().into(),
                _ => self.as_reg64().into(),
            },
            RegKind::Reg32 => match self {
                Reg::VReg(reg) => reg.as_reg32().into(),
                _ => self.as_reg32().into(),
            },
            RegKind::Reg16 => match self {
                Reg::VReg(reg) => reg.as_reg16().into(),
                _ => self.as_reg16().into(),
            },
            RegKind::Reg8 => match self {
                Reg::VReg(reg) => reg.as_reg8().into(),
                _ => self.as_reg8().into(),
            },
        }
    }

    pub fn as_phys(self) -> PhysReg {
        let reg64 = self.as_reg64();
        PhysReg::from(reg64)
    }

    pub fn as_reg64(self) -> Reg64 {
        match self {
            Reg::Reg64(reg) => reg,
            Reg::Reg32(reg) => Reg64::from(reg),
            Reg::Reg16(reg) => Reg64::from(reg),
            Reg::Reg8(reg) => Reg64::from(reg),
            Reg::VReg(reg) => unreachable!(
                "{} is a virtual register and can not be used as a physical register",
                reg
            ),
        }
    }

    pub fn as_reg32(self) -> Reg32 {
        match self {
            Reg::Reg64(reg) => Reg32::from(reg),
            Reg::Reg32(reg) => reg,
            Reg::Reg16(reg) => Reg32::from(reg),
            Reg::Reg8(reg) => Reg32::from(reg),
            Reg::VReg(reg) => unreachable!(
                "{} is a virtual register and can not be used as a physical register",
                reg
            ),
        }
    }

    pub fn as_reg16(self) -> Reg16 {
        match self {
            Reg::Reg64(reg) => Reg16::from(reg),
            Reg::Reg32(reg) => Reg16::from(reg),
            Reg::Reg16(reg) => reg,
            Reg::Reg8(reg) => Reg16::from(reg),
            Reg::VReg(reg) => unreachable!(
                "{} is a virtual register and can not be used as a physical register",
                reg
            ),
        }
    }

    pub fn as_reg8(self) -> Reg8 {
        match self {
            Reg::Reg64(reg) => Reg8::from(reg),
            Reg::Reg32(reg) => Reg8::from(reg),
            Reg::Reg16(reg) => Reg8::from(reg),
            Reg::Reg8(reg) => reg,
            Reg::VReg(reg) => unreachable!(
                "{} is a virtual register and can not be used as a physical register",
                reg
            ),
        }
    }

    pub fn kind(&self) -> RegKind {
        match self {
            Reg::Reg64(_) => RegKind::Reg64,
            Reg::Reg32(_) => RegKind::Reg32,
            Reg::Reg16(_) => RegKind::Reg16,
            Reg::Reg8(_) => RegKind::Reg8,
            Reg::VReg(reg) => reg.kind,
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
            Reg::VReg(reg) => write!(f, "{}", reg),
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

impl From<VReg> for Reg {
    fn from(reg: VReg) -> Self {
        Reg::VReg(reg)
    }
}
