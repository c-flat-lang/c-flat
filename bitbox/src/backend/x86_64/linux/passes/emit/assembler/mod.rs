pub mod register;

use crate::{backend::x86_64::linux::passes::emit::allocator::Allocator, ir::Type};

pub use register::*;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Stack {
    /// Distance from rbp
    pub offset: i32,
    /// Size of ONE element
    pub access_size: i32,
}

impl Stack {
    pub fn new(offset: i32, access_size: i32) -> Self {
        Self {
            offset,
            access_size,
        }
    }

    pub fn access_size(ty: &Type) -> i32 {
        match ty {
            Type::Unsigned(bits) | Type::Signed(bits) | Type::Float(bits) => *bits as i32 / 8,
            Type::Pointer(_) => 8,
            Type::Array(_, elem) => Self::access_size(elem),
            Type::Struct(_) => 8,
            Type::Void => 0,
        }
    }
}

impl std::fmt::Display for Stack {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let prefix = match self.access_size {
            1 => "byte ptr",
            2 => "word ptr",
            4 => "dword ptr",
            8 => "qword ptr",
            _ => unreachable!(),
        };
        write!(f, "{} [rbp-{}]", prefix, self.offset)
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct MemIndexed {
    pub base: Reg,
    pub index: Reg,
    pub scale: i32,
}

impl MemIndexed {
    pub fn new(base: Reg, index: Reg, scale: i32) -> Self {
        debug_assert!([1, 2, 4, 8].contains(&scale));
        Self { base, index, scale }
    }
}

impl std::fmt::Display for MemIndexed {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "[{} + {} * {}]", self.base, self.index, self.scale)
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Location {
    Address(Stack),
    Imm(i64),
    MemIndexed(MemIndexed),
    Reg(Reg),
    Stack(Stack),
}

impl Location {
    pub fn is_reg(&self) -> bool {
        matches!(self, Location::Reg(_))
    }

    pub fn is_stack(&self) -> bool {
        matches!(self, Location::Stack(_))
    }

    pub fn is_mem_indexed(&self) -> bool {
        matches!(self, Location::MemIndexed(_))
    }

    pub fn is_imm(&self) -> bool {
        matches!(self, Location::Imm(_))
    }

    pub fn map_reg(&self, f: impl FnOnce(Reg) -> Reg) -> Self {
        match self {
            Self::Reg(reg) => Location::Reg(f(*reg)),
            _ => self.clone(),
        }
    }

    pub fn map_stack(&mut self, f: impl FnOnce(&Stack) -> Stack) -> Self {
        match self {
            Self::Stack(stack) => Location::Stack(f(stack)),
            _ => self.clone(),
        }
    }
    pub fn map_imm(&self, f: impl FnOnce(i64) -> i64) -> Self {
        match self {
            Self::Imm(imm) => Location::Imm(f(*imm)),
            _ => self.clone(),
        }
    }
    pub fn map_mem_indexed(&self, f: impl FnOnce(&MemIndexed) -> MemIndexed) -> Self {
        match self {
            Self::MemIndexed(mem_indexed) => Location::MemIndexed(f(mem_indexed)),
            _ => self.clone(),
        }
    }
}

impl From<PhysReg> for Location {
    fn from(reg: PhysReg) -> Self {
        Location::Reg(Reg::from(reg))
    }
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

impl From<XmmReg> for Location {
    fn from(reg: XmmReg) -> Self {
        Location::Reg(Reg::Xmm(reg))
    }
}

impl From<Stack> for Location {
    fn from(stack: Stack) -> Self {
        Location::Stack(stack)
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
            Location::Address(stack) => write!(f, "{}", stack),
            Location::MemIndexed(mem_indexed) => write!(f, "{}", mem_indexed),
            Location::Stack(stack) => write!(f, "{}", stack),
            Location::Imm(val) => write!(f, "{}", val),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Label(pub String);

impl Label {
    pub fn prepare(&self) -> String {
        self.0.replace("-", "_").replace(".", "_")
    }
}

impl std::fmt::Display for Label {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.prepare())
    }
}

impl From<String> for Label {
    fn from(value: String) -> Self {
        Self(value)
    }
}

impl From<&String> for Label {
    fn from(value: &String) -> Self {
        Self(value.clone())
    }
}

impl From<&str> for Label {
    fn from(value: &str) -> Self {
        Self(value.to_string())
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Instruction {
    Add(Location, Location),
    And(Location, Location),
    Call(String),
    Cmp(Location, Location),
    Comment(String),
    DefineLabel(Label),
    Jmp(Label),
    Jnz(Label),
    Jz(Label),
    Lea(Location, Location),
    Mov(Location, Location),
    Movezx(Location, Location),
    Pop(Location),
    Push(Location),
    Ret,
    Sete(Location),
    Setg(Location),
    Setge(Location),
    Setl(Location),
    Seta(Location),
    Setae(Location),
    Setb(Location),
    Sub(Location, Location),
    Test(Location, Location),
    // Integer multiplication (2-operand: dst *= src)
    Imul(Location, Location),
    // SSE / XMM instructions
    Movd(Location, Location),
    Movss(Location, Location),
    Movsd(Location, Location),
    Addss(Location, Location),
    Addsd(Location, Location),
    Subss(Location, Location),
    Subsd(Location, Location),
    Mulss(Location, Location),
    Mulsd(Location, Location),
    Divss(Location, Location),
    Divsd(Location, Location),
    Ucomiss(Location, Location),
    Ucomisd(Location, Location),
    // Integer → float conversions
    Cvtsi2ss(Location, Location),
    Cvtsi2sd(Location, Location),
    // Integer division (rdx:rax implicit)
    Cqo,
    Idiv(Location),
}

impl std::fmt::Display for Instruction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Add(lhs, rhs) => write!(f, "  add {lhs}, {rhs}"),
            Self::And(lhs, rhs) => write!(f, "  and {lhs}, {rhs}"),
            Self::Call(name) => write!(f, "  call {name}"),
            Self::Cmp(lhs, rhs) => write!(f, "  cmp {lhs}, {rhs}"),
            Self::Comment(comment) => write!(f, "  # {comment}"),
            Self::DefineLabel(label) => write!(f, "{label}:"),
            Self::Jmp(label) => write!(f, "  jmp {label}"),
            Self::Jnz(label) => write!(f, "  jnz {label}"),
            Self::Jz(label) => write!(f, "  jz {label}"),
            Self::Lea(dst, src) => {
                let src_str = match src {
                    Location::Stack(s) | Location::Address(s) => {
                        format!("qword ptr [rbp-{}]", s.offset)
                    }
                    other => format!("{other}"),
                };
                write!(f, "  lea {dst}, {src_str}")
            }
            Self::Mov(dst, src) => write!(f, "  mov {dst}, {src}"),
            Self::Movezx(dst, src) => write!(f, "  movzx {dst}, {src}"),
            Self::Pop(dst) => write!(f, "  pop {dst}"),
            Self::Push(src) => write!(f, "  push {src}"),
            Self::Ret => write!(f, "  ret"),
            Self::Sete(dst) => write!(f, "  sete {dst}"),
            Self::Setg(dst) => write!(f, "  setg {dst}"),
            Self::Setge(dst) => write!(f, "  setge {dst}"),
            Self::Setl(dst) => write!(f, "  setl {dst}"),
            Self::Seta(dst) => write!(f, "  seta {dst}"),
            Self::Setae(dst) => write!(f, "  setae {dst}"),
            Self::Setb(dst) => write!(f, "  setb {dst}"),
            Self::Sub(lhs, rhs) => write!(f, "  sub {lhs}, {rhs}"),
            Self::Test(dst, src) => write!(f, "  test {dst}, {src}"),
            Self::Imul(dst, src) => write!(f, "  imul {dst}, {src}"),
            Self::Movd(dst, src) => write!(f, "  movd {dst}, {src}"),
            Self::Movss(dst, src) => write!(f, "  movss {dst}, {src}"),
            Self::Movsd(dst, src) => write!(f, "  movsd {dst}, {src}"),
            Self::Addss(dst, src) => write!(f, "  addss {dst}, {src}"),
            Self::Addsd(dst, src) => write!(f, "  addsd {dst}, {src}"),
            Self::Subss(dst, src) => write!(f, "  subss {dst}, {src}"),
            Self::Subsd(dst, src) => write!(f, "  subsd {dst}, {src}"),
            Self::Mulss(dst, src) => write!(f, "  mulss {dst}, {src}"),
            Self::Mulsd(dst, src) => write!(f, "  mulsd {dst}, {src}"),
            Self::Divss(dst, src) => write!(f, "  divss {dst}, {src}"),
            Self::Divsd(dst, src) => write!(f, "  divsd {dst}, {src}"),
            Self::Ucomiss(dst, src) => write!(f, "  ucomiss {dst}, {src}"),
            Self::Ucomisd(dst, src) => write!(f, "  ucomisd {dst}, {src}"),
            Self::Cvtsi2ss(dst, src) => write!(f, "  cvtsi2ss {dst}, {src}"),
            Self::Cvtsi2sd(dst, src) => write!(f, "  cvtsi2sd {dst}, {src}"),
            Self::Cqo => write!(f, "  cqo"),
            Self::Idiv(src) => write!(f, "  idiv {src}"),
        }
    }
}

struct PreparedOperands {
    pub dst: Location,
    pub src: Location,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum FunctionSection {
    Prolog,
    Body,
    Epilog,
}

/// caller must preserve, rax, rdi, rsi, rdx, rcx, r8, r9
/// callee must preserve, rbx, rbp, r12–r15
/// | Argument | Register |
/// | -------- | -------- |
/// | 1st      | `rdi`    |
/// | 2nd      | `rsi`    |
/// | 3rd      | `rdx`    |
/// | 4th      | `rcx`    |
/// | 5th      | `r8`     |
/// | 6th      | `r9`     |
#[derive(Debug)]
pub struct Assembler {
    pub prolog: Vec<Instruction>,
    pub instructions: Vec<Instruction>,
    pub epilog: Vec<Instruction>,
    current_section: FunctionSection,
    pub(crate) alloc: Allocator,
    /// changes as we push and pop stack
    pub(crate) stack_offset: i64,
    pub debug: bool,
}

impl std::fmt::Display for Assembler {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for i in &self.prolog {
            if self.debug && matches!(i, Instruction::Comment(_)) {
                continue;
            }
            writeln!(f, "{}", i)?;
        }

        for i in &self.instructions {
            if self.debug && matches!(i, Instruction::Comment(_)) {
                continue;
            }
            writeln!(f, "{}", i)?;
        }

        for i in &self.epilog {
            if self.debug && matches!(i, Instruction::Comment(_)) {
                continue;
            }
            writeln!(f, "{}", i)?;
        }
        Ok(())
    }
}

impl Default for Assembler {
    fn default() -> Self {
        Self {
            prolog: Vec::new(),
            instructions: Vec::new(),
            epilog: Vec::new(),
            current_section: FunctionSection::Prolog,
            stack_offset: 0,
            alloc: Allocator::default(),
            debug: true,
        }
    }
}

impl Assembler {
    pub fn set_section(&mut self, section: FunctionSection) {
        self.current_section = section;
    }

    fn push_to(&mut self, section: FunctionSection, inst: Instruction) {
        match section {
            FunctionSection::Prolog => self.prolog.push(inst),
            FunctionSection::Body => self.instructions.push(inst),
            FunctionSection::Epilog => self.epilog.push(inst),
        }
    }

    /// callee must preserve, rbx, rbp, r12–r15
    pub fn callee_preserved_regs(&self, alloc: &Allocator) -> Vec<PhysReg> {
        alloc
            .used_registers
            .iter()
            .filter(|&r| {
                [
                    PhysReg::Rbx,
                    PhysReg::Rbp,
                    PhysReg::R12,
                    PhysReg::R13,
                    PhysReg::R14,
                    PhysReg::R15,
                ]
                .contains(r)
            })
            .copied()
            .collect::<Vec<PhysReg>>()
    }

    /// caller must preserve, rax, rdi, rsi, rdx, rcx, r8, r9
    pub fn caller_preserved_regs(&self) -> Vec<PhysReg> {
        self.alloc
            .used_registers
            .iter()
            .filter(|&r| {
                [
                    PhysReg::Rax,
                    PhysReg::Rdi,
                    PhysReg::Rsi,
                    PhysReg::Rdx,
                    PhysReg::Rcx,
                    PhysReg::R8,
                    PhysReg::R9,
                ]
                .contains(r)
            })
            .copied()
            .collect::<Vec<PhysReg>>()
    }

    pub fn arg_regs<T>(&self, index: usize) -> Option<T>
    where
        T: From<PhysReg>,
    {
        if index >= 6 {
            return None;
        }

        Some(
            [
                PhysReg::Rdi,
                PhysReg::Rsi,
                PhysReg::Rdx,
                PhysReg::Rcx,
                PhysReg::R8,
                PhysReg::R9,
            ][index]
                .into(),
        )
    }

    /// Returns the XMM argument register at position `index` (xmm0–xmm7).
    pub fn xmm_arg_reg(&self, index: usize) -> Option<XmmReg> {
        [
            XmmReg::Xmm0,
            XmmReg::Xmm1,
            XmmReg::Xmm2,
            XmmReg::Xmm3,
            XmmReg::Xmm4,
            XmmReg::Xmm5,
            XmmReg::Xmm6,
            XmmReg::Xmm7,
        ]
        .get(index)
        .copied()
    }

    pub fn materialize_address(&mut self, loc: &Location) -> Reg {
        match loc {
            Location::Address(slot) => {
                let r = self.alloc.vreg::<Reg64>();
                self.lea(r, Location::Address(*slot));
                r
            }

            Location::Stack(slot) => {
                let r = self.alloc.vreg::<Reg64>();
                self.lea(r, Location::Stack(*slot));
                r
            }

            Location::MemIndexed(mem) => {
                let r = self.alloc.vreg::<Reg64>();
                self.lea(r, Location::MemIndexed(mem.clone()));
                r
            }

            l @ Location::Reg(_) => {
                // For pointer variables, the register value IS the address.
                let Location::Reg(r) = l else { unreachable!() };
                *r
            }

            l => {
                panic!("materialize_address called on non-addressable value {l:?}");
            }
        }
    }

    pub fn materialize_value(&mut self, loc: &Location) -> Reg {
        match loc {
            Location::Reg(r) => *r,

            Location::Imm(imm) => {
                let r = self.alloc.vreg::<Reg64>();
                self.mov(r, Location::Imm(*imm));
                r
            }

            Location::Stack(slot) => {
                let r = self.alloc.vreg::<Reg64>();
                self.mov(r, Location::Stack(*slot));
                r
            }

            Location::MemIndexed(mem) => {
                let r = self.alloc.vreg::<Reg64>();
                self.mov(r, Location::MemIndexed(mem.clone()));
                r
            }

            Location::Address(_) => {
                panic!("materialize_value called on an address");
            }
        }
    }

    pub fn store_indexed(&mut self, base: Reg, index: Reg, scale: i32, value: Reg) {
        debug_assert!(
            [1, 2, 4, 8].contains(&scale),
            "store_indexed: scale must be 1, 2, 4, or 8"
        );

        self.push_to(
            self.current_section,
            Instruction::Mov(
                Location::MemIndexed(MemIndexed { base, index, scale }),
                value.into(),
            ),
        );
    }

    pub fn load_indexed(&mut self, base: Reg, index: Reg, scale: i32, out: Location) {
        debug_assert!(!matches!(out, Location::Stack(_) | Location::MemIndexed(_)));

        self.push_to(
            self.current_section,
            Instruction::Mov(
                out,
                Location::MemIndexed(MemIndexed::new(base, index, scale)),
            ),
        );
    }

    fn prepare_binary_operands(&mut self, dst: Location, src: Location) -> PreparedOperands {
        debug_assert!(
            !matches!(dst, Location::Imm(_)),
            "prepare_binary_operands, dst is imm"
        );

        debug_assert!(
            !matches!(dst, Location::Address(_)),
            "prepare_binary_operands: dst is Address"
        );

        debug_assert!(
            !matches!(src, Location::Address(_)),
            "prepare_binary_operands: src is Address"
        );

        // Case 1: mem ← mem into mem ← temp-reg
        if let (Location::Stack(lhs), Location::Stack(rhs)) = (&dst, &src) {
            let temp: Location = match rhs.access_size {
                1 => self.alloc.vreg::<Reg8>(),
                2 => self.alloc.vreg::<Reg16>(),
                4 => self.alloc.vreg::<Reg32>(),
                8 => self.alloc.vreg::<Reg64>(),
                _ => unreachable!(),
            }
            .into();

            // load → operate → store
            self.push_to(
                self.current_section,
                Instruction::Mov(temp.clone(), Location::Stack(*rhs)),
            );

            return PreparedOperands {
                dst: Location::Stack(*lhs),
                src: temp,
            };
        }

        // Case 2: reg ← reg width fixup
        if let (Location::Reg(dst_reg), Location::Reg(src_reg)) = (&dst, &src) {
            let fixed_src = match dst_reg.kind() {
                RegKind::Reg8 => src_reg.cast_to::<Reg8>(),
                RegKind::Reg16 => src_reg.cast_to::<Reg16>(),
                RegKind::Reg32 => src_reg.cast_to::<Reg32>(),
                RegKind::Reg64 => src_reg.cast_to::<Reg64>(),
                RegKind::Xmm => *src_reg,
            }
            .into();

            return PreparedOperands {
                dst,
                src: fixed_src,
            };
        }

        // Case 3: mem ← reg (or reg ← mem) width fixup
        if let (Location::Stack(stack), Location::Reg(reg)) = (&dst, &src) {
            let fixed = match stack.access_size {
                1 => reg.cast_to::<Reg8>(),
                2 => reg.cast_to::<Reg16>(),
                4 => reg.cast_to::<Reg32>(),
                8 => reg.cast_to::<Reg64>(),
                _ => unreachable!(),
            }
            .into();

            return PreparedOperands { dst, src: fixed };
        }

        // Case 4: reg ← mem width fixup
        if let (Location::Reg(reg), Location::Stack(stack)) = (&dst, &src) {
            let fixed_dst: Location = match stack.access_size {
                1 => reg.cast_to::<Reg8>(),
                2 => reg.cast_to::<Reg16>(),
                4 => reg.cast_to::<Reg32>(),
                8 => reg.cast_to::<Reg64>(),
                _ => unreachable!(),
            }
            .into();

            return PreparedOperands {
                dst: fixed_dst,
                src,
            };
        }

        // Case 5: already legal (reg ← imm, reg ← mem, etc.)
        PreparedOperands { dst, src }
    }

    // -----------------------------------------------------

    pub fn lea(&mut self, dst: impl Into<Location>, offset: impl Into<Location>) -> &mut Self {
        let dst = dst.into();
        self.push_to(self.current_section, Instruction::Lea(dst, offset.into()));
        self
    }

    pub fn ret(&mut self) -> &mut Self {
        self.push_to(self.current_section, Instruction::Ret);
        self
    }

    pub fn test(&mut self, lhs: impl Into<Location>, rhs: impl Into<Location>) -> &mut Self {
        let PreparedOperands { dst, src } = self.prepare_binary_operands(lhs.into(), rhs.into());
        self.push_to(self.current_section, Instruction::Test(dst, src));
        self
    }

    pub fn jmp(&mut self, label: impl Into<String>) -> &mut Self {
        self.push_to(self.current_section, Instruction::Jmp(Label(label.into())));
        self
    }

    pub fn jz(&mut self, label: impl Into<String>) -> &mut Self {
        self.push_to(self.current_section, Instruction::Jz(Label(label.into())));
        self
    }

    pub fn jnz(&mut self, label: impl Into<String>) -> &mut Self {
        self.push_to(self.current_section, Instruction::Jnz(Label(label.into())));
        self
    }

    pub fn define_label(&mut self, label: impl Into<String>) -> &mut Self {
        self.push_to(
            self.current_section,
            Instruction::DefineLabel(Label(label.into())),
        );
        self
    }

    pub fn mov(&mut self, dst: impl Into<Location>, src: impl Into<Location>) -> &mut Self {
        let PreparedOperands { dst, src } = self.prepare_binary_operands(dst.into(), src.into());
        self.push_to(self.current_section, Instruction::Mov(dst, src));

        self
    }

    pub fn movezx(&mut self, lhs: impl Into<Location>, rhs: impl Into<Location>) -> &mut Self {
        let lhs = lhs.into();
        debug_assert!(!matches!(lhs, Location::Imm(_)));
        self.push_to(self.current_section, Instruction::Movezx(lhs, rhs.into()));
        self
    }

    pub fn cmp(&mut self, lhs: impl Into<Location>, rhs: impl Into<Location>) -> &mut Self {
        let PreparedOperands { dst, src } = self.prepare_binary_operands(lhs.into(), rhs.into());
        self.push_to(self.current_section, Instruction::Cmp(dst, src));
        self
    }

    pub fn comment(&mut self, comment: impl Into<String>) -> &mut Self {
        self.push_to(self.current_section, Instruction::Comment(comment.into()));
        self
    }

    pub fn setge(&mut self, src: impl Into<Location>) -> &mut Self {
        let src = src.into();
        debug_assert!(!matches!(src, Location::Imm(_)));
        self.push_to(self.current_section, Instruction::Setge(src));
        self
    }

    pub fn setg(&mut self, src: impl Into<Location>) -> &mut Self {
        let src = src.into();
        debug_assert!(!matches!(src, Location::Imm(_)));
        self.push_to(self.current_section, Instruction::Setg(src));
        self
    }

    pub fn setl(&mut self, src: impl Into<Location>) -> &mut Self {
        let src = src.into();
        debug_assert!(!matches!(src, Location::Imm(_)));
        self.push_to(self.current_section, Instruction::Setl(src));
        self
    }

    pub fn seta(&mut self, src: impl Into<Location>) -> &mut Self {
        let src = src.into();
        self.push_to(self.current_section, Instruction::Seta(src));
        self
    }

    pub fn setae(&mut self, src: impl Into<Location>) -> &mut Self {
        let src = src.into();
        self.push_to(self.current_section, Instruction::Setae(src));
        self
    }

    pub fn setb(&mut self, src: impl Into<Location>) -> &mut Self {
        let src = src.into();
        self.push_to(self.current_section, Instruction::Setb(src));
        self
    }

    pub fn sete(&mut self, src: impl Into<Location>) -> &mut Self {
        let src = src.into();
        debug_assert!(!matches!(src, Location::Imm(_)));
        self.push_to(self.current_section, Instruction::Sete(src));
        self
    }

    pub fn and(&mut self, lhs: impl Into<Location>, rhs: impl Into<Location>) -> &mut Self {
        let PreparedOperands { dst, src } = self.prepare_binary_operands(lhs.into(), rhs.into());
        self.push_to(self.current_section, Instruction::And(dst, src));
        self
    }

    pub fn push(&mut self, src: impl Into<Location>) -> &mut Self {
        let src = src.into();
        debug_assert!(!matches!(src, Location::Imm(_)));
        self.stack_offset += 8;
        self.push_to(self.current_section, Instruction::Push(src));
        self
    }

    pub fn pop(&mut self, dst: impl Into<Location>) -> &mut Self {
        let dst = dst.into();
        debug_assert!(!matches!(dst, Location::Imm(_)));
        self.stack_offset -= 8;
        self.push_to(self.current_section, Instruction::Pop(dst));
        self
    }

    pub fn add(&mut self, lhs: impl Into<Location>, rhs: impl Into<Location>) -> &mut Self {
        let PreparedOperands { dst, src } = self.prepare_binary_operands(lhs.into(), rhs.into());
        self.push_to(self.current_section, Instruction::Add(dst, src));
        self
    }

    pub fn sub(&mut self, lhs: impl Into<Location>, rhs: impl Into<Location>) -> &mut Self {
        let PreparedOperands { dst, src } = self.prepare_binary_operands(lhs.into(), rhs.into());
        self.push_to(self.current_section, Instruction::Sub(dst, src));
        self
    }

    pub fn imul(&mut self, lhs: impl Into<Location>, rhs: impl Into<Location>) -> &mut Self {
        let PreparedOperands { dst, src } = self.prepare_binary_operands(lhs.into(), rhs.into());
        self.push_to(self.current_section, Instruction::Imul(dst, src));
        self
    }

    pub fn call(&mut self, name: impl Into<String>) -> &mut Self {
        let misalignment = (self.stack_offset + 8) % 16;
        let pad = 16 - misalignment;
        if misalignment != 0 {
            self.sub(Reg64::Rsp, pad);
        }

        self.push_to(self.current_section, Instruction::Call(name.into()));

        if misalignment != 0 {
            self.add(Reg64::Rsp, pad);
        }
        self
    }

    pub fn movd(&mut self, dst: impl Into<Location>, src: impl Into<Location>) -> &mut Self {
        self.push_to(
            self.current_section,
            Instruction::Movd(dst.into(), src.into()),
        );
        self
    }

    pub fn movss(&mut self, dst: impl Into<Location>, src: impl Into<Location>) -> &mut Self {
        self.push_to(
            self.current_section,
            Instruction::Movss(dst.into(), src.into()),
        );
        self
    }

    pub fn movsd(&mut self, dst: impl Into<Location>, src: impl Into<Location>) -> &mut Self {
        self.push_to(
            self.current_section,
            Instruction::Movsd(dst.into(), src.into()),
        );
        self
    }

    pub fn addss(&mut self, dst: impl Into<Location>, src: impl Into<Location>) -> &mut Self {
        self.push_to(
            self.current_section,
            Instruction::Addss(dst.into(), src.into()),
        );
        self
    }

    pub fn addsd(&mut self, dst: impl Into<Location>, src: impl Into<Location>) -> &mut Self {
        self.push_to(
            self.current_section,
            Instruction::Addsd(dst.into(), src.into()),
        );
        self
    }

    pub fn subss(&mut self, dst: impl Into<Location>, src: impl Into<Location>) -> &mut Self {
        self.push_to(
            self.current_section,
            Instruction::Subss(dst.into(), src.into()),
        );
        self
    }

    pub fn subsd(&mut self, dst: impl Into<Location>, src: impl Into<Location>) -> &mut Self {
        self.push_to(
            self.current_section,
            Instruction::Subsd(dst.into(), src.into()),
        );
        self
    }

    pub fn mulss(&mut self, dst: impl Into<Location>, src: impl Into<Location>) -> &mut Self {
        self.push_to(
            self.current_section,
            Instruction::Mulss(dst.into(), src.into()),
        );
        self
    }

    pub fn mulsd(&mut self, dst: impl Into<Location>, src: impl Into<Location>) -> &mut Self {
        self.push_to(
            self.current_section,
            Instruction::Mulsd(dst.into(), src.into()),
        );
        self
    }

    pub fn divss(&mut self, dst: impl Into<Location>, src: impl Into<Location>) -> &mut Self {
        self.push_to(
            self.current_section,
            Instruction::Divss(dst.into(), src.into()),
        );
        self
    }

    pub fn divsd(&mut self, dst: impl Into<Location>, src: impl Into<Location>) -> &mut Self {
        self.push_to(
            self.current_section,
            Instruction::Divsd(dst.into(), src.into()),
        );
        self
    }

    pub fn ucomiss(&mut self, dst: impl Into<Location>, src: impl Into<Location>) -> &mut Self {
        self.push_to(
            self.current_section,
            Instruction::Ucomiss(dst.into(), src.into()),
        );
        self
    }

    pub fn ucomisd(&mut self, dst: impl Into<Location>, src: impl Into<Location>) -> &mut Self {
        self.push_to(
            self.current_section,
            Instruction::Ucomisd(dst.into(), src.into()),
        );
        self
    }

    pub fn cvtsi2ss(&mut self, dst: impl Into<Location>, src: impl Into<Location>) -> &mut Self {
        self.push_to(
            self.current_section,
            Instruction::Cvtsi2ss(dst.into(), src.into()),
        );
        self
    }

    pub fn cvtsi2sd(&mut self, dst: impl Into<Location>, src: impl Into<Location>) -> &mut Self {
        self.push_to(
            self.current_section,
            Instruction::Cvtsi2sd(dst.into(), src.into()),
        );
        self
    }

    pub fn cqo(&mut self) -> &mut Self {
        self.push_to(self.current_section, Instruction::Cqo);
        self
    }

    pub fn idiv(&mut self, src: impl Into<Location>) -> &mut Self {
        self.push_to(self.current_section, Instruction::Idiv(src.into()));
        self
    }
}
