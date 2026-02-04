use crate::{
    backend::x86_64::linux::passes::emit::{allocator::Allocator, assembler::RegKind},
    ir::Type,
};

use super::register::{PhysReg, Reg, Reg8, Reg16, Reg32, Reg64};

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
    base: Reg,
    index: Reg,
    scale: i32,
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
    Temp(Reg),
}

impl Location {
    pub fn is_temp(&self) -> bool {
        matches!(self, Location::Temp(_))
    }

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

    pub fn map_temp(&self, f: impl FnOnce(Reg) -> Reg) -> Self {
        match self {
            Self::Temp(reg) => Self::Temp(f(*reg)),
            _ => self.clone(),
        }
    }

    pub fn map_stack(&mut self, f: impl FnOnce(&Stack) -> Stack) -> Self {
        match self {
            Self::Stack(stack) => Location::Stack(f(&stack)),
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
            Location::Temp(reg) => write!(f, "{}", reg),
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
        self.0.replace("-", "_")
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
    Call(String),
    Cmp(Location, Location),
    Comment(String),
    DefineLabel(Label),
    Jmp(Label),
    Jnz(Label),
    Lea(Location, Location),
    Mov(Location, Location),
    Movezx(Location, Location),
    Pop(Location),
    Push(Location),
    Ret,
    Setg(Location),
    Setl(Location),
    Sub(Location, Location),
    Test(Location, Location),
}

impl std::fmt::Display for Instruction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Add(lhs, rhs) => write!(f, "  add {lhs}, {rhs}"),
            Self::Call(name) => write!(f, "  call {name}"),
            Self::Cmp(lhs, rhs) => write!(f, "  cmp {lhs}, {rhs}"),
            Self::Comment(comment) => write!(f, "  # {comment}"),
            Self::DefineLabel(label) => write!(f, "{label}:"),
            Self::Jmp(label) => write!(f, "  jmp {label}"),
            Self::Jnz(label) => write!(f, "  jnz {label}"),
            Self::Lea(dst, src) => write!(f, "  lea {dst}, {src}"),
            Self::Mov(dst, src) => write!(f, "  mov {dst}, {src}"),
            Self::Movezx(dst, src) => write!(f, "  movzx {dst}, {src}"),
            Self::Pop(dst) => write!(f, "  pop {dst}"),
            Self::Push(src) => write!(f, "  push {src}"),
            Self::Ret => write!(f, "  ret"),
            Self::Setg(dst) => write!(f, "  setg {dst}"),
            Self::Setl(dst) => write!(f, "  setl {dst}"),
            Self::Sub(lhs, rhs) => write!(f, "  sub {lhs}, {rhs}"),
            Self::Test(dst, src) => write!(f, "  test {dst}, {src}"),
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

pub struct EmitCtx<'a> {
    asm: &'a mut Assembler,
    section: FunctionSection,
}

impl<'a> EmitCtx<'a> {
    pub fn new(asm: &'a mut Assembler, section: FunctionSection) -> Self {
        Self { asm, section }
    }

    pub(crate) fn alloc(&mut self) -> &mut Allocator {
        &mut self.asm.alloc
    }

    pub fn emit(&mut self, inst: Instruction) {
        self.asm.push_to(self.section, inst);
    }

    // ---------------------------------
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
        self.asm
            .alloc
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

    pub fn materialize_address(&mut self, loc: &Location) -> Reg {
        match loc {
            Location::Address(slot) => {
                let r = self.alloc().alloc_reg::<Reg64>();
                self.lea(r, Location::Address(slot.clone()));
                r.into()
            }

            Location::Stack(slot) => {
                let r = self.alloc().alloc_reg::<Reg64>();
                self.lea(r, Location::Stack(slot.clone()));
                r.into()
            }

            Location::MemIndexed(mem) => {
                let r = self.alloc().alloc_reg::<Reg64>();
                self.lea(r, Location::MemIndexed(mem.clone()));
                r.into()
            }

            l => {
                panic!("materialize_address called on non-addressable value {l:?}");
            }
        }
    }

    pub fn materialize_value(&mut self, loc: &Location) -> Reg {
        match loc {
            Location::Reg(r) | Location::Temp(r) => r.clone(),

            Location::Imm(imm) => {
                let r = self.alloc().alloc_reg::<Reg64>();
                self.mov(r, Location::Imm(imm.clone()));
                r.into()
            }

            Location::Stack(slot) => {
                let r = self.alloc().alloc_reg::<Reg64>();
                self.mov(r, Location::Stack(slot.clone()));
                r.into()
            }

            Location::MemIndexed(mem) => {
                let r = self.alloc().alloc_reg::<Reg64>();
                self.mov(r, Location::MemIndexed(mem.clone()));
                r.into()
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

        self.emit(Instruction::Mov(
            Location::MemIndexed(MemIndexed { base, index, scale }),
            value.into(),
        ));
    }

    pub fn load_indexed(&mut self, base: Reg, index: Reg, scale: i32, out: Location) {
        debug_assert!(!matches!(out, Location::Stack(_) | Location::MemIndexed(_)));

        self.emit(Instruction::Mov(
            out,
            Location::MemIndexed(MemIndexed::new(base, index, scale)),
        ));
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
                1 => self.asm.alloc.alloc_temp_reg::<Reg8>(),
                2 => self.asm.alloc.alloc_temp_reg::<Reg16>(),
                4 => self.asm.alloc.alloc_temp_reg::<Reg32>(),
                8 => self.asm.alloc.alloc_temp_reg::<Reg64>(),
                _ => unreachable!(),
            };

            // load → operate → store
            self.emit(Instruction::Mov(temp.clone(), Location::Stack(rhs.clone())));

            return PreparedOperands {
                dst: Location::Stack(lhs.clone()),
                src: temp,
            };
        }

        // Case 2: reg ← reg width fixup
        if let (
            Location::Reg(dst_reg) | Location::Temp(dst_reg),
            Location::Reg(src_reg) | Location::Temp(src_reg),
        ) = (&dst, &src)
        {
            let fixed_src = match dst_reg.kind() {
                RegKind::Reg8 => src_reg.as_reg8().into(),
                RegKind::Reg16 => src_reg.as_reg16().into(),
                RegKind::Reg32 => src_reg.as_reg32().into(),
                RegKind::Reg64 => src_reg.as_reg64().into(),
            };

            return PreparedOperands {
                dst,
                src: fixed_src,
            };
        }

        // Case 3: mem ← reg (or reg ← mem) width fixup
        if let (Location::Stack(stack), Location::Reg(reg) | Location::Temp(reg)) = (&dst, &src) {
            let fixed = match stack.access_size {
                1 => reg.as_reg8().into(),
                2 => reg.as_reg16().into(),
                4 => reg.as_reg32().into(),
                8 => reg.as_reg64().into(),
                _ => unreachable!(),
            };

            return PreparedOperands { dst, src: fixed };
        }

        // Case 4: reg ← mem width fixup
        if let (Location::Reg(reg) | Location::Temp(reg), Location::Stack(stack)) = (&dst, &src) {
            let fixed_dst: Location = match stack.access_size {
                1 => reg.as_reg8().into(),
                2 => reg.as_reg16().into(),
                4 => reg.as_reg32().into(),
                8 => reg.as_reg64().into(),
                _ => unreachable!(),
            };

            return PreparedOperands {
                dst: fixed_dst,
                src,
            };
        }

        // Case 5: already legal (reg ← imm, reg ← mem, etc.)
        PreparedOperands { dst, src }
    }

    // ------------------

    pub fn lea(&mut self, dst: impl Into<Location>, offset: impl Into<Location>) -> &mut Self {
        let dst = dst.into();
        self.emit(Instruction::Lea(dst, offset.into()));
        self
    }

    pub fn ret(&mut self) -> &mut Self {
        self.emit(Instruction::Ret);
        self
    }

    pub fn jnz(&mut self, label: impl Into<String>) -> &mut Self {
        self.emit(Instruction::Jnz(Label(label.into())));
        self
    }

    pub fn test(&mut self, lhs: impl Into<Location>, rhs: impl Into<Location>) -> &mut Self {
        let PreparedOperands { dst, src } = self.prepare_binary_operands(lhs.into(), rhs.into());
        if let Location::Temp(r) = src {
            self.asm.alloc.free_reg(r);
        }
        self.emit(Instruction::Test(dst, src));
        self
    }

    pub fn jmp(&mut self, label: impl Into<String>) -> &mut Self {
        self.emit(Instruction::Jmp(Label(label.into())));
        self
    }

    pub fn define_label(&mut self, label: impl Into<String>) -> &mut Self {
        self.emit(Instruction::DefineLabel(Label(label.into())));
        self
    }

    pub fn mov(&mut self, dst: impl Into<Location>, src: impl Into<Location>) -> &mut Self {
        let PreparedOperands { dst, src } = self.prepare_binary_operands(dst.into(), src.into());

        if let Location::Temp(r) = src {
            self.asm.alloc.free_reg(r);
        }

        self.emit(Instruction::Mov(dst.clone(), src.clone()));

        self
    }

    pub fn movezx(&mut self, lhs: impl Into<Location>, rhs: impl Into<Location>) -> &mut Self {
        let lhs = lhs.into();
        debug_assert!(!matches!(lhs, Location::Imm(_)));
        self.emit(Instruction::Movezx(lhs, rhs.into()));
        self
    }

    pub fn cmp(&mut self, lhs: impl Into<Location>, rhs: impl Into<Location>) -> &mut Self {
        let PreparedOperands { dst, src } = self.prepare_binary_operands(lhs.into(), rhs.into());
        if let Location::Temp(r) = src {
            self.asm.alloc.free_reg(r);
        }
        self.emit(Instruction::Cmp(dst, src));
        self
    }

    pub fn comment(&mut self, comment: impl Into<String>) -> &mut Self {
        self.emit(Instruction::Comment(comment.into()));
        self
    }

    pub fn setg(&mut self, src: impl Into<Location>) -> &mut Self {
        let src = src.into();
        debug_assert!(!matches!(src, Location::Imm(_)));
        self.emit(Instruction::Setg(src));
        self
    }

    pub fn setl(&mut self, src: impl Into<Location>) -> &mut Self {
        let src = src.into();
        debug_assert!(!matches!(src, Location::Imm(_)));
        self.emit(Instruction::Setl(src));
        self
    }

    pub fn push(&mut self, src: impl Into<Location>) -> &mut Self {
        let src = src.into();
        debug_assert!(!matches!(src, Location::Imm(_)));
        self.asm.stack_offset += 8;
        self.emit(Instruction::Push(src));
        self
    }

    pub fn pop(&mut self, dst: impl Into<Location>) -> &mut Self {
        let dst = dst.into();
        debug_assert!(!matches!(dst, Location::Imm(_)));
        self.asm.stack_offset -= 8;
        self.emit(Instruction::Pop(dst));
        self
    }

    pub fn add(&mut self, lhs: impl Into<Location>, rhs: impl Into<Location>) -> &mut Self {
        let PreparedOperands { dst, src } = self.prepare_binary_operands(lhs.into(), rhs.into());
        if let Location::Temp(r) = src {
            self.asm.alloc.free_reg(r);
        }
        self.emit(Instruction::Add(dst, src));
        self
    }

    pub fn sub(&mut self, lhs: impl Into<Location>, rhs: impl Into<Location>) -> &mut Self {
        let PreparedOperands { dst, src } = self.prepare_binary_operands(lhs.into(), rhs.into());
        if let Location::Temp(r) = src {
            self.asm.alloc.free_reg(r);
        }
        self.emit(Instruction::Sub(dst, src));
        self
    }

    pub fn call(&mut self, name: impl Into<String>) -> &mut Self {
        let misalignment = (self.asm.stack_offset + 8) % 16;
        let pad = 16 - misalignment;
        if misalignment != 0 {
            self.sub(Reg64::Rsp, pad);
        }

        self.emit(Instruction::Call(name.into()));

        if misalignment != 0 {
            self.add(Reg64::Rsp, pad);
        }
        self
    }
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
    prolog: Vec<Instruction>,
    instructions: Vec<Instruction>,
    epilog: Vec<Instruction>,
    pub(crate) alloc: Allocator,
    /// changes as we push and pop stack
    pub(crate) stack_offset: i64,
}

impl std::fmt::Display for Assembler {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for i in &self.prolog {
            writeln!(f, "{}", i)?;
        }

        for i in &self.instructions {
            writeln!(f, "{}", i)?;
        }

        for i in &self.epilog {
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
            stack_offset: 0,
            alloc: Allocator::default(),
        }
    }
}

impl Assembler {
    pub fn emit(&mut self, section: FunctionSection) -> EmitCtx<'_> {
        EmitCtx::new(self, section)
    }

    fn push_to(&mut self, section: FunctionSection, inst: Instruction) {
        match section {
            FunctionSection::Prolog => self.prolog.push(inst),
            FunctionSection::Body => self.instructions.push(inst),
            FunctionSection::Epilog => self.epilog.push(inst),
        }
    }
}
