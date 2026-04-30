pub mod builder;
pub mod instruction;

pub use instruction::{Instruction, color_var};
use yansi::Paint;

pub type Span = std::ops::Range<usize>;

#[macro_export]
macro_rules! ty {
    // signed integers: i 1, s 32, s 64, etc
    (s $bits:literal) => {
        $crate::ir::Type::Signed($bits)
    };

    // unsigned integers: u 1, u 32, u 64, etc
    (u $bits:literal) => {
        $crate::ir::Type::Unsigned($bits)
    };

    // floats: f 32, f 64
    (f $bits:literal) => {
        $crate::ir::Type::Float($bits)
    };

    // pointer: *T
    (* $inner:tt) => {
        $crate::ir::Type::Pointer(Box::new(ty!($inner)))
    };

    // array: [N x T]
    ([ $len:literal x $elem:tt ]) => {
        $crate::ir::Type::Array($len, Box::new(ty!($elem)))
    };

    // void
    (void) => {
        $crate::ir::Type::Void
    };
}

#[macro_export]
macro_rules! var {
    ($name:literal : $($ty:tt)+) => {
        $crate::ir::Variable::new($name.to_string(), ty!($($ty)+))
    };
}

#[macro_export]
macro_rules! tmp {
    ($asm:expr, $($ty:tt)+) => {
        $asm.var(ty!($($ty)+))
    };
}

#[macro_export]
macro_rules! op {
    ($value:literal : $($ty:tt)+) => {
        $crate::ir::Operand::ConstantInt($crate::ir::ConstantInt {
            value: $value,
            ty: ty!($($ty)+),
        })
    };
    ($var:expr) => {
        $crate::ir::Operand::Variable($var)
    };
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Symbol {
    Label { name: String, index: usize },
    Variable(Variable),
}

impl Symbol {
    pub fn label(label: &str, index: usize) -> Symbol {
        Symbol::Label {
            name: label.to_string(),
            index,
        }
    }
}

#[derive(Debug, Default, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub enum Type {
    Unsigned(u8),
    Signed(u8),
    Float(u8),
    Pointer(Box<Type>),
    Array(usize, Box<Type>),
    Struct(StructType),
    #[default]
    Void,
}

impl Type {
    pub fn size(&self) -> i32 {
        match self {
            Self::Unsigned(bits) => *bits as i32 / 8,
            Self::Signed(bits) => *bits as i32 / 8,
            Self::Float(bits) => *bits as i32 / 8,
            Self::Pointer(..) => 64,
            Self::Array(size, ty) => ty.size() * (*size as i32),
            Self::Struct(s) => s.size(),
            Self::Void => 0,
        }
    }

    pub fn element_size(&self) -> i32 {
        match self {
            Self::Unsigned(bits) => *bits as i32 / 8,
            Self::Signed(bits) => *bits as i32 / 8,
            Self::Float(bits) => *bits as i32 / 8,
            Self::Pointer(..) => 64,
            Self::Array(_, ty) => ty.element_size(),
            Self::Struct(s) => s.size(),
            Self::Void => 0,
        }
    }

    pub fn is_ptr(&self) -> bool {
        matches!(self, Self::Pointer(_))
    }
}

impl std::fmt::Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Unsigned(bytes) => write!(f, "u{}", bytes),
            Self::Signed(bytes) => write!(f, "s{}", bytes),
            Self::Float(bytes) => write!(f, "f{}", bytes),
            Self::Pointer(ty) => write!(f, "*{}", ty),
            Self::Array(size, ty) => write!(f, "[{} x {}]", size, ty),
            Self::Struct(s) => write!(f, "{}", s),
            Self::Void => write!(f, "void"),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct StructType {
    pub name: String,
    pub fields: Vec<(String, Type)>,
    pub packed: bool,
}

/// System V AMD64 ABI chunk classification for register-passing.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum AbiChunk {
    /// INTEGER class: passed in next GP register (rdi, rsi, ...).
    Integer,
    /// SSE class: passed in next XMM register (xmm0, xmm1, ...).
    Sse,
}

impl StructType {
    /// Total byte size of this struct (sum of all field sizes, no padding yet).
    pub fn size(&self) -> i32 {
        self.fields.iter().map(|(_, ty)| ty.size()).sum()
    }

    /// Byte offset of field at position `idx` within the struct.
    pub fn field_offset(&self, idx: usize) -> i32 {
        self.fields[..idx].iter().map(|(_, ty)| ty.size()).sum()
    }

    /// Returns the SysV AMD64 ABI chunks for passing this struct in registers.
    /// Returns None if the struct is > 16 bytes (MEMORY class — must be passed by pointer).
    pub fn abi_chunks(&self) -> Option<Vec<AbiChunk>> {
        let size = self.size();
        if size > 16 {
            return None;
        }
        let num_chunks = ((size as usize) + 7) / 8;
        let mut chunks = vec![AbiChunk::Sse; num_chunks];
        let mut byte_offset = 0usize;
        for (_, field_ty) in &self.fields {
            let chunk_idx = byte_offset / 8;
            match field_ty {
                Type::Float(_) => {}                        // SSE stays
                _ => chunks[chunk_idx] = AbiChunk::Integer, // INTEGER dominates
            }
            byte_offset += field_ty.size() as usize;
        }
        Some(chunks)
    }
}

impl std::fmt::Display for StructType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.name)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Variable {
    pub name: String,
    pub ty: Type,
    pub version: usize,
    pub temporary: bool,
}

impl Variable {
    pub fn new(name: impl Into<String>, ty: Type) -> Self {
        Self {
            name: name.into(),
            ty,
            version: 0,
            temporary: false,
        }
    }

    pub fn versioned_to(mut self, version: usize) -> Self {
        self.version = version;
        self
    }

    pub fn temp(mut self) -> Self {
        self.temporary = true;
        self
    }

    pub fn new_version(&self) -> Self {
        Self {
            version: self.version + 1,
            ..self.clone()
        }
    }
}

impl std::fmt::Display for Variable {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}_v{}", self.name, self.version)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ConstantInt {
    pub value: String,
    pub ty: Type,
}

impl ConstantInt {
    pub fn new(value: impl Into<String>, ty: Type) -> Self {
        Self {
            value: value.into(),
            ty,
        }
    }

    pub fn parse<T>(&self) -> Result<T, crate::error::Error>
    where
        T: std::str::FromStr,
    {
        let value = self.value.replace("_", "");
        value
            .parse::<T>()
            .map_err(|_| crate::error::Error::MalformedNumber {
                value: self.value.clone(),
                ty: self.ty.clone(),
            })
    }
}

impl From<(&str, Type)> for ConstantInt {
    fn from((value, ty): (&str, Type)) -> Self {
        ConstantInt {
            value: value.to_string(),
            ty,
        }
    }
}

impl std::fmt::Display for ConstantInt {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.value)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Operand {
    ConstantInt(ConstantInt),
    Variable(Variable),
    None,
}

impl Operand {
    pub fn const_bool(value: bool) -> Self {
        if value {
            Operand::ConstantInt(ConstantInt {
                value: "1".to_string(),
                ty: Type::Unsigned(32),
            })
        } else {
            Operand::ConstantInt(ConstantInt {
                value: "0".to_string(),
                ty: Type::Unsigned(32),
            })
        }
    }

    pub fn const_unsigned(value: impl Into<String>, bits: u8) -> Self {
        Operand::ConstantInt(ConstantInt::from((
            value.into().as_str(),
            Type::Unsigned(bits),
        )))
    }

    pub fn const_signed(value: impl Into<String>, bits: u8) -> Self {
        Operand::ConstantInt(ConstantInt::from((
            value.into().as_str(),
            Type::Signed(bits),
        )))
    }

    pub fn const_float(value: impl Into<String>, bits: u8) -> Self {
        Operand::ConstantInt(ConstantInt::from((
            value.into().as_str(),
            Type::Float(bits),
        )))
    }

    pub fn is_variable(&self) -> bool {
        matches!(self, Operand::Variable(_))
    }

    pub fn is_none(&self) -> bool {
        matches!(self, Operand::None)
    }

    pub fn ty(&self) -> Option<&Type> {
        match self {
            Operand::ConstantInt(c) => Some(&c.ty),
            Operand::Variable(v) => Some(&v.ty),
            Operand::None => None,
        }
    }
}

impl From<Variable> for Operand {
    fn from(variable: Variable) -> Self {
        Operand::Variable(variable)
    }
}

impl From<&Variable> for Operand {
    fn from(variable: &Variable) -> Self {
        Operand::from(variable.clone())
    }
}

impl From<ConstantInt> for Operand {
    fn from(constant: ConstantInt) -> Self {
        Operand::ConstantInt(constant)
    }
}

impl From<&ConstantInt> for Operand {
    fn from(constant: &ConstantInt) -> Self {
        Operand::from(constant.clone())
    }
}

impl std::fmt::Display for Operand {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Operand::ConstantInt(constant) => write!(f, "{}", constant),
            Operand::Variable(variable) => write!(f, "{}", variable),
            Operand::None => write!(f, ""),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct BlockId(pub usize);

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct BasicBlock {
    pub id: BlockId,
    pub label: String,
    pub instructions: Vec<Instruction>,
}

impl std::fmt::Display for BasicBlock {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(
            f,
            "%{}: {}",
            Paint::yellow(&self.label),
            Paint::dim(&format!("// {}", &self.id.0)).rgb(128, 128, 128)
        )?;

        for instruction in &self.instructions {
            writeln!(f, "    {}", instruction)?;
        }

        Ok(())
    }
}

#[derive(Debug, Default, Clone, Copy, PartialEq, Eq)]
pub enum Visibility {
    Public,
    #[default]
    Private,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Function {
    pub visibility: Visibility,
    pub name: String,
    pub params: Vec<Variable>,
    pub return_type: Type,
    pub blocks: Vec<BasicBlock>,
}

impl std::fmt::Display for Function {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if let Visibility::Public = self.visibility {
            write!(f, "{} ", Paint::red("public"))?;
        }
        let args = self
            .params
            .iter()
            .map(|p| format!("{}: {}, ", color_var(p), Paint::yellow(&p.ty)))
            .collect::<String>();
        writeln!(
            f,
            "{} {}({}) {}{{",
            Paint::red("function"),
            Paint::magenta(&self.name),
            args,
            self.return_type
        )?;
        for block in &self.blocks {
            write!(f, "  {}", block)?;
        }
        write!(f, "}}")
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FunctionSpec {
    pub module_name: String,
    pub name: String,
    pub params: Vec<Type>,
    pub return_type: Type,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Import {
    Function(FunctionSpec),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Directive {
    Len(String),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ConstantValue {
    String(String),
    Number(String),
    Directive(Directive),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Constant {
    pub name: String,
    pub ty: Type,
    pub value: ConstantValue,
}

#[derive(Debug, Default, Clone, PartialEq, Eq)]
pub struct Module {
    pub imports: Vec<Import>,
    pub constants: Vec<Constant>,
    pub externs: Vec<ExternDecl>,
    pub functions: Vec<Function>,
}

/// A declaration of an external C symbol resolved at link time.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ExternDecl {
    pub name: String,
    pub params: Vec<Type>,
    pub return_type: Type,
}

impl std::fmt::Display for Module {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for import in &self.imports {
            // TODO: pretty print
            writeln!(f, "{:?}", import)?;
        }
        for ext in &self.externs {
            let params = ext
                .params
                .iter()
                .map(|t| t.to_string())
                .collect::<Vec<_>>()
                .join(", ");
            writeln!(f, "extern {} ({}) -> {}", ext.name, params, ext.return_type)?;
        }
        for constant in &self.constants {
            // TODO: pretty print
            writeln!(f, "{:?}", constant)?;
        }
        for function in &self.functions {
            writeln!(f, "{}", function)?;
        }
        Ok(())
    }
}
