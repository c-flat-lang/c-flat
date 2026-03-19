pub mod builder;
pub mod instruction;

pub use instruction::{Instruction, color_var};
use yansi::Paint;

pub type Span = std::ops::Range<usize>;

#[macro_export]
macro_rules! ty {
    // signed integers: i 1, i 32, i 64, etc
    (i $bits:literal) => {
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
    ($name:literal : $ty:tt) => {
        $crate::ir::Variable::new($name.to_string(), ty!($ty))
    };
}

#[macro_export]
macro_rules! tmp {
    ($asm:expr, $ty:tt) => {
        $asm.var(ty!($ty))
    };
}

#[macro_export]
macro_rules! op {
    ($value:literal : $ty:tt) => {
        $crate::ir::Operand::ConstantInt($crate::ir::ConstantInt {
            value: $value,
            ty: ty!($ty),
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
    #[default]
    Void,
}

impl Type {
    pub fn size(&self) -> i32 {
        match self {
            Type::Unsigned(bits) => *bits as i32 / 8,
            Type::Signed(bits) => *bits as i32 / 8,
            Type::Float(bits) => *bits as i32 / 8,
            Type::Pointer(..) => 64,
            Type::Array(size, ty) => ty.size() * (*size as i32),
            Type::Void => 0,
        }
    }

    pub fn element_size(&self) -> i32 {
        match self {
            Type::Unsigned(bits) => *bits as i32 / 8,
            Type::Signed(bits) => *bits as i32 / 8,
            Type::Float(bits) => *bits as i32 / 8,
            Type::Pointer(..) => 64,
            Type::Array(_, ty) => ty.element_size(),
            Type::Void => 0,
        }
    }

    pub fn is_ptr(&self) -> bool {
        matches!(self, Type::Pointer(_))
    }
}

impl std::fmt::Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Type::Unsigned(bytes) => write!(f, "u{}", bytes),
            Type::Signed(bytes) => write!(f, "s{}", bytes),
            Type::Float(bytes) => write!(f, "f{}", bytes),
            Type::Pointer(ty) => write!(f, "*{}", ty),
            Type::Array(size, ty) => write!(f, "[{} x {}]", size, ty),
            Type::Void => write!(f, "void"),
        }
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
    pub value: i64,
    pub ty: Type,
}

impl ConstantInt {
    pub fn new(value: i64, ty: Type) -> Self {
        Self { value, ty }
    }
}

impl From<(&str, Type)> for ConstantInt {
    fn from((value, ty): (&str, Type)) -> Self {
        let value = value.replace("_", "");
        if let Some(stripped) = value.strip_prefix("0x") {
            return ConstantInt {
                value: i64::from_str_radix(stripped, 16).unwrap(),
                ty,
            };
        }
        ConstantInt {
            value: value.parse().unwrap(),
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
                value: 1,
                ty: Type::Unsigned(32),
            })
        } else {
            Operand::ConstantInt(ConstantInt {
                value: 0,
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
            "{} {} {}:",
            Paint::red("block"),
            Paint::bold(&self.id.0),
            Paint::yellow(&self.label)
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
    pub functions: Vec<Function>,
}

impl std::fmt::Display for Module {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for import in &self.imports {
            // TODO: pretty print
            writeln!(f, "{:?}", import)?;
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
