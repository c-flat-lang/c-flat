pub type Span = std::ops::Range<usize>;

pub type SymbolTable = std::collections::HashMap<String, Symbol>;

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

#[derive(Debug, Default, Clone, PartialEq, Eq, Hash)]
pub enum Type {
    Unsigned(u8),
    Signed(u8),
    Float(u8),
    Pointer(Box<Type>),
    Array(usize, Box<Type>),
    #[default]
    Void,
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

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Identifier {
    pub name: String,
    pub span: Option<Span>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Variable {
    pub name: String,
    pub ty: Type,
    pub version: usize,
}

impl Variable {
    pub fn new(name: impl Into<String>, ty: Type) -> Self {
        Self {
            name: name.into(),
            ty,
            version: 0,
        }
    }
    pub fn new_version(self) -> Self {
        Self {
            version: self.version + 1,
            ..self
        }
    }
}

impl std::fmt::Display for Variable {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "_{}_v{}_", self.name, self.version)
    }
}

/// All instructions in text form start with @
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Instruction {
    /// @noop
    NoOp,
    /// `@add <type> : <des>, <lhs>, <rhs>`
    Add(Variable, Operand, Operand),
    /// @assign <type> : <des>, <rhs>
    Assign(Variable, Operand),
    /// @call <type> : <des> <func>(<args>)
    /// @call s32 : exit_code write(fact_result, 0)
    Call(Variable, String, Vec<Operand>),
    /// `@cmp <type> : <des>, <lhs>, <rhs>`
    /// `@cmp u1 : is_one, n, 1`
    Cmp(Variable, Operand, Operand),
    /// `@gt <type> : <des>, <lhs>, <rhs>`
    /// `@gt u1 : is_one, n, 1`
    Gt(Variable, Operand, Operand),
    /// @jump <label>
    /// @jump %recursive_case
    Jump(String),
    /// @jumpif <reg>, <label>
    /// @jumpif is_one, %return_one
    JumpIf(Operand, String),
    /// @load <type> : <des>, <addr>
    Load(Variable, Operand),
    /// @mul <type> : <des>, <lhs>, <rhs>
    Mul(Variable, Operand, Operand),
    /// @phi <type> : <des>, [(<var>, <label>)]
    Phi(Variable, Vec<(Variable, String)>),
    /// @ret <type> : <val>
    Return(Type, Operand),
    /// @sub <type> : <des>, <lhs>, <rhs>
    /// @sub s32 : n_minus_one, n, 1
    Sub(Variable, Operand, Operand),
    /// @div <type> : <des>, <lhs>, <rhs>
    Div(Variable, Operand, Operand),
}

impl std::fmt::Display for Instruction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Instruction::NoOp => write!(f, "@noop"),
            Instruction::Add(variable, operand, operand1) => write!(
                f,
                "@add {} : {}, {}, {}",
                variable.ty, variable, operand, operand1
            ),
            Instruction::Assign(variable, operand) => {
                write!(f, "@assign {} : {}, {}", variable.ty, variable, operand)
            }
            Instruction::Call(des, caller, args) => {
                write!(
                    f,
                    "@call {} : {}, {}({})",
                    des.ty,
                    des,
                    caller,
                    args.iter()
                        .map(|arg| format!("{}, ", arg))
                        .collect::<String>()
                )
            }
            Instruction::Cmp(variable, operand, operand1) => write!(
                f,
                "@cmp {} : {}, {}, {}",
                variable.ty, variable, operand, operand1
            ),
            Instruction::Gt(variable, operand, operand1) => write!(
                f,
                "@gt {} : {}, {}, {}",
                variable.ty, variable, operand, operand1
            ),
            Instruction::Jump(label) => write!(f, "@jump {}", label),
            Instruction::JumpIf(operand, label) => write!(f, "@jumpif {}, {}", operand, label),
            Instruction::Load(variable, operand) => {
                write!(f, "@load {} : {}, {}", variable.ty, variable, operand)
            }
            Instruction::Mul(variable, operand, operand1) => write!(
                f,
                "@mul {} : {}, {}, {}",
                variable.ty, variable, operand, operand1
            ),
            Instruction::Phi(variable, items) => {
                write!(
                    f,
                    "@phi {} : {}, [{}]",
                    variable.ty,
                    variable,
                    items
                        .iter()
                        .map(|(var, block)| format!("[{}, {}]", var, block))
                        .collect::<String>()
                )
            }
            Instruction::Return(ty, operand) => write!(f, "@ret {}, {}", ty, operand),
            Instruction::Sub(variable, operand, operand1) => write!(
                f,
                "@sub {} : {}, {}, {}",
                variable.ty, variable, operand, operand1
            ),
            Instruction::Div(variable, operand, operand1) => write!(
                f,
                "@div {} : {}, {}, {}",
                variable.ty, variable, operand, operand1
            ),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Operand {
    ConstantInt { value: String, ty: Type },
    Variable(Variable),
    None,
}

impl Operand {
    pub fn const_bool(value: bool) -> Self {
        if value {
            Operand::ConstantInt {
                value: "1".to_string(),
                ty: Type::Unsigned(32),
            }
        } else {
            Operand::ConstantInt {
                value: "0".to_string(),
                ty: Type::Unsigned(32),
            }
        }
    }

    pub fn const_unsigned(value: impl Into<String>, bits: u8) -> Self {
        Operand::ConstantInt {
            value: value.into(),
            ty: Type::Unsigned(bits),
        }
    }

    pub fn const_signed(value: impl Into<String>, bits: u8) -> Self {
        Operand::ConstantInt {
            value: value.into(),
            ty: Type::Signed(bits),
        }
    }

    pub fn const_float(value: impl Into<String>, bits: u8) -> Self {
        Operand::ConstantInt {
            value: value.into(),
            ty: Type::Float(bits),
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
        Operand::Variable(variable.clone())
    }
}

impl std::fmt::Display for Operand {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Operand::ConstantInt { value, .. } => write!(f, "{}", value),
            Operand::Variable(variable) => write!(f, "{}", variable),
            Operand::None => write!(f, ""),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct BlockId(pub usize);

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct BasicBlock {
    pub id: BlockId,
    pub label: String,
    pub instructions: Vec<Instruction>,
    pub successors: Vec<usize>,
    pub predecessors: Vec<usize>,
}

impl std::fmt::Display for BasicBlock {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "block {} {}:\n", self.id.0, self.label)?;

        for instruction in &self.instructions {
            write!(f, "{}\n", instruction)?;
        }

        write!(
            f,
            "in: {}\n",
            self.predecessors
                .iter()
                .map(ToString::to_string)
                .collect::<Vec<String>>()
                .join(", ")
        )?;

        write!(
            f,
            "out: {}\n",
            self.successors
                .iter()
                .map(ToString::to_string)
                .collect::<Vec<String>>()
                .join(", ")
        )?;

        Ok(())
    }
}

#[derive(Debug, Default, Clone, PartialEq, Eq)]
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

    pub locals: Vec<Variable>,
    pub symbols: SymbolTable,

    pub blocks: Vec<BasicBlock>,
}

impl std::fmt::Display for Function {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if let Visibility::Public = self.visibility {
            write!(f, "public ")?;
        }
        let args = self
            .params
            .iter()
            .map(|p| format!("{}, ", p))
            .collect::<String>();
        write!(
            f,
            "function {}({}) {}{{\n",
            self.name, args, self.return_type
        )?;
        for block in &self.blocks {
            write!(f, "  {}\n", block)?;
        }
        write!(f, "{}", "}")
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
            write!(f, "{:?}\n", import)?;
        }
        for constant in &self.constants {
            // TODO: pretty print
            write!(f, "{:?}\n", constant)?;
        }
        for function in &self.functions {
            write!(f, "{}\n", function)?;
        }
        Ok(())
    }
}
