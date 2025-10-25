pub mod builder;

pub type Span = std::ops::Range<usize>;

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
        write!(f, "{}_v{}", self.name, self.version)
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
    /// @alloc <type> : <des>
    Alloc(Type, Variable),
    /// @call <type> : <des> <func>(<args>)
    /// @call s32 : exit_code write(fact_result, 0)
    Call(Option<Variable>, String, Vec<Operand>),
    /// `@cmp <type> : <des>, <lhs>, <rhs>`
    /// `@cmp u1 : is_one, n, 1`
    Cmp(Variable, Operand, Operand),
    /// `@gt <type> : <des>, <lhs>, <rhs>`
    /// `@gt u1 : is_one, n, 1`
    Gt(Variable, Operand, Operand),
    /// `@lt <type> : <des>, <lhs>, <rhs>`
    /// `@lt u1 : is_one, n, 1`
    Lt(Variable, Operand, Operand),
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
    /// @if <cond> : <optional result> then [<then_branch>] else [<else_branch>]
    /// @if is_one : result then [@assign s32 : result, 1] else [@assign s32 : result, 0]
    /// With out a result
    /// @if condition then [...] else [...]
    IfElse {
        cond: Vec<BasicBlock>,
        cond_result: Variable,
        then_branch: Vec<BasicBlock>,
        else_branch: Vec<BasicBlock>,
        result: Option<Variable>,
    },
}

use std::fmt;
use yansi::Paint;

// helper to print binary ops with alignment
fn bin_op(
    f: &mut fmt::Formatter<'_>,
    op: &str,
    var: &crate::ir::Variable,
    a: &crate::ir::Operand,
    b: &crate::ir::Operand,
) -> fmt::Result {
    write!(
        f,
        "{} {:<5} : {}, {}, {}",
        Paint::blue(&format!("@{}", op)),
        Paint::yellow(&var.ty),
        color_var(var),
        color_op(a),
        color_op(b),
    )
}

// helper to shorten UUIDs for readability
fn short_uuid(s: &impl fmt::Display) -> String {
    let s = s.to_string();
    let id = if s.len() > 8 { s[..8].to_string() } else { s };
    Paint::bold(&id).cyan().to_string()
}

fn color_var(v: &Variable) -> impl fmt::Display + use<'_> {
    Paint::bold(v).cyan()
}

fn color_op(o: &Operand) -> Box<dyn fmt::Display + '_> {
    match o {
        Operand::ConstantInt { value, .. } => Box::new(Paint::yellow(value)),
        Operand::Variable(variable) => Box::new(color_var(variable)),
        Operand::None => Box::new(""),
    }
}

impl fmt::Display for Instruction {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Instruction::NoOp => write!(f, "{}", Paint::blue("@noop")),

            Instruction::Add(var, a, b) => bin_op(f, "add", var, a, b),
            Instruction::Sub(var, a, b) => bin_op(f, "sub", var, a, b),
            Instruction::Mul(var, a, b) => bin_op(f, "mul", var, a, b),
            Instruction::Div(var, a, b) => bin_op(f, "div", var, a, b),

            Instruction::Cmp(var, a, b) => bin_op(f, "cmp", var, a, b),
            Instruction::Gt(var, a, b) => bin_op(f, "gt", var, a, b),
            Instruction::Lt(var, a, b) => bin_op(f, "lt", var, a, b),

            Instruction::Assign(var, op) => {
                write!(
                    f,
                    "{} {:<5} : {}, {}",
                    Paint::blue("@assign"),
                    Paint::yellow(&var.ty),
                    color_var(var),
                    color_op(op),
                )
            }

            Instruction::Alloc(ty, var) => {
                write!(
                    f,
                    "{} {:<5} : {}",
                    Paint::blue("@alloc"),
                    Paint::yellow(&ty),
                    color_var(var),
                )
            }

            Instruction::Load(var, op) => {
                write!(
                    f,
                    "{} {:<5} : {}, {}",
                    Paint::blue("@load"),
                    Paint::yellow(&var.ty),
                    color_var(var),
                    color_op(op),
                )
            }

            Instruction::Call(dest, caller, args) => {
                let args_str = args
                    .iter()
                    .map(|a| format!("{}", color_op(a)))
                    .collect::<Vec<_>>()
                    .join(", ");
                if let Some(dest) = dest {
                    return write!(
                        f,
                        "{} {:<5} : {}, {}({})",
                        Paint::blue("@call"),
                        Paint::yellow(&dest.ty),
                        color_var(dest),
                        Paint::magenta(caller),
                        args_str
                    );
                }
                write!(
                    f,
                    "{} {}({})",
                    Paint::blue("@call"),
                    Paint::magenta(caller),
                    args_str
                )
            }

            Instruction::Jump(label) => write!(f, "{} {}", Paint::blue("@jump"), short_uuid(label)),
            Instruction::JumpIf(cond, label) => {
                write!(
                    f,
                    "{} {}, {}",
                    Paint::blue("@jumpif"),
                    color_op(cond),
                    short_uuid(label)
                )
            }

            Instruction::Return(ty, op) => {
                write!(
                    f,
                    "{} {:<5}, {}",
                    Paint::blue("@ret"),
                    Paint::yellow(&ty),
                    color_op(op),
                )
            }

            Instruction::Phi(var, items) => {
                writeln!(
                    f,
                    "{} {:<5} : {},",
                    Paint::blue("@phi"),
                    Paint::yellow(&var.ty),
                    color_var(var),
                )?;
                for (v, b) in items {
                    writeln!(f, "    [{}, {}]", color_var(v), short_uuid(b))?;
                }
                Ok(())
            }

            Instruction::IfElse {
                cond,
                cond_result,
                then_branch,
                else_branch,
                result,
            } => {
                let res_str = if let Some(r) = result {
                    format!(" -> {}", color_var(r))
                } else {
                    "".to_string()
                };

                // header
                writeln!(
                    f,
                    "{} {}{}:",
                    Paint::blue("@if"),
                    color_var(cond_result),
                    res_str
                )?;

                // cond instructions, only if non-empty
                if !cond.is_empty() {
                    if cond.len() > 1 {
                        writeln!(f, "  cond:")?;
                        for b in cond {
                            writeln!(f, "    block {}:", short_uuid(&b.label))?;
                            for i in &b.instructions {
                                writeln!(f, "        {}", i)?;
                            }
                        }
                    } else {
                        // inline single instruction
                        for b in cond {
                            for i in &b.instructions {
                                writeln!(f, "    {}", i)?;
                            }
                        }
                    }
                }

                // then branch
                if !then_branch.is_empty() {
                    writeln!(f, "  then:")?;
                    for b in then_branch {
                        for i in &b.instructions {
                            writeln!(f, "    {}", i)?;
                        }
                    }
                }

                // else branch
                if !else_branch.is_empty() {
                    writeln!(f, "  else:")?;
                    for b in else_branch {
                        for i in &b.instructions {
                            writeln!(f, "    {}", i)?;
                        }
                    }
                }

                Ok(())
            }
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
