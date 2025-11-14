use crate::ir::{BasicBlock, Operand, Type, Variable};
use std::fmt;
use yansi::Paint;

/// All instructions in text form start with @
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Instruction {
    /// @noop
    NoOp(INoOp),
    /// `@add <type> : <des>, <lhs>, <rhs>`
    Add(IAdd),
    /// @assign <type> : <des>, <rhs>
    Assign(IAssign),
    /// @alloc <type> : <des>, <size>
    Alloc(IAlloc),
    /// @call <type> : <des> <func>(<args>)
    /// @call s32 : exit_code write(fact_result, 0)
    Call(ICall),
    /// `@cmp <type> : <des>, <lhs>, <rhs>`
    /// `@cmp u1 : is_one, n, 1`
    Cmp(ICmp),
    /// `@elemget <type> : <des>, <ptr>, <index>`
    ElemGet(IElemGet),
    /// `@elemset <type> : <addr>, <index>, <value>`
    ElemSet(IElemSet),
    /// `@gt <type> : <des>, <lhs>, <rhs>`
    /// `@gt u1 : is_one, n, 1`
    Gt(IGt),
    /// `@lt <type> : <des>, <lhs>, <rhs>`
    /// `@lt u1 : is_one, n, 1`
    Lt(ILt),
    /// @jump <label>
    /// @jump %recursive_case
    Jump(String),
    /// @jumpif <reg>, <label>
    /// @jumpif is_one, %return_one
    JumpIf(Operand, String),
    /// @load <type> : <des>, <addr>
    Load(Variable, Operand),
    /// @mul <type> : <des>, <lhs>, <rhs>
    Mul(IMul),
    /// @phi <type> : <des>, [(<var>, <label>)]
    Phi(Variable, Vec<(Variable, String)>),
    /// @ret <type> : <val>
    Return(Type, Operand),
    /// @sub <type> : <des>, <lhs>, <rhs>
    /// @sub s32 : n_minus_one, n, 1
    Sub(ISub),
    /// @div <type> : <des>, <lhs>, <rhs>
    Div(IDiv),
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

impl fmt::Display for Instruction {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Instruction::NoOp(inoop) => write!(f, "{inoop}"),
            Instruction::Add(iadd) => write!(f, "{iadd}"),
            Instruction::Sub(isub) => write!(f, "{isub}"),
            Instruction::Mul(imul) => write!(f, "{imul}"),
            Instruction::Div(idiv) => write!(f, "{idiv}"),
            Instruction::Cmp(icmp) => write!(f, "{icmp}"),
            Instruction::ElemGet(ielemget) => write!(f, "{ielemget}"),
            Instruction::ElemSet(ielemset) => write!(f, "{ielemset}"),
            Instruction::Gt(igt) => write!(f, "{igt}"),
            Instruction::Lt(ilt) => write!(f, "{ilt}"),
            Instruction::Assign(iassign) => write!(f, "{iassign}"),
            Instruction::Alloc(ialloc) => write!(f, "{ialloc}"),
            Instruction::Call(icall) => write!(f, "{icall}"),

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

// helper to print binary ops with alignment
pub fn bin_op(
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
pub fn short_uuid(s: &impl fmt::Display) -> String {
    let s = s.to_string();
    let id = if s.len() > 8 { s[..8].to_string() } else { s };
    Paint::bold(&id).cyan().to_string()
}

pub fn color_var(v: &Variable) -> impl fmt::Display + use<'_> {
    Paint::bold(v).cyan()
}

pub fn color_op(o: &Operand) -> Box<dyn fmt::Display + '_> {
    match o {
        Operand::ConstantInt { value, .. } => Box::new(Paint::yellow(value)),
        Operand::Variable(variable) => Box::new(color_var(variable)),
        Operand::None => Box::new(""),
    }
}

macro_rules! create_binary_instruction {
    ($documentation:literal, $struct_name:ident, $enum_name:ident, $name:expr) => {
        #[doc = $documentation]
        #[derive(Debug, Clone, PartialEq, Eq)]
        pub struct $struct_name {
            pub des: Variable,
            pub lhs: Operand,
            pub rhs: Operand,
        }

        impl $struct_name {
            pub fn new(des: Variable, lhs: Operand, rhs: Operand) -> Self {
                Self { des, lhs, rhs }
            }
        }

        impl fmt::Display for $struct_name {
            fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
                bin_op(f, $name, &self.des, &self.lhs, &self.rhs)
            }
        }

        impl From<$struct_name> for Instruction {
            fn from(i: $struct_name) -> Self {
                Instruction::$enum_name(i)
            }
        }
    };
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct INoOp;

impl fmt::Display for INoOp {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", Paint::blue("@noop"))
    }
}

impl From<INoOp> for Instruction {
    fn from(i: INoOp) -> Self {
        Instruction::NoOp(i)
    }
}

create_binary_instruction!("@add <type> : <des>, <lhs>, <rhs>", IAdd, Add, "add");
create_binary_instruction!(
    "@sub <type> : <des>, <lhs>, <rhs>

@sub s32 : n_minus_one, n, 1",
    ISub,
    Sub,
    "sub"
);
create_binary_instruction!("@mul <type> : <des>, <lhs>, <rhs>", IMul, Mul, "mul");
create_binary_instruction!("@div <type> : <des>, <lhs>, <rhs>", IDiv, Div, "div");

create_binary_instruction!(
    "`@cmp <type> : <des>, <lhs>, <rhs>`

`@cmp u1 : is_one, n, 1`
",
    ICmp,
    Cmp,
    "cmp"
);
create_binary_instruction!(
    "`@gt <type> : <des>, <lhs>, <rhs>`

`@gt u1 : is_one, n, 1`
",
    IGt,
    Gt,
    "gt"
);
create_binary_instruction!(
    "`@lt <type> : <des>, <lhs>, <rhs>`

`@lt u1 : is_one, n, 1`
",
    ILt,
    Lt,
    "lt"
);

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct IElemGet {
    pub des: Variable,
    pub ptr: Operand,
    pub index: Operand,
}

impl IElemGet {
    pub fn new(des: Variable, ptr: Operand, index: Operand) -> Self {
        Self { des, ptr, index }
    }
}

impl fmt::Display for IElemGet {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        bin_op(f, "elemget", &self.des, &self.ptr, &self.index)
    }
}

impl From<IElemGet> for Instruction {
    fn from(i: IElemGet) -> Self {
        Instruction::ElemGet(i)
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct IElemSet {
    pub addr: Variable,
    pub index: Operand,
    pub value: Operand,
}

impl IElemSet {
    pub fn new(addr: Variable, index: Operand, value: Operand) -> Self {
        Self { addr, index, value }
    }
}

impl fmt::Display for IElemSet {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        bin_op(f, "elemget", &self.addr, &self.index, &self.value)
    }
}

impl From<IElemSet> for Instruction {
    fn from(i: IElemSet) -> Self {
        Instruction::ElemSet(i)
    }
}

/// @assign <type> : <des>, <rhs>
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct IAssign {
    pub des: Variable,
    pub src: Operand,
}

impl IAssign {
    pub fn new(des: Variable, src: Operand) -> Self {
        Self { des, src }
    }
}

impl fmt::Display for IAssign {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{} {:<5} : {}, {}",
            Paint::blue("@assign"),
            Paint::yellow(&self.des.ty),
            color_var(&self.des),
            color_op(&self.src),
        )
    }
}

impl From<IAssign> for Instruction {
    fn from(i: IAssign) -> Self {
        Instruction::Assign(i)
    }
}

/// @alloc <type> : <des>, <size>
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct IAlloc {
    pub ty: Type,
    pub des: Variable,
    pub size: Operand,
}

impl IAlloc {
    pub fn new(ty: Type, des: Variable, size: Operand) -> Self {
        Self { ty, des, size }
    }
}

impl fmt::Display for IAlloc {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{} {:<5} : {}, {}",
            Paint::blue("@alloc"),
            Paint::yellow(&self.ty),
            color_var(&self.des),
            color_op(&self.size),
        )
    }
}

impl From<IAlloc> for Instruction {
    fn from(i: IAlloc) -> Self {
        Instruction::Alloc(i)
    }
}

/// @call <type> : <des> <func>(<args>)
/// @call s32 : exit_code write(fact_result, 0)
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ICall {
    pub des: Option<Variable>,
    pub callee: String,
    pub args: Vec<Operand>,
}

impl ICall {
    pub fn new(des: Option<Variable>, callee: String, args: Vec<Operand>) -> Self {
        Self { des, callee, args }
    }
}

impl fmt::Display for ICall {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let args_str = self
            .args
            .iter()
            .map(|a| format!("{}", color_op(a)))
            .collect::<Vec<_>>()
            .join(", ");
        if let Some(dest) = &self.des {
            return write!(
                f,
                "{} {:<5} : {}, {}({})",
                Paint::blue("@call"),
                Paint::yellow(&dest.ty),
                color_var(&dest),
                Paint::magenta(&self.callee),
                args_str
            );
        }
        write!(
            f,
            "{} {}({})",
            Paint::blue("@call"),
            Paint::magenta(&self.callee),
            args_str
        )
    }
}

impl From<ICall> for Instruction {
    fn from(i: ICall) -> Self {
        Instruction::Call(i)
    }
}
