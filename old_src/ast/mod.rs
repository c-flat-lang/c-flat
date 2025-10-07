#![allow(unused)]
use crate::lexer::token::{Span, Token};

#[derive(Debug)]
pub enum Item {
    Function(Function),
    Type(TypeDef),
    Use(Use),
}

#[derive(Debug, Clone)]
pub struct Use {
    pub use_token: Token,
    pub path: Vec<Token>,
}

#[derive(Debug, Clone)]
pub enum TypeDef {
    Struct(Struct),
}

#[derive(Debug, Clone)]
pub struct Struct {
    pub visibility: Visibility,
    pub type_token: Token,
    pub name: Token,
    pub expr: Box<Expr>,
}

#[derive(Debug, Default, Clone, PartialEq, Eq)]
pub enum Visibility {
    Public,
    #[default]
    Private,
}

#[derive(Debug)]
pub struct Function {
    pub visibility: Visibility,
    pub fn_token: Token,
    pub name: Token,
    pub params: Vec<Param>,
    pub return_type: Type,
    pub body: Block,
}

#[derive(Debug, Clone)]
pub struct Block {
    pub open_brace: Token,
    pub statements: Vec<Statement>,
    pub close_brace: Token,
}

impl Block {
    pub fn span(&self) -> Span {
        let start = self.open_brace.span.start;
        let end = self.close_brace.span.end;
        start..end
    }
}

#[derive(Debug, Clone)]
pub struct Statement {
    pub expr: Box<Expr>,
    pub delem: Option<Token>,
}

impl Statement {
    pub fn span(&self) -> Span {
        let start = self.expr.span();
        let end = self.delem.as_ref().map(|d| d.span.end).unwrap_or(start.end);
        start.start..end
    }
}

#[derive(Debug)]
pub struct Param {
    pub name: Token,
    pub ty: Type,
}

#[derive(Debug, Default, Clone, PartialEq, Eq)]
pub enum Type {
    Bool,
    UnsignedNumber(u8),
    SignedNumber(u8),
    Float(u8),
    Array(usize, Box<Self>),
    Pointer(Box<Self>),
    Struct(String),
    Enum(String),
    #[default]
    Void,
}

impl Type {
    pub fn into_bitbox_type(&self) -> bitbox::ir::Type {
        match self {
            Self::Bool => bitbox::ir::Type::Unsigned(32),
            Self::UnsignedNumber(bytes) => bitbox::ir::Type::Unsigned(*bytes),
            Self::SignedNumber(bytes) => bitbox::ir::Type::Signed(*bytes),
            Self::Float(bytes) => bitbox::ir::Type::Float(*bytes),
            Self::Array(_, _) => todo!(),
            Self::Pointer(_) => todo!(),
            Self::Struct(_) => todo!(),
            Self::Enum(_) => todo!(),
            Self::Void => bitbox::ir::Type::Void,
        }
    }
}

impl std::fmt::Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Type::Bool => write!(f, "bool"),
            Type::UnsignedNumber(n) => write!(f, "u{}", n),
            Type::SignedNumber(n) => write!(f, "s{}", n),
            Type::Float(n) => write!(f, "f{}", n),
            Type::Array(size, ty) => write!(f, "[{}; {}]", ty, size),
            Type::Pointer(ty) => write!(f, "*{}", ty),
            Type::Struct(name) => write!(f, "{}", name),
            Type::Enum(name) => write!(f, "{}", name),
            Type::Void => write!(f, "void"),
        }
    }
}

#[derive(Debug, Clone)]
pub enum Expr {
    Return(ExprReturn),
    Struct(ExprStruct),
    Assignment(ExprAssignment),
    Litral(Litral),
    Call(ExprCall),
    Binary(ExprBinary),
    Identifier(Token),
    IfElse(ExprIfElse),
}

impl Expr {
    pub fn span(&self) -> Span {
        match self {
            Expr::Return(expr_return) => expr_return.span(),
            Expr::Struct(expr_struct) => expr_struct.span(),
            Expr::Assignment(expr_assignment) => expr_assignment.span(),
            Expr::Litral(litral) => litral.span(),
            Expr::Call(expr_call) => expr_call.span(),
            Expr::Binary(expr_binary) => expr_binary.span(),
            Expr::Identifier(token) => token.span.clone(),
            Expr::IfElse(expr_if_else) => expr_if_else.span(),
        }
    }
}

#[derive(Debug, Clone)]
pub struct ExprReturn {
    pub return_token: Token,
    pub expr: Option<Box<Expr>>,
}

impl ExprReturn {
    pub fn span(&self) -> Span {
        let start = self.return_token.span.start;
        let end = self.expr.as_ref().map(|e| e.span().end).unwrap_or(start);
        start..end
    }
}

#[derive(Debug, Clone)]
pub struct ExprStruct {
    pub struct_token: Token,
    pub fields: Vec<Field>,
}

impl ExprStruct {
    pub fn span(&self) -> Span {
        let start = self.struct_token.span.clone();
        let end = self
            .fields
            .last()
            .map(|f| f.span().end)
            .unwrap_or(start.end);
        start.start..end
    }
}

#[derive(Debug, Clone)]
pub struct Field {
    pub visibility: Visibility,
    pub name: Token,
    pub colon: Token,
    pub ty: Type,
    pub default_expr: Option<Box<Expr>>,
}

impl Field {
    pub fn span(&self) -> Span {
        let start = self.name.span.clone();
        let end = self
            .default_expr
            .as_ref()
            .map(|d| d.span().end)
            .unwrap_or(start.end);
        start.start..end
    }
}

#[derive(Debug, Clone)]
pub struct ExprAssignment {
    pub const_token: Token,
    pub ty: Option<Type>,
    pub ident: Token,
    pub expr: Box<Expr>,
}

impl ExprAssignment {
    pub fn span(&self) -> Span {
        let start = self.const_token.span.start;
        let end = self.expr.span();
        start..end.end
    }
}

#[derive(Debug, Clone)]
pub struct ExprCall {
    pub caller: Box<Expr>,
    pub left_paren: Token,
    pub args: Vec<Expr>,
    pub right_paren: Token,
}

impl ExprCall {
    pub fn span(&self) -> Span {
        let start = self.caller.span();
        let end = self.right_paren.span.end;
        start.start..end
    }
}

#[derive(Debug, Clone)]
pub struct ExprBinary {
    pub left: Box<Expr>,
    pub op: Token,
    pub right: Box<Expr>,
}

impl ExprBinary {
    pub fn span(&self) -> Span {
        let start = self.left.span();
        let end = self.right.span();
        start.start..end.end
    }
}

#[derive(Debug, Clone)]
pub struct ExprIfElse {
    pub condition: Box<Expr>,
    pub then_branch: Block,
    pub else_branch: Option<Block>,
    pub ty: Type,
}

impl ExprIfElse {
    pub fn span(&self) -> Span {
        let start = self.condition.span();
        let end = self
            .else_branch
            .as_ref()
            .map(|b| b.span())
            .unwrap_or(self.then_branch.span());
        start.start..end.end
    }
}

#[derive(Debug, Clone)]
pub enum Litral {
    String(Token),
    Integer(Token),
    Float(Token),
    Char(Token),
    BoolTrue(Token),
    BoolFalse(Token),
}

impl Litral {
    pub fn span(&self) -> Span {
        match self {
            Litral::String(token) => token.span.clone(),
            Litral::Integer(token) => token.span.clone(),
            Litral::Float(token) => token.span.clone(),
            Litral::Char(token) => token.span.clone(),
            Litral::BoolTrue(token) => token.span.clone(),
            Litral::BoolFalse(token) => token.span.clone(),
        }
    }
}
