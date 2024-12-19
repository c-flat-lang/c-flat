#![allow(unused)]
use crate::lexer::token::{Span, Token};

#[derive(Debug)]
pub enum Item {
    Function(Function),
}

#[derive(Debug)]
pub enum Visibility {
    Public,
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
    pub delem: Token,
}

impl Statement {
    pub fn span(&self) -> Span {
        let start = self.expr.span();
        let end = self.delem.span.end;
        start.start..end
    }
}

#[derive(Debug)]
pub struct Param {
    pub name: Token,
    pub ty: Type,
}

#[derive(Debug)]
pub enum Type {
    UnsignedNumber(u8),
    SignedNumber(u8),
    Float(u8),
    Array(usize, Box<Self>),
    Pointer(Box<Self>),
    Struct(String),
    Enum(String),
    Void,
}

#[derive(Debug, Clone)]
pub enum Expr {
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
pub struct ExprAssignment {
    pub const_token: Token,
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
}

impl Litral {
    pub fn span(&self) -> Span {
        match self {
            Litral::String(token) => token.span.clone(),
            Litral::Integer(token) => token.span.clone(),
            Litral::Float(token) => token.span.clone(),
            Litral::Char(token) => token.span.clone(),
        }
    }
}
