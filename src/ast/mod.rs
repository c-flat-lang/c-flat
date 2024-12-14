#![allow(unused)]
use crate::lexer::token::Token;

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

#[derive(Debug)]
pub struct Block {
    pub statements: Vec<Statement>,
}

#[derive(Debug)]
pub struct Statement {
    pub expr: Expr,
    pub delem: Token,
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

#[derive(Debug)]
pub enum Expr {
    Litral(Litral),
    Call(Token, Vec<Expr>),
}

#[derive(Debug)]
pub enum Litral {
    String(Token),
}
