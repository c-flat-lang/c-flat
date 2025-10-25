use crate::ir;
use crate::text::lexer::token::{Instruction as TokenInstruction, Token};

#[derive(Debug, Default)]
pub struct Module {
    pub functions: Vec<Function>,
    pub imports: Vec<Import>,
    pub constants: Vec<Constant>,
}

/// All instructions are the same
/// @jump <target>
/// @add <type> : <des>, <lhs>, <rhs>
/// @call <type> : <des> <func>(<args>)
/// @load <type> : <des>, <addr>
/// This are all tokens.
/// instuction kind -> @jump
/// arguments -> <target> which is a token
/// or
/// instuction kind -> @call
/// arguments -> <type> : <des> <func>(<args>)
///               ^^^^     ^^^   ^^^^   ^^^^
///               All tokens
#[derive(Debug)]
pub struct Instruction {
    pub instruction_kind: TokenInstruction,
    pub arguments: Vec<Token>,
}

#[derive(Debug)]
pub struct BasicBlock {
    pub label: Token,
    pub instructions: Vec<Instruction>,
}

#[derive(Debug)]
pub struct Function {
    pub name: Token,
    pub visibility: ir::Visibility,
    pub params: Vec<(Token, Token)>,
    pub return_type: Token,
    pub block: Vec<BasicBlock>,
}

#[derive(Debug)]
pub enum ImportKind {
    Function,
}

#[derive(Debug)]
pub struct Import {
    pub module_name: Token,
    pub kind: ImportKind,
    pub name: Token,
    pub params: Vec<Token>,
    pub return_type: Token,
}

#[derive(Debug)]
pub struct Constant {
    pub name: Token,
    pub ty: Token,
    pub value: Token,
}
