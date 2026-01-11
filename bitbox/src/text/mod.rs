use crate::{ir, text::lexer::token::Token};

pub mod emitter;
pub mod error;
pub mod lexer;
pub mod parser;
pub mod semantic_analyzer;

impl From<&str> for ir::Type {
    fn from(s: &str) -> Self {
        if let Some(stripped) = s.strip_prefix("u") {
            ir::Type::Unsigned(stripped.parse().unwrap())
        } else if let Some(stripped) = s.strip_prefix("s") {
            ir::Type::Signed(stripped.parse().unwrap())
        } else if let Some(stripped) = s.strip_prefix("f") {
            ir::Type::Float(stripped.parse().unwrap())
        } else {
            unreachable!()
        }
    }
}

impl From<&Token> for ir::Type {
    fn from(s: &Token) -> Self {
        ir::Type::from(s.lexeme.as_str())
    }
}
