use crate::lexer::token::Token;

pub enum CompilerError {
    InvalidToken(Token),
}
