use crate::lexer::token::{Token, TokenKind};

#[derive(Debug)]
pub enum CompilerError {
    InvalidToken(Token),
    ExpectedToken { actual: Token, expected: TokenKind },
    UnexpectedEndOfInput,
    ExpectedAType(Token),
}
