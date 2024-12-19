use crate::lexer::token::{Span, Token, TokenKind};

#[derive(Debug)]
pub enum CompilerError {
    InvalidToken(Token),
    ExpectedToken { actual: Token, expected: TokenKind },
    UnexpectedEndOfInput,
    ExpectedAType(Token),
    MissingClosingParen(Span),
}
