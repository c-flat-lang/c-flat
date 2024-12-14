pub type Span = std::ops::Range<usize>;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Token {
    pub kind: TokenKind,
    pub lexeme: String,
    pub span: Span,
}

impl Token {
    pub fn new(kind: TokenKind, lexeme: impl Into<String>, span: Span) -> Self {
        Self {
            kind,
            lexeme: lexeme.into(),
            span,
        }
    }

    pub fn is_keyword(&self, expected: Keyword) -> bool {
        matches!(&self.kind, TokenKind::Keyword(actual) if actual == &expected)
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TokenKind {
    Identifier,
    Keyword(Keyword),
    Number,
    Float,
    String,
    Char,
    LeftParen,
    RightParen,
    LeftBrace,
    RightBrace,
    LeftBracket,
    RightBracket,
    Comma,
    Dot,
    Minus,
    Plus,
    Semicolon,
    Slash,
    Star,
    Bang,
    Equal,
    EqualEqual,
    Greater,
    GreaterEqual,
    Less,
    LessEqual,
    InvalidToken,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Keyword {
    Pub,
    Fn,
    Let,
    Const,
    If,
    Else,
    While,
    For,
    Return,
}
