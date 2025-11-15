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

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TokenKind {
    Percent,
    Bang,
    BangEqual,
    Char,
    Comma,
    Colon,
    Dot,
    Equal,
    EqualEqual,
    Float,
    Greater,
    GreaterEqual,
    Identifier,
    InvalidToken,
    Keyword(Keyword),
    LeftBrace,
    LeftBracket,
    LeftParen,
    Less,
    LessEqual,
    Minus,
    Number,
    Plus,
    RightBrace,
    RightBracket,
    RightParen,
    Semicolon,
    Slash,
    Star,
    String,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Keyword {
    Const,
    Else,
    Enum,
    False,
    Fn,
    For,
    If,
    Let,
    Mut,
    Pub,
    Return,
    Struct,
    True,
    Type,
    Use,
    While,
}

impl std::fmt::Display for Keyword {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Keyword::Const => write!(f, "const"),
            Keyword::Else => write!(f, "else"),
            Keyword::Enum => write!(f, "enum"),
            Keyword::False => write!(f, "false"),
            Keyword::Fn => write!(f, "fn"),
            Keyword::For => write!(f, "for"),
            Keyword::If => write!(f, "if"),
            Keyword::Let => write!(f, "let"),
            Keyword::Mut => write!(f, "mut"),
            Keyword::Pub => write!(f, "pub"),
            Keyword::Return => write!(f, "return"),
            Keyword::Struct => write!(f, "struct"),
            Keyword::True => write!(f, "true"),
            Keyword::Type => write!(f, "type"),
            Keyword::Use => write!(f, "use"),
            Keyword::While => write!(f, "while"),
        }
    }
}
