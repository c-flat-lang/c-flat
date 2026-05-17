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
    Ampersand,
    Bang,
    BangEqual,
    Char,
    Colon,
    Comma,
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
    LeftTypeCradle,
    Less,
    LessEqual,
    Minus,
    Number,
    Percent,
    Plus,
    RightBrace,
    RightBracket,
    RightParen,
    RightTypeCradle,
    Semicolon,
    Slash,
    Star,
    String,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Keyword {
    And,
    As,
    Const,
    DeRef,
    Else,
    Enum,
    Extern,
    False,
    Fn,
    For,
    If,
    Let,
    Mut,
    Or,
    Pub,
    Ref,
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
            Self::And => write!(f, "and"),
            Self::As => write!(f, "as"),
            Self::Const => write!(f, "const"),
            Self::DeRef => write!(f, "deref"),
            Self::Else => write!(f, "else"),
            Self::Enum => write!(f, "enum"),
            Self::Extern => write!(f, "extern"),
            Self::False => write!(f, "false"),
            Self::Fn => write!(f, "fn"),
            Self::For => write!(f, "for"),
            Self::If => write!(f, "if"),
            Self::Let => write!(f, "let"),
            Self::Mut => write!(f, "mut"),
            Self::Or => write!(f, "or"),
            Self::Pub => write!(f, "pub"),
            Self::Ref => write!(f, "ref"),
            Self::Return => write!(f, "return"),
            Self::Struct => write!(f, "struct"),
            Self::True => write!(f, "true"),
            Self::Type => write!(f, "type"),
            Self::Use => write!(f, "use"),
            Self::While => write!(f, "while"),
        }
    }
}
