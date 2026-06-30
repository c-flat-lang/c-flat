use report::SpanSite;
use std::fmt;

#[derive(Default, Clone, PartialEq, Eq, Hash)]
pub struct Span {
    pub start: usize,
    pub end: usize,
    pub filename: String,
}

impl fmt::Debug for Span {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("Span")
            .field("start", &self.start)
            .field("end", &self.end)
            .field("filename", &self.filename.replace('\\', "/"))
            .finish()
    }
}

impl Span {
    pub fn new(filename: impl Into<String>) -> Self {
        Self {
            filename: filename.into(),
            end: 0,
            start: 0,
        }
    }

    pub fn with_range(&mut self, range: std::ops::Range<usize>) -> &mut Self {
        self.start = range.start;
        self.end = range.end;
        self
    }
}

impl SpanSite for Span {
    fn range(&self) -> std::ops::Range<usize> {
        self.start..self.end
    }

    fn start(&self) -> usize {
        self.start
    }

    fn end(&self) -> usize {
        self.end
    }

    fn filename(&self) -> &str {
        &self.filename
    }
}

impl Iterator for Span {
    type Item = usize;

    fn next(&mut self) -> Option<Self::Item> {
        if self.start < self.end {
            let current = self.start;
            self.start += 1;
            Some(current)
        } else {
            None
        }
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        let len = self.end.saturating_sub(self.start);
        (len, Some(len))
    }
}

impl ExactSizeIterator for Span {
    fn len(&self) -> usize {
        self.end.saturating_sub(self.start)
    }
}

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
    BitShiftRight,
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
            Self::Return => write!(f, "return"),
            Self::Struct => write!(f, "struct"),
            Self::True => write!(f, "true"),
            Self::Type => write!(f, "type"),
            Self::Use => write!(f, "use"),
            Self::While => write!(f, "while"),
        }
    }
}
