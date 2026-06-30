pub type Span = std::ops::Range<usize>;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Token {
    pub kind: TokenKind,
    pub lexeme: String,
    pub span: Span,
}

impl Token {
    pub fn is_keyword(&self, keyword: Keyword) -> bool {
        matches!(self.kind, TokenKind::Keyword(ref k) if k == &keyword)
    }

    pub fn is_instruction(&self, instruction: Instruction) -> bool {
        matches!(self.kind, TokenKind::Instruction(ref i) if i == &instruction)
    }

    pub fn is_directive(&self, directive: Directive) -> bool {
        matches!(self.kind, TokenKind::Directive(ref d) if d == &directive)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum TokenKind {
    Colon,
    Comma,
    Delimiter,
    Directive(Directive),
    Dot,
    Equals,
    Identifier,
    Instruction(Instruction),
    InvalidToken,
    Keyword(Keyword),
    Label,
    LabelDefinition,
    /// '{'
    LeftBrace,
    /// '['
    LeftBracket,
    /// '('
    LeftParen,
    Number,
    PathSeparator,
    Plus,
    /// '}'
    RightBrace,
    /// ']'
    RightBracket,
    /// ')'
    RightParen,
    Semicolon,
    Star,
    String,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Instruction {
    Add,
    Alloc,
    Assign,
    Call,
    Cmp,
    ElemGet,
    ElemSet,
    Jump,
    JumpIf,
    Load,
    Mul,
    Phi,
    Ret,
    Sub,
}

impl std::fmt::Display for Instruction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Add => write!(f, "@add"),
            Self::Alloc => write!(f, "@alloc"),
            Self::Assign => write!(f, "@assign"),
            Self::Call => write!(f, "@call"),
            Self::Cmp => write!(f, "@cmp"),
            Self::ElemGet => write!(f, "@elemget"),
            Self::ElemSet => write!(f, "@elemset"),
            Self::Jump => write!(f, "@jump"),
            Self::JumpIf => write!(f, "@jumpif"),
            Self::Load => write!(f, "@load"),
            Self::Mul => write!(f, "@mul"),
            Self::Phi => write!(f, "@phi"),
            Self::Ret => write!(f, "@ret"),
            Self::Sub => write!(f, "@sub"),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Directive {
    Len,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Keyword {
    Const,
    Extern,
    Import,
    Function,
    Public,
    Then,
    Else,
}

impl std::fmt::Display for Keyword {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Keyword::Const => write!(f, "const"),
            Keyword::Extern => write!(f, "extern"),
            Keyword::Import => write!(f, "import"),
            Keyword::Function => write!(f, "function"),
            Keyword::Public => write!(f, "public"),
            Keyword::Then => write!(f, "then"),
            Keyword::Else => write!(f, "else"),
        }
    }
}
