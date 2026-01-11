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
            Instruction::Add => write!(f, "@add"),
            Instruction::Alloc => write!(f, "@alloc"),
            Instruction::Call => write!(f, "@call"),
            Instruction::Cmp => write!(f, "@cmp"),
            Instruction::ElemGet => write!(f, "@elemget"),
            Instruction::ElemSet => write!(f, "@elemset"),
            Instruction::Jump => write!(f, "@jump"),
            Instruction::JumpIf => write!(f, "@jumpif"),
            Instruction::Load => write!(f, "@load"),
            Instruction::Mul => write!(f, "@mul"),
            Instruction::Phi => write!(f, "@phi"),
            Instruction::Ret => write!(f, "@ret"),
            Instruction::Sub => write!(f, "@sub"),
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
    Import,
    Function,
    Public,
}

impl std::fmt::Display for Keyword {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Keyword::Const => write!(f, "const"),
            Keyword::Import => write!(f, "import"),
            Keyword::Function => write!(f, "function"),
            Keyword::Public => write!(f, "public"),
        }
    }
}
