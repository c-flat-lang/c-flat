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
    LeftBrace,
    LeftBracket,
    LeftParen,
    Number,
    PathSeparator,
    Plus,
    RightBrace,
    RightBracket,
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
