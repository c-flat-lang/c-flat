use super::token::{Directive, Instruction, Keyword, Span, Token, TokenKind};

pub struct Lexer<'a> {
    chars: std::iter::Peekable<std::str::Chars<'a>>,
    span: Span,
}

impl<'a> Lexer<'a> {
    pub fn new(input: &'a str) -> Self {
        Self {
            chars: input.chars().peekable(),
            span: 0..0,
        }
    }

    fn next(&mut self) -> Option<char> {
        let value = self.chars.next();
        if value.is_some() {
            self.span.end += 1;
        }
        value
    }

    fn next_if(&mut self, predicate: impl Fn(char) -> bool) -> Option<char> {
        match self.chars.peek() {
            Some(value) if predicate(*value) => self.next(),
            _ => None,
        }
    }

    fn peek(&mut self, expected: char) -> bool {
        matches!(self.chars.peek(), Some(value) if *value == expected)
    }

    fn spanned(&mut self, kind: TokenKind, lexeme: impl Into<String>) -> Token {
        let span = self.span.clone();
        self.span = self.span.end..self.span.end;
        Token {
            span,
            kind,
            lexeme: lexeme.into(),
        }
    }

    fn parse_number(&mut self, value: char) -> Token {
        let mut lexeme = String::from(value);

        while let Some(value) =
            self.next_if(|value| value.is_ascii_digit() || ['.', '_'].contains(&value))
        {
            lexeme.push(value);
        }

        self.spanned(TokenKind::Number, lexeme)
    }

    fn parse_identifier(&mut self, value: char) -> Token {
        let mut lexeme = String::from(value);
        while let Some(value) = self.next_if(|value| value.is_ascii_alphanumeric() || value == '_')
        {
            lexeme.push(value);
        }

        match lexeme.as_str() {
            "import" => self.spanned(TokenKind::Keyword(Keyword::Import), lexeme),
            "const" => self.spanned(TokenKind::Keyword(Keyword::Const), lexeme),
            "function" => self.spanned(TokenKind::Keyword(Keyword::Function), lexeme),
            "public" => self.spanned(TokenKind::Keyword(Keyword::Public), lexeme),
            _ => self.spanned(TokenKind::Identifier, lexeme),
        }
    }

    fn parse_builtin(&mut self) -> Token {
        let mut lexeme = String::from('@');
        while let Some(value) = self.next_if(|value| value.is_ascii_alphanumeric() || value == '_')
        {
            lexeme.push(value);
        }

        let kind = match lexeme.as_str() {
            "@add" => Instruction::Add,
            "@call" => Instruction::Call,
            "@cmp" => Instruction::Cmp,
            "@jump" => Instruction::Jump,
            "@jumpif" => Instruction::JumpIf,
            "@load" => Instruction::Load,
            "@mul" => Instruction::Mul,
            "@ret" => Instruction::Ret,
            "@sub" => Instruction::Sub,
            _ => return self.spanned(TokenKind::InvalidToken, lexeme),
        };

        self.spanned(TokenKind::Instruction(kind), lexeme)
    }

    fn parse_string(&mut self) -> Token {
        let mut lexeme = String::from('#');
        while let Some(value) = self.next() {
            lexeme.push(value);
            if lexeme.ends_with("\"#") {
                break;
            }
        }
        let lexeme = lexeme[2..lexeme.len() - 2].replace("\\n", "\n");
        self.spanned(TokenKind::String, lexeme)
    }

    fn parse_directive(&mut self) -> Token {
        let mut lexeme = String::from('.');
        while let Some(value) = self.next_if(|value| value.is_ascii_alphanumeric() || value == '_')
        {
            lexeme.push(value);
        }

        let kind = match lexeme.as_str() {
            ".len" => Directive::Len,
            _ => return self.spanned(TokenKind::InvalidToken, lexeme),
        };

        self.spanned(TokenKind::Directive(kind), lexeme)
    }

    fn parse_delimiter(&mut self) -> Token {
        while let Some(_) = self.next_if(|value| value == '\n') {}
        self.spanned(TokenKind::Delimiter, "\\n")
    }

    fn parser_char_delemiter(&mut self, kind: TokenKind, char: char) -> Option<Token> {
        while let Some(_) = self.next_if(|value| value == '\n') {}
        Some(self.spanned(kind, char.to_string()))
    }

    fn parse_label(&mut self) -> Token {
        let mut lexeme = String::new();
        while let Some(value) = self.next_if(|value| value.is_ascii_alphanumeric() || value == '_')
        {
            lexeme.push(value);
        }

        if self.peek(':') {
            self.next();
            return self.spanned(TokenKind::LabelDefinition, lexeme);
        }
        self.spanned(TokenKind::Label, lexeme)
    }

    fn skip_char(&mut self) -> Option<Token> {
        self.spanned(TokenKind::InvalidToken, ' ');
        self.parse()
    }

    fn double_char(&mut self, kind: TokenKind, value: impl Into<String>) -> Option<Token> {
        self.next();
        Some(self.spanned(kind, value))
    }

    fn comment(&mut self) -> Option<Token> {
        while let Some(value) = self.next() {
            if value == '\n' {
                break;
            }
        }
        self.parse()
    }

    fn parse(&mut self) -> Option<Token> {
        match self.next() {
            Some(value @ '0'..='9') => Some(self.parse_number(value)),
            Some(value) if value.is_ascii_alphabetic() => Some(self.parse_identifier(value)),
            Some('\n') => Some(self.parse_delimiter()),
            Some(value) if value.is_ascii_whitespace() => self.skip_char(),
            Some('#') if self.peek('"') => Some(self.parse_string()),
            Some('/') if self.peek('/') => self.comment(),
            Some('.') if !self.peek(' ') => Some(self.parse_directive()),
            Some(':') if self.peek(':') => self.double_char(TokenKind::PathSeparator, "::"),
            Some('%') => Some(self.parse_label()),
            Some('@') => Some(self.parse_builtin()),
            Some('+') => Some(self.spanned(TokenKind::Plus, '+')),
            Some('(') => Some(self.spanned(TokenKind::LeftParen, '(')),
            Some(')') => Some(self.spanned(TokenKind::RightParen, ')')),
            Some('{') if self.peek('\n') => self.parser_char_delemiter(TokenKind::LeftBrace, '{'),
            Some('}') if self.peek('\n') => self.parser_char_delemiter(TokenKind::RightBrace, '}'),
            Some('[') => Some(self.spanned(TokenKind::LeftBracket, '[')),
            Some(']') => Some(self.spanned(TokenKind::RightBracket, ']')),
            Some(':') => Some(self.spanned(TokenKind::Colon, ':')),
            Some(';') => Some(self.spanned(TokenKind::Semicolon, ';')),
            Some(',') => Some(self.spanned(TokenKind::Comma, ',')),
            Some('=') => Some(self.spanned(TokenKind::Equals, '=')),
            Some('.') => Some(self.spanned(TokenKind::Dot, '.')),
            Some('*') => Some(self.spanned(TokenKind::Star, '*')),
            Some(value) => Some(self.spanned(TokenKind::InvalidToken, value)),
            None => None,
        }
    }
}

impl Iterator for Lexer<'_> {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        self.parse()
    }
}
