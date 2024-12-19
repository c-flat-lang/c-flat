use super::token::{Keyword, Span, Token, TokenKind};
pub struct Tokenizer<'a> {
    chars: std::iter::Peekable<std::str::Chars<'a>>,
    span: Span,
}

impl<'a> Tokenizer<'a> {
    pub fn new(src: &'a str) -> Self {
        Self {
            chars: src.chars().peekable(),
            span: 0..0,
        }
    }

    fn next_char(&mut self) -> Option<char> {
        let value = self.chars.next();
        if value.is_some() {
            self.span.end += 1;
        }
        value
    }

    fn next_char_if(&mut self, predicate: impl Fn(char) -> bool) -> Option<char> {
        match self.chars.peek() {
            Some(value) if predicate(*value) => self.next_char(),
            _ => None,
        }
    }

    fn peek_char(&mut self, expected: char) -> bool {
        matches!(self.chars.peek(), Some(actual) if actual == &expected)
    }

    fn spanned(&mut self, kind: TokenKind, lexeme: impl Into<String>) -> Token {
        let span = self.span.clone();
        self.span = self.span.end..self.span.end;
        Token::new(kind, lexeme, span)
    }

    fn parse_number(&mut self, value: char) -> Token {
        let mut lexeme = String::from(value);

        while let Some(value) =
            self.next_char_if(|value| value.is_ascii_digit() || ['.', '_'].contains(&value))
        {
            lexeme.push(value);
        }

        if lexeme.contains('.') {
            return self.spanned(TokenKind::Float, lexeme);
        }

        self.spanned(TokenKind::Number, lexeme)
    }

    fn parse_identifier(&mut self, value: char) -> Token {
        let mut lexeme = String::from(value);
        while let Some(value) =
            self.next_char_if(|value| value.is_ascii_alphanumeric() || value == '_')
        {
            lexeme.push(value);
        }

        let kind = match lexeme.as_str() {
            "const" => TokenKind::Keyword(Keyword::Const),
            "else" => TokenKind::Keyword(Keyword::Else),
            "false" => TokenKind::Keyword(Keyword::False),
            "fn" => TokenKind::Keyword(Keyword::Fn),
            "for" => TokenKind::Keyword(Keyword::For),
            "if" => TokenKind::Keyword(Keyword::If),
            "let" => TokenKind::Keyword(Keyword::Let),
            "pub" => TokenKind::Keyword(Keyword::Pub),
            "return" => TokenKind::Keyword(Keyword::Return),
            "true" => TokenKind::Keyword(Keyword::True),
            "while" => TokenKind::Keyword(Keyword::While),
            _ => TokenKind::Identifier,
        };
        self.spanned(kind, lexeme)
    }

    fn parse_string(&mut self) -> Token {
        let mut lexeme = String::from('#');
        while let Some(value) = self.next_char() {
            lexeme.push(value);
            if lexeme.ends_with("\"#") {
                break;
            }
        }
        let lexeme = lexeme[2..lexeme.len() - 2].replace("\\n", "\n");
        self.spanned(TokenKind::String, lexeme)
    }

    fn parse_char(&mut self) -> Token {
        let mut lexeme = String::from('#');
        while let Some(value) = self.next_char() {
            lexeme.push(value);
            if lexeme.ends_with("\'#") {
                break;
            }
        }
        let lexeme = lexeme[2..lexeme.len() - 2].replace("\\n", "\n");
        self.spanned(TokenKind::Char, lexeme)
    }

    fn double_char(&mut self, kind: TokenKind, lexeme: impl Into<String>) -> Option<Token> {
        self.next_char();
        Some(self.spanned(kind, lexeme.into()))
    }

    fn skip_char(&mut self) -> Option<Token> {
        self.spanned(TokenKind::InvalidToken, ' ');
        self.next()
    }
}

impl<'a> Iterator for Tokenizer<'a> {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        let Some(c) = self.next_char() else {
            return None;
        };
        match c {
            '0'..='9' => Some(self.parse_number(c)),
            value if value.is_ascii_alphabetic() => Some(self.parse_identifier(value)),
            value if value.is_ascii_whitespace() => self.skip_char(),
            '#' if self.peek_char('\'') => Some(self.parse_char()),
            '#' if self.peek_char('"') => Some(self.parse_string()),
            '=' if self.peek_char('=') => Some(self.spanned(TokenKind::EqualEqual, c)),
            '>' if self.peek_char('=') => Some(self.spanned(TokenKind::GreaterEqual, c)),
            '<' if self.peek_char('=') => Some(self.spanned(TokenKind::LessEqual, c)),
            '!' if self.peek_char('=') => self.double_char(TokenKind::BangEqual, "!="),
            '(' => Some(self.spanned(TokenKind::LeftParen, c)),
            ')' => Some(self.spanned(TokenKind::RightParen, c)),
            '[' => Some(self.spanned(TokenKind::LeftBracket, c)),
            ']' => Some(self.spanned(TokenKind::RightBracket, c)),
            '{' => Some(self.spanned(TokenKind::LeftBrace, c)),
            '}' => Some(self.spanned(TokenKind::RightBrace, c)),
            ',' => Some(self.spanned(TokenKind::Comma, c)),
            '.' => Some(self.spanned(TokenKind::Dot, c)),
            '-' => Some(self.spanned(TokenKind::Minus, c)),
            '+' => Some(self.spanned(TokenKind::Plus, c)),
            ';' => Some(self.spanned(TokenKind::Semicolon, c)),
            ':' => Some(self.spanned(TokenKind::Colon, c)),
            '/' => Some(self.spanned(TokenKind::Slash, c)),
            '*' => Some(self.spanned(TokenKind::Star, c)),
            '!' => Some(self.spanned(TokenKind::Bang, c)),
            '=' => Some(self.spanned(TokenKind::Equal, c)),
            '>' => Some(self.spanned(TokenKind::Greater, c)),
            '<' => Some(self.spanned(TokenKind::Less, c)),
            c => Some(self.spanned(TokenKind::InvalidToken, c)),
        }
    }
}
