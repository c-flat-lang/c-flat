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
            "let" => TokenKind::Keyword(Keyword::Let),
            "const" => TokenKind::Keyword(Keyword::Const),
            "fn" => TokenKind::Keyword(Keyword::Fn),
            "pub" => TokenKind::Keyword(Keyword::Pub),
            "if" => TokenKind::Keyword(Keyword::If),
            "else" => TokenKind::Keyword(Keyword::Else),
            "while" => TokenKind::Keyword(Keyword::While),
            "for" => TokenKind::Keyword(Keyword::For),
            "return" => TokenKind::Keyword(Keyword::Return),
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

    fn skip_char(&mut self) -> Option<Token> {
        self.spanned(TokenKind::InvalidToken, ' ');
        self.next()
    }
}

impl<'a> Iterator for Tokenizer<'a> {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        match self.next_char() {
            Some(value @ '0'..='9') => Some(self.parse_number(value)),
            Some(value) if value.is_ascii_alphabetic() => Some(self.parse_identifier(value)),
            Some(value) if value.is_ascii_whitespace() => self.skip_char(),
            Some('#') if self.chars.peek() == Some(&'"') => Some(self.parse_string()),
            Some(c @ '=') if self.peek_char('=') => Some(self.spanned(TokenKind::EqualEqual, c)),
            Some(c @ '>') if self.peek_char('=') => Some(self.spanned(TokenKind::GreaterEqual, c)),
            Some(c @ '<') if self.peek_char('=') => Some(self.spanned(TokenKind::LessEqual, c)),
            // Char
            Some(c @ '(') => Some(self.spanned(TokenKind::LeftParen, c)),
            Some(c @ ')') => Some(self.spanned(TokenKind::RightParen, c)),
            Some(c @ '[') => Some(self.spanned(TokenKind::LeftBracket, c)),
            Some(c @ ']') => Some(self.spanned(TokenKind::RightBracket, c)),
            Some(c @ '{') => Some(self.spanned(TokenKind::LeftBrace, c)),
            Some(c @ '}') => Some(self.spanned(TokenKind::RightBrace, c)),
            Some(c @ ',') => Some(self.spanned(TokenKind::Comma, c)),
            Some(c @ '.') => Some(self.spanned(TokenKind::Dot, c)),
            Some(c @ '-') => Some(self.spanned(TokenKind::Minus, c)),
            Some(c @ '+') => Some(self.spanned(TokenKind::Plus, c)),
            Some(c @ ';') => Some(self.spanned(TokenKind::Semicolon, c)),
            Some(c @ '/') => Some(self.spanned(TokenKind::Slash, c)),
            Some(c @ '*') => Some(self.spanned(TokenKind::Star, c)),
            Some(c @ '!') => Some(self.spanned(TokenKind::Bang, c)),
            Some(c @ '=') => Some(self.spanned(TokenKind::Equal, c)),
            Some(c @ '>') => Some(self.spanned(TokenKind::Greater, c)),
            Some(c @ '<') => Some(self.spanned(TokenKind::Less, c)),
            Some(c) => Some(self.spanned(TokenKind::InvalidToken, c)),
            None => None,
        }
    }
}
