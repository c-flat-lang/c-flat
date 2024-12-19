use crate::ast;
use crate::error::CompilerError;
use crate::lexer::{
    token::{Keyword, Token, TokenKind},
    tokenizer::Tokenizer,
};

pub struct Parser<'a> {
    lexer: std::iter::Peekable<Tokenizer<'a>>,
}

impl<'a> Parser<'a> {
    pub fn new(src: &'a str) -> Self {
        Self {
            lexer: Tokenizer::new(src).peekable(),
        }
    }

    pub fn parse(&mut self) -> Result<Vec<ast::Item>, CompilerError> {
        let mut items = vec![];
        while self.lexer.peek().is_some() {
            let visibility = self.parse_visibility()?;
            let Some(token) = self.lexer.peek() else {
                return Err(CompilerError::UnexpectedEndOfInput);
            };

            match token.kind {
                TokenKind::Keyword(Keyword::Fn) => {
                    let function = self.parse_function(visibility)?;
                    items.push(ast::Item::Function(function));
                }
                _ => {
                    return Err(CompilerError::UnexpectedEndOfInput);
                }
            }
        }
        Ok(items)
    }

    fn peek(&mut self, kind: TokenKind) -> bool {
        matches!(self.lexer.peek(), Some(token) if token.kind == kind)
    }

    fn consume(&mut self, kind: TokenKind) -> Result<Token, CompilerError> {
        match self.lexer.next() {
            Some(token) if token.kind == kind => Ok(token),
            Some(token) => Err(CompilerError::ExpectedToken {
                actual: token,
                expected: kind,
            }),
            None => Err(CompilerError::UnexpectedEndOfInput),
        }
    }

    fn parse_visibility(&mut self) -> Result<ast::Visibility, CompilerError> {
        match self.lexer.peek() {
            Some(token) if token.is_keyword(Keyword::Pub) => {
                self.lexer.next();
                Ok(ast::Visibility::Public)
            }
            Some(_) => Ok(ast::Visibility::Private),
            None => Err(CompilerError::UnexpectedEndOfInput),
        }
    }

    fn parse_function(
        &mut self,
        visibility: ast::Visibility,
    ) -> Result<ast::Function, CompilerError> {
        let fn_token = self.consume(TokenKind::Keyword(Keyword::Fn))?;
        let name = self.consume(TokenKind::Identifier)?;
        let params = self.parse_params()?;
        let return_type = self.parse_type()?;
        let body = self.parse_block()?;
        let function = ast::Function {
            visibility,
            fn_token,
            name,
            params,
            return_type,
            body,
        };
        Ok(function)
    }

    fn parse_params(&mut self) -> Result<Vec<ast::Param>, CompilerError> {
        let mut params = vec![];
        self.consume(TokenKind::LeftParen)?;
        while self.lexer.peek().is_some() && !self.peek(TokenKind::RightParen) {
            let name = self.consume(TokenKind::Identifier)?;
            let ty = self.parse_type()?;
            params.push(ast::Param { name, ty });
            if self.lexer.peek().is_some() {
                self.consume(TokenKind::Comma)?;
            }
        }
        self.consume(TokenKind::RightParen)?;
        Ok(params)
    }

    fn parse_type(&mut self) -> Result<ast::Type, CompilerError> {
        let Some(tok) = self.lexer.next() else {
            return Err(CompilerError::UnexpectedEndOfInput);
        };
        match tok.kind {
            TokenKind::Identifier => {
                token_as_type(&tok).map_err(|tok| CompilerError::ExpectedAType(tok))
            }
            TokenKind::Star => {
                let ty = self.parse_type()?;
                Ok(ast::Type::Pointer(Box::new(ty)))
            }
            TokenKind::LeftBracket => {
                let count = self.consume(TokenKind::Number)?;
                self.consume(TokenKind::Semicolon)?;
                let ty = self.parse_type()?;
                self.consume(TokenKind::RightBracket)?;
                Ok(ast::Type::Array(
                    count.lexeme.parse().unwrap(),
                    Box::new(ty),
                ))
            }
            _ => Err(CompilerError::ExpectedAType(tok)),
        }
    }

    fn parse_block(&mut self) -> Result<ast::Block, CompilerError> {
        let mut statements = vec![];
        self.consume(TokenKind::LeftBrace)?;
        while self.lexer.peek().is_some() && !self.peek(TokenKind::RightBrace) {
            statements.push(self.parse_statement()?);
        }
        self.consume(TokenKind::RightBrace)?;
        Ok(ast::Block { statements })
    }

    fn parse_statement(&mut self) -> Result<ast::Statement, CompilerError> {
        let expr = self.parse_expr()?;
        let delem = self.consume(TokenKind::Semicolon)?;
        Ok(ast::Statement { expr, delem })
    }

    fn parse_expr(&mut self) -> Result<ast::Expr, CompilerError> {
        self.parse_comparison()
    }

    fn parse_comparison(&mut self) -> Result<ast::Expr, CompilerError> {
        use TokenKind::*;
        let mut left = self.parse_term()?;
        while let Some(op) = self.lexer.next_if(one_of(&[
            Greater,
            Less,
            GreaterEqual,
            LessEqual,
            EqualEqual,
            BangEqual,
        ])) {
            let right = self.parse_term()?;
            let binary_expr = ast::ExprBinary {
                left: Box::new(left),
                op,
                right: Box::new(right),
            };
            left = ast::Expr::Binary(binary_expr);
        }
        Ok(left)
    }

    fn parse_term(&mut self) -> Result<ast::Expr, CompilerError> {
        let mut left = self.parse_factor()?;
        while let Some(op) = self
            .lexer
            .next_if(one_of(&[TokenKind::Plus, TokenKind::Minus]))
        {
            let right = Box::new(self.parse_factor()?);
            let binary_expr = ast::ExprBinary {
                left: Box::new(left),
                right,
                op,
            };
            left = ast::Expr::Binary(binary_expr)
        }
        Ok(left)
    }

    fn parse_factor(&mut self) -> Result<ast::Expr, CompilerError> {
        let mut left = self.parse_call()?;
        while let Some(op) = self
            .lexer
            .next_if(one_of(&[TokenKind::Star, TokenKind::Slash]))
        {
            let right = Box::new(self.parse_call()?);
            let expr_binary = ast::ExprBinary {
                left: Box::new(left),
                right,
                op,
            };
            left = ast::Expr::Binary(expr_binary);
        }
        Ok(left)
    }

    fn parse_call(&mut self) -> Result<ast::Expr, CompilerError> {
        let mut expr = self.parse_primary()?;

        if let Some(left_paran) = self.lexer.next_if(one_of(&[TokenKind::LeftParen])) {
            expr = self.finish_call(expr, left_paran)?;
        }

        Ok(expr)
    }

    fn finish_call(
        &mut self,
        caller: ast::Expr,
        left_paran: Token,
    ) -> Result<ast::Expr, CompilerError> {
        let mut args = vec![];
        while matches!(self.lexer.peek(), Some(token) if token.kind != TokenKind::RightParen) {
            args.push(self.parse_expr()?);
            self.lexer.next_if(|tok| tok.kind == TokenKind::Comma);
        }
        let Some(right_paran) = self.lexer.next_if(|tok| tok.kind == TokenKind::RightParen) else {
            // TODO: make this report an error
            panic!("expected a right paran");
        };
        Ok(ast::Expr::Call(ast::ExprCall {
            caller: Box::new(caller),
            left_paran,
            args,
            right_paran,
        }))
    }

    fn parse_primary(&mut self) -> Result<ast::Expr, CompilerError> {
        let Some(token) = self.lexer.next() else {
            return Err(CompilerError::UnexpectedEndOfInput);
        };
        match token.kind {
            TokenKind::Number => Ok(ast::Expr::Litral(ast::Litral::Integer(token))),
            TokenKind::Float => Ok(ast::Expr::Litral(ast::Litral::Float(token))),
            TokenKind::String => Ok(ast::Expr::Litral(ast::Litral::String(token))),
            TokenKind::Char => Ok(ast::Expr::Litral(ast::Litral::Char(token))),
            TokenKind::Identifier => Ok(ast::Expr::Identifier(token)),
            _ => Err(CompilerError::ExpectedToken {
                actual: token,
                expected: TokenKind::Number,
            }),
        }
    }
}

fn one_of(tokens: &[TokenKind]) -> impl Fn(&Token) -> bool + use<'_> {
    move |token| tokens.contains(&token.kind)
}

fn token_as_type<'a>(token: &'a Token) -> Result<ast::Type, Token> {
    let parse_type = |input: &'a str| -> Option<(&'a str, &'a str)> {
        let (prefix, rest) = input.split_at(1);
        if prefix.chars().all(char::is_alphabetic) && rest.chars().all(char::is_numeric) {
            Some((prefix, rest))
        } else {
            None
        }
    };
    if token.lexeme == "void" {
        return Ok(ast::Type::Void);
    }
    let Some((prefix, number)) = parse_type(&token.lexeme) else {
        return Err(token.clone());
    };
    match prefix {
        "u" => Ok(ast::Type::UnsignedNumber(number.parse().unwrap())),
        "s" => Ok(ast::Type::SignedNumber(number.parse().unwrap())),
        "f" => Ok(ast::Type::Float(number.parse().unwrap())),
        "*" => {
            let ty = token_as_type(&Token::new(TokenKind::Number, number, token.span.clone()))?;
            Ok(ast::Type::Pointer(Box::new(ty)))
        }
        _ => Err(token.clone()),
    }
}
