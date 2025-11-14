use std::iter::Peekable;

use crate::error::CompilerError;
use crate::stage::lexer::token::{Keyword, Token, TokenKind};
use crate::stage::parser::ast;

pub struct Parser {
    lexer: std::iter::Peekable<std::vec::IntoIter<Token>>,
}

impl Parser {
    pub fn new(lexer: Peekable<std::vec::IntoIter<Token>>) -> Self {
        Self { lexer }
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
                TokenKind::Keyword(Keyword::Type) => {
                    let type_def = self.parse_type_def(visibility)?;
                    items.push(ast::Item::Type(type_def));
                }
                TokenKind::Keyword(Keyword::Use) => {
                    let use_token = self.consume(TokenKind::Keyword(Keyword::Use))?;
                    let mut path = Vec::new();
                    while !self.peek(TokenKind::Semicolon) {
                        let token = self.consume(TokenKind::Identifier)?;
                        path.push(token);
                        if self.peek(TokenKind::Colon) {
                            self.consume(TokenKind::Colon)?;
                            self.consume(TokenKind::Colon)?;
                        }
                    }
                    self.consume(TokenKind::Semicolon)?;

                    items.push(ast::Item::Use(ast::Use { use_token, path }));
                }
                _ => {
                    return Err(CompilerError::UnexpectedTopLevelItem(token.clone()));
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

    fn consume_if_eq(&mut self, kind: TokenKind) -> Option<Token> {
        if !self.peek(kind) {
            return None;
        }
        Some(self.consume(kind).unwrap())
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

    fn parse_type_def(
        &mut self,
        visibility: ast::Visibility,
    ) -> Result<ast::TypeDef, CompilerError> {
        let type_token = self.consume(TokenKind::Keyword(Keyword::Type))?;
        let name = self.consume(TokenKind::Identifier)?;
        if self.peek(TokenKind::Keyword(Keyword::Struct)) {
            let expr_struct = self.parse_struct()?;
            return Ok(ast::TypeDef::Struct(ast::Struct {
                visibility,
                type_token,
                name,
                expr: Box::new(expr_struct),
            }));
        }
        let Some(token) = self.lexer.next() else {
            return Err(CompilerError::UnexpectedEndOfInput);
        };
        Err(CompilerError::InvalidToken(token))
    }

    fn parse_struct(&mut self) -> Result<ast::Expr, CompilerError> {
        let struct_token = self.consume(TokenKind::Keyword(Keyword::Struct))?;
        self.consume(TokenKind::LeftBrace)?;
        let fields = self.parse_fields()?;
        self.consume(TokenKind::RightBrace)?;
        let expr_struct = ast::ExprStruct {
            struct_token,
            fields,
        };
        Ok(ast::Expr::Struct(expr_struct))
    }

    fn parse_fields(&mut self) -> Result<Vec<ast::Field>, CompilerError> {
        let mut fields = vec![];
        while self.lexer.peek().is_some() && !self.peek(TokenKind::RightBrace) {
            let visibility = self.parse_visibility()?;
            let name = self.consume(TokenKind::Identifier)?;
            let colon = self.consume(TokenKind::Colon)?;
            let ty = self.parse_type()?;
            let default_expr = if self.consume_if_eq(TokenKind::Equal).is_some() {
                let expr = self.parse_expr()?;
                self.consume(TokenKind::Comma)?;
                Some(Box::new(expr))
            } else {
                self.consume(TokenKind::Comma)?;
                None
            };
            fields.push(ast::Field {
                visibility,
                name,
                colon,
                ty,
                default_expr,
            });
            if self.peek(TokenKind::Comma) {
                self.consume(TokenKind::Comma)?;
            }
        }
        Ok(fields)
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
            self.consume(TokenKind::Colon)?;
            let ty = self.parse_type()?;
            params.push(ast::Param { name, ty });
            if self.peek(TokenKind::Comma) {
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
            TokenKind::Identifier => token_as_type(&tok).map_err(CompilerError::ExpectedAType),
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
        let open_brace = self.consume(TokenKind::LeftBrace)?;
        while self.lexer.peek().is_some() && !self.peek(TokenKind::RightBrace) {
            statements.push(self.parse_statement()?);
        }
        let close_brace = self.consume(TokenKind::RightBrace)?;
        Ok(ast::Block {
            open_brace,
            statements,
            close_brace,
        })
    }

    fn parse_statement(&mut self) -> Result<ast::Statement, CompilerError> {
        let expr = self.parse_let_declaration()?;
        let delem = if self.peek(TokenKind::Semicolon) {
            Some(self.consume(TokenKind::Semicolon)?)
        } else {
            None
        };
        Ok(ast::Statement {
            expr: Box::new(expr),
            delem,
        })
    }

    fn parse_let_declaration(&mut self) -> Result<ast::Expr, CompilerError> {
        if !self.peek(TokenKind::Keyword(Keyword::Let)) {
            return self.return_expression();
        }

        let let_token = self.consume(TokenKind::Keyword(Keyword::Let))?;
        let mutable = if self.peek(TokenKind::Keyword(Keyword::Mut)) {
            self.consume(TokenKind::Keyword(Keyword::Mut))?;
            true
        } else {
            false
        };
        let ident = self.consume(TokenKind::Identifier)?;
        let ty = if self.peek(TokenKind::Colon) {
            self.consume(TokenKind::Colon)?;
            Some(self.parse_type()?)
        } else {
            None
        };
        self.consume(TokenKind::Equal)?;
        let expr = self.parse_expr()?;

        let decl = ast::ExprDecl {
            let_token,
            mutable,
            ident,
            ty,
            expr: Box::new(expr),
        };
        Ok(ast::Expr::Declare(decl))
    }

    fn return_expression(&mut self) -> Result<ast::Expr, CompilerError> {
        if !self.peek(TokenKind::Keyword(Keyword::Return)) {
            return self.parse_expr();
        }
        let return_token = self.consume(TokenKind::Keyword(Keyword::Return))?;
        let expr = self.parse_expr()?;
        let return_expr = ast::ExprReturn {
            return_token,
            expr: Some(Box::new(expr)),
        };
        Ok(ast::Expr::Return(return_expr))
    }

    fn parse_expr(&mut self) -> Result<ast::Expr, CompilerError> {
        self.parse_if_else()
    }

    fn parse_if_else(&mut self) -> Result<ast::Expr, CompilerError> {
        if !self.peek(TokenKind::Keyword(Keyword::If)) {
            return self.parse_assignment();
        }
        self.consume(TokenKind::Keyword(Keyword::If))?;

        let condition = self.parse_expr()?;
        let then_branch = self.parse_block()?;
        let else_branch = if self.peek(TokenKind::Keyword(Keyword::Else)) {
            self.consume(TokenKind::Keyword(Keyword::Else))?;
            Some(self.parse_block()?)
        } else {
            None
        };

        let if_else_expr = ast::ExprIfElse {
            condition: Box::new(condition),
            then_branch,
            else_branch,
            ty: ast::Type::Void,
        };
        Ok(ast::Expr::IfElse(if_else_expr))
    }

    fn parse_assignment(&mut self) -> Result<ast::Expr, CompilerError> {
        let expr = self.parse_comparison()?;
        if !self.peek(TokenKind::Equal) {
            return Ok(expr);
        }

        let equal = self.consume(TokenKind::Equal)?;
        let right_expr = self.parse_expr()?;

        let assignment = ast::ExprAssignment {
            left: Box::new(expr),
            equal,
            right: Box::new(right_expr),
        };
        Ok(ast::Expr::Assignment(assignment))
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

    fn parse_postfix(&mut self, mut expr: ast::Expr) -> Result<ast::Expr, CompilerError> {
        loop {
            if self.peek(TokenKind::LeftParen) {
                let left_paren = self.consume(TokenKind::LeftParen)?;
                expr = self.finish_call(expr, left_paren)?;
            } else if self.peek(TokenKind::LeftBracket) {
                let left_bracket = self.consume(TokenKind::LeftBracket)?;
                let index = self.parse_expr()?;
                let right_bracket = self.consume(TokenKind::RightBracket)?;
                let array_index = ast::ExprArrayIndex {
                    expr: Box::new(expr),
                    open_bracket: left_bracket,
                    index: Box::new(index),
                    close_bracket: right_bracket,
                    ty: ast::Type::Void,
                };
                expr = ast::Expr::ArrayIndex(array_index);
            } else {
                break;
            }
        }
        Ok(expr)
    }

    fn parse_call(&mut self) -> Result<ast::Expr, CompilerError> {
        let expr = self.parse_primary()?;
        self.parse_postfix(expr)
    }

    fn finish_call(
        &mut self,
        caller: ast::Expr,
        left_paren: Token,
    ) -> Result<ast::Expr, CompilerError> {
        let mut args = vec![];
        while matches!(self.lexer.peek(), Some(token) if token.kind != TokenKind::RightParen) {
            args.push(self.parse_expr()?);
            self.lexer.next_if(|tok| tok.kind == TokenKind::Comma);
        }
        let Some(right_paren) = self.lexer.next_if(|tok| tok.kind == TokenKind::RightParen) else {
            let span = args.last().map(|e| e.span()).unwrap_or(left_paren.span);
            return Err(CompilerError::MissingClosingParen(span));
        };
        Ok(ast::Expr::Call(ast::ExprCall {
            caller: Box::new(caller),
            left_paren,
            args,
            right_paren,
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
            TokenKind::Keyword(Keyword::True) => {
                Ok(ast::Expr::Litral(ast::Litral::BoolTrue(token)))
            }
            TokenKind::Keyword(Keyword::False) => {
                Ok(ast::Expr::Litral(ast::Litral::BoolFalse(token)))
            }
            TokenKind::LeftBracket => self.parse_array_literal(token),
            _ => Err(CompilerError::ExpectedToken {
                actual: token,
                expected: TokenKind::Number,
            }),
        }
    }

    fn parse_array_literal(&mut self, open_bracket: Token) -> Result<ast::Expr, CompilerError> {
        if self.peek(TokenKind::RightBracket) {
            let close_bracket = self.consume(TokenKind::RightBracket)?;
            return Ok(ast::Expr::Array(ast::ExprArray {
                open_bracket,
                elements: vec![],
                close_bracket,
                ty: ast::Type::Void,
            }));
        }

        let first = self.parse_expr()?;

        if self.peek(TokenKind::Semicolon) {
            let semicolon = self.consume(TokenKind::Semicolon)?;
            let count = self.parse_expr()?; // must be compile-time integer
            let close_bracket = self.consume(TokenKind::RightBracket)?;

            return Ok(ast::Expr::ArrayRepeat(ast::ExprArrayRepeat {
                open_bracket,
                count: Box::new(count),
                semicolon,
                value: Box::new(first),
                close_bracket,
                ty: ast::Type::Void,
            }));
        }

        let mut elements = vec![first];

        while !self.peek(TokenKind::RightBracket) {
            elements.push(self.parse_expr()?);
            if self.peek(TokenKind::Comma) {
                self.consume(TokenKind::Comma)?;
            } else {
                break;
            }
        }

        let close_bracket = self.consume(TokenKind::RightBracket)?;
        Ok(ast::Expr::Array(ast::ExprArray {
            open_bracket,
            elements,
            // Set to Void because we don't know the type of the array.
            // We will sent when we type check.
            ty: ast::Type::Void,
            close_bracket,
        }))
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
    match token.lexeme.as_str() {
        "void" => return Ok(ast::Type::Void),
        "bool" => return Ok(ast::Type::Bool),
        _ => (),
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
