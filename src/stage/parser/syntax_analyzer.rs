use std::iter::Peekable;

use crate::error::{
    ErrorExpectedToken, ErrorExpectedType, ErrorMissingPairedClosingChar,
    ErrorUnexpectedEndOfInput, ErrorUnexpectedToken, ErrorUnexpectedTopLevelItem, Result,
};
use crate::stage::lexer::token::{Keyword, Token, TokenKind};
use crate::stage::parser::ast::{self, ExprMemberAccess, TypeParams};

pub struct Parser {
    lexer: std::iter::Peekable<std::vec::IntoIter<Token>>,
    /// When true, struct literal expressions (`Foo { .field = val }`) are not
    /// parsed in primary position. Set while parsing `if`/`while` conditions so
    /// that `if cond {` is never mistaken for a struct instantiation.
    restrict_struct_literal: bool,
    last_token: Option<Token>,
    filename: String,
}

impl Parser {
    fn next_if_token_kind_eq(&mut self, kind: TokenKind) -> Option<Token> {
        self.next_if(|token| token.kind == kind)
    }
}

impl Parser {
    pub fn new(filename: impl Into<String>, lexer: Peekable<std::vec::IntoIter<Token>>) -> Self {
        Self {
            lexer,
            restrict_struct_literal: false,
            last_token: None,
            filename: filename.into(),
        }
    }

    pub fn parse(&mut self) -> Result<Vec<ast::Item>> {
        let mut items = vec![];
        while self.lexer.peek().is_some() {
            let visibility = self.parse_visibility()?;
            let Some(token) = self.lexer.peek() else {
                return Err(Box::new(ErrorUnexpectedEndOfInput::new(
                    self.last_token.clone(),
                    &self.filename,
                    #[cfg(feature = "debug")]
                    format!("{} {}:{}", file!(), line!(), column!()),
                )));
            };

            match token.kind {
                TokenKind::Keyword(Keyword::Extern) => {
                    let extern_function = self.parse_extern_function(visibility)?;
                    items.push(ast::Item::ExternFunction(extern_function));
                }
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
                    while !self.peek_is(TokenKind::Semicolon) {
                        let token = self.consume(TokenKind::Identifier)?;
                        path.push(token);
                        if self.peek_is(TokenKind::Colon) {
                            self.consume(TokenKind::Colon)?;
                            self.consume(TokenKind::Colon)?;
                        }
                    }
                    self.consume(TokenKind::Semicolon)?;

                    items.push(ast::Item::Use(ast::Use { use_token, path }));
                }
                _ => {
                    return Err(Box::new(ErrorUnexpectedTopLevelItem::new(
                        token.clone(),
                        &[Keyword::Fn, Keyword::Type, Keyword::Use, Keyword::Const],
                        #[cfg(feature = "debug")]
                        format!("{} {}:{}", file!(), line!(), column!()),
                    )));
                }
            }
        }
        Ok(items)
    }

    fn peek_is(&mut self, kind: TokenKind) -> bool {
        matches!(self.lexer.peek(), Some(token) if token.kind == kind)
    }

    fn peek_kind(&mut self) -> Option<&TokenKind> {
        let token = self.lexer.peek()?;
        Some(&token.kind)
    }

    fn next(&mut self) -> Option<Token> {
        let token = self.lexer.next();
        if token.is_some() {
            self.last_token = token.clone();
        }
        token
    }

    fn next_if(&mut self, f: impl Fn(&Token) -> bool) -> Option<Token> {
        let token = self.lexer.next_if(f);
        if token.is_some() {
            self.last_token = token.clone();
        }
        token
    }

    fn consume(&mut self, kind: TokenKind) -> Result<Token> {
        match self.next() {
            Some(token) if token.kind == kind => Ok(token),
            Some(token) => Err(Box::new(ErrorExpectedToken::new(
                token,
                &[kind],
                #[cfg(feature = "debug")]
                format!("{} {}:{}", file!(), line!(), column!()),
            ))),
            None => Err(Box::new(ErrorUnexpectedEndOfInput::new(
                self.last_token.clone(),
                &self.filename,
                #[cfg(feature = "debug")]
                format!("{} {}:{}", file!(), line!(), column!()),
            ))),
        }
    }

    fn consume_if_eq(&mut self, kind: TokenKind) -> Option<Token> {
        if !self.peek_is(kind) {
            return None;
        }
        Some(self.consume(kind).unwrap())
    }

    fn parse_visibility(&mut self) -> Result<ast::Visibility> {
        match self.lexer.peek() {
            Some(token) if token.is_keyword(Keyword::Pub) => {
                self.next();
                Ok(ast::Visibility::Public)
            }
            Some(_) => Ok(ast::Visibility::Private),
            None => Err(Box::new(ErrorUnexpectedEndOfInput::new(
                self.last_token.clone(),
                &self.filename,
                #[cfg(feature = "debug")]
                format!("{} {}:{}", file!(), line!(), column!()),
            ))),
        }
    }

    fn parse_extern_function(
        &mut self,
        visibility: ast::Visibility,
    ) -> Result<ast::ExternFunction> {
        let extern_token = self.consume(TokenKind::Keyword(Keyword::Extern))?;
        let calling_convention = self.consume(TokenKind::Identifier)?;
        let fn_token = self.consume(TokenKind::Keyword(Keyword::Fn))?;
        let (binding_name, local_name) = self.consume(TokenKind::Identifier).and_then(|bn| {
            if !self.peek_is(TokenKind::Keyword(Keyword::As)) {
                return Ok((bn, None));
            }
            self.consume(TokenKind::Keyword(Keyword::As))?;
            Ok((bn, self.consume(TokenKind::Identifier).ok()))
        })?;
        self.consume(TokenKind::LeftParen)?;
        let mut params = Vec::new();
        while !self.peek_is(TokenKind::RightParen) {
            let ty = self.parse_type()?;
            params.push(ty);
            if self.peek_is(TokenKind::Comma) {
                self.consume(TokenKind::Comma)?;
            }
        }
        self.consume(TokenKind::RightParen)?;
        let return_type = self.parse_type()?;
        self.consume(TokenKind::Semicolon)?;
        Ok(ast::ExternFunction {
            visibility,
            extern_token,
            fn_token,
            calling_convention,
            binding_name,
            local_name,
            params,
            return_type,
        })
    }

    fn parse_type_def(&mut self, visibility: ast::Visibility) -> Result<ast::TypeDef> {
        let type_token = self.consume(TokenKind::Keyword(Keyword::Type))?;
        let name = self.consume(TokenKind::Identifier)?;

        let type_params = self.parse_type_params()?;

        match self.peek_kind() {
            Some(TokenKind::Keyword(Keyword::Struct)) => {
                self.parse_type_def_struct(visibility, type_token, name, type_params)
            }
            Some(TokenKind::Keyword(Keyword::Enum)) => {
                self.parse_type_def_enum(visibility, type_token, name, type_params)
            }
            Some(_) => Err(Box::new(ErrorUnexpectedToken::new(
                // NOTE: unwrap is ok here cause we have a some type from prevous peek
                self.lexer.peek().unwrap().clone(),
                &[
                    TokenKind::Keyword(Keyword::Struct),
                    TokenKind::Keyword(Keyword::Enum),
                ],
                #[cfg(feature = "debug")]
                format!("{} {}:{}", file!(), line!(), column!()),
            ))),
            None => Err(Box::new(ErrorUnexpectedEndOfInput::new(
                self.last_token.clone(),
                &self.filename,
                #[cfg(feature = "debug")]
                format!("{} {}:{}", file!(), line!(), column!()),
            ))),
        }
    }

    fn parse_type_def_enum(
        &mut self,
        visibility: ast::Visibility,
        type_token: Token,
        name: Token,
        type_params: Option<Vec<(Token, ast::Type)>>,
    ) -> Result<ast::TypeDef> {
        let enum_token = self.consume(TokenKind::Keyword(Keyword::Enum))?;
        let open_brace = self.consume(TokenKind::LeftBrace)?;
        let variant = self.parse_variants()?;
        let close_brace = self.consume(TokenKind::RightBrace)?;

        Ok(ast::TypeDef::Enum(ast::Enum {
            visibility,
            type_token,
            name,
            type_params,
            enum_token,
            variants: variant,
            open_brace,
            close_brace,
        }))
    }

    fn parse_variants(&mut self) -> Result<Vec<ast::Variant>> {
        let mut variants = vec![];
        while self.lexer.peek().is_some() && !self.peek_is(TokenKind::RightBrace) {
            let name = self.consume(TokenKind::Identifier)?;
            let equals = self.consume(TokenKind::Equal).ok();
            let value = if equals.is_some() { self.next() } else { None };
            variants.push(ast::Variant {
                name,
                equals,
                value,
            });
            self.next_if_token_kind_eq(TokenKind::Comma);
        }
        Ok(variants)
    }

    fn parse_type_def_struct(
        &mut self,
        visibility: ast::Visibility,
        type_token: Token,
        name: Token,
        type_params: Option<Vec<(Token, ast::Type)>>,
    ) -> Result<ast::TypeDef> {
        let struct_token = self.consume(TokenKind::Keyword(Keyword::Struct))?;
        let open_brace = self.consume(TokenKind::LeftBrace)?;
        let fields = self.parse_fields()?;
        let close_brace = self.consume(TokenKind::RightBrace)?;

        Ok(ast::TypeDef::Struct(ast::Struct {
            visibility,
            type_token,
            name,
            type_params,
            struct_token,
            fields,
            open_brace,
            close_brace,
        }))
    }

    fn parse_type_params(&mut self) -> Result<Option<Vec<(Token, ast::Type)>>> {
        let mut args = Vec::new();

        if self.consume_if_eq(TokenKind::LeftTypeCradle).is_none() {
            return Ok(None);
        }

        while !self.peek_is(TokenKind::RightTypeCradle) {
            let name = self.consume(TokenKind::Identifier)?;
            self.consume(TokenKind::Colon)?;
            let ty = self.parse_type()?;
            args.push((name, ty));
            self.consume_if_eq(TokenKind::Comma);
        }

        self.consume(TokenKind::RightTypeCradle)?;

        Ok(Some(args))
    }

    fn parse_type_args(&mut self) -> Result<Option<Vec<ast::Type>>> {
        let mut args = Vec::new();

        if self.consume_if_eq(TokenKind::LeftTypeCradle).is_none() {
            return Ok(None);
        }

        while !self.peek_is(TokenKind::RightTypeCradle) {
            let ty = self.parse_type()?;
            args.push(ty);
            self.consume_if_eq(TokenKind::Comma);
        }

        self.consume(TokenKind::RightTypeCradle)?;

        Ok(Some(args))
    }

    fn parse_fields(&mut self) -> Result<Vec<ast::Field>> {
        let mut fields = vec![];
        while self.lexer.peek().is_some() && !self.peek_is(TokenKind::RightBrace) {
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
            if self.peek_is(TokenKind::Comma) {
                self.consume(TokenKind::Comma)?;
            }
        }
        Ok(fields)
    }

    fn parse_function(&mut self, visibility: ast::Visibility) -> Result<ast::Function> {
        let fn_token = self.consume(TokenKind::Keyword(Keyword::Fn))?;
        let name = self.consume(TokenKind::Identifier)?;
        let type_args = self.parse_type_params()?;
        let params = self.parse_params()?;
        let return_type = self.parse_type()?;
        let body = self.parse_block()?;
        let function = ast::Function {
            visibility,
            fn_token,
            name,
            type_args,
            params,
            return_type,
            body,
        };
        Ok(function)
    }

    fn parse_params(&mut self) -> Result<Vec<ast::Param>> {
        let mut params = vec![];
        self.consume(TokenKind::LeftParen)?;
        while self.lexer.peek().is_some() && !self.peek_is(TokenKind::RightParen) {
            let name = self.consume(TokenKind::Identifier)?;
            self.consume(TokenKind::Colon)?;
            let ty = self.parse_type()?;
            params.push(ast::Param { name, ty });
            if self.peek_is(TokenKind::Comma) {
                self.consume(TokenKind::Comma)?;
            }
        }
        self.consume(TokenKind::RightParen)?;
        Ok(params)
    }

    fn parse_type(&mut self) -> Result<ast::Type> {
        let mut_token = self.consume_if_eq(TokenKind::Keyword(Keyword::Mut));
        let Some(tok) = self.next() else {
            return Err(Box::new(ErrorUnexpectedEndOfInput::new(
                self.last_token.clone(),
                &self.filename,
                #[cfg(feature = "debug")]
                format!("{} {}:{}", file!(), line!(), column!()),
            )));
        };

        match tok.kind {
            TokenKind::Identifier if self.peek_is(TokenKind::LeftTypeCradle) => {
                self.consume(TokenKind::LeftTypeCradle)?;
                let mut params = Vec::new();
                while !self.peek_is(TokenKind::RightTypeCradle) {
                    let ty = self.parse_type()?;
                    params.push(ty);
                    self.consume_if_eq(TokenKind::Comma);
                }
                let right_param = self.consume(TokenKind::RightTypeCradle)?;
                let span_start = mut_token.as_ref().unwrap_or(&tok).span.start;
                let mut span = right_param.span.clone();
                span.start = span_start;
                Ok(ast::Type {
                    mut_token,
                    kind: ast::TypeKind::NameWithParams(tok, TypeParams { params }),
                    span,
                })
            }
            TokenKind::Identifier => token_as_type(mut_token, &tok),
            TokenKind::Star => {
                let ty = self.parse_type()?;
                let mut span = tok.span.clone();
                span.end = ty.span.end;
                Ok(ast::Type {
                    mut_token,
                    kind: ast::TypeKind::Pointer(Box::new(ty)),
                    span,
                })
            }
            TokenKind::LeftBracket => {
                // Disambiguate: [N; T] is an array, [T] is a slice
                // Arrays always start with a number literal
                if self.peek_is(TokenKind::Number) {
                    // Array: [N; T]
                    let count = self.consume(TokenKind::Number)?;
                    self.consume(TokenKind::Semicolon)?;
                    let ty = self.parse_type()?;
                    let closing_bracket = self.consume(TokenKind::RightBracket)?;
                    let mut span = tok.span.clone();
                    span.end = closing_bracket.span.end;
                    Ok(ast::Type {
                        mut_token,
                        kind: ast::TypeKind::Array(count.lexeme.parse().unwrap(), Box::new(ty)),
                        span,
                    })
                } else {
                    // Slice: [T] — T can be identifier, keyword type, pointer, etc.
                    let ty = self.parse_type()?;
                    let right_brace = self.consume(TokenKind::RightBracket)?;
                    let mut span = tok.span.clone();
                    span.end = right_brace.span.end;
                    Ok(ast::Type {
                        mut_token,
                        kind: ast::TypeKind::Slice(Box::new(ty)),
                        span,
                    })
                }
            }
            TokenKind::Keyword(Keyword::Type) => Ok(ast::Type {
                kind: ast::TypeKind::Type,
                span: tok.span,
                mut_token,
            }),

            _ => Err(Box::new(ErrorExpectedType::new(
                tok,
                #[cfg(feature = "debug")]
                format!("{} {}:{}", file!(), line!(), column!()),
            ))),
        }
    }

    fn parse_block(&mut self) -> Result<ast::ExprBlock> {
        let mut statements = vec![];
        let open_brace = self.consume(TokenKind::LeftBrace)?;
        while self.lexer.peek().is_some() && !self.peek_is(TokenKind::RightBrace) {
            statements.push(self.parse_statement()?);
        }
        let close_brace = self.consume(TokenKind::RightBrace)?;
        Ok(ast::ExprBlock {
            open_brace,
            statements,
            close_brace,
        })
    }

    fn parse_statement(&mut self) -> Result<ast::Statement> {
        let expr = self.parse_let_declaration()?;
        let delem = if self.peek_is(TokenKind::Semicolon) {
            Some(self.consume(TokenKind::Semicolon)?)
        } else {
            None
        };
        Ok(ast::Statement {
            expr: Box::new(expr),
            delem,
        })
    }

    fn parse_let_declaration(&mut self) -> Result<ast::Expr> {
        if !self.peek_is(TokenKind::Keyword(Keyword::Let)) {
            return self.while_expression();
        }

        let let_token = self.consume(TokenKind::Keyword(Keyword::Let))?;
        let mutable = if self.peek_is(TokenKind::Keyword(Keyword::Mut)) {
            self.consume(TokenKind::Keyword(Keyword::Mut))?;
            true
        } else {
            false
        };
        let ident = self.consume(TokenKind::Identifier)?;
        let ty = if self.peek_is(TokenKind::Colon) {
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

    fn while_expression(&mut self) -> Result<ast::Expr> {
        if !self.peek_is(TokenKind::Keyword(Keyword::While)) {
            return self.parse_return_expression();
        }
        let while_token = self.consume(TokenKind::Keyword(Keyword::While))?;
        self.restrict_struct_literal = true;
        let condition = self.parse_expr()?;
        self.restrict_struct_literal = false;
        let body = self.parse_block()?;
        let while_expr = ast::ExprWhile {
            while_token,
            condition: Box::new(condition),
            body,
        };
        Ok(ast::Expr::While(while_expr))
    }

    fn parse_return_expression(&mut self) -> Result<ast::Expr> {
        if !self.peek_is(TokenKind::Keyword(Keyword::Return)) {
            return self.parse_expr();
        }
        let return_token = self.consume(TokenKind::Keyword(Keyword::Return))?;
        if self.peek_is(TokenKind::Semicolon) {
            let return_expr = ast::ExprReturn {
                return_token,
                expr: None,
            };
            return Ok(ast::Expr::Return(return_expr));
        }
        let expr = self.parse_expr()?;
        let return_expr = ast::ExprReturn {
            return_token,
            expr: Some(Box::new(expr)),
        };
        Ok(ast::Expr::Return(return_expr))
    }

    fn parse_expr(&mut self) -> Result<ast::Expr> {
        self.parse_if_else()
    }

    fn parse_if_else(&mut self) -> Result<ast::Expr> {
        if !self.peek_is(TokenKind::Keyword(Keyword::If)) {
            return self.parse_assignment();
        }
        let if_token = self.consume(TokenKind::Keyword(Keyword::If))?;

        self.restrict_struct_literal = true;
        let condition = self.parse_expr()?;
        self.restrict_struct_literal = false;
        let then_branch = self.parse_block()?;
        let mut else_token = None;
        let else_branch = if self.peek_is(TokenKind::Keyword(Keyword::Else)) {
            else_token = Some(self.consume(TokenKind::Keyword(Keyword::Else))?);
            // check if it's "else if"
            if self.peek_is(TokenKind::Keyword(Keyword::If)) {
                Some(Box::new(self.parse_if_else()?))
            } else {
                // else { ... }
                Some(Box::new(ast::Expr::Block(self.parse_block()?)))
            }
        } else {
            None
        };

        let if_else_expr = ast::ExprIfElse {
            if_token,
            condition: Box::new(condition),
            then_branch,
            else_token,
            else_branch,
            ty: ast::Type {
                kind: ast::TypeKind::Void,
                ..Default::default()
            },
        };
        Ok(ast::Expr::IfElse(if_else_expr))
    }

    fn parse_assignment(&mut self) -> Result<ast::Expr> {
        let expr = self.parse_or()?;
        if !self.peek_is(TokenKind::Equal) {
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

    fn parse_or(&mut self) -> Result<ast::Expr> {
        let mut left = self.parse_and()?;
        while let Some(op) = self.next_if_token_kind_eq(TokenKind::Keyword(Keyword::Or)) {
            let right = Box::new(self.parse_and()?);
            let binary_expr = ast::ExprBinary {
                left: Box::new(left),
                right,
                op,
            };
            left = ast::Expr::Binary(binary_expr);
        }
        Ok(left)
    }

    fn parse_and(&mut self) -> Result<ast::Expr> {
        let mut left = self.parse_bitwise()?;
        while let Some(op) = self.next_if_token_kind_eq(TokenKind::Keyword(Keyword::And)) {
            let right = Box::new(self.parse_comparison()?);
            let binary_expr = ast::ExprBinary {
                left: Box::new(left),
                right,
                op,
            };
            left = ast::Expr::Binary(binary_expr);
        }
        Ok(left)
    }

    fn parse_bitwise(&mut self) -> Result<ast::Expr> {
        let mut left = self.parse_comparison()?;
        while let Some(op) = self.next_if(one_of(&[TokenKind::Ampersand])) {
            let right = self.parse_comparison()?;
            let binary_expr = ast::ExprBinary {
                left: Box::new(left),
                op,
                right: Box::new(right),
            };
            left = ast::Expr::Binary(binary_expr);
        }
        Ok(left)
    }

    fn parse_comparison(&mut self) -> Result<ast::Expr> {
        use TokenKind::*;
        let mut left = self.parse_type_casting()?;
        while let Some(op) = self.next_if(one_of(&[
            Greater,
            Less,
            GreaterEqual,
            LessEqual,
            EqualEqual,
            BangEqual,
        ])) {
            let right = self.parse_type_casting()?;
            let binary_expr = ast::ExprBinary {
                left: Box::new(left),
                op,
                right: Box::new(right),
            };
            left = ast::Expr::Binary(binary_expr);
        }
        Ok(left)
    }

    fn parse_type_casting(&mut self) -> Result<ast::Expr> {
        let mut left = self.parse_bit_shift()?;
        while let Some(op) = self
            .lexer
            .next_if(one_of(&[TokenKind::Keyword(Keyword::As)]))
        {
            let target_type = self.parse_type()?;
            let binary_expr = ast::ExprTypeCast {
                expr: Box::new(left),
                target_type,
                as_token: op,
            };
            left = ast::Expr::TypeCast(binary_expr)
        }
        Ok(left)
    }

    fn parse_bit_shift(&mut self) -> Result<ast::Expr> {
        let mut left = self.parse_term()?;
        while let Some(op) = self.next_if(one_of(&[TokenKind::BitShiftRight])) {
            let right = Box::new(self.parse_term()?);
            let binary_expr = ast::ExprBinary {
                left: Box::new(left),
                right,
                op,
            };
            left = ast::Expr::Binary(binary_expr)
        }
        Ok(left)
    }

    fn parse_term(&mut self) -> Result<ast::Expr> {
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

    fn parse_factor(&mut self) -> Result<ast::Expr> {
        let mut left = self.parse_call()?;
        while let Some(op) = self.next_if(one_of(&[
            TokenKind::Star,
            TokenKind::Slash,
            TokenKind::Percent,
        ])) {
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

    fn parse_postfix(
        &mut self,
        mut expr: ast::Expr,
        type_args: Option<&[ast::Type]>,
    ) -> Result<ast::Expr> {
        loop {
            if self.peek_is(TokenKind::LeftParen) {
                let left_paren = self.consume(TokenKind::LeftParen)?;
                expr = self.finish_call(expr, left_paren, type_args)?;
            // This cause more problems. To add in generic syntax we will need to implement a pratt
            // parser.
            // } else if self.peek(TokenKind::LeftBrace) && matches!(expr, ast::Expr::Identifier(_)) {
            //     let ast::Expr::Identifier(token) = expr else {
            //         return Err(Box::new(ErrorUnexpectedExpression::new(
            //             expr,
            //             &[TokenKind::Identifier],
            //             #[cfg(feature = "debug")]
            //             format!("{} {}:{}", file!(), line!(), column!()),
            //         )));
            //     };
            //     expr = self.parse_struct_instantiation(token, type_args)?;
            } else if self.peek_is(TokenKind::LeftBracket) {
                let left_bracket = self.consume(TokenKind::LeftBracket)?;
                let index = self.parse_expr()?;
                let right_bracket = self.consume(TokenKind::RightBracket)?;
                let array_index = ast::ExprArrayIndex {
                    expr: Box::new(expr),
                    open_bracket: left_bracket,
                    index: Box::new(index),
                    close_bracket: right_bracket,
                    ty: ast::Type {
                        kind: ast::TypeKind::Void,
                        ..Default::default()
                    },
                };
                expr = ast::Expr::ArrayIndex(array_index);
            } else if self.peek_is(TokenKind::Dot) {
                let dot = self.consume(TokenKind::Dot)?;
                if self.peek_is(TokenKind::Star) {
                    let star = self.consume(TokenKind::Star)?;
                    expr = ast::Expr::Deref(ast::ExprDeref {
                        base: Box::new(expr),
                        dot,
                        star,
                    })
                } else {
                    let member = self.consume(TokenKind::Identifier)?;
                    expr = ast::Expr::MemberAccess(ExprMemberAccess {
                        base: Box::new(expr),
                        dot,
                        member,
                    })
                }
            } else {
                break;
            }
        }
        Ok(expr)
    }

    fn parse_call(&mut self) -> Result<ast::Expr> {
        let expr = self.parse_primary()?;
        let type_args = self.parse_type_args()?;
        self.parse_postfix(expr, type_args.as_deref())
    }

    fn finish_call(
        &mut self,
        caller: ast::Expr,
        left_paren: Token,
        type_args: Option<&[ast::Type]>,
    ) -> Result<ast::Expr> {
        let mut args = vec![];
        while matches!(self.lexer.peek(), Some(token) if token.kind != TokenKind::RightParen) {
            args.push(self.parse_expr()?);
            self.next_if(|tok| tok.kind == TokenKind::Comma);
        }
        let Some(right_paren) = self.next_if(|tok| tok.kind == TokenKind::RightParen) else {
            let span = args.last().map(|e| e.span()).unwrap_or(left_paren.span);
            return Err(Box::new(ErrorMissingPairedClosingChar::new(
                span,
                TokenKind::RightParen,
                #[cfg(feature = "debug")]
                format!("{} {}:{}", file!(), line!(), column!()),
            )));
        };
        Ok(ast::Expr::Call(ast::ExprCall {
            caller: Box::new(caller),
            type_args: type_args.map(|t| t.to_vec()),
            left_paren,
            args,
            right_paren,
        }))
    }

    fn parse_primary(&mut self) -> Result<ast::Expr> {
        let Some(token) = self.next() else {
            return Err(Box::new(ErrorUnexpectedEndOfInput::new(
                self.last_token.clone(),
                &self.filename,
                #[cfg(feature = "debug")]
                format!("{} {}:{}", file!(), line!(), column!()),
            )));
        };
        match token.kind {
            TokenKind::Ampersand => {
                let expr = self.parse_call()?;
                Ok(ast::Expr::AddressOf(ast::ExprAddressOf {
                    ampersand: token,
                    expr: Box::new(expr),
                }))
            }
            TokenKind::Bang => {
                let expr = self.parse_call()?;
                Ok(ast::Expr::Not(ast::ExprNot {
                    bang: token,
                    expr: Box::new(expr),
                }))
            }
            TokenKind::Number => {
                let span = token.span.clone();
                let integer_litral = ast::IntegerLitral {
                    token,
                    ty: ast::Type {
                        mut_token: None,
                        kind: ast::TypeKind::SignedNumber(32),
                        span,
                    },
                };
                Ok(ast::Expr::Litral(ast::Litral::Integer(integer_litral)))
            }
            TokenKind::Float => Ok(ast::Expr::Litral(ast::Litral::Float(token))),
            TokenKind::String => Ok(ast::Expr::Litral(ast::Litral::String(token))),
            TokenKind::Char => Ok(ast::Expr::Litral(ast::Litral::Char(token))),
            TokenKind::Builtin(..) => {
                let type_args = self.parse_type_args().ok().flatten();

                let left_paren = self.consume(TokenKind::LeftParen)?;
                let mut args = vec![];
                while matches!(self.lexer.peek(), Some(token) if token.kind != TokenKind::RightParen)
                {
                    args.push(self.parse_expr()?);
                    self.next_if(|tok| tok.kind == TokenKind::Comma);
                }
                let Some(right_paren) = self.next_if(|tok| tok.kind == TokenKind::RightParen)
                else {
                    let span = args.last().map(|e| e.span()).unwrap_or(left_paren.span);
                    return Err(Box::new(ErrorMissingPairedClosingChar::new(
                        span,
                        TokenKind::RightParen,
                        #[cfg(feature = "debug")]
                        format!("{} {}:{}", file!(), line!(), column!()),
                    )));
                };

                Ok(ast::Expr::Builtin(ast::ExprCall {
                    caller: Box::new(ast::Expr::Identifier(token)),
                    type_args: type_args.map(|t| t.to_vec()),
                    left_paren,
                    args,
                    right_paren,
                }))
            }
            TokenKind::Identifier => {
                // Qualified path: `a::b::c`. `::` is two `Colon` tokens.
                if self.peek_is(TokenKind::Colon) {
                    let mut segments = vec![token];
                    while self.peek_is(TokenKind::Colon) {
                        self.consume(TokenKind::Colon)?;
                        self.consume(TokenKind::Colon)?;
                        segments.push(self.consume(TokenKind::Identifier)?);
                    }
                    return Ok(ast::Expr::Path(ast::ExprPath { segments }));
                }

                if self.peek_is(TokenKind::LeftTypeCradle) {
                    let type_args = self.parse_type_args()?;
                    if !self.restrict_struct_literal && self.peek_is(TokenKind::LeftBrace) {
                        return self.parse_struct_instantiation(token, type_args.as_deref());
                    }
                    let left_paren = self.consume(TokenKind::LeftParen)?;
                    return self.finish_call(
                        ast::Expr::Identifier(token),
                        left_paren,
                        type_args.as_deref(),
                    );
                }
                if !self.restrict_struct_literal && self.peek_is(TokenKind::LeftBrace) {
                    return self.parse_struct_instantiation(token, None);
                }

                Ok(ast::Expr::Identifier(token))
            }
            TokenKind::Keyword(Keyword::True) => {
                Ok(ast::Expr::Litral(ast::Litral::BoolTrue(token)))
            }
            TokenKind::Keyword(Keyword::False) => {
                Ok(ast::Expr::Litral(ast::Litral::BoolFalse(token)))
            }
            TokenKind::LeftBracket => self.parse_array_literal(token),
            TokenKind::LeftParen => {
                let expr = self.parse_expr()?;
                let right_paren = self.consume(TokenKind::RightParen)?;
                Ok(ast::Expr::Grouping(ast::ExprGrouping {
                    open_paren: token,
                    expr: Box::new(expr),
                    close_paren: right_paren,
                }))
            }
            _ => Err(Box::new(ErrorExpectedToken::new(
                token,
                &[
                    TokenKind::Number,
                    TokenKind::Float,
                    TokenKind::String,
                    TokenKind::Char,
                    TokenKind::Identifier,
                    TokenKind::Keyword(Keyword::True),
                    TokenKind::Keyword(Keyword::False),
                    TokenKind::LeftBracket,
                ],
                #[cfg(feature = "debug")]
                format!("{} {}:{}", file!(), line!(), column!()),
            ))),
        }
    }

    fn parse_struct_init_fields(&mut self) -> Result<Vec<ast::InitField>> {
        let mut fields = vec![];
        while self.lexer.peek().is_some() && !self.peek_is(TokenKind::RightBrace) {
            let dot = self.consume(TokenKind::Dot)?;
            let name = self.consume(TokenKind::Identifier)?;
            let equal = self.consume(TokenKind::Equal)?;
            fields.push(ast::InitField {
                dot,
                name,
                equal,
                expr: Box::new(self.parse_expr()?),
            });
            if self.peek_is(TokenKind::Comma) {
                self.consume(TokenKind::Comma)?;
            }
        }
        Ok(fields)
    }

    fn parse_struct_instantiation(
        &mut self,
        name: Token,
        type_args: Option<&[ast::Type]>,
    ) -> Result<ast::Expr> {
        let open_brace = self.consume(TokenKind::LeftBrace)?;

        let init_fields = self.parse_struct_init_fields()?;

        let close_brace = self.consume(TokenKind::RightBrace)?;
        Ok(ast::Expr::Struct(ast::ExprStruct {
            name,
            type_args: type_args.map(|t| t.to_vec()),
            open_brace,
            init_fields,
            close_brace,
        }))
    }

    fn parse_array_literal(&mut self, open_bracket: Token) -> Result<ast::Expr> {
        if self.peek_is(TokenKind::RightBracket) {
            let close_bracket = self.consume(TokenKind::RightBracket)?;
            return Ok(ast::Expr::Array(ast::ExprArray {
                open_bracket,
                elements: vec![],
                close_bracket,
                ty: ast::Type {
                    kind: ast::TypeKind::Void,
                    ..Default::default()
                },
            }));
        }

        let first = self.parse_expr()?;

        if self.peek_is(TokenKind::Semicolon) {
            let semicolon = self.consume(TokenKind::Semicolon)?;
            let count = self.parse_expr()?; // must be compile-time integer
            let close_bracket = self.consume(TokenKind::RightBracket)?;

            return Ok(ast::Expr::ArrayRepeat(ast::ExprArrayRepeat {
                open_bracket,
                count: Box::new(count),
                semicolon,
                value: Box::new(first),
                close_bracket,
                ty: ast::Type {
                    kind: ast::TypeKind::Void,
                    ..Default::default()
                },
            }));
        }

        let mut elements = vec![first];
        if self.peek_is(TokenKind::Comma) {
            self.consume(TokenKind::Comma)?;
        }

        while !self.peek_is(TokenKind::RightBracket) {
            elements.push(self.parse_expr()?);
            if self.peek_is(TokenKind::Comma) {
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
            ty: ast::Type {
                kind: ast::TypeKind::Void,
                ..Default::default()
            },
            close_bracket,
        }))
    }
}

fn one_of(tokens: &[TokenKind]) -> impl Fn(&Token) -> bool + use<'_> {
    move |token| tokens.contains(&token.kind)
}

fn token_as_type<'a>(mut_token: Option<Token>, token: &'a Token) -> Result<ast::Type> {
    let parse_type = |input: &'a str| -> Option<(&'a str, &'a str)> {
        let (prefix, rest) = input.split_at(1);
        if rest.is_empty() {
            return None;
        }
        if prefix.chars().all(char::is_alphabetic) && rest.chars().all(char::is_numeric) {
            Some((prefix, rest))
        } else {
            None
        }
    };
    match token.lexeme.as_str() {
        "void" => {
            return Ok(ast::Type {
                kind: ast::TypeKind::Void,
                span: token.span.clone(),
                // Maybe this should be an error?
                mut_token,
            });
        }
        "bool" => {
            return Ok(ast::Type {
                kind: ast::TypeKind::Bool,
                span: token.span.clone(),
                mut_token,
            });
        }
        "usize" => {
            return Ok(ast::Type {
                kind: ast::TypeKind::UnsignedTargetPointerNumber,
                span: token.span.clone(),
                mut_token,
            });
        }
        "ssize" => {
            return Ok(ast::Type {
                kind: ast::TypeKind::SignedTargetPointerNumber,
                span: token.span.clone(),
                mut_token,
            });
        }
        _ => (),
    }
    let Some((prefix, number)) = parse_type(&token.lexeme) else {
        if token.kind == TokenKind::Identifier {
            return Ok(ast::Type {
                kind: ast::TypeKind::Name(token.clone()),
                span: token.span.clone(),
                ..Default::default()
            });
        }
        return Err(Box::new(ErrorExpectedType::new(
            token.clone(),
            #[cfg(feature = "debug")]
            format!("{} {}:{}", file!(), line!(), column!()),
        )));
    };
    match prefix {
        "u" => Ok(ast::Type {
            kind: ast::TypeKind::UnsignedNumber(number.parse().unwrap()),
            span: token.span.clone(),
            mut_token,
        }),
        "s" => Ok(ast::Type {
            kind: ast::TypeKind::SignedNumber(number.parse().unwrap()),
            span: token.span.clone(),
            mut_token,
        }),
        "f" => Ok(ast::Type {
            kind: ast::TypeKind::Float(number.parse().unwrap()),
            span: token.span.clone(),
            mut_token,
        }),
        _ => Err(Box::new(ErrorExpectedType::new(
            token.clone(),
            #[cfg(feature = "debug")]
            format!("{} {}:{}", file!(), line!(), column!()),
        ))),
    }
}
