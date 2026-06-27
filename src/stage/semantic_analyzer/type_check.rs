#![allow(unused)]
use std::str::FromStr;

use super::type_resolver::TypeResolver;
use crate::error::{
    ErrorMissMatchedType, ErrorUndefinedSymbol, ErrorUnsupportedBinaryOp, Errors, Report, Result,
};
use crate::stage::lexer::token::{Keyword as Kw, Span, Token, TokenKind};
use crate::stage::parser::ast::{self, Expr, StructType, Type, TypeKind};
use crate::stage::semantic_analyzer::symbol_table::SymbolTable;

impl Type {
    pub fn supports_binary_op(&self, op: &TokenKind, other: &Type, span: Span) -> Option<Type> {
        use TokenKind::*;
        match (&self.kind, op, &other.kind) {
            // (Plus | Minus | Star | Slash | Percent) only work on numbers and return the same type
            (
                TypeKind::SignedTargetPointerNumber,
                (Plus | Minus | Star | Slash | Percent | BitShiftRight | Ampersand),
                TypeKind::SignedTargetPointerNumber,
            ) => Some(self.clone()),
            (
                TypeKind::UnsignedTargetPointerNumber,
                (Plus | Minus | Star | Slash | Percent | BitShiftRight | Ampersand),
                TypeKind::UnsignedTargetPointerNumber,
            ) => Some(self.clone()),
            (
                TypeKind::UnsignedNumber(lhs),
                (Plus | Minus | Star | Slash | Percent | BitShiftRight | Ampersand),
                TypeKind::UnsignedNumber(rhs),
            ) if lhs == rhs => Some(self.clone()),
            (
                TypeKind::SignedNumber(lhs),
                (Plus | Minus | Star | Slash | Percent | BitShiftRight | Ampersand),
                TypeKind::SignedNumber(rhs),
            ) if lhs == rhs => Some(self.clone()),
            (
                TypeKind::Float(lhs),
                (Plus | Minus | Star | Slash | Percent | BitShiftRight | Ampersand),
                TypeKind::Float(rhs),
            ) if lhs == rhs => Some(self.clone()),

            // (EqualEqual | Greater | GreaterEqual | Less | LessEqual) Comparison ops work on numbers and return bools
            (
                TypeKind::UnsignedTargetPointerNumber,
                (EqualEqual | Greater | GreaterEqual | Less | LessEqual),
                TypeKind::UnsignedTargetPointerNumber,
            ) => Some(self.map_kind(|_| TypeKind::Bool)),
            (
                TypeKind::SignedTargetPointerNumber,
                (EqualEqual | Greater | GreaterEqual | Less | LessEqual),
                TypeKind::SignedTargetPointerNumber,
            ) => Some(self.map_kind(|_| TypeKind::Bool)),
            (
                TypeKind::UnsignedNumber(lhs),
                (EqualEqual | Greater | GreaterEqual | Less | LessEqual),
                TypeKind::UnsignedNumber(rhs),
            ) if lhs == rhs => Some(self.map_kind(|_| TypeKind::Bool)),
            (
                TypeKind::SignedNumber(lhs),
                (EqualEqual | Greater | GreaterEqual | Less | LessEqual),
                TypeKind::SignedNumber(rhs),
            ) if lhs == rhs => Some(self.map_kind(|_| TypeKind::Bool)),
            (
                TypeKind::Float(lhs),
                (EqualEqual | Greater | GreaterEqual | Less | LessEqual),
                TypeKind::Float(rhs),
            ) if lhs == rhs => Some(self.map_kind(|_| TypeKind::Bool)),

            // (AND | OR) only work on bools and return bools
            (TypeKind::Bool, (EqualEqual | Keyword(Kw::And) | Keyword(Kw::Or)), TypeKind::Bool) => {
                Some(self.map_kind(|_| TypeKind::Bool))
            }
            _ => None,
        }
        .map(|mut t| {
            t.span = span;
            t
        })
    }

    fn resolve_overloaded_op(&self, op: &TokenKind, rhs: &str) -> Option<Type> {
        // You could delegate this to symbol table, traits, whatever
        None
    }
}

pub struct TypeChecker<'st> {
    symbol_table: &'st mut SymbolTable,
    errors: Vec<Box<dyn Report>>,
    /// When set, integer literals inside an array literal are typed as this
    /// instead of the default `SignedNumber(32)`. Allows `[10; u8] = [65, 66, ...]`.
    numeric_hint: Option<Type>,
}

impl<'st> TypeChecker<'st> {
    pub fn new(symbol_table: &'st mut SymbolTable) -> Self {
        Self {
            symbol_table,
            errors: Vec::new(),
            numeric_hint: None,
        }
    }

    pub fn check(mut self, ast: &mut [ast::Item]) -> Result<()> {
        let mut resolver = TypeResolver::new(self.symbol_table);
        resolver.walk_items(ast)?;

        for item in ast.iter_mut() {
            self.walk_item(item);
        }
        if !self.errors.is_empty() {
            return Err(Box::new(Errors {
                errors: self.errors,
            }));
        }
        Ok(())
    }

    fn walk_item(&mut self, item: &mut ast::Item) -> ast::Type {
        match item {
            ast::Item::Function(function) => self.walk_function(function),
            ast::Item::Type(type_def) => self.walk_type_def(type_def),
            ast::Item::Use(r#use) => self.walk_use(r#use),
            ast::Item::ExternFunction(extern_function) => {
                self.walk_extern_function(extern_function)
            }
        }
    }

    fn walk_extern_function(&self, extern_function: &mut ast::ExternFunction) -> Type {
        // Assume extern functions are always correct since we have no way to verify them 🤷
        extern_function.return_type.clone()
    }

    fn walk_use(&mut self, _item: &ast::Use) -> ast::Type {
        Type {
            kind: TypeKind::Void,
            span: 0..1,
            mut_token: None,
        }
    }

    fn walk_function(&mut self, function: &mut ast::Function) -> ast::Type {
        self.symbol_table.enter_scope(function.name.lexeme.as_str());
        let calulated_return_type = self.walk_block(&mut function.body);
        self.symbol_table.exit_scope();
        if calulated_return_type.kind != function.return_type.kind {
            self.errors.push(Box::new(ErrorMissMatchedType::new(
                calulated_return_type,
                function.return_type.kind.clone(),
                #[cfg(feature = "debug")]
                format!("{} {}:{}", file!(), line!(), column!()),
            )));
        }
        function.return_type.clone()
    }

    fn walk_type_def(&mut self, type_def: &mut ast::TypeDef) -> ast::Type {
        match type_def {
            ast::TypeDef::Struct(struct_def) => self.walk_struct_def(struct_def),
        }
    }

    fn walk_struct_def(&mut self, struct_def: &mut ast::Struct) -> ast::Type {
        Type {
            mut_token: None,
            span: struct_def.span(),
            kind: TypeKind::Struct(StructType {
                name: struct_def.name.lexeme.clone(),
                fields: struct_def
                    .fields
                    .iter()
                    .map(|field| (field.name.lexeme.clone(), field.ty.clone()))
                    .collect(),
                packed: false,
            }),
        }
    }

    fn walk_block(&mut self, block: &mut ast::ExprBlock) -> ast::Type {
        let mut last_type = Type {
            kind: TypeKind::Void,
            span: 0..1,
            mut_token: None,
        };

        let last_index = block.statements.len().saturating_sub(1);
        for (i, statement) in block.statements.iter_mut().enumerate() {
            if i != last_index {
                self.walk_expr(&mut statement.expr);
                continue;
            }

            let ty = self.walk_expr(&mut statement.expr);

            if let ast::Expr::Return(expr) = &*statement.expr {
                last_type = ty;
            } else if statement.delem.is_some() {
                last_type = Type {
                    mut_token: None,
                    kind: TypeKind::Void,
                    span: 0..1,
                };
            } else {
                last_type = ty;
            }
        }

        last_type
    }

    fn walk_expr(&mut self, expr: &mut ast::Expr) -> ast::Type {
        match expr {
            ast::Expr::AddressOf(expr) => self.walk_expr_address_of(expr),
            ast::Expr::Array(expr) => self.walk_expr_array(expr),
            ast::Expr::ArrayIndex(expr) => self.walk_expr_array_index(expr),
            ast::Expr::ArrayRepeat(expr) => self.walk_expr_array_repeat(expr),
            ast::Expr::Assignment(expr) => self.walk_expr_assignment(expr),
            ast::Expr::Binary(expr) => self.walk_expr_binary(expr),
            ast::Expr::Block(expr) => self.walk_block(expr),
            ast::Expr::Call(expr) => self.walk_expr_call(expr),
            ast::Expr::Declare(expr) => self.walk_expr_declare(expr),
            ast::Expr::Identifier(expr) => self.walk_expr_identifier(expr),
            ast::Expr::Path(expr) => self.walk_expr_path(expr),
            ast::Expr::IfElse(expr) => self.walk_expr_if_else(expr),
            ast::Expr::Litral(expr) => self.walk_expr_litral(expr),
            ast::Expr::MemberAccess(..) => self.walk_expr_member_access(expr),
            ast::Expr::Not(expr) => self.walk_expr_not(expr),
            ast::Expr::Return(expr) => self.walk_expr_return(expr),
            ast::Expr::Struct(expr) => self.walk_expr_struct(expr),
            ast::Expr::While(expr) => self.walk_expr_while(expr),
            ast::Expr::Grouping(expr_grouping) => self.walk_expr(&mut expr_grouping.expr),
            ast::Expr::TypeCast(expr_cast) => expr_cast.target_type.clone(),
        }
    }

    fn walk_expr_return(&mut self, expr: &mut ast::ExprReturn) -> ast::Type {
        let Some(expr) = expr.expr.as_mut() else {
            return Type {
                kind: TypeKind::Void,
                span: expr.span(),
                mut_token: None,
            };
        };
        self.walk_expr(expr)
    }

    fn walk_expr_struct(&mut self, expr: &mut ast::ExprStruct) -> ast::Type {
        for field in expr.init_fields.iter_mut() {
            self.walk_expr(&mut field.expr);
        }
        let Some(symbol) = self.symbol_table.get(&expr.name.lexeme) else {
            unreachable!("Should never get here, should fail in semantic analyzer");
        };
        symbol.ty.clone()
    }

    fn walk_expr_assignment(&mut self, expr: &mut ast::ExprAssignment) -> ast::Type {
        let lhs = self.walk_expr(&mut expr.left);
        self.maybe_numeric_hint(&lhs);
        let rhs = self.walk_expr(&mut expr.right);
        self.numeric_hint = None;
        if lhs != rhs {
            self.errors.push(Box::new({
                let mut error = ErrorMissMatchedType::new(
                    rhs,
                    lhs.kind.clone(),
                    #[cfg(feature = "debug")]
                    format!("{} {}:{}", file!(), line!(), column!()),
                );
                error.alt_span(expr.left.span());
                error
            }));
        }
        lhs
    }

    fn walk_expr_declare(&mut self, expr: &mut ast::ExprDecl) -> ast::Type {
        // If there's an array type annotation, hint the element type to integer literals
        // so `let x: [10; u8] = [65, 66, ...]` doesn't error with "expected u8, found s32".
        // Furthermore, if there's a non-array numeric type annotation, hint integer literals to that type so
        if let Some(maybe) = &expr.ty {
            self.maybe_numeric_hint(maybe);
        }
        let value_type = self.walk_expr(&mut expr.expr);
        self.numeric_hint = None;

        if let Some(ty) = &expr.ty
            && ty != &value_type
        {
            self.errors.push(Box::new(ErrorMissMatchedType::new(
                value_type.clone(),
                ty.kind.clone(),
                #[cfg(feature = "debug")]
                format!("{} {}:{}", file!(), line!(), column!()),
            )));
        }
        self.symbol_table.get_mut(&expr.ident.lexeme, |s| {
            if s.ty.kind == TypeKind::Void && s.ty != value_type {
                s.ty = value_type.clone()
            }
        });
        value_type
    }

    fn walk_expr_litral(&mut self, expr: &ast::Litral) -> ast::Type {
        match expr {
            ast::Litral::Integer(_) => self.numeric_hint.clone().unwrap_or(Type {
                kind: TypeKind::SignedNumber(32),
                span: expr.span(),
                mut_token: None,
            }),
            ast::Litral::Float(_) => self.numeric_hint.clone().unwrap_or(Type {
                kind: TypeKind::Float(32),
                span: expr.span(),
                mut_token: None,
            }),
            ast::Litral::Char(token) => {
                let bytes = match SmallestCharInt::from_str(&token.lexeme) {
                    Ok(SmallestCharInt::U8(_)) => 8,
                    Ok(SmallestCharInt::U16(_)) => 16,
                    Ok(SmallestCharInt::U32(_)) => 32,
                    v => panic!("not a char {v:?}"),
                };

                Type {
                    kind: TypeKind::UnsignedNumber(bytes),
                    span: expr.span(),
                    mut_token: None,
                }
            }
            ast::Litral::String(s) => Type {
                kind: TypeKind::Array(
                    s.lexeme.len(),
                    Box::new(Type {
                        kind: TypeKind::UnsignedNumber(8),
                        span: expr.span(),
                        mut_token: None,
                    }),
                ),
                span: expr.span(),
                mut_token: None,
            },
            ast::Litral::BoolTrue(_) | ast::Litral::BoolFalse(_) => Type {
                kind: TypeKind::Bool,
                span: expr.span(),
                mut_token: None,
            },
        }
    }

    fn walk_expr_call(&mut self, expr: &mut ast::ExprCall) -> ast::Type {
        let name = match expr.caller.as_ref() {
            ast::Expr::Identifier(ident) => ident.lexeme.clone(),
            ast::Expr::Path(path) => path.leaf().lexeme.clone(),
            _ => panic!("Caller must be an identifier or path"),
        };

        let Some(symbol) = self.symbol_table.get(name.as_str()).cloned() else {
            unreachable!("If seeing this then. Welp I guess I was wrong.");
        };

        for (arg, ty) in expr.args.iter_mut().zip(&symbol.params.unwrap_or_default()) {
            self.maybe_numeric_hint(ty);
            self.walk_expr(arg);
            self.numeric_hint = None;
        }

        symbol.ty.clone()
    }

    fn walk_expr_binary(&mut self, expr: &mut ast::ExprBinary) -> ast::Type {
        let left_ty = self.walk_expr(&mut expr.left);
        self.maybe_numeric_hint(&left_ty);
        let right_ty = self.walk_expr(&mut expr.right);
        self.numeric_hint = None;
        let result_ty = left_ty.supports_binary_op(&expr.op.kind, &right_ty, expr.span());
        let Some(result_ty) = result_ty else {
            self.errors.push(Box::new(ErrorUnsupportedBinaryOp::new(
                expr.op.clone(),
                left_ty.clone(),
                right_ty.clone(),
                #[cfg(feature = "debug")]
                format!("{} {}:{}", file!(), line!(), column!()),
            )));

            return Type {
                kind: TypeKind::Void,
                span: left_ty.span,
                mut_token: None,
            };
        };
        result_ty
    }

    fn walk_expr_identifier(&mut self, expr: &Token) -> ast::Type {
        let Some(symbol) = self.symbol_table.get(expr.lexeme.as_str()) else {
            #[cfg(not(feature = "debug"))]
            let error = ErrorUndefinedSymbol::Token(expr.clone());
            #[cfg(feature = "debug")]
            let compiler_line = format!("{} {}:{}", file!(), line!(), column!());
            #[cfg(feature = "debug")]
            let error = ErrorUndefinedSymbol::TokenDebug(expr.clone(), compiler_line);
            self.errors.push(Box::new(error));
            return Type {
                kind: TypeKind::Void,
                span: expr.span.clone(),
                mut_token: None,
            };
        };
        symbol.ty.clone()
    }

    fn walk_expr_path(&mut self, path: &ast::ExprPath) -> ast::Type {
        let leaf = path.leaf();
        let Some(symbol) = self.symbol_table.get(leaf.lexeme.as_str()) else {
            return Type {
                kind: TypeKind::Void,
                span: leaf.span.clone(),
                mut_token: None,
            };
        };
        symbol.ty.clone()
    }

    fn walk_expr_if_else(&mut self, expr: &mut ast::ExprIfElse) -> ast::Type {
        let condition = self.walk_expr(&mut expr.condition);
        if !matches!(condition.kind, TypeKind::Bool) {
            let error = ErrorMissMatchedType::new(
                condition,
                TypeKind::Bool,
                #[cfg(feature = "debug")]
                format!("{} {}:{}", file!(), line!(), column!()),
            );
            self.errors.push(Box::new(error));
        }
        let then_branch_type = self.walk_block(&mut expr.then_branch);
        if let Some(else_branch) = expr.else_branch.as_mut() {
            let else_branch_type = self.walk_expr(else_branch);
            if then_branch_type != else_branch_type {
                self.errors.push(Box::new(ErrorMissMatchedType::new(
                    then_branch_type.clone(),
                    else_branch_type.kind.clone(),
                    #[cfg(feature = "debug")]
                    format!("{} {}:{}", file!(), line!(), column!()),
                )));
            }
            expr.ty = then_branch_type.clone();
            return then_branch_type;
        }
        Type {
            kind: TypeKind::Void,
            span: expr.span(),
            mut_token: None,
        }
    }

    fn walk_expr_array(&mut self, expr: &mut ast::ExprArray) -> Type {
        let size = expr.elements.len();
        let ty = self.walk_expr(&mut expr.elements[0]);
        for mut element in expr.elements.iter_mut().skip(1) {
            let other = self.walk_expr(element);
            if ty != other {
                self.errors.push(Box::new(ErrorMissMatchedType::new(
                    ty.clone(),
                    other.kind,
                    #[cfg(feature = "debug")]
                    format!("{} {}:{}", file!(), line!(), column!()),
                )));
            }
        }
        expr.ty = ty.clone();
        Type {
            kind: TypeKind::Array(size, Box::new(ty)),
            span: expr.span(),
            mut_token: None,
        }
    }

    fn walk_expr_array_index(&mut self, expr: &mut ast::ExprArrayIndex) -> Type {
        let array_type = self.walk_expr(&mut expr.expr);
        self.maybe_numeric_hint(&ast::Type {
            mut_token: None,
            kind: ast::TypeKind::UnsignedTargetPointerNumber,
            span: expr.index.span(),
        });
        let index_type = self.walk_expr(&mut expr.index);
        if index_type.kind != TypeKind::UnsignedTargetPointerNumber {
            self.errors.push(Box::new(ErrorMissMatchedType::new(
                index_type.clone(),
                TypeKind::UnsignedTargetPointerNumber,
                #[cfg(feature = "debug")]
                format!("{} {}:{}", file!(), line!(), column!()),
            )));
        }

        expr.ty = array_type.clone();

        let (TypeKind::Array(_, array_type) | TypeKind::Slice(array_type)) =
            &array_type.de_ref().kind
        else {
            eprintln!("{}", array_type.kind);
            // NOTE: This probably will cause some missleading errors.
            self.errors.push(Box::new(ErrorMissMatchedType::new(
                array_type.clone(),
                TypeKind::Array(
                    0,
                    Box::new(Type {
                        kind: TypeKind::Void,
                        span: 0..1,
                        mut_token: None,
                    }),
                ),
                #[cfg(feature = "debug")]
                format!("{} {}:{}", file!(), line!(), column!()),
            )));
            return array_type;
        };
        *array_type.clone()
    }

    fn walk_expr_address_of(&mut self, expr: &mut ast::ExprAddressOf) -> Type {
        let inner_type = self.walk_expr(&mut expr.expr);
        Type {
            kind: TypeKind::Ref(Box::new(inner_type)),
            span: expr.span(),
            mut_token: None,
        }
    }

    fn walk_expr_not(&mut self, expr: &mut ast::ExprNot) -> Type {
        self.walk_expr(&mut expr.expr);
        Type {
            kind: TypeKind::Bool,
            span: expr.span(),
            mut_token: None,
        }
    }

    fn walk_expr_array_repeat(&mut self, expr: &mut ast::ExprArrayRepeat) -> Type {
        let ast::ExprArrayRepeat { value, count, .. } = expr;
        let value_type = self.walk_expr(value);
        let count_type = self.walk_expr(count);
        let Expr::Litral(ast::Litral::Integer(count_token)) = &**count else {
            self.errors.push(Box::new(ErrorMissMatchedType::new(
                count_type.clone(),
                TypeKind::SignedNumber(32),
                #[cfg(feature = "debug")]
                format!("{} {}:{}", file!(), line!(), column!()),
            )));
            return value_type;
        };

        expr.ty = Type {
            kind: TypeKind::Array(
                count_token.lexeme.parse::<usize>().unwrap(),
                Box::new(value_type.clone()),
            ),
            span: expr.span(),
            mut_token: None,
        };

        expr.ty.clone()
    }

    fn walk_expr_while(&mut self, expr: &mut ast::ExprWhile) -> Type {
        let condition = self.walk_expr(&mut expr.condition);
        if !matches!(condition.kind, TypeKind::Bool) {
            self.errors.push(Box::new(ErrorMissMatchedType::new(
                condition,
                TypeKind::Bool,
                #[cfg(feature = "debug")]
                format!("{} {}:{}", file!(), line!(), column!()),
            )));
        }
        self.walk_block(&mut expr.body)
    }

    fn walk_expr_member_access(&mut self, expr: &mut ast::Expr) -> Type {
        let ast::Expr::MemberAccess(mut member_access) = expr.clone() else {
            panic!("Expected ExprArray");
        };
        let base_type = self.walk_expr(&mut member_access.base);
        match base_type.kind {
            TypeKind::Array(size, _) if member_access.member.lexeme == "len" => {
                let token = Token {
                    kind: TokenKind::Number,
                    lexeme: size.to_string(),
                    span: expr.span(),
                };
                *expr = ast::Expr::Litral(ast::Litral::Integer(token));
                Type {
                    kind: TypeKind::UnsignedTargetPointerNumber,
                    span: expr.span(),
                    mut_token: None,
                }
            }
            TypeKind::Struct(struct_def) => {
                self.lookup_struct_member(&struct_def, &member_access.member.lexeme)
            }
            TypeKind::Slice(_) if member_access.member.lexeme == "len" => Type {
                kind: TypeKind::UnsignedTargetPointerNumber,
                span: expr.span(),
                mut_token: None,
            },
            TypeKind::Slice(inner_ty) if member_access.member.lexeme == "data" => Type {
                kind: TypeKind::Ref(inner_ty.clone()),
                span: expr.span(),
                mut_token: None,
            },
            TypeKind::Ref(inner) => match &inner.kind {
                TypeKind::Struct(struct_def) => {
                    self.lookup_struct_member(struct_def, &member_access.member.lexeme)
                }
                TypeKind::Slice(inner_ty) if member_access.member.lexeme == "data" => Type {
                    kind: TypeKind::Ref(inner_ty.clone()),
                    span: expr.span(),
                    mut_token: None,
                },
                TypeKind::Slice(_) if member_access.member.lexeme == "len" => Type {
                    kind: TypeKind::UnsignedTargetPointerNumber,
                    span: expr.span(),
                    mut_token: None,
                },
                _ => unimplemented!("Handle member access for non struct pointer types"),
            },
            _ => unimplemented!("Handle member access for non array types"),
        }
    }

    fn maybe_numeric_hint(&mut self, maybe: &Type) {
        match &maybe.kind {
            TypeKind::Array(_, elem_ty) => {
                self.numeric_hint = Some(Type {
                    kind: elem_ty.kind.clone(),
                    span: maybe.span.clone(),
                    mut_token: None,
                })
            }
            ty @ TypeKind::SignedNumber(_) => {
                self.numeric_hint = Some(Type {
                    kind: ty.clone(),
                    span: maybe.span.clone(),
                    mut_token: None,
                })
            }
            ty @ TypeKind::UnsignedNumber(_) => {
                self.numeric_hint = Some(Type {
                    kind: ty.clone(),
                    span: maybe.span.clone(),
                    mut_token: None,
                })
            }
            ty @ TypeKind::Float(_) => {
                self.numeric_hint = Some(Type {
                    kind: ty.clone(),
                    span: maybe.span.clone(),
                    mut_token: None,
                })
            }
            ty @ TypeKind::SignedTargetPointerNumber => {
                self.numeric_hint = Some(Type {
                    kind: ty.clone(),
                    span: maybe.span.clone(),
                    mut_token: None,
                })
            }
            ty @ TypeKind::UnsignedTargetPointerNumber => {
                self.numeric_hint = Some(Type {
                    kind: ty.clone(),
                    span: maybe.span.clone(),
                    mut_token: None,
                })
            }
            _ => {}
        }
    }

    fn lookup_struct_member(&self, struct_def: &StructType, member: &str) -> Type {
        let Some(symbol) = self.symbol_table.get(struct_def.name.as_str()) else {
            panic!(
                "Could not find struct `{}` in symbol table",
                struct_def.name
            );
        };
        let Some(members) = &symbol.fields else {
            panic!(
                "Could not find fields for struct `{}` in symbol table",
                struct_def.name
            );
        };
        let Some(field) = members.iter().find(|f| f.name == member) else {
            panic!(
                "Field `{}` not found on struct `{}`",
                member, struct_def.name
            );
        };
        field.ty.clone()
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum SmallestCharInt {
    U8(u8),
    U16(u16),
    U32(u32),
}

impl SmallestCharInt {
    pub fn value(&self) -> u32 {
        match self {
            SmallestCharInt::U8(v) => *v as u32,
            SmallestCharInt::U16(v) => *v as u32,
            SmallestCharInt::U32(v) => *v,
        }
    }

    fn parse_char(s: &str) -> std::result::Result<char, char> {
        match s {
            "\\n" => Ok('\n'),
            "\\t" => Ok('\t'),
            "\\r" => Ok('\r'),
            "\\0" => Ok('\0'),
            _ => {
                let mut chars = s.chars();
                let ch = chars.next().ok_or('\0')?;
                if let Some(c) = chars.next() {
                    return Err(c);
                }
                Ok(ch)
            }
        }
    }
}

impl FromStr for SmallestCharInt {
    type Err = char;
    fn from_str(s: &str) -> std::result::Result<Self, Self::Err> {
        let ch = SmallestCharInt::parse_char(s)?;

        let code = ch as u32;

        if code <= u8::MAX as u32 {
            Ok(SmallestCharInt::U8(code as u8))
        } else if code <= u16::MAX as u32 {
            Ok(SmallestCharInt::U16(code as u16))
        } else {
            Ok(SmallestCharInt::U32(code))
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::stage::Stage;

    #[test]
    fn test_type_check() {
        let src = r#"
        fn add(a: s32, b: s32) s32 {
            return a + b;
        }
        "#;

        let tokens = crate::stage::lexer::Lexer.run(src);
        let mut ast = crate::stage::parser::Parser::default().run(tokens).unwrap();
        let mut symbol_table =
            crate::stage::semantic_analyzer::symbol_table::SymbolTableBuilder::default()
                .build(&mut ast)
                .unwrap();
        let type_checker = TypeChecker::new(&mut symbol_table);
        if let Err(errors) = type_checker.check(&mut ast) {
            eprintln!("{}", errors.report("type_check.cb", src));
            assert!(false);
        }
        assert!(true);
    }
}
