#![allow(unused)]
use super::type_resolver::TypeResolver;
use crate::error::{
    ErrorMissMatchedType, ErrorUndefinedSymbol, ErrorUnsupportedBinaryOp, Errors, Report, Result,
};
use crate::stage::lexer::token::{Keyword as Kw, Token, TokenKind};
use crate::stage::parser::ast::{self, Expr, StructType, Type};
use crate::stage::semantic_analyzer::symbol_table::SymbolTable;

impl Type {
    pub fn supports_binary_op(&self, op: &TokenKind, other: &Type) -> Option<Type> {
        use TokenKind::*;
        match (self, op, other) {
            // (Plus | Minus | Star | Slash | Percent) only work on numbers and return the same type
            (
                Type::UnsignedNumber(lhs),
                (Plus | Minus | Star | Slash | Percent),
                Type::UnsignedNumber(rhs),
            ) if lhs == rhs => Some(Type::UnsignedNumber(*lhs)),
            (
                Type::SignedNumber(lhs),
                (Plus | Minus | Star | Slash | Percent),
                Type::SignedNumber(rhs),
            ) if lhs == rhs => Some(Type::SignedNumber(*lhs)),
            (Type::Float(lhs), (Plus | Minus | Star | Slash | Percent), Type::Float(rhs))
                if lhs == rhs =>
            {
                Some(Type::Float(*lhs))
            }

            // (EqualEqual | Greater | GreaterEqual | Less | LessEqual) Comparison ops work on numbers and return bools
            (
                Type::UnsignedNumber(lhs),
                (EqualEqual | Greater | GreaterEqual | Less | LessEqual),
                Type::UnsignedNumber(rhs),
            ) if lhs == rhs => Some(Type::Bool),
            (
                Type::SignedNumber(lhs),
                (EqualEqual | Greater | GreaterEqual | Less | LessEqual),
                Type::SignedNumber(rhs),
            ) if lhs == rhs => Some(Type::Bool),
            (
                Type::Float(lhs),
                (EqualEqual | Greater | GreaterEqual | Less | LessEqual),
                Type::Float(rhs),
            ) if lhs == rhs => Some(Type::Bool),

            // (AND | OR) only work on bools and return bools
            (Type::Bool, (EqualEqual | Keyword(Kw::And) | Keyword(Kw::Or)), Type::Bool) => {
                Some(Type::Bool)
            }

            // (Plus | Minus | Star | Slash) only work on numbers and return the same type
            (Type::SignedNumber(lhs), (Plus | Minus | Star | Slash), Type::SignedNumber(rhs))
                if lhs == rhs =>
            {
                Some(Type::SignedNumber(*lhs))
            }
            (Type::Float(lhs), Plus | Minus | Star | Slash, Type::Float(rhs)) => {
                Some(Type::Float(*lhs))
            }

            //(Type::Custom(name), op, Type::Custom(rhs)) => {
            //    // For operator overloading — check user-defined impls
            //    // e.g., lookup "impl Add for MyType { ... }" in your symbol table maybe
            //    self.resolve_overloaded_op(op, rhs)
            //}
            _ => None,
        }
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
        // For now we just assume extern functions are always correct since we have no way to verify them
        extern_function.return_type.clone()
    }

    fn walk_use(&mut self, item: &ast::Use) -> ast::Type {
        todo!()
    }

    fn walk_function(&mut self, function: &mut ast::Function) -> ast::Type {
        self.symbol_table.enter_scope(function.name.lexeme.as_str());
        let calulated_return_type = self.walk_block(&mut function.body);
        self.symbol_table.exit_scope();
        if calulated_return_type != function.return_type {
            self.errors.push(Box::new(ErrorMissMatchedType {
                span: function.fn_token.span.clone(),
                found: calulated_return_type,
                expected: function.return_type.clone(),
                #[cfg(feature = "debug")]
                compiler_line: format!("{} {}:{}", file!(), line!(), column!()),
            }));
        }
        function.return_type.clone()
    }

    fn walk_type_def(&mut self, type_def: &mut ast::TypeDef) -> ast::Type {
        match type_def {
            ast::TypeDef::Struct(struct_def) => self.walk_struct_def(struct_def),
        }
    }

    fn walk_struct_def(&mut self, struct_def: &mut ast::Struct) -> ast::Type {
        Type::Struct(StructType {
            name: struct_def.name.lexeme.clone(),
            fields: struct_def
                .fields
                .iter()
                .map(|field| (field.name.lexeme.clone(), field.ty.clone()))
                .collect(),
            packed: false,
        })
    }

    fn walk_block(&mut self, block: &mut ast::ExprBlock) -> ast::Type {
        let mut last_type = Type::Void;

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
                last_type = Type::Void;
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
            ast::Expr::IfElse(expr) => self.walk_expr_if_else(expr),
            ast::Expr::Litral(expr) => self.walk_expr_litral(expr),
            ast::Expr::MemberAccess(..) => self.walk_expr_member_access(expr),
            ast::Expr::Not(expr) => self.walk_expr_not(expr),
            ast::Expr::Return(expr) => self.walk_expr_return(expr),
            ast::Expr::Struct(expr) => self.walk_expr_struct(expr),
            ast::Expr::While(expr) => self.walk_expr_while(expr),
            ast::Expr::Grouping(expr_grouping) => self.walk_expr(&mut expr_grouping.expr),
        }
    }

    fn walk_expr_return(&mut self, expr: &mut ast::ExprReturn) -> ast::Type {
        let Some(expr) = expr.expr.as_mut() else {
            return Type::Void;
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
            self.errors.push(Box::new(ErrorMissMatchedType {
                span: expr.right.span(),
                found: rhs,
                expected: lhs.clone(),
                #[cfg(feature = "debug")]
                compiler_line: format!("{} {}:{}", file!(), line!(), column!()),
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
            self.errors.push(Box::new(ErrorMissMatchedType {
                span: expr.span(),
                found: value_type.clone(),
                expected: ty.clone(),
                #[cfg(feature = "debug")]
                compiler_line: format!("{} {}:{}", file!(), line!(), column!()),
            }));
        }
        self.symbol_table.get_mut(&expr.ident.lexeme, |s| {
            if s.ty == Type::Void && s.ty != value_type {
                s.ty = value_type.clone()
            }
        });
        value_type
    }

    fn walk_expr_litral(&mut self, expr: &ast::Litral) -> ast::Type {
        match expr {
            ast::Litral::Integer(_) => self.numeric_hint.clone().unwrap_or(Type::SignedNumber(32)),
            ast::Litral::Float(_) => self.numeric_hint.clone().unwrap_or(Type::Float(32)),
            ast::Litral::Char(_) => Type::UnsignedNumber(8),
            ast::Litral::String(s) => {
                Type::Array(s.lexeme.len(), Box::new(Type::UnsignedNumber(8)))
            }
            ast::Litral::BoolTrue(_) => Type::Bool,
            ast::Litral::BoolFalse(_) => Type::Bool,
        }
    }

    fn walk_expr_call(&mut self, expr: &ast::ExprCall) -> ast::Type {
        let ast::Expr::Identifier(ident) = &expr.caller.as_ref() else {
            panic!("Caller must be an identifier");
        };

        let Some(symbol) = self.symbol_table.get(ident.lexeme.as_str()) else {
            unreachable!("If seeing this then. Welp I guess I was wrong.");
        };
        symbol.ty.clone()
    }

    fn walk_expr_binary(&mut self, expr: &mut ast::ExprBinary) -> ast::Type {
        let left_ty = self.walk_expr(&mut expr.left);
        let right_ty = self.walk_expr(&mut expr.right);
        let result_ty = left_ty.supports_binary_op(&expr.op.kind, &right_ty);
        let Some(result_ty) = result_ty else {
            self.errors.push(Box::new(ErrorUnsupportedBinaryOp {
                span: expr.span(),
                lhs: left_ty.clone(),
                rhs: right_ty.clone(),
                op: expr.op.clone(),
            }));

            return Type::Void;
        };
        result_ty
    }

    fn walk_expr_identifier(&mut self, expr: &Token) -> ast::Type {
        let Some(symbol) = self.symbol_table.get(expr.lexeme.as_str()) else {
            self.errors.push(Box::new(ErrorUndefinedSymbol {
                found: expr.clone(),
            }));
            return Type::Void;
        };
        symbol.ty.clone()
    }

    fn walk_expr_if_else(&mut self, expr: &mut ast::ExprIfElse) -> ast::Type {
        let condition = self.walk_expr(&mut expr.condition);
        if !matches!(condition, Type::Bool) {
            self.errors.push(Box::new(ErrorMissMatchedType {
                span: expr.condition.span(),
                found: condition,
                expected: Type::Bool,
                #[cfg(feature = "debug")]
                compiler_line: format!("{} {}:{}", file!(), line!(), column!()),
            }));
        }
        let then_branch_type = self.walk_block(&mut expr.then_branch);
        if let Some(else_branch) = expr.else_branch.as_mut() {
            let else_branch_type = self.walk_expr(else_branch);
            if then_branch_type != else_branch_type {
                self.errors.push(Box::new(ErrorMissMatchedType {
                    span: expr.span(),
                    found: then_branch_type.clone(),
                    expected: else_branch_type.clone(),
                    #[cfg(feature = "debug")]
                    compiler_line: format!("{} {}:{}", file!(), line!(), column!()),
                }));
            }
            expr.ty = then_branch_type.clone();
            return then_branch_type;
        }
        Type::Void
    }

    fn walk_expr_array(&mut self, expr: &mut ast::ExprArray) -> Type {
        let size = expr.elements.len();
        let ty = self.walk_expr(&mut expr.elements[0]);
        for mut element in expr.elements.iter_mut().skip(1) {
            let other = self.walk_expr(element);
            if ty != other {
                self.errors.push(Box::new(ErrorMissMatchedType {
                    span: element.span(),
                    found: ty.clone(),
                    expected: other,
                    #[cfg(feature = "debug")]
                    compiler_line: format!("{} {}:{}", file!(), line!(), column!()),
                }));
            }
        }
        expr.ty = ty.clone();
        Type::Array(size, Box::new(ty))
    }
    fn walk_expr_array_index(&mut self, expr: &mut ast::ExprArrayIndex) -> Type {
        let array_type = self.walk_expr(&mut expr.expr);
        let index_type = self.walk_expr(&mut expr.index);
        // HACK: This should be of type `usize` later once we have support for pointers
        if index_type != Type::SignedNumber(32) {
            self.errors.push(Box::new(ErrorMissMatchedType {
                span: expr.span(),
                found: index_type.clone(),
                // HACK: usize
                expected: Type::SignedNumber(32),
                #[cfg(feature = "debug")]
                compiler_line: format!("{} {}:{}", file!(), line!(), column!()),
            }));
        }

        expr.ty = array_type.clone();

        let Type::Array(_, array_type) = array_type else {
            panic!("Expected array type")
        };
        *array_type
    }

    fn walk_expr_address_of(&mut self, expr: &mut ast::ExprAddressOf) -> Type {
        let inner_type = self.walk_expr(&mut expr.expr);
        Type::Pointer(Box::new(inner_type))
    }

    fn walk_expr_not(&mut self, expr: &mut ast::ExprNot) -> Type {
        self.walk_expr(&mut expr.expr);
        Type::Bool
    }

    fn walk_expr_array_repeat(&mut self, expr: &mut ast::ExprArrayRepeat) -> Type {
        let ast::ExprArrayRepeat { value, count, .. } = expr;
        let value_type = self.walk_expr(value);
        let count_type = self.walk_expr(count);
        let Expr::Litral(ast::Litral::Integer(count_token)) = &**count else {
            self.errors.push(Box::new(ErrorMissMatchedType {
                span: count.span(),
                found: count_type.clone(),
                expected: Type::SignedNumber(32),
                #[cfg(feature = "debug")]
                compiler_line: format!("{} {}:{}", file!(), line!(), column!()),
            }));
            return value_type;
        };

        expr.ty = Type::Array(
            count_token.lexeme.parse::<usize>().unwrap(),
            Box::new(value_type.clone()),
        );

        expr.ty.clone()
    }

    fn walk_expr_while(&mut self, expr: &mut ast::ExprWhile) -> Type {
        let condition = self.walk_expr(&mut expr.condition);
        if !matches!(condition, Type::Bool) {
            self.errors.push(Box::new(ErrorMissMatchedType {
                span: expr.condition.span(),
                found: condition,
                expected: Type::Bool,
                #[cfg(feature = "debug")]
                compiler_line: format!("{} {}:{}", file!(), line!(), column!()),
            }));
        }
        self.walk_block(&mut expr.body)
    }

    fn walk_expr_member_access(&mut self, expr: &mut ast::Expr) -> Type {
        let ast::Expr::MemberAccess(mut member_access) = expr.clone() else {
            panic!("Expected ExprArray");
        };
        let base_type = self.walk_expr(&mut member_access.base);
        match base_type {
            Type::Array(size, _) if member_access.member.lexeme == "len" => {
                let token = Token {
                    kind: TokenKind::Number,
                    lexeme: size.to_string(),
                    span: expr.span(),
                };
                *expr = ast::Expr::Litral(ast::Litral::Integer(token));
                Type::SignedNumber(32)
            }
            Type::Struct(struct_def) => {
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

                let Some(field) = members
                    .iter()
                    .find(|field| field.name == member_access.member.lexeme.as_str())
                else {
                    unimplemented!(
                        "I also do not think this is a reachable state. Hope I never see this."
                    );
                };
                field.ty.clone()
            }
            _ => unimplemented!("Handle member access for non array types"),
        }
    }

    fn maybe_numeric_hint(&mut self, maybe: &Type) {
        match maybe {
            Type::Array(_, elem_ty) => self.numeric_hint = Some(*elem_ty.clone()),
            ty @ Type::SignedNumber(_) => self.numeric_hint = Some(ty.clone()),
            ty @ Type::UnsignedNumber(_) => self.numeric_hint = Some(ty.clone()),
            ty @ Type::Float(_) => self.numeric_hint = Some(ty.clone()),
            _ => {}
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
