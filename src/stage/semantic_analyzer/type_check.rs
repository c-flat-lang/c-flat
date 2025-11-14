#![allow(unused)]
use crate::error::CompilerError;
use crate::stage::lexer::token::{Token, TokenKind};
use crate::stage::parser::ast::{self, Expr, Type};
use crate::stage::semantic_analyzer::symbol_table::SymbolTable;

impl Type {
    pub fn supports_binary_op(&self, op: &TokenKind, other: &Type) -> Option<Type> {
        use TokenKind::*;
        match (self, op, other) {
            (
                Type::UnsignedNumber(lhs),
                (Plus | Minus | Star | Slash),
                Type::UnsignedNumber(rhs),
            ) if lhs == rhs => Some(Type::UnsignedNumber(*lhs)),
            (
                Type::UnsignedNumber(lhs),
                (EqualEqual | Greater | GreaterEqual | Less | LessEqual),
                Type::UnsignedNumber(rhs),
            ) if lhs == rhs => Some(Type::Bool),
            (Type::SignedNumber(lhs), (Plus | Minus | Star | Slash), Type::SignedNumber(rhs))
                if lhs == rhs =>
            {
                Some(Type::SignedNumber(*lhs))
            }
            (
                Type::SignedNumber(lhs),
                (EqualEqual | Greater | GreaterEqual | Less | LessEqual),
                Type::SignedNumber(rhs),
            ) if lhs == rhs => Some(Type::Bool),
            (Type::Float(lhs), TokenKind::Plus, Type::Float(rhs)) => Some(Type::Float(*lhs)),
            // (Type::Bool, TokenKind::, Type::Bool) => Some(Type::Bool),
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
    current_type: Option<Type>,
    errors: Vec<String>,
}

impl<'st> TypeChecker<'st> {
    pub fn new(symbol_table: &'st mut SymbolTable) -> Self {
        Self {
            symbol_table,
            current_type: None,
            errors: Vec::new(),
        }
    }

    pub fn check(mut self, ast: &mut [ast::Item]) -> Result<(), CompilerError> {
        for item in ast.iter_mut() {
            self.walk_item(item);
        }
        if !self.errors.is_empty() {
            return Err(CompilerError::TypeErrors(self.errors));
        }
        Ok(())
    }

    fn walk_item(&mut self, item: &mut ast::Item) -> ast::Type {
        match item {
            ast::Item::Function(function) => self.walk_function(function),
            ast::Item::Type(type_def) => self.walk_type_def(type_def),
            ast::Item::Use(r#use) => self.walk_use(r#use),
        }
    }

    fn walk_use(&mut self, item: &ast::Use) -> ast::Type {
        todo!()
    }

    fn walk_function(&mut self, function: &mut ast::Function) -> ast::Type {
        self.symbol_table.enter_scope(function.name.lexeme.as_str());
        let calulated_return_type = self.walk_block(&mut function.body);
        self.symbol_table.exit_scope();
        if calulated_return_type != function.return_type {
            let error_message = format!(
                "{} expected return type `{}` but found `{}` instead",
                function.name.lexeme, calulated_return_type, function.return_type
            );
            self.errors.push(error_message);
        }
        function.return_type.clone()
    }

    fn walk_type_def(&mut self, type_def: &mut ast::TypeDef) -> ast::Type {
        todo!()
    }

    fn walk_struct(&mut self, struct_def: &mut ast::Struct) -> ast::Type {
        todo!()
    }

    fn walk_block(&mut self, block: &mut ast::Block) -> ast::Type {
        let mut result = Type::Void;
        for statement in block.statements.iter_mut() {
            result = self.walk_stmt(statement);
        }
        result
    }

    fn walk_stmt(&mut self, stmt: &mut ast::Statement) -> ast::Type {
        self.walk_expr(&mut stmt.expr)
    }

    fn walk_expr(&mut self, expr: &mut ast::Expr) -> ast::Type {
        match expr {
            ast::Expr::Return(expr) => self.walk_expr_return(expr),
            ast::Expr::Struct(expr) => self.walk_expr_struct(expr),
            ast::Expr::Assignment(expr) => self.walk_expr_assignment(expr),
            ast::Expr::Declare(expr) => self.walk_expr_declare(expr),
            ast::Expr::Litral(expr) => self.walk_expr_litral(expr),
            ast::Expr::Call(expr) => self.walk_expr_call(expr),
            ast::Expr::Binary(expr) => self.walk_expr_binary(expr),
            ast::Expr::Identifier(expr) => self.walk_expr_identifier(expr),
            ast::Expr::IfElse(expr) => self.walk_expr_if_else(expr),
            ast::Expr::Array(expr) => self.walk_expr_array(expr),
            ast::Expr::ArrayIndex(expr) => self.walk_expr_array_index(expr),
            ast::Expr::ArrayRepeat(expr) => self.walk_expr_array_repeat(expr),
        }
    }

    fn walk_expr_return(&mut self, expr: &mut ast::ExprReturn) -> ast::Type {
        let Some(expr) = expr.expr.as_mut() else {
            return Type::Void;
        };
        self.walk_expr(expr)
    }

    fn walk_expr_struct(&mut self, expr: &ast::ExprStruct) -> ast::Type {
        todo!()
    }

    fn walk_expr_assignment(&mut self, expr: &mut ast::ExprAssignment) -> ast::Type {
        let lhs = self.walk_expr(&mut expr.left);
        let rhs = self.walk_expr(&mut expr.right);
        if lhs != rhs {
            self.errors.push(format!(
                "Type mismatch in assignment found `{}` expected `{}`",
                rhs, lhs
            ))
        }
        lhs
    }

    fn walk_expr_declare(&mut self, expr: &mut ast::ExprDecl) -> ast::Type {
        let value_type = self.walk_expr(&mut expr.expr);
        if let Some(ty) = &expr.ty
            && ty != &value_type
        {
            // TODO: log error and return the value_type
            // Do not block
            self.errors.push(format!(
                "Type mismatch: expected {} but got {}",
                ty, value_type
            ))
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
            ast::Litral::Integer(_) => Type::SignedNumber(32),
            ast::Litral::Float(_) => Type::Float(32),
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

        if ident.lexeme == "print" {
            return Type::Void;
        }

        let Some(symbol) = self.symbol_table.get(ident.lexeme.as_str()) else {
            self.errors
                .push(format!("Undefined function: {}", ident.lexeme));
            return Type::Void;
        };
        symbol.ty.clone()
    }

    fn walk_expr_binary(&mut self, expr: &mut ast::ExprBinary) -> ast::Type {
        let left_ty = self.walk_expr(&mut expr.left);
        let right_ty = self.walk_expr(&mut expr.right);

        match left_ty.supports_binary_op(&expr.op.kind, &right_ty) {
            Some(result_ty) => result_ty,
            None => {
                self.errors.push(format!(
                    "Unsupported binary operation: lhs: {} {} rhs: {}",
                    left_ty, expr.op.lexeme, right_ty
                ));

                Type::Void
            }
        }
    }

    fn walk_expr_identifier(&mut self, expr: &Token) -> ast::Type {
        let Some(symbol) = self.symbol_table.get(expr.lexeme.as_str()) else {
            self.errors
                .push(format!("Undefined variable: {}", expr.lexeme));
            return Type::Void;
        };
        symbol.ty.clone()
    }

    fn walk_expr_if_else(&mut self, expr: &mut ast::ExprIfElse) -> ast::Type {
        let condition = self.walk_expr(&mut expr.condition);
        if !matches!(condition, Type::Bool) {
            self.errors
                .push(format!("Condition must be a boolean {:?}", condition));
        }
        let then_branch_type = self.walk_block(&mut expr.then_branch);
        if let Some(else_branch) = expr.else_branch.as_mut() {
            let else_branch_type = self.walk_block(else_branch);
            if then_branch_type != else_branch_type {
                self.errors.push(format!(
                    "Type mismatch in if-else block: {} != {}",
                    then_branch_type, else_branch_type
                ));
            }
            expr.ty = then_branch_type.clone();
        }
        then_branch_type
    }

    fn walk_expr_array(&mut self, expr: &mut ast::ExprArray) -> Type {
        let size = expr.elements.len();
        let ty = self.walk_expr(&mut expr.elements[0]);
        for mut element in expr.elements.iter_mut().skip(1) {
            let other = self.walk_expr(element);
            if ty != other {
                self.errors.push(format!(
                    "Type mismatch in array literal: {} != {}",
                    ty, other
                ));
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
            self.errors
                .push("Index must be a signed number".to_string());
        }

        expr.ty = array_type.clone();

        let Type::Array(_, array_type) = array_type else {
            panic!("Expected array type")
        };
        *array_type
    }

    fn walk_expr_array_repeat(&mut self, expr: &mut ast::ExprArrayRepeat) -> Type {
        let ast::ExprArrayRepeat { value, count, .. } = expr;
        let value_type = self.walk_expr(value);
        let count_type = self.walk_expr(count);
        let Expr::Litral(ast::Litral::Integer(count_token)) = &**count else {
            self.errors.push("count must be an integer".to_string());
            return value_type;
        };

        expr.ty = Type::Array(
            count_token.lexeme.parse::<usize>().unwrap(),
            Box::new(value_type.clone()),
        );

        expr.ty.clone()
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

        let tokens = crate::stage::lexer::Lexer::default().run(src);
        let mut ast = crate::stage::parser::Parser::default().run(tokens).unwrap();
        let mut symbol_table =
            crate::stage::semantic_analyzer::symbol_table::SymbolTableBuilder::default()
                .build(&mut ast)
                .unwrap();
        eprintln!("{:#?}", symbol_table);
        let type_checker = TypeChecker::new(&mut symbol_table);
        if let Err(errors) = type_checker.check(&mut ast) {
            let CompilerError::TypeErrors(errors) = errors else {
                panic!("Expected TypeErrors");
            };
            for error in errors {
                eprintln!("{}", error);
            }
            assert!(false);
        }
    }
}
