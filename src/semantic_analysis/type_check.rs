#![allow(unused)]
use crate::ast;
use crate::ast::{Expr, Type};
use crate::lexer::token::{Token, TokenKind};
use crate::semantic_analysis::symbol_table::SymbolTable;

impl Type {
    pub fn supports_binary_op(&self, op: &TokenKind, other: &Type) -> Option<Type> {
        match (self, op, other) {
            (Type::UnsignedNumber(lhs), TokenKind::Plus, Type::UnsignedNumber(rhs))
                if lhs == rhs =>
            {
                Some(Type::UnsignedNumber(*lhs))
            }
            (Type::SignedNumber(lhs), TokenKind::Plus, Type::SignedNumber(rhs)) if lhs == rhs => {
                Some(Type::SignedNumber(*lhs))
            }
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

pub struct TypeChecker {
    symbol_table: SymbolTable,
    current_type: Option<Type>,
    errors: Vec<String>,
}

impl TypeChecker {
    pub fn new(symbol_table: SymbolTable) -> Self {
        Self {
            symbol_table,
            current_type: None,
            errors: Vec::new(),
        }
    }

    pub fn check(mut self, ast: &[ast::Item]) -> Result<(), Vec<String>> {
        for item in ast {
            self.walk_item(&item);
        }
        if !self.errors.is_empty() {
            return Err(self.errors);
        }
        Ok(())
    }

    fn walk_item(&mut self, item: &ast::Item) -> ast::Type {
        match item {
            ast::Item::Function(function) => self.walk_function(function),
            ast::Item::Type(type_def) => self.walk_type_def(type_def),
            ast::Item::Use(r#use) => self.walk_use(r#use),
        }
    }

    fn walk_use(&mut self, item: &ast::Use) -> ast::Type {
        todo!()
    }

    fn walk_function(&mut self, function: &ast::Function) -> ast::Type {
        let ast::Function {
            visibility,
            fn_token,
            name,
            params,
            return_type,
            body,
        } = function;

        self.symbol_table.enter_scope(name.lexeme.as_str());
        let calulated_return_type = self.walk_block(&body);
        self.symbol_table.exit_scope();
        if &calulated_return_type != return_type {
            // TODO: log error and return the return_type
            // Do not block
        }
        return_type.clone()
    }

    fn walk_type_def(&mut self, type_def: &ast::TypeDef) -> ast::Type {
        todo!()
    }

    fn walk_struct(&mut self, struct_def: &ast::Struct) -> ast::Type {
        todo!()
    }

    fn walk_block(&mut self, block: &ast::Block) -> ast::Type {
        let ast::Block { statements, .. } = block;
        let mut result = Type::Void;
        for statement in statements {
            result = self.walk_stmt(&statement);
        }
        result
    }

    fn walk_stmt(&mut self, stmt: &ast::Statement) -> ast::Type {
        let ast::Statement { expr, .. } = stmt;
        self.walk_expr(&expr)
    }

    fn walk_expr(&mut self, expr: &ast::Expr) -> ast::Type {
        match expr {
            ast::Expr::Return(expr) => self.walk_expr_return(expr),
            ast::Expr::Struct(expr) => self.walk_expr_struct(expr),
            ast::Expr::Assignment(expr) => self.walk_expr_assignment(expr),
            ast::Expr::Litral(expr) => self.walk_expr_litral(expr),
            ast::Expr::Call(expr) => self.walk_expr_call(expr),
            ast::Expr::Binary(expr) => self.walk_expr_binary(expr),
            ast::Expr::Identifier(expr) => self.walk_expr_identifier(expr),
            ast::Expr::IfElse(expr) => self.walk_expr_if_else(expr),
        }
    }

    fn walk_expr_return(&mut self, expr: &ast::ExprReturn) -> ast::Type {
        let ast::ExprReturn { expr, .. } = expr;
        let Some(expr) = expr else {
            return Type::Void;
        };
        let current_type = self.walk_expr(&expr);
        current_type
    }

    fn walk_expr_struct(&mut self, expr: &ast::ExprStruct) -> ast::Type {
        todo!()
    }

    fn walk_expr_assignment(&mut self, expr: &ast::ExprAssignment) -> ast::Type {
        let value_type = self.walk_expr(&expr.expr);
        if let Some(ty) = &expr.ty {
            if ty != &value_type {
                // TODO: log error and return the value_type
                // Do not block
                self.errors.push(format!(
                    "Type mismatch: expected {} but got {}",
                    ty, value_type
                ))
            }
        }
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
            ast::Litral::BoolTrue(_) => Type::SignedNumber(32),
            ast::Litral::BoolFalse(_) => Type::SignedNumber(32),
        }
    }

    fn walk_expr_call(&mut self, expr: &ast::ExprCall) -> ast::Type {
        let ast::Expr::Identifier(ident) = &expr.caller.as_ref() else {
            panic!("Caller must be an identifier");
        };

        let Some(symbol) = self.symbol_table.get(ident.lexeme.as_str()) else {
            self.errors
                .push(format!("Undefined function: {}", ident.lexeme));
            return Type::Void;
        };
        symbol.ty.clone()
    }

    fn walk_expr_binary(&mut self, expr: &ast::ExprBinary) -> ast::Type {
        let left_ty = self.walk_expr(&expr.left);
        let right_ty = self.walk_expr(&expr.right);

        match left_ty.supports_binary_op(&expr.op.kind, &right_ty) {
            Some(result_ty) => result_ty,
            None => {
                self.errors.push(format!(
                    "Unsupported binary operation: {} {} {}",
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

    fn walk_expr_if_else(&mut self, expr: &ast::ExprIfElse) -> ast::Type {
        let ast::ExprIfElse {
            condition,
            then_branch,
            else_branch,
            ..
        } = expr;
        if self.walk_expr(&condition) == ast::Type::Bool {}
        let then_branch_type = self.walk_block(then_branch);
        if let Some(else_branch) = else_branch.as_ref() {
            let else_branch_type = self.walk_block(else_branch);
            if then_branch_type != else_branch_type {
                self.errors.push(format!(
                    "Type mismatch in if-else block: {} != {}",
                    then_branch_type, else_branch_type
                ));
            }
        }
        then_branch_type
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::semantic_analysis::symbol_table::SymbolTableBuilder;
    use pretty_assertions::assert_eq;

    #[test]
    fn test_type_check() {
        let src = r#"
        fn add(a: s32, b: s32) s32 {
            return a + b;
        }
        "#;

        let ast = crate::parser::Parser::new(&src).parse().unwrap();
        let symbol_table = SymbolTableBuilder::new().build(&ast);
        eprintln!("{:#?}", symbol_table);
        let type_checker = TypeChecker::new(symbol_table);
        if let Err(errors) = type_checker.check(&ast) {
            for error in errors {
                eprintln!("{}", error);
            }
            assert!(false);
        }
    }
}
