#![allow(unused)]
use crate::error::CompilerError;
use crate::stage::lexer::token::Token;

use crate::stage::parser::ast::{self, Expr};
use std::collections::HashMap;

#[derive(Debug)]
pub struct SymbolTableBuilder {
    pub table: SymbolTable,
    errors: Vec<String>,
}

impl Default for SymbolTableBuilder {
    fn default() -> Self {
        let mut table = SymbolTable::default();
        table.enter_scope("global");
        table.push(Symbol {
            name: "print".to_string(),
            ty: ast::Type::Void,
            visibility: ast::Visibility::Public,
            kind: SymbolKind::Function,
            is_mutable: false,
            params: Some(vec![ast::Type::UnsignedNumber(32)]),
        });
        table.exit_scope();

        Self {
            table,
            errors: Vec::new(),
        }
    }
}

impl SymbolTableBuilder {
    pub fn build(mut self, items: &[ast::Item]) -> Result<SymbolTable, CompilerError> {
        for item in items {
            self.walk_item(item);
        }
        if !self.errors.is_empty() {
            return Err(CompilerError::SymbolTableErrors(self.errors));
        }
        Ok(self.table)
    }

    fn walk_item(&mut self, item: &ast::Item) {
        match item {
            ast::Item::Function(function) => self.walk_function(function),
            ast::Item::Type(type_def) => self.walk_type_def(type_def),
            ast::Item::Use(r#use) => self.walk_use(r#use),
        }
    }

    fn walk_use(&mut self, item: &ast::Use) {
        todo!()
    }

    fn walk_function(&mut self, function: &ast::Function) {
        let ast::Function {
            visibility,
            fn_token,
            name,
            params,
            return_type,
            body,
        } = function;

        self.table.push(Symbol {
            name: name.lexeme.clone(),
            kind: SymbolKind::Function,
            ty: return_type.clone(),
            is_mutable: false,
            visibility: visibility.clone(),
            params: Some(params.iter().map(|param| param.ty.clone()).collect()),
        });
        self.table.enter_scope(&name.lexeme);

        for param in params {
            self.table.push(Symbol {
                name: param.name.lexeme.clone(),
                kind: SymbolKind::Parameter,
                ty: param.ty.clone(),
                is_mutable: false,
                visibility: ast::Visibility::Private,
                params: None,
            });
        }

        self.walk_block(body);
        self.table.exit_scope();
    }

    fn walk_type_def(&mut self, type_def: &ast::TypeDef) {
        todo!()
    }

    fn walk_struct(&mut self, struct_def: &ast::Struct) {
        todo!()
    }

    fn walk_block(&mut self, block: &ast::Block) {
        let ast::Block { statements, .. } = block;
        for stmt in statements {
            self.walk_stmt(stmt);
        }
    }

    fn walk_stmt(&mut self, stmt: &ast::Statement) {
        let ast::Statement { expr, .. } = stmt;
        self.walk_expr(expr);
    }

    fn walk_expr(&mut self, expr: &ast::Expr) {
        match expr {
            ast::Expr::Return(ast::ExprReturn {
                expr: Some(expr), ..
            }) => self.walk_expr(expr),
            ast::Expr::Struct(expr) => todo!("walk_expr_struct"),
            ast::Expr::Assignment(expr) => self.walk_expr_assignment(expr),
            ast::Expr::Declare(expr) => self.walk_expr_declare(expr),
            ast::Expr::Call(expr) => self.walk_expr_call(expr),
            ast::Expr::Binary(expr) => self.walk_expr_binary(expr),
            ast::Expr::Identifier(expr) => self.walk_expr_identifier(expr),
            ast::Expr::IfElse(expr) => self.walk_expr_if_else(expr),
            Expr::Return(expr_return) => todo!(),
            Expr::Struct(expr_struct) => todo!(),
            Expr::Declare(expr_decl) => todo!(),
            Expr::Assignment(expr_assignment) => todo!(),
            Expr::Litral(litral) => {}
            Expr::Call(expr_call) => todo!(),
            Expr::Binary(expr_binary) => todo!(),
            Expr::Identifier(token) => todo!(),
            Expr::IfElse(expr_if_else) => todo!(),
            Expr::Array(expr_array) => todo!(),
            Expr::ArrayIndex(expr) => {
                self.walk_expr(&expr.expr);
                self.walk_expr(&expr.index);
            }
            Expr::ArrayRepeat(expr) => {
                // No need to do anything
            }
        }
    }

    fn walk_expr_assignment(&mut self, expr: &ast::ExprAssignment) {
        let ast::ExprAssignment { left, right, .. } = expr;
        self.walk_expr(left);
        self.walk_expr(right);
    }

    fn walk_expr_declare(&mut self, expr: &ast::ExprDecl) {
        let ty = expr.ty.as_ref().map_or(ast::Type::Void, |ty| ty.clone());
        let symbol = Symbol {
            name: expr.ident.lexeme.clone(),
            kind: SymbolKind::Variable,
            ty,
            is_mutable: true,
            visibility: ast::Visibility::Private,
            params: None,
        };

        self.table.push(symbol);
    }

    fn walk_expr_call(&mut self, expr: &ast::ExprCall) {
        let ast::ExprCall {
            caller,
            left_paren: _,
            args,
            right_paren: _,
        } = expr;
        self.walk_expr(caller);
        for arg in args.iter() {
            self.walk_expr(arg);
        }
    }

    fn walk_expr_binary(&mut self, expr: &ast::ExprBinary) {
        let ast::ExprBinary { left, right, .. } = expr;
        self.walk_expr(left);
        self.walk_expr(right);
    }

    fn walk_expr_identifier(&mut self, token: &Token) {
        let Some(symbol) = self.table.get(token.lexeme.as_str()) else {
            self.errors.push(format!(
                "{:?}: Undefined variable {}",
                token.span, token.lexeme
            ));
            return;
        };
    }

    fn walk_expr_if_else(&mut self, expr: &ast::ExprIfElse) {
        let ast::ExprIfElse {
            condition,
            then_branch,
            else_branch,
            ty,
        } = expr;

        self.walk_expr(condition);
        self.walk_block(then_branch);
        let Some(else_branch) = else_branch.as_ref() else {
            return;
        };
        self.walk_block(else_branch);
    }
}

#[derive(Debug, Default, Clone, PartialEq, Eq)]
pub enum SymbolKind {
    #[default]
    Variable,
    Function,
    Parameter,
}

#[derive(Debug, Default, Clone, PartialEq, Eq)]
pub struct Symbol {
    pub name: String,
    pub kind: SymbolKind,
    pub ty: ast::Type,
    pub is_mutable: bool,
    pub visibility: ast::Visibility,
    pub params: Option<Vec<ast::Type>>,
}

#[derive(Debug, Default, Clone, PartialEq, Eq)]
pub struct Scope {
    symbols: HashMap<String, Symbol>,
}

impl Scope {
    pub fn insert(&mut self, symbol: Symbol) {
        let name = symbol.name.clone();
        self.symbols.insert(name, symbol);
    }

    pub fn get(&self, name: &str) -> Option<&Symbol> {
        self.symbols.get(name)
    }

    pub fn get_mut(&mut self, name: &str) -> Option<&mut Symbol> {
        self.symbols.get_mut(name)
    }

    pub fn lookup(&self, name: &str) -> Option<&Symbol> {
        self.symbols.get(name)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ScopePath(Vec<String>);

impl ScopePath {
    fn new(parts: &[String]) -> Self {
        Self(parts.to_vec())
    }

    fn as_key(&self) -> String {
        self.0.join(SymbolTable::SEPARATOR)
    }

    fn global() -> Self {
        Self::new(&[SymbolTable::GLOBAL_SCOPE.to_string()])
    }
}

#[derive(Debug, Default, Clone, PartialEq, Eq)]
pub struct SymbolTable {
    pub scopes: HashMap<ScopePath, Scope>,
    pub scope_stack: Vec<String>,
}

impl SymbolTable {
    pub const SEPARATOR: &'static str = "::";
    pub const GLOBAL_SCOPE: &'static str = "global";

    pub fn get_scope_name(&self) -> String {
        self.scope_stack.last().cloned().unwrap_or_default()
    }

    fn get_full_scope_name(&self) -> ScopePath {
        ScopePath::new(&self.scope_stack)
    }

    pub fn enter_scope(&mut self, name: &str) {
        self.scope_stack.push(name.to_string());
    }

    pub fn exit_scope(&mut self) {
        self.scope_stack.pop();
    }

    pub fn scope_level(&self) -> usize {
        self.scope_stack.len()
    }

    pub fn get(&self, name: &str) -> Option<&Symbol> {
        for i in (0..=self.scope_stack.len()).rev() {
            let scope_path = ScopePath::new(&self.scope_stack[0..i]);

            if let Some(scope) = self.scopes.get(&scope_path)
                && let Some(symbol) = scope.lookup(name)
            {
                return Some(symbol);
            }
        }

        self.scopes
            .get(&ScopePath::global())
            .and_then(|scope| scope.lookup(name))
    }

    pub fn get_mut(&mut self, name: &str, mut f: impl FnMut(&mut Symbol)) {
        for i in (0..self.scope_stack.len()).rev() {
            let scope_path = ScopePath::new(&self.scope_stack[0..=i]);
            if let Some(scope) = self.scopes.get_mut(&scope_path)
                && let Some(symbol) = scope.get_mut(name)
            {
                f(symbol);
                return;
            }
        }

        if let Some(symbol) = self
            .scopes
            .get_mut(&ScopePath::global())
            .and_then(|scope| scope.get_mut(name))
        {
            f(symbol);
        }
    }

    fn push(&mut self, symbol: Symbol) {
        let scope_name = self.get_full_scope_name();
        self.scopes.entry(scope_name).or_default().insert(symbol);
    }

    pub fn extend(&mut self, other: SymbolTable) {
        self.scopes.extend(other.scopes);
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::stage::Stage;
    use pretty_assertions::assert_eq;

    fn create_symbol(
        name: &str,
        kind: SymbolKind,
        ty: ast::Type,
        // scope: usize,
        is_mutable: bool,
    ) -> Symbol {
        Symbol {
            visibility: ast::Visibility::Public,
            name: name.to_string(),
            kind,
            ty,
            is_mutable,
            params: None,
        }
    }

    #[test]
    fn test_insert_and_lookup() {
        let mut table = SymbolTable::default();

        let sym = create_symbol("x", SymbolKind::Variable, ast::Type::SignedNumber(32), true);
        table.push(sym.clone());

        let found = table.get("x");
        assert!(found.is_some());
        assert_eq!(found.unwrap().ty, ast::Type::SignedNumber(32));
    }

    #[test]
    fn test_scope_enter_exit() {
        let mut table = SymbolTable::default();

        let sym1 = create_symbol("x", SymbolKind::Variable, ast::Type::SignedNumber(32), true);
        table.push(sym1.clone());

        table.enter_scope(&sym1.name);

        let sym2 = create_symbol("y", SymbolKind::Variable, ast::Type::Bool, false);
        table.push(sym2);

        assert!(table.get("y").is_some());

        table.exit_scope();

        assert!(table.get("y").is_none());
        assert!(table.get("x").is_some());
    }

    #[test]
    fn test_variable_shadowing() {
        let mut table = SymbolTable::default();

        let sym1 = create_symbol("x", SymbolKind::Variable, ast::Type::SignedNumber(32), true);
        table.push(sym1.clone());

        table.enter_scope(&sym1.name);

        let sym2 = create_symbol("x", SymbolKind::Variable, ast::Type::Bool, false);
        table.push(sym2.clone());

        let found = table.get("x").unwrap();
        assert_eq!(found.ty, ast::Type::Bool);
        assert!(!found.is_mutable);

        table.exit_scope();

        let found = table.get("x").unwrap();
        assert_eq!(found.ty, ast::Type::SignedNumber(32));
        assert!(found.is_mutable);
    }

    #[test]
    fn test_lookup_nonexistent() {
        let table = SymbolTable::default();
        assert!(table.get("nonexistent").is_none());
    }

    #[test]
    fn test_build_symbol_table() {
        let src = r#"
        fn add(a: s32, b: s32) s32 {
            return a + b;
        }
        "#;

        let tokens = crate::stage::lexer::Lexer::default().run(src);
        let ast = crate::stage::parser::Parser::default().run(tokens).unwrap();
        let symbol_table = match SymbolTableBuilder::default().build(&ast) {
            Ok(table) => table,
            Err(errors) => {
                let CompilerError::TypeErrors(errors) = errors else {
                    panic!("Expected TypeErrors");
                };
                for error in errors {
                    eprintln!("{}", error);
                }
                panic!("Failed to build symbol table");
            }
        };
        let symbol = symbol_table.get("add");
        println!("{:#?}", symbol_table);
        assert_eq!(
            symbol,
            Some(&Symbol {
                name: "add".to_string(),
                kind: SymbolKind::Function,
                ty: ast::Type::SignedNumber(32),
                is_mutable: false,
                visibility: ast::Visibility::Private,
                params: Some(vec![
                    ast::Type::SignedNumber(32),
                    ast::Type::SignedNumber(32)
                ]),
            })
        );
    }

    #[test]
    fn test_function_lookup_from_inner_scope() {
        let mut table = SymbolTable::default();

        // Define a function symbol in global scope
        let func_symbol = Symbol {
            name: "do_something".to_string(),
            kind: SymbolKind::Function,
            ty: ast::Type::Void,
            is_mutable: false,
            visibility: ast::Visibility::Public,
            params: Some(vec![ast::Type::Bool]),
        };

        table.push(func_symbol.clone());

        let func_symbol1 = Symbol {
            name: "another_function".to_string(),
            kind: SymbolKind::Function,
            ty: ast::Type::Void,
            is_mutable: false,
            visibility: ast::Visibility::Public,
            params: Some(vec![ast::Type::Bool]),
        };

        table.push(func_symbol1.clone());

        // Now enter a new scope (e.g., another function body)
        table.enter_scope("another_function");

        // Try to look up the function defined in the global scope
        let symbol = table.get("do_something");

        assert!(
            symbol.is_some(),
            "Expected to find 'do_something' in outer/global scope"
        );
        let symbol = symbol.unwrap();
        assert_eq!(symbol.name, "do_something");
        assert_eq!(symbol.kind, SymbolKind::Function);
        assert_eq!(symbol.params, Some(vec![ast::Type::Bool]));
    }
}
