#![allow(unused)]

use crate::error::{ErrorMissMatchedType, ErrorUnsupportedBinaryOp, Errors, Report, Result};
use crate::stage::lexer::token::Span;
use crate::stage::parser::ast::{self, Expr, StructType, Type};
use crate::stage::semantic_analyzer::symbol_table::SymbolTable;

pub struct TypeResolver<'st> {
    symbol_table: &'st mut SymbolTable,
    errors: Vec<Box<dyn Report>>,
}

impl<'st> TypeResolver<'st> {
    pub fn new(symbol_table: &'st mut SymbolTable) -> Self {
        Self {
            symbol_table,
            errors: Vec::new(),
        }
    }

    pub fn walk_items(mut self, items: &mut [ast::Item]) -> Result<()> {
        for item in items.iter_mut() {
            self.walk_item(item);
        }
        if !self.errors.is_empty() {
            return Err(Box::new(Errors {
                errors: self.errors,
            }));
        }
        Ok(())
    }

    fn walk_item(&mut self, item: &mut ast::Item) {
        match item {
            ast::Item::Function(function) => self.walk_function(function),
            ast::Item::Type(type_def) => self.walk_type_def(type_def),
            ast::Item::Use(r#use) => self.walk_use(r#use),
        }
    }

    fn walk_function(&mut self, function: &mut ast::Function) {
        // FIX: span is incorrect
        self.walk_type(&mut function.return_type, function.fn_token.span.clone());
        self.symbol_table.enter_scope(function.name.lexeme.as_str());
        self.walk_block(&mut function.body);
        self.symbol_table.exit_scope();
    }

    fn walk_block(&mut self, block: &mut ast::ExprBlock) {
        for statement in block.statements.iter_mut() {
            self.walk_stmt(statement);
        }
    }

    fn walk_stmt(&mut self, stmt: &mut ast::Statement) {
        self.walk_expr(&mut stmt.expr);
    }

    fn walk_expr(&mut self, expr: &mut Expr) {
        match expr {
            Expr::Return(expr_return) => self.walk_expr_return(expr_return),
            Expr::Struct(expr_struct) => self.walk_expr_struct(expr_struct),
            Expr::Declare(expr_decl) => self.walk_expr_declare(expr_decl),
            Expr::Assignment(expr_assignment) => todo!("Assignment"),
            Expr::Litral(litral) => {}
            Expr::Call(expr_call) => self.walk_expr_call(expr_call),
            Expr::Binary(expr_binary) => todo!("Binary"),
            Expr::While(expr_while) => todo!("While"),
            Expr::Identifier(token) => {}
            Expr::IfElse(expr_if_else) => todo!("IfElse"),
            Expr::MemberAccess(expr_member_access) => {
                self.walk_expr_member_access(expr_member_access)
            }
            Expr::Array(expr_array) => todo!("Array"),
            Expr::ArrayIndex(expr_array_index) => self.walk_expr_array_index(expr_array_index),
            Expr::ArrayRepeat(expr_array_repeat) => self.walk_expr_array_repeat(expr_array_repeat),
            Expr::Block(expr_block) => todo!("Block"),
        }
    }

    fn walk_expr_declare(&mut self, expr_decl: &mut ast::ExprDecl) {
        self.walk_expr(&mut expr_decl.expr);
        let span = expr_decl.span().clone();
        let Some(ty) = &mut expr_decl.ty else {
            return;
        };
        self.walk_type(ty, span);
    }

    fn walk_expr_struct(&mut self, expr_struct: &mut ast::ExprStruct) {
        for field in expr_struct.init_fields.iter_mut() {
            self.walk_expr(&mut field.expr);
        }
    }

    fn walk_expr_call(&mut self, expr_call: &mut ast::ExprCall) {
        for arg in expr_call.args.iter_mut() {
            self.walk_expr(arg);
        }
    }

    fn walk_expr_member_access(&mut self, expr_member_access: &mut ast::ExprMemberAccess) {
        self.walk_expr(&mut expr_member_access.base);
    }

    fn walk_expr_array_index(&mut self, expr_array_index: &mut ast::ExprArrayIndex) {
        self.walk_expr(&mut expr_array_index.expr);
        self.walk_expr(&mut expr_array_index.index);
        let span = expr_array_index.span().clone();
        self.walk_type(&mut expr_array_index.ty, span);
    }

    fn walk_expr_array_repeat(&mut self, expr_array_repeat: &mut ast::ExprArrayRepeat) {
        self.walk_expr(&mut expr_array_repeat.count);
        self.walk_expr(&mut expr_array_repeat.value);
        let span = expr_array_repeat.span().clone();
        self.walk_type(&mut expr_array_repeat.ty, span);
    }

    fn walk_expr_return(&mut self, expr_return: &mut ast::ExprReturn) {
        let Some(expr) = &mut expr_return.expr else {
            return;
        };
        self.walk_expr(expr);
    }

    fn walk_type_def(&mut self, type_def: &mut ast::TypeDef) {
        match type_def {
            ast::TypeDef::Struct(struct_def) => self.walk_struct_def(struct_def),
        }
    }

    fn walk_struct_def(&mut self, struct_def: &mut ast::Struct) {
        for field in struct_def.fields.iter_mut() {
            let span = field.span().clone();
            self.walk_type(&mut field.ty, span);
        }
    }

    fn walk_type(&mut self, ty: &mut Type, span: Span) {
        let found = ty.clone();
        match ty {
            Type::Bool
            | Type::UnsignedNumber(_)
            | Type::SignedNumber(_)
            | Type::Float(_)
            | Type::Void => {}
            Type::Array(_, ty) => self.walk_type(ty, span),
            Type::Pointer(ty) => self.walk_type(ty, span),
            Type::Struct(struct_type) => {
                for (_, ty) in struct_type.fields.iter_mut() {
                    self.walk_type(ty, span.clone());
                }
            }
            Type::Enum(_) => todo!("Enum"),
            Type::Name(name) => {
                let Some(symbol) = self.symbol_table.get(&name) else {
                    self.errors.push(Box::new(ErrorMissMatchedType {
                        span: span,
                        found,
                        expected: Type::Name(name.clone()),
                        #[cfg(feature = "debug")]
                        compiler_line: format!("{} {}:{}", file!(), line!(), column!()),
                    }));
                    return;
                };
                *ty = symbol.ty.clone();
            }
        }
    }

    fn walk_use(&mut self, r#use: &mut ast::Use) {
        todo!("{:#?}", r#use);
    }
}
