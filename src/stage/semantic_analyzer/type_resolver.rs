#![allow(unused)]

use crate::error::{
    ErrorMissMatchedType, ErrorUndefinedSymbol, ErrorUnsupportedBinaryOp, Errors, Report, Result,
};
use crate::stage::lexer::token::{Span, Token};
use crate::stage::parser::ast::{self, Expr, StructType, Type, TypeKind, TypeParams};
use crate::stage::semantic_analyzer::symbol_table::{
    ConcreteTypeId, ConcreteTypeMap, InstantiationCache, Symbol, SymbolField, SymbolKind,
    SymbolTable,
};

pub struct TypeResolver<'st> {
    instantiation_cache: InstantiationCache,
    concrete_type_map: ConcreteTypeMap,
    symbol_table: &'st mut SymbolTable,
    errors: Vec<Box<dyn Report>>,
    target_pointer_width: u8,
    instantiation_cache_id_counter: u32,
}

// HELPERS
impl TypeResolver<'_> {
    fn generate_instantiation_cache_id(&mut self) -> ConcreteTypeId {
        let id = self.instantiation_cache_id_counter;
        self.instantiation_cache_id_counter += 1;
        ConcreteTypeId(id)
    }

    fn cache_type(&mut self, name: impl Into<String>, params: &TypeParams) -> Option<Type> {
        let name = name.into();
        let key = (name.clone(), params.params.to_vec());

        // Already interned, nothing to do
        if self.instantiation_cache.contains_key(&key) {
            return None;
        }

        let id = self.generate_instantiation_cache_id();
        self.instantiation_cache.insert(key, id);

        let Some(generic_sym) = self.symbol_table.get(&name).cloned() else {
            // error already pushed by walk_type caller
            return None;
        };

        // Build the type_param → concrete type substitution map
        // Example:
        //   ["T"] + [i32] -> {"T": i32}
        let type_args = generic_sym.type_args.as_deref().unwrap_or(&[]);
        let substitution: std::collections::HashMap<String, Type> = type_args
            .iter()
            .zip(params.params.iter())
            .map(|(param, ty)| (param.name.lexeme.clone(), ty.clone()))
            .collect();

        let concrete_fields = generic_sym
            .fields
            .as_deref()
            .unwrap_or(&[])
            .iter()
            .map(|field| SymbolField {
                name: field.name.clone(),
                ty: substitute_type(&field.ty, &substitution, self.target_pointer_width),
                default_value: field.default_value.clone(),
            })
            .collect::<Vec<_>>();

        let concrete_symbol = Symbol {
            name: format!("{}({})", name, params),
            kind: SymbolKind::Struct,
            fields: Some(concrete_fields),
            ty: substitute_type(&generic_sym.ty, &substitution, self.target_pointer_width),
            binding_name: None,
            is_mutable: generic_sym.is_mutable,
            visibility: generic_sym.visibility,
            type_args: generic_sym.type_args.map(|tr| {
                tr.iter()
                    .map(|type_arg| ast::TypeArg {
                        name: type_arg.name.clone(),
                        ty: substitute_type(&type_arg.ty, &substitution, self.target_pointer_width),
                    })
                    .collect::<Vec<_>>()
            }),
            params: generic_sym.params.map(|tr| {
                tr.iter()
                    .map(|ty| substitute_type(ty, &substitution, self.target_pointer_width))
                    .collect::<Vec<_>>()
            }),
        };

        let return_type = concrete_symbol.ty.clone();
        self.concrete_type_map.insert(id, concrete_symbol);
        Some(return_type)
    }
}

impl<'st> TypeResolver<'st> {
    pub fn new(symbol_table: &'st mut SymbolTable, target_pointer_width: u8) -> Self {
        Self {
            instantiation_cache: InstantiationCache::default(),
            concrete_type_map: ConcreteTypeMap::default(),
            symbol_table,
            errors: Vec::new(),
            target_pointer_width,
            instantiation_cache_id_counter: 0,
        }
    }

    pub fn walk_items(mut self, items: &mut [ast::Item]) -> Result<()> {
        let mut symbols: Vec<_> = self
            .symbol_table
            .iter_all()
            .filter(|(_, symbol)| symbol.kind != SymbolKind::GenericStruct)
            .map(|(path, symbol)| (path, symbol.name.clone(), symbol.ty.clone()))
            .collect();

        for (scope, name, ty) in symbols.iter_mut() {
            self.walk_type(ty);
        }

        for (path, name, ty) in symbols {
            self.symbol_table
                .get_mut_from_full_scope_path(&path, &name, |symbol| symbol.ty = ty.clone());
        }

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
            ast::Item::ExternFunction(extern_function) => {
                self.walk_extern_function(extern_function)
            }
        }
    }

    fn walk_extern_function(&mut self, extern_function: &mut ast::ExternFunction) {
        // FIX: span is incorrect
        self.walk_type(&mut extern_function.return_type);
        let span = extern_function.binding_name.span.clone();
        for ty in extern_function.params.iter_mut() {
            self.walk_type(ty);
        }
    }

    fn walk_function(&mut self, function: &mut ast::Function) {
        // FIX: span is incorrect
        self.walk_type(&mut function.return_type);
        for param in function.params.iter_mut() {
            let span = param.name.span.clone();
            self.walk_type(&mut param.ty);
        }
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
            Expr::Assignment(expr_assignment) => self.walk_expr_assignment(expr_assignment),
            Expr::Litral(litral) => {}
            Expr::Call(expr_call) => self.walk_expr_call(expr_call),
            Expr::Binary(expr_binary) => self.walk_expr_binary(expr_binary),
            Expr::While(expr_while) => self.walk_expr_while(expr_while),
            Expr::Identifier(token) => {}
            Expr::IfElse(expr_if_else) => self.walk_expr_if_else(expr_if_else),
            Expr::MemberAccess(expr_member_access) => {
                self.walk_expr_member_access(expr_member_access)
            }
            Expr::Array(expr_array) => self.walk_expr_array(expr_array),
            Expr::ArrayIndex(expr_array_index) => self.walk_expr_array_index(expr_array_index),
            Expr::ArrayRepeat(expr_array_repeat) => self.walk_expr_array_repeat(expr_array_repeat),
            Expr::Block(expr_block) => self.walk_expr_block(expr_block),
            Expr::AddressOf(expr_address_of) => self.walk_expr(&mut expr_address_of.expr),
            Expr::Not(expr_not) => self.walk_expr(&mut expr_not.expr),
            Expr::Grouping(expr_grouping) => self.walk_expr(&mut expr_grouping.expr),
            Expr::TypeCast(expr_cast) => self.walk_expr(&mut expr_cast.expr),
        }
    }

    fn walk_expr_declare(&mut self, expr_decl: &mut ast::ExprDecl) {
        self.walk_expr(&mut expr_decl.expr);
        let span = expr_decl.span().clone();
        let Some(ty) = &mut expr_decl.ty else {
            return;
        };
        self.walk_type(ty);
    }

    fn walk_expr_assignment(&mut self, expr_assignment: &mut ast::ExprAssignment) {
        self.walk_expr(&mut expr_assignment.left);
        self.walk_expr(&mut expr_assignment.right);
    }

    fn walk_expr_struct(&mut self, expr_struct: &mut ast::ExprStruct) {
        for field in expr_struct.init_fields.iter_mut() {
            self.walk_expr(&mut field.expr);
        }
    }

    fn walk_expr_call(&mut self, expr_call: &mut ast::ExprCall) {
        self.walk_expr(&mut expr_call.caller);
        for arg in expr_call.args.iter_mut() {
            self.walk_expr(arg);
        }
    }

    fn walk_expr_binary(&mut self, expr_binary: &mut ast::ExprBinary) {
        self.walk_expr(&mut expr_binary.left);
        self.walk_expr(&mut expr_binary.right);
    }

    fn walk_expr_while(&mut self, expr_while: &mut ast::ExprWhile) {
        self.walk_expr(&mut expr_while.condition);
        for stmt in expr_while.body.statements.iter_mut() {
            self.walk_stmt(stmt);
        }
    }

    fn walk_expr_if_else(&mut self, expr_if_else: &mut ast::ExprIfElse) {
        self.walk_expr(&mut expr_if_else.condition);
        for stmt in expr_if_else.then_branch.statements.iter_mut() {
            self.walk_stmt(stmt);
        }
        if let Some(else_branch) = &mut expr_if_else.else_branch {
            self.walk_expr(else_branch);
        }
    }

    fn walk_expr_member_access(&mut self, expr_member_access: &mut ast::ExprMemberAccess) {
        self.walk_expr(&mut expr_member_access.base);
    }

    fn walk_expr_array(&mut self, expr_array: &mut ast::ExprArray) {
        let span = expr_array.span().clone();
        self.walk_type(&mut expr_array.ty);
        for expr in expr_array.elements.iter_mut() {
            self.walk_expr(expr);
        }
    }

    fn walk_expr_array_index(&mut self, expr_array_index: &mut ast::ExprArrayIndex) {
        self.walk_expr(&mut expr_array_index.expr);
        self.walk_expr(&mut expr_array_index.index);
        let span = expr_array_index.span().clone();
        self.walk_type(&mut expr_array_index.ty);
    }

    fn walk_expr_array_repeat(&mut self, expr_array_repeat: &mut ast::ExprArrayRepeat) {
        self.walk_expr(&mut expr_array_repeat.count);
        self.walk_expr(&mut expr_array_repeat.value);
        let span = expr_array_repeat.span().clone();
        self.walk_type(&mut expr_array_repeat.ty);
    }

    fn walk_expr_block(&mut self, block: &mut ast::ExprBlock) {
        for statement in block.statements.iter_mut() {
            self.walk_stmt(statement);
        }
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
        let arg_type_names = struct_def
            .type_args
            .iter()
            .map(|a| &a.name.lexeme)
            .collect::<Vec<_>>();
        for field in struct_def.fields.iter_mut() {
            if arg_type_names.iter().any(|name| field.ty.is_name(name)) {
                continue;
            }
            self.walk_type(&mut field.ty);
        }
    }

    fn walk_type(&mut self, ty: &mut Type) {
        let found = ty.clone();
        match &mut ty.kind {
            TypeKind::Type
            | TypeKind::Bool
            | TypeKind::UnsignedNumber(_)
            | TypeKind::UnsignedNumber(_)
            | TypeKind::SignedNumber(_)
            | TypeKind::Float(_)
            | TypeKind::Void => {}
            TypeKind::Array(_, ty) => self.walk_type(ty),
            TypeKind::Ref(ty) => self.walk_type(ty),
            TypeKind::Struct(struct_type) => {
                for (_, ty) in struct_type.fields.iter_mut() {
                    self.walk_type(ty);
                }
            }
            TypeKind::Enum(_) => todo!("Enum"),
            TypeKind::Name(name) => {
                if let Some(symbol) = self.symbol_table.get(name) {
                    *ty = symbol.ty.clone();
                    return;
                }

                #[cfg(feature = "debug")]
                self.errors.push(Box::new(ErrorUndefinedSymbol::TypeDebug(
                    found,
                    format!("{} {}:{}", file!(), line!(), column!()),
                )));

                #[cfg(not(feature = "debug"))]
                self.errors
                    .push(Box::new(ErrorUndefinedSymbol::Type(found)));
            }
            TypeKind::NameWithParams(name, params) => {
                if let Some(new_ty) = self.cache_type(&name.lexeme, &params) {
                    *ty = new_ty;
                    return;
                }

                // Get cached type when you have already cashed type
                let key = (name.lexeme.clone(), params.params.to_vec());
                if let Some(id) = self.instantiation_cache.get(&key) {
                    if let Some(symbol) = self.concrete_type_map.get(id) {
                        *ty = symbol.ty.clone();
                        return;
                    }
                }

                #[cfg(feature = "debug")]
                self.errors.push(Box::new(ErrorUndefinedSymbol::TypeDebug(
                    found,
                    format!("{} {}:{}", file!(), line!(), column!()),
                )));

                #[cfg(not(feature = "debug"))]
                self.errors
                    .push(Box::new(ErrorUndefinedSymbol::Type(found)));
            }
            kind @ TypeKind::UnsignedTargetPointerNumber => {
                *kind = TypeKind::UnsignedNumber(self.target_pointer_width);
            }
            kind @ TypeKind::SignedTargetPointerNumber => {
                *kind = TypeKind::SignedNumber(self.target_pointer_width);
            }
        }
    }

    fn walk_use(&mut self, r#use: &mut ast::Use) {
        todo!("{:#?}", r#use);
    }
}

fn substitute_type(
    ty: &Type,
    substitution: &std::collections::HashMap<String, Type>,
    target_pointer_width: u8,
) -> Type {
    match &ty.kind {
        // A bare name — if it's a type param, replace it
        TypeKind::Name(name) => {
            if let Some(concrete) = substitution.get(name.as_str()) {
                return concrete.clone();
            }
            ty.clone()
        }
        // Recurse into compound types
        TypeKind::Ref(inner) => Type {
            kind: TypeKind::Ref(Box::new(substitute_type(
                inner,
                substitution,
                target_pointer_width,
            ))),
            ..ty.clone()
        },
        TypeKind::Array(size, inner) => Type {
            kind: TypeKind::Array(
                *size,
                Box::new(substitute_type(inner, substitution, target_pointer_width)),
            ),
            ..ty.clone()
        },
        TypeKind::NameWithParams(name, params) => {
            let new_params = params
                .params
                .iter()
                .map(|p| substitute_type(p, substitution, target_pointer_width))
                .collect();
            Type {
                kind: TypeKind::NameWithParams(
                    name.clone(),
                    ast::TypeParams { params: new_params },
                ),
                ..ty.clone()
            }
        }
        TypeKind::Struct(tstruct) => {
            let fields = tstruct
                .fields
                .iter()
                .map(|(name, ty)| {
                    (
                        name.to_string(),
                        substitute_type(ty, substitution, target_pointer_width),
                    )
                })
                .collect::<Vec<_>>();

            Type {
                kind: TypeKind::Struct(ast::StructType {
                    fields,
                    ..tstruct.clone()
                }),
                ..ty.clone()
            }
        }
        TypeKind::UnsignedTargetPointerNumber => Type {
            kind: TypeKind::UnsignedNumber(target_pointer_width),
            ..ty.clone()
        },
        TypeKind::SignedTargetPointerNumber => Type {
            kind: TypeKind::SignedNumber(target_pointer_width),
            ..ty.clone()
        },
        // Primitives and anything else — no substitution needed
        _ => ty.clone(),
    }
}
