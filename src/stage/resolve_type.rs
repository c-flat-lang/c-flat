use std::collections::HashMap;

use crate::DebugMode;
use crate::error::{ErrorMessage, ErrorUndefinedSymbol, Errors, Report, Result};
use crate::stage::lexer::token::Token;
use crate::stage::parser::ast::{self, Expr, Type, TypeKind};
use crate::stage::{Stage, StageContext, StageOutput};
use crate::type_interner::{StructField, TypeId, TypeInterner};

pub struct TypeResolver<'a> {
    interner: &'a mut TypeInterner,
    env: Vec<HashMap<String, TypeId>>,
    errors: Vec<Box<dyn Report>>,
}

impl<'a> TypeResolver<'a> {
    pub fn new(interner: &'a mut TypeInterner) -> Self {
        Self {
            interner,
            env: Vec::new(),
            errors: Vec::new(),
        }
    }

    pub fn push_env(&mut self, scope: HashMap<String, TypeId>) {
        self.env.push(scope);
    }

    pub fn pop_env(&mut self) {
        self.env.pop();
    }

    pub fn take_errors(&mut self) -> Vec<Box<dyn Report>> {
        std::mem::take(&mut self.errors)
    }

    pub fn interner(&mut self) -> &mut TypeInterner {
        self.interner
    }

    fn lookup_env(&self, name: &str) -> Option<TypeId> {
        self.env
            .iter()
            .rev()
            .find_map(|scope| scope.get(name).copied())
    }

    fn resolve_named(&mut self, token: &Token, ty: &Type) -> TypeId {
        if let Some(id) = self.lookup_env(&token.lexeme) {
            return id;
        }
        if let Some(id) = self.interner.lookup_name(&token.lexeme) {
            return id;
        }
        self.errors
            .push(Box::new(ErrorUndefinedSymbol::Type(ty.clone())));
        TypeId::UNRESOLVED
    }

    pub fn resolve_type_id(&mut self, ty: &Type) -> TypeId {
        match &ty.kind {
            TypeKind::Void => TypeId::VOID,
            TypeKind::Bool => TypeId::BOOL,
            TypeKind::Type => TypeId::TYPE,
            TypeKind::UnsignedNumber(bits) => self.interner.unsigned(*bits),
            TypeKind::SignedNumber(bits) => self.interner.signed(*bits),
            TypeKind::Float(bits) => self.interner.float(*bits),
            TypeKind::UnsignedTargetPointerNumber => TypeId::USIZE,
            TypeKind::SignedTargetPointerNumber => TypeId::SSIZE,
            TypeKind::Resolved(id) => *id,
            TypeKind::Pointer(inner) => {
                let inner = self.resolve_type_id(inner);
                self.interner.pointer_to(inner)
            }
            TypeKind::Slice(inner) => {
                let inner = self.resolve_type_id(inner);
                self.interner.slice_of(inner)
            }
            TypeKind::Array(length, inner) => {
                let inner = self.resolve_type_id(inner);
                self.interner.array_of(*length as u64, inner)
            }
            TypeKind::Name(token) => self.resolve_named(token, ty),
            TypeKind::NameWithParams(token, _) => self.resolve_named(token, ty),
            TypeKind::Struct(def) => {
                let name = def.name.clone();
                self.resolve_by_name(&name, ty)
            }
            TypeKind::Enum(def) => {
                let name = def.name.clone();
                self.resolve_by_name(&name, ty)
            }
        }
    }

    fn resolve_by_name(&mut self, name: &str, ty: &Type) -> TypeId {
        match self.interner.lookup_name(name) {
            Some(id) => id,
            None => {
                self.errors
                    .push(Box::new(ErrorUndefinedSymbol::Type(ty.clone())));
                TypeId::UNRESOLVED
            }
        }
    }

    pub fn fill_struct_def(&mut self, id: TypeId, struct_def: &ast::Struct) {
        let fields = struct_def
            .fields
            .iter()
            .map(|field| StructField {
                name: field.name.lexeme.clone(),
                ty: self.resolve_type_id(&field.ty),
                default: field.default_expr.clone(),
            })
            .collect();
        self.interner.fill_struct(id, fields);
    }

    pub fn fill_enum_def(&mut self, id: TypeId, enum_def: &ast::Enum) {
        let members = enum_def
            .variants
            .iter()
            .map(|variant| {
                (
                    variant.name.lexeme.clone(),
                    variant.value.as_ref().map(|value| value.lexeme.clone()),
                )
            })
            .collect();
        self.interner.fill_enum(id, members, TypeId::U32);
    }

    pub fn resolve_item(&mut self, item: &ast::Item) {
        match item {
            ast::Item::Function(function) => self.resolve_function(function),
            ast::Item::ExternFunction(extern_function) => {
                for param in extern_function.params.iter() {
                    self.resolve_type_id(param);
                }
                self.resolve_type_id(&extern_function.return_type);
            }
            ast::Item::Type(_) | ast::Item::Use(_) => {}
        }
    }

    fn resolve_function(&mut self, function: &ast::Function) {
        for param in function.params.iter() {
            self.resolve_type_id(&param.ty);
        }
        self.resolve_type_id(&function.return_type);
        self.resolve_block(&function.body);
    }

    pub fn resolve_block(&mut self, block: &ast::ExprBlock) {
        for statement in block.statements.iter() {
            self.resolve_expr(&statement.expr);
        }
    }

    fn resolve_type_args(&mut self, type_args: &Option<Vec<Type>>) {
        let Some(args) = type_args else {
            return;
        };
        for arg in args.iter() {
            self.resolve_type_id(arg);
        }
    }

    pub fn resolve_expr(&mut self, expr: &Expr) {
        match expr {
            Expr::Identifier(_) | Expr::Path(_) => {}
            Expr::Litral(ast::Litral::Integer(integer)) => {
                self.resolve_type_id(&integer.ty);
            }
            Expr::Litral(_) => {}
            Expr::Return(expr_return) => {
                if let Some(inner) = &expr_return.expr {
                    self.resolve_expr(inner);
                }
            }
            Expr::Struct(expr_struct) => {
                self.resolve_type_args(&expr_struct.type_args);
                for field in expr_struct.init_fields.iter() {
                    self.resolve_expr(&field.expr);
                }
            }
            Expr::Declare(expr_decl) => {
                self.resolve_expr(&expr_decl.expr);
                if let Some(ty) = &expr_decl.ty {
                    self.resolve_type_id(ty);
                }
            }
            Expr::Assignment(assignment) => {
                self.resolve_expr(&assignment.left);
                self.resolve_expr(&assignment.right);
            }
            Expr::Builtin(call) | Expr::Call(call) => {
                self.resolve_type_args(&call.type_args);
                self.resolve_expr(&call.caller);
                for arg in call.args.iter() {
                    self.resolve_expr(arg);
                }
            }
            Expr::Binary(binary) => {
                self.resolve_expr(&binary.left);
                self.resolve_expr(&binary.right);
            }
            Expr::While(expr_while) => {
                self.resolve_expr(&expr_while.condition);
                self.resolve_block(&expr_while.body);
            }
            Expr::IfElse(if_else) => {
                self.resolve_expr(&if_else.condition);
                self.resolve_block(&if_else.then_branch);
                if let Some(else_branch) = &if_else.else_branch {
                    self.resolve_expr(else_branch);
                }
                self.resolve_type_id(&if_else.ty);
            }
            Expr::MemberAccess(member_access) => self.resolve_expr(&member_access.base),
            Expr::Array(array) => {
                self.resolve_type_id(&array.ty);
                for element in array.elements.iter() {
                    self.resolve_expr(element);
                }
            }
            Expr::ArrayIndex(index) => {
                self.resolve_expr(&index.expr);
                self.resolve_expr(&index.index);
                self.resolve_type_id(&index.ty);
            }
            Expr::ArrayRepeat(repeat) => {
                self.resolve_expr(&repeat.count);
                self.resolve_expr(&repeat.value);
                self.resolve_type_id(&repeat.ty);
            }
            Expr::Block(block) => self.resolve_block(block),
            Expr::AddressOf(address_of) => self.resolve_expr(&address_of.expr),
            Expr::Deref(deref) => self.resolve_expr(&deref.base),
            Expr::Not(not) => self.resolve_expr(&not.expr),
            Expr::Grouping(grouping) => self.resolve_expr(&grouping.expr),
            Expr::TypeCast(cast) => {
                self.resolve_expr(&cast.expr);
                self.resolve_type_id(&cast.target_type);
            }
        }
    }
}

pub struct ResolveTypeStage;

impl Stage for ResolveTypeStage {
    fn name(&self) -> &'static str {
        "Resolving Types"
    }

    fn debug_mode(&self) -> &'static [DebugMode] {
        &[DebugMode::TypeCollection]
    }

    fn debug(&self, ctx: &mut StageContext) -> StageOutput {
        let mut output = String::new();
        for id in ctx.interner.nominal_ids() {
            let size = match ctx.interner.try_size_of(id) {
                Some(size) => size.to_string(),
                None => "?".to_string(),
            };
            output.push_str(&format!(
                "{:<24} {:<24} {:?} size={}\n",
                ctx.interner.name_of(id),
                ctx.interner.mangled_name_of(id),
                ctx.interner.as_bitbox_type(id),
                size,
            ));
        }
        StageOutput::Output(output)
    }

    fn run(&mut self, ctx: &mut StageContext) -> Result<()> {
        let items = ctx.take_items();
        let mut resolver = TypeResolver::new(&mut ctx.interner);

        for item in items.iter() {
            let ast::Item::Type(type_def) = item else {
                continue;
            };
            match type_def {
                ast::TypeDef::Struct(struct_def) if struct_def.type_params.is_none() => {
                    if let Some(id) = resolver.interner().lookup_name(&struct_def.name.lexeme) {
                        resolver.fill_struct_def(id, struct_def);
                    }
                }
                ast::TypeDef::Enum(enum_def) if enum_def.type_params.is_none() => {
                    if let Some(id) = resolver.interner().lookup_name(&enum_def.name.lexeme) {
                        resolver.fill_enum_def(id, enum_def);
                    }
                }
                _ => {}
            }
        }

        for item in items.iter() {
            resolver.resolve_item(item);
        }

        let mut errors = resolver.take_errors();

        for (id, path) in resolver.interner().check_finite_sizes() {
            let interner = resolver.interner();
            let cycle = path
                .iter()
                .map(|step| interner.name_of(*step))
                .collect::<Vec<_>>()
                .join(" -> ");
            errors.push(Box::new(ErrorMessage(format!(
                "recursive type `{}` has infinite size: {} -> {}\nhelp: insert an indirection, such as `*{}`",
                interner.name_of(id),
                interner.name_of(id),
                cycle,
                interner.name_of(id),
            ))));
        }

        let unfilled: Vec<String> = resolver
            .interner()
            .unfilled()
            .collect::<Vec<_>>()
            .into_iter()
            .map(|id| resolver.interner().name_of(id))
            .collect();

        ctx.items = items;

        if !errors.is_empty() {
            let errors = errors
                .into_iter()
                .map(|error| ctx.scope_error(error))
                .collect();
            return Err(Box::new(Errors { errors }));
        }

        if !unfilled.is_empty() {
            return Err(Box::new(ErrorMessage(format!(
                "internal error: types declared but never filled: {}",
                unfilled.join(", ")
            ))));
        }

        Ok(())
    }
}

#[cfg(all(test, not(feature = "wasm")))]
mod tests {
    use super::*;
    use crate::stage::define_type::DefineTypeStage;
    use crate::stage::module_loader::{FlattenModulesStage, LoadedModuleStage};
    use crate::stage::monomorphize::MonomorphizerStage;

    fn resolve_types_for(entry: &str) -> StageContext {
        let mut ctx = StageContext {
            entry: std::path::PathBuf::from(entry),
            ..Default::default()
        };

        let mut pipeline: Vec<Box<dyn Stage>> = vec![
            Box::new(LoadedModuleStage),
            Box::new(FlattenModulesStage),
            Box::new(MonomorphizerStage),
            Box::new(DefineTypeStage),
            Box::new(ResolveTypeStage),
        ];

        for pass in pipeline.iter_mut() {
            let name = pass.name();
            pass.execute(&mut ctx)
                .unwrap_or_else(|err| panic!("stage `{name}` failed: {err:?}"));
        }

        ctx
    }

    #[test]
    fn every_declared_type_gets_filled() {
        let ctx = resolve_types_for("./testing/test/structs.cb");

        assert_eq!(ctx.interner.unfilled().count(), 0);
    }

    #[test]
    fn struct_fields_are_interned_in_declaration_order() {
        let ctx = resolve_types_for("./testing/test/structs.cb");
        let list = ctx.interner.lookup_name("List").expect("List is declared");

        let fields = ctx.interner.struct_fields(list).expect("List is a struct");
        assert_eq!(fields.len(), 1);
        assert_eq!(fields[0].name, "data");
        assert_eq!(ctx.interner.name_of(fields[0].ty), "[2; s32]");
    }

    #[test]
    fn generic_structs_are_interned_under_their_mangled_name() {
        let ctx = resolve_types_for("./testing/test/generics.cb");

        assert!(ctx.interner.lookup_name("Pair").is_none());
        let pair = ctx
            .interner
            .lookup_name("Pair__s32")
            .expect("the specialized Pair is declared");

        let fields = ctx.interner.struct_fields(pair).expect("a struct");
        assert_eq!(fields.len(), 2);
        assert_eq!(ctx.interner.name_of(fields[0].ty), "s32");
        assert_eq!(ctx.interner.name_of(fields[1].ty), "s32");
    }

    #[test]
    fn no_corpus_type_has_infinite_size() {
        let ctx = resolve_types_for("./testing/test/structs.cb");

        assert!(ctx.interner.check_finite_sizes().is_empty());
    }
}
