//! Monomorphization.
//!
//! c-flat generics are compiled away here: every concrete instantiation of a
//! generic struct (`Pair<(s32)>`) or generic function (`add<(s32)>`) becomes its
//! own specialized, name-mangled item with the type parameters substituted for
//! concrete types. Generic *templates* are dropped from the output entirely.
//!
//! This runs as a pure AST -> AST transform *before* symbol-table building and
//! type checking, so every later stage (resolver, type checker, IR builder, both
//! backends) only ever sees concrete types. A type parameter `T` never reaches
//! codegen.
//!
//! Algorithm:
//!   1. Split items into generic templates and concrete items.
//!   2. Rewrite references in the concrete items, queuing each instantiation we
//!      find (`Pair<(s32)>` -> `Pair__s32`, `add<(s32)>(..)` -> `add__s32(..)`).
//!   3. Drain the queue: clone each template, substitute its type parameters,
//!      rewrite its body (which may queue further instantiations), and emit it.
//!      A fixpoint over the queue handles generics that use other generics.

use crate::DebugMode;
use crate::stage::{Stage, StageContext, StageOutput};
use std::collections::{HashMap, HashSet};

use crate::error::{ErrorMessage, Errors, Report, Result};
use crate::stage::lexer::token::{Span, Token};
use crate::stage::parser::ast::{
    self, Expr, ExprBlock, ExprCall, ExprStruct, Litral, Type, TypeKind,
};

const GENERIC_SEPARATOR: &str = "__";

pub struct MonomorphizerStage;

impl Stage for MonomorphizerStage {
    fn name(&self) -> &'static str {
        "Monomorphizing"
    }
    fn debug_mode(&self) -> &'static [DebugMode] {
        // TODO: MonomorphizerStage does not have a debug_mode
        &[DebugMode::Monomorphizer]
    }

    fn debug(&self, _ctx: &mut StageContext) -> StageOutput {
        todo!("MonomorphizerStage does not have a debug output")
    }

    fn run(&mut self, ctx: &mut StageContext) -> Result<()> {
        let monomorphize = crate::stage::monomorphize::Monomorphizer::default();
        let items = ctx.take_items();
        ctx.items = monomorphize.run(items)?;
        Ok(())
    }
}

/// Example:
/// `Pair` with args `[s32]` -> `Pair__s32`.
struct Instantiation {
    base: String,
    args: Vec<Type>,
    mangled: String,
}

#[derive(Default)]
pub struct Monomorphizer {
    struct_templates: HashMap<String, ast::Struct>,
    fn_templates: HashMap<String, ast::Function>,
    queue: Vec<Instantiation>,
    done: HashSet<String>,
    specialized: Vec<ast::Item>,
    errors: Vec<Box<dyn Report>>,
}

impl Monomorphizer {
    pub fn run(mut self, items: Vec<ast::Item>) -> Result<Vec<ast::Item>> {
        let mut concrete: Vec<ast::Item> = Vec::new();
        for item in items {
            match item {
                ast::Item::Type(ast::TypeDef::Struct(s)) if s.type_params.is_some() => {
                    self.struct_templates.insert(s.name.lexeme.clone(), s);
                }
                ast::Item::Function(f) if f.type_args.is_some() => {
                    self.fn_templates.insert(f.name.lexeme.clone(), f);
                }
                other => concrete.push(other),
            }
        }

        for item in concrete.iter_mut() {
            self.rewrite_item(item);
        }

        while let Some(inst) = self.queue.pop() {
            if !self.done.insert(inst.mangled.clone()) {
                continue;
            }
            self.specialize(&inst);
        }

        if !self.errors.is_empty() {
            return Err(Box::new(Errors {
                errors: self.errors,
            }));
        }

        concrete.extend(self.specialized);
        Ok(concrete)
    }

    fn specialize(&mut self, inst: &Instantiation) {
        if let Some(template) = self.struct_templates.get(&inst.base).cloned() {
            let subst = build_subst(&template.type_params, &inst.args);
            let mut s = template;
            s.type_params = None;
            s.name.lexeme = inst.mangled.clone();
            for field in s.fields.iter_mut() {
                substitute_type(&mut field.ty, &subst);
            }
            let mut item = ast::Item::Type(ast::TypeDef::Struct(s));
            self.rewrite_item(&mut item);
            self.specialized.push(item);
        } else if let Some(template) = self.fn_templates.get(&inst.base).cloned() {
            let subst = build_subst(&template.type_args, &inst.args);
            let mut f = template;
            f.type_args = None;
            f.name.lexeme = inst.mangled.clone();
            for param in f.params.iter_mut() {
                substitute_type(&mut param.ty, &subst);
            }
            substitute_type(&mut f.return_type, &subst);
            substitute_block(&mut f.body, &subst);
            let mut item = ast::Item::Function(f);
            self.rewrite_item(&mut item);
            self.specialized.push(item);
        }
    }

    fn rewrite_item(&mut self, item: &mut ast::Item) {
        match item {
            ast::Item::Function(f) => {
                for param in f.params.iter_mut() {
                    self.rewrite_type(&mut param.ty);
                }
                self.rewrite_type(&mut f.return_type);
                self.rewrite_block(&mut f.body);
            }
            ast::Item::Type(ast::TypeDef::Struct(s)) => {
                for field in s.fields.iter_mut() {
                    self.rewrite_type(&mut field.ty);
                }
            }
            ast::Item::Type(ast::TypeDef::Enum(..)) => {
                // For now i think we can skip this
            }
            ast::Item::ExternFunction(ef) => {
                for param in ef.params.iter_mut() {
                    self.rewrite_type(param);
                }
                self.rewrite_type(&mut ef.return_type);
            }
            ast::Item::Use(_) => {}
        }
    }

    /// Resolves any [`TypeKind::NameWithParams`] into a mangled [`TypeKind::Name`]
    fn rewrite_type(&mut self, ty: &mut Type) {
        match &mut ty.kind {
            TypeKind::Pointer(inner) => self.rewrite_type(inner),
            TypeKind::Slice(inner) => self.rewrite_type(inner),
            TypeKind::Array(_, inner) => self.rewrite_type(inner),
            TypeKind::Struct(st) => {
                for (_, fty) in st.fields.iter_mut() {
                    self.rewrite_type(fty);
                }
            }
            TypeKind::NameWithParams(name, params) => {
                for param in params.params.iter_mut() {
                    self.rewrite_type(param);
                }
                if self.struct_templates.contains_key(&name.lexeme) {
                    let args = params.params.clone();
                    let mangled = mangle_name(&name.lexeme, &args);
                    self.enqueue(name.lexeme.clone(), args, mangled.clone(), true);
                    let mut tok = name.clone();
                    tok.lexeme = mangled;
                    ty.kind = TypeKind::Name(tok);
                }
            }
            _ => {}
        }
    }

    fn rewrite_block(&mut self, block: &mut ExprBlock) {
        for stmt in block.statements.iter_mut() {
            self.rewrite_expr(&mut stmt.expr);
        }
    }

    fn rewrite_expr(&mut self, expr: &mut Expr) {
        match expr {
            Expr::Builtin(call) => self.rewrite_call(call),
            Expr::Call(call) => self.rewrite_call(call),
            Expr::Struct(s) => self.rewrite_struct_expr(s),
            Expr::Return(r) => {
                if let Some(e) = r.expr.as_mut() {
                    self.rewrite_expr(e);
                }
            }
            Expr::Declare(d) => {
                if let Some(t) = d.ty.as_mut() {
                    self.rewrite_type(t);
                }
                self.rewrite_expr(&mut d.expr);
            }
            Expr::Assignment(a) => {
                self.rewrite_expr(&mut a.left);
                self.rewrite_expr(&mut a.right);
            }
            Expr::Binary(b) => {
                self.rewrite_expr(&mut b.left);
                self.rewrite_expr(&mut b.right);
            }
            Expr::While(w) => {
                self.rewrite_expr(&mut w.condition);
                self.rewrite_block(&mut w.body);
            }
            Expr::IfElse(i) => {
                self.rewrite_expr(&mut i.condition);
                self.rewrite_block(&mut i.then_branch);
                if let Some(e) = i.else_branch.as_mut() {
                    self.rewrite_expr(e);
                }
                self.rewrite_type(&mut i.ty);
            }
            Expr::MemberAccess(m) => self.rewrite_expr(&mut m.base),
            Expr::Array(a) => {
                self.rewrite_type(&mut a.ty);
                for e in a.elements.iter_mut() {
                    self.rewrite_expr(e);
                }
            }
            Expr::ArrayIndex(a) => {
                self.rewrite_expr(&mut a.expr);
                self.rewrite_expr(&mut a.index);
                self.rewrite_type(&mut a.ty);
            }
            Expr::ArrayRepeat(a) => {
                self.rewrite_expr(&mut a.count);
                self.rewrite_expr(&mut a.value);
                self.rewrite_type(&mut a.ty);
            }
            Expr::Block(b) => self.rewrite_block(b),
            Expr::AddressOf(a) => self.rewrite_expr(&mut a.expr),
            Expr::Deref(d) => self.rewrite_expr(&mut d.base),
            Expr::Not(n) => self.rewrite_expr(&mut n.expr),
            Expr::Grouping(g) => self.rewrite_expr(&mut g.expr),
            Expr::TypeCast(c) => {
                self.rewrite_expr(&mut c.expr);
                self.rewrite_type(&mut c.target_type);
            }
            Expr::Litral(_) | Expr::Identifier(_) | Expr::Path(_) => {}
        }
    }

    fn rewrite_call(&mut self, call: &mut ExprCall) {
        for arg in call.args.iter_mut() {
            self.rewrite_expr(arg);
        }
        if let Some(targs) = call.type_args.as_mut() {
            for t in targs.iter_mut() {
                self.rewrite_type(t);
            }
        }

        let caller_tok = match call.caller.as_ref() {
            Expr::Identifier(t) => t.clone(),
            Expr::Path(p) => p.leaf().clone(),
            _ => return,
        };
        if !self.fn_templates.contains_key(&caller_tok.lexeme) {
            return;
        }

        let args = if let Some(targs) = &call.type_args {
            targs.clone()
        } else {
            match self.infer_fn_args(&caller_tok.lexeme, call) {
                Some(a) => a,
                None => {
                    self.errors.push(Box::new(ErrorMessage(format!(
                        "cannot infer type arguments for generic call `{}`; use explicit `<( .. )>`",
                        caller_tok.lexeme
                    ))));
                    return;
                }
            }
        };

        let mangled = mangle_name(&caller_tok.lexeme, &args);
        self.enqueue(caller_tok.lexeme.clone(), args, mangled.clone(), false);

        let mut tok = caller_tok;
        tok.lexeme = mangled;
        *call.caller = Expr::Identifier(tok);
        call.type_args = None;
    }

    fn rewrite_struct_expr(&mut self, s: &mut ExprStruct) {
        for field in s.init_fields.iter_mut() {
            self.rewrite_expr(&mut field.expr);
        }
        if let Some(targs) = s.type_args.as_mut() {
            for t in targs.iter_mut() {
                self.rewrite_type(t);
            }
        }
        if !self.struct_templates.contains_key(&s.name.lexeme) {
            return;
        }

        let Some(args) = s.type_args.clone() else {
            self.errors.push(Box::new(ErrorMessage(format!(
                "cannot infer type arguments for generic struct `{}`; use explicit `<( .. )>`",
                s.name.lexeme
            ))));
            return;
        };

        let mangled = mangle_name(&s.name.lexeme, &args);
        self.enqueue(s.name.lexeme.clone(), args, mangled.clone(), true);
        s.name.lexeme = mangled;
        s.type_args = None;
    }

    fn enqueue(&mut self, base: String, args: Vec<Type>, mangled: String, is_struct: bool) {
        if self.done.contains(&mangled) {
            return;
        }
        let known = if is_struct {
            self.struct_templates.contains_key(&base)
        } else {
            self.fn_templates.contains_key(&base)
        };
        if !known {
            return;
        }
        self.queue.push(Instantiation {
            base,
            args,
            mangled,
        });
    }

    fn infer_fn_args(&self, name: &str, call: &ExprCall) -> Option<Vec<Type>> {
        let template = self.fn_templates.get(name)?;
        let params = template.type_args.as_ref()?;
        let names: HashSet<String> = params.iter().map(|(t, _)| t.lexeme.clone()).collect();

        let mut bindings: HashMap<String, Type> = HashMap::new();
        for (decl, arg) in template.params.iter().zip(&call.args) {
            let actual = infer_expr_type(arg)?;
            unify(&decl.ty, &actual, &names, &mut bindings);
        }

        let mut out = Vec::with_capacity(params.len());
        for (tok, _) in params {
            out.push(bindings.get(&tok.lexeme)?.clone());
        }
        Some(out)
    }
}

fn ty(kind: TypeKind, span: Span) -> Type {
    Type {
        mut_token: None,
        kind,
        span,
    }
}

fn build_subst(params: &Option<Vec<(Token, Type)>>, args: &[Type]) -> HashMap<String, Type> {
    let mut map = HashMap::new();
    if let Some(params) = params {
        for ((tok, _), arg) in params.iter().zip(args) {
            map.insert(tok.lexeme.clone(), arg.clone());
        }
    }
    map
}

fn substitute_type(ty: &mut Type, subst: &HashMap<String, Type>) {
    match &mut ty.kind {
        TypeKind::Name(tok) => {
            if let Some(replacement) = subst.get(&tok.lexeme) {
                let mut_token = ty.mut_token.take();
                let span = ty.span.clone();
                *ty = replacement.clone();
                if mut_token.is_some() {
                    ty.mut_token = mut_token;
                }
                ty.span = span;
            }
        }
        TypeKind::NameWithParams(_, params) => {
            for param in params.params.iter_mut() {
                substitute_type(param, subst);
            }
        }
        TypeKind::Pointer(inner) => substitute_type(inner, subst),
        TypeKind::Slice(inner) => substitute_type(inner, subst),
        TypeKind::Array(_, inner) => substitute_type(inner, subst),
        TypeKind::Struct(st) => {
            for (_, fty) in st.fields.iter_mut() {
                substitute_type(fty, subst);
            }
        }
        _ => {}
    }
}

fn substitute_block(block: &mut ExprBlock, subst: &HashMap<String, Type>) {
    for stmt in block.statements.iter_mut() {
        substitute_expr(&mut stmt.expr, subst);
    }
}

fn substitute_expr(expr: &mut Expr, subst: &HashMap<String, Type>) {
    match expr {
        Expr::Call(c) | Expr::Builtin(c) => {
            if let Some(targs) = c.type_args.as_mut() {
                for t in targs.iter_mut() {
                    substitute_type(t, subst);
                }
            }
            for arg in c.args.iter_mut() {
                substitute_expr(arg, subst);
            }
        }
        Expr::Struct(s) => {
            if let Some(targs) = s.type_args.as_mut() {
                for t in targs.iter_mut() {
                    substitute_type(t, subst);
                }
            }
            for field in s.init_fields.iter_mut() {
                substitute_expr(&mut field.expr, subst);
            }
        }
        Expr::Return(r) => {
            if let Some(e) = r.expr.as_mut() {
                substitute_expr(e, subst);
            }
        }
        Expr::Declare(d) => {
            if let Some(t) = d.ty.as_mut() {
                substitute_type(t, subst);
            }
            substitute_expr(&mut d.expr, subst);
        }
        Expr::Assignment(a) => {
            substitute_expr(&mut a.left, subst);
            substitute_expr(&mut a.right, subst);
        }
        Expr::Binary(b) => {
            substitute_expr(&mut b.left, subst);
            substitute_expr(&mut b.right, subst);
        }
        Expr::While(w) => {
            substitute_expr(&mut w.condition, subst);
            substitute_block(&mut w.body, subst);
        }
        Expr::IfElse(i) => {
            substitute_expr(&mut i.condition, subst);
            substitute_block(&mut i.then_branch, subst);
            if let Some(e) = i.else_branch.as_mut() {
                substitute_expr(e, subst);
            }
            substitute_type(&mut i.ty, subst);
        }
        Expr::MemberAccess(m) => substitute_expr(&mut m.base, subst),
        Expr::Array(a) => {
            substitute_type(&mut a.ty, subst);
            for e in a.elements.iter_mut() {
                substitute_expr(e, subst);
            }
        }
        Expr::ArrayIndex(a) => {
            substitute_expr(&mut a.expr, subst);
            substitute_expr(&mut a.index, subst);
            substitute_type(&mut a.ty, subst);
        }
        Expr::ArrayRepeat(a) => {
            substitute_expr(&mut a.count, subst);
            substitute_expr(&mut a.value, subst);
            substitute_type(&mut a.ty, subst);
        }
        Expr::Block(b) => substitute_block(b, subst),
        Expr::AddressOf(a) => substitute_expr(&mut a.expr, subst),
        Expr::Deref(d) => substitute_expr(&mut d.base, subst),
        Expr::Not(n) => substitute_expr(&mut n.expr, subst),
        Expr::Grouping(g) => substitute_expr(&mut g.expr, subst),
        Expr::TypeCast(c) => {
            substitute_expr(&mut c.expr, subst);
            substitute_type(&mut c.target_type, subst);
        }
        Expr::Litral(_) | Expr::Identifier(_) | Expr::Path(_) => {}
    }
}

fn unify(
    decl: &Type,
    actual: &Type,
    names: &HashSet<String>,
    bindings: &mut HashMap<String, Type>,
) {
    match (&decl.kind, &actual.kind) {
        (TypeKind::Name(p), _) if names.contains(&p.lexeme) => {
            bindings
                .entry(p.lexeme.clone())
                .or_insert_with(|| actual.clone());
        }
        (TypeKind::Pointer(a), TypeKind::Pointer(b)) => unify(a, b, names, bindings),
        (TypeKind::Slice(a), TypeKind::Slice(b)) => unify(a, b, names, bindings),
        (TypeKind::Array(_, a), TypeKind::Array(_, b)) => unify(a, b, names, bindings),
        _ => {}
    }
}

fn infer_expr_type(expr: &Expr) -> Option<Type> {
    let ty_with_span = |kind: TypeKind| ty(kind, expr.span());
    match expr {
        Expr::Litral(Litral::Integer(_)) => Some(ty_with_span(TypeKind::SignedNumber(32))),
        Expr::Litral(Litral::Float(_)) => Some(ty_with_span(TypeKind::Float(32))),
        Expr::Litral(Litral::BoolTrue(_)) | Expr::Litral(Litral::BoolFalse(_)) => {
            Some(ty_with_span(TypeKind::Bool))
        }
        Expr::Grouping(g) => infer_expr_type(&g.expr),
        Expr::Binary(b) => infer_expr_type(&b.left),
        Expr::AddressOf(a) => Some(ty_with_span(TypeKind::Pointer(Box::new(infer_expr_type(
            &a.expr,
        )?)))),
        Expr::Deref(d) => match infer_expr_type(&d.base)?.kind {
            TypeKind::Pointer(inner) => Some(*inner),
            _ => None,
        },
        _ => None,
    }
}

fn mangle_name(base: &str, args: &[Type]) -> String {
    let mut s = base.to_string();
    for arg in args {
        s.push_str(GENERIC_SEPARATOR);
        s.push_str(&mangle_type(&arg.kind));
    }
    s
}

fn mangle_type(kind: &TypeKind) -> String {
    match kind {
        TypeKind::Bool => "bool".into(),
        TypeKind::Void => "void".into(),
        TypeKind::Type => "type".into(),
        TypeKind::Enum(e) => e.name.clone(),
        TypeKind::Float(n) => format!("f{n}"),
        TypeKind::SignedNumber(n) => format!("s{n}"),
        TypeKind::UnsignedNumber(n) => format!("u{n}"),
        TypeKind::SignedTargetPointerNumber => "ssize".into(),
        TypeKind::UnsignedTargetPointerNumber => "usize".into(),
        TypeKind::Pointer(inner) => format!("ptr_{}", mangle_type(&inner.kind)),
        TypeKind::Slice(inner) => format!("slice_{}", mangle_type(&inner.kind)),
        TypeKind::Array(n, inner) => format!("arr{n}_{}", mangle_type(&inner.kind)),
        TypeKind::Name(tok) => tok.lexeme.clone(),
        TypeKind::Struct(st) => st.name.clone(),
        TypeKind::NameWithParams(name, params) => {
            let mut s = name.lexeme.clone();
            for param in &params.params {
                s.push_str(GENERIC_SEPARATOR);
                s.push_str(&mangle_type(&param.kind));
            }
            s
        }
    }
}
