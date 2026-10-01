use std::str::FromStr;

use crate::{
    DebugMode,
    error::{
        ErrorMemberAccess, ErrorMissMatchedType, ErrorUndefinedSymbol, ErrorUnsupportedBinaryOp,
        Errors, Report, Result,
    },
    stage::{
        Stage, StageContext, StageOutput,
        lexer::token::{Keyword as Kw, Span, Token, TokenKind},
        parser::ast::{self, EnumType, Expr, StructType, Type, TypeKind},
        symbol_table::{ScopePath, SymbolTable},
    },
    type_interner::{TypeId, TypeInterner},
};

pub struct TypeCheckerStage;

impl Stage for TypeCheckerStage {
    fn name(&self) -> &'static str {
        "Type Checking"
    }
    fn debug_mode(&self) -> &'static [DebugMode] {
        &[DebugMode::TypeChecker]
    }

    fn debug(&self, _ctx: &mut StageContext) -> StageOutput {
        StageOutput::Nothing
    }

    fn run(&mut self, ctx: &mut StageContext) -> Result<()> {
        let mut items = ctx.take_items();

        let interner = ctx.interner.clone();
        let symbol_table = ctx.symbol_table_mut()?;
        TypeChecker::new(symbol_table, interner).check(&mut items)?;

        ctx.items = items;
        Ok(())
    }
}

fn unsigned_kind_of(t: &TypeKind) -> Option<&TypeKind> {
    match t {
        TypeKind::UnsignedNumber(_) => Some(t),
        TypeKind::Enum(EnumType { number_kind, .. }) => match &**number_kind {
            TypeKind::UnsignedNumber(_) => Some(&**number_kind),
            _ => None,
        },
        _ => None,
    }
}

impl Type {
    pub fn supports_binary_op(
        &self,
        op: &TokenKind,
        other: &Type,
        span: Span,
        interner: &TypeInterner,
    ) -> Option<Type> {
        use TokenKind::*;

        if let (Some(lhs_id), op, Some(rhs_id)) = (&self.id, op, &other.id)
            && interner.binary_op_result(*lhs_id, op, *rhs_id).is_some()
        {
            let id = interner.binary_op_result(*lhs_id, op, *rhs_id).unwrap();
            let mut new_ty = self.clone();
            new_ty.id = Some(id);
            new_ty.span = span;
            return Some(new_ty);
        }

        match (&self.kind, op, &other.kind) {
            // (Plus | Minus | Star | Slash | Percent) only work on numbers and return the same type
            (
                TypeKind::SignedTargetPointerNumber,
                Plus | Minus | Star | Slash | Percent | BitShiftRight | Ampersand,
                TypeKind::SignedTargetPointerNumber,
            ) => Some(self.clone()),
            (
                TypeKind::UnsignedTargetPointerNumber,
                Plus | Minus | Star | Slash | Percent | BitShiftRight | Ampersand,
                TypeKind::UnsignedTargetPointerNumber,
            ) => Some(self.clone()),
            (
                TypeKind::UnsignedNumber(lhs),
                Plus | Minus | Star | Slash | Percent | BitShiftRight | Ampersand,
                TypeKind::UnsignedNumber(rhs),
            ) if lhs == rhs => Some(self.clone()),
            (
                TypeKind::SignedNumber(lhs),
                Plus | Minus | Star | Slash | Percent | BitShiftRight | Ampersand,
                TypeKind::SignedNumber(rhs),
            ) if lhs == rhs => Some(self.clone()),
            (
                TypeKind::Float(lhs),
                Plus | Minus | Star | Slash | Percent | BitShiftRight | Ampersand,
                TypeKind::Float(rhs),
            ) if lhs == rhs => Some(self.clone()),

            // (EqualEqual | Greater | GreaterEqual | Less | LessEqual) Comparison ops work on numbers and return bools
            (
                TypeKind::UnsignedTargetPointerNumber,
                EqualEqual | Greater | GreaterEqual | Less | LessEqual,
                TypeKind::UnsignedTargetPointerNumber,
            ) => Some(self.map_kind(|_| TypeKind::Bool)),
            (
                TypeKind::SignedTargetPointerNumber,
                EqualEqual | Greater | GreaterEqual | Less | LessEqual,
                TypeKind::SignedTargetPointerNumber,
            ) => Some(self.map_kind(|_| TypeKind::Bool)),
            (
                lhs_ty @ (TypeKind::Enum(_) | TypeKind::UnsignedNumber(_)),
                EqualEqual | Greater | GreaterEqual | Less | LessEqual,
                rhs_ty @ (TypeKind::Enum(_) | TypeKind::UnsignedNumber(_)),
            ) => match (unsigned_kind_of(lhs_ty), unsigned_kind_of(rhs_ty)) {
                (Some(TypeKind::UnsignedNumber(lhs)), Some(TypeKind::UnsignedNumber(rhs)))
                    if lhs == rhs =>
                {
                    Some(self.map_kind(|_| TypeKind::Bool))
                }
                _ => None,
            },
            (
                TypeKind::SignedNumber(lhs),
                EqualEqual | Greater | GreaterEqual | Less | LessEqual,
                TypeKind::SignedNumber(rhs),
            ) if lhs == rhs => Some(self.map_kind(|_| TypeKind::Bool)),
            (
                TypeKind::Float(lhs),
                EqualEqual | Greater | GreaterEqual | Less | LessEqual,
                TypeKind::Float(rhs),
            ) if lhs == rhs => Some(self.map_kind(|_| TypeKind::Bool)),

            // (AND | OR) only work on bools and return bools
            (TypeKind::Bool, EqualEqual | Keyword(Kw::And) | Keyword(Kw::Or), TypeKind::Bool) => {
                Some(self.map_kind(|_| TypeKind::Bool))
            }
            _ => None,
        }
        .map(|mut t| {
            t.span = span;
            t
        })
    }
}

pub struct TypeChecker<'st> {
    interner: TypeInterner,
    symbol_table: &'st mut SymbolTable,
    errors: Vec<Box<dyn Report>>,
    /// When set, integer literals inside an array literal are typed as this
    /// instead of the default `SignedNumber(32)`. Allows `[10; u8] = [65, 66, ...]`.
    numeric_hint: Option<Type>,
}

impl<'st> TypeChecker<'st> {
    pub fn new(symbol_table: &'st mut SymbolTable, interner: TypeInterner) -> Self {
        Self {
            interner,
            symbol_table,
            errors: Vec::new(),
            numeric_hint: None,
        }
    }

    pub fn check(mut self, ast: &mut [ast::Item]) -> Result<()> {
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

    fn walk_use(&mut self, item: &ast::Use) -> ast::Type {
        Type {
            kind: TypeKind::Void,
            span: item.span(),
            mut_token: None,
            id: Some(TypeId::VOID),
        }
    }

    fn walk_function(&mut self, function: &mut ast::Function) -> ast::Type {
        self.symbol_table.enter_scope(function.name.lexeme.as_str());
        let calulated_return_type = self.walk_block(&mut function.body);
        self.symbol_table.exit_scope();
        if let (Some(crt_id), Some(rt_id)) = (&calulated_return_type.id, &function.return_type.id)
            && !self.interner.same_type(*crt_id, *rt_id)
        {
            self.errors.push(Box::new(ErrorMissMatchedType::new(
                calulated_return_type.name(&self.interner),
                function.return_type.name(&self.interner),
                function.return_type.span.clone(),
                #[cfg(feature = "debug")]
                format!("{} {}:{}", file!(), line!(), column!()),
            )));
        }

        function.return_type.clone()
    }

    fn walk_type_def(&mut self, type_def: &mut ast::TypeDef) -> ast::Type {
        match type_def {
            ast::TypeDef::Struct(struct_def) => self.walk_struct_def(struct_def),
            ast::TypeDef::Enum(enum_def) => self.walk_enum_def(enum_def),
        }
    }

    fn walk_enum_def(&mut self, enum_def: &mut ast::Enum) -> ast::Type {
        Type {
            mut_token: None,
            span: enum_def.span(),
            kind: TypeKind::Void,
            id: self.interner.lookup_name(&enum_def.name.lexeme),
        }
    }

    fn walk_struct_def(&mut self, struct_def: &mut ast::Struct) -> ast::Type {
        Type {
            mut_token: None,
            span: struct_def.span(),
            kind: TypeKind::Void,
            id: self.interner.lookup_name(&struct_def.name.lexeme),
        }
    }

    fn walk_block(&mut self, block: &mut ast::ExprBlock) -> ast::Type {
        let last_index = block.statements.len().saturating_sub(1);
        let span = block
            .statements
            .get(last_index)
            .map(|e| e.span())
            .unwrap_or_default();

        let mut last_type = Type {
            kind: TypeKind::Void,
            span: span.clone(),
            mut_token: None,
            id: Some(TypeId::VOID),
        };

        for (i, statement) in block.statements.iter_mut().enumerate() {
            if i != last_index {
                self.walk_expr(&mut statement.expr);
                continue;
            }

            let ty = self.walk_expr(&mut statement.expr);

            if let ast::Expr::Return(..) = &*statement.expr {
                last_type = ty;
            } else if statement.delem.is_some() {
                last_type = Type {
                    mut_token: None,
                    kind: TypeKind::Void,
                    span: span.clone(),
                    id: Some(TypeId::VOID),
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
            ast::Expr::Deref(expr) => self.walk_expr_deref(expr),
            ast::Expr::Array(expr) => self.walk_expr_array(expr),
            ast::Expr::ArrayIndex(expr) => self.walk_expr_array_index(expr),
            ast::Expr::ArrayRepeat(expr) => self.walk_expr_array_repeat(expr),
            ast::Expr::Assignment(expr) => self.walk_expr_assignment(expr),
            ast::Expr::Binary(expr) => self.walk_expr_binary(expr),
            ast::Expr::Block(expr) => self.walk_block(expr),
            ast::Expr::Builtin(expr) => self.walk_expr_builtin(expr),
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
                id: Some(TypeId::VOID),
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
                ErrorMissMatchedType::new(
                    rhs.name(&self.interner),
                    lhs.name(&self.interner),
                    rhs.span,
                    #[cfg(feature = "debug")]
                    format!("{} {}:{}", file!(), line!(), column!()),
                )
                .alt_span(expr.left.span())
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
            && !ty.kind.compair(&value_type.kind)
        {
            eprintln!("{:?}", ty);
            eprintln!("{:?}", value_type);
            self.errors.push(Box::new(ErrorMissMatchedType::new(
                value_type.name(&self.interner),
                ty.name(&self.interner),
                ty.span.clone(),
                #[cfg(feature = "debug")]
                format!("{} {}:{}", file!(), line!(), column!()),
            )));
        }
        self.symbol_table.get_mut(&expr.ident.lexeme, |s| {
            if let (Some(lhs), Some(rhs)) = (s.ty.id, value_type.id)
                && self.interner.same_type(lhs, TypeId::VOID)
                && !self.interner.same_type(lhs, rhs)
            {
                s.ty = value_type.clone()
            }
        });
        value_type
    }

    fn walk_expr_litral(&mut self, expr: &mut ast::Litral) -> ast::Type {
        let span = expr.span();
        match expr {
            ast::Litral::Integer(integer_litral) => {
                let ty = self
                    .numeric_hint
                    .clone()
                    .unwrap_or(integer_litral.ty.clone());
                integer_litral.ty = ty.clone();
                ty
            }
            ast::Litral::Float(_) => self.numeric_hint.clone().unwrap_or(Type {
                kind: TypeKind::Float(32),
                span: expr.span(),
                mut_token: None,
                id: Some(TypeId::F32),
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
                    span,
                    mut_token: None,
                    id: Some(self.interner.unsigned(bytes)),
                }
            }
            ast::Litral::String(s) => Type {
                kind: TypeKind::Void,
                span,
                mut_token: None,
                id: Some(self.interner.array_of(s.lexeme.len() as u64, TypeId::U8)),
            },
            ast::Litral::BoolTrue(_) | ast::Litral::BoolFalse(_) => Type {
                kind: TypeKind::Bool,
                span,
                mut_token: None,
                id: Some(TypeId::BOOL),
            },
        }
    }

    fn walk_expr_builtin(&mut self, expr: &mut ast::ExprCall) -> ast::Type {
        let name = match expr.caller.as_ref() {
            ast::Expr::Identifier(token) if matches!(token.kind, TokenKind::Builtin(..)) => {
                token.lexeme.clone()
            }
            c => panic!("Unknown Builtin {c:?}"),
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

    fn walk_expr_call(&mut self, expr: &mut ast::ExprCall) -> ast::Type {
        let name = match expr.caller.as_ref() {
            ast::Expr::Identifier(ident) => ident.lexeme.clone(),
            ast::Expr::Path(path) => path.leaf().lexeme.clone(),
            _ => panic!("Caller must be an identifier or path"),
        };

        let Some(symbol) = self.symbol_table.get(name.as_str()).cloned() else {
            unreachable!("unhandled error: Unknown function {}", name);
        };

        let expected_arg_count = symbol.params.as_ref().map(|v| v.len()).unwrap_or_default();

        for (arg, ty) in expr.args.iter_mut().zip(&symbol.params.unwrap_or_default()) {
            self.maybe_numeric_hint(ty);
            self.walk_expr(arg);
            self.numeric_hint = None;
        }

        // TODO: Make this a user error
        if expr.args.len() != expected_arg_count {
            panic!(
                "function: {} @ index {} expected {} args but found {} in {}",
                symbol.name,
                expr.span().start,
                expected_arg_count,
                expr.args.len(),
                expr.span().filename,
            );
        }

        symbol.ty.clone()
    }

    fn walk_expr_binary(&mut self, expr: &mut ast::ExprBinary) -> ast::Type {
        let left_ty = self.walk_expr(&mut expr.left);
        self.maybe_numeric_hint(&left_ty);
        let right_ty = self.walk_expr(&mut expr.right);
        self.numeric_hint = None;
        let result_ty =
            left_ty.supports_binary_op(&expr.op.kind, &right_ty, expr.span(), &self.interner);
        let Some(result_ty) = result_ty else {
            self.errors.push(Box::new(ErrorUnsupportedBinaryOp::new(
                &expr.op.lexeme,
                left_ty.name(&self.interner),
                right_ty.name(&self.interner),
                left_ty.span.clone(),
                #[cfg(feature = "debug")]
                format!("{} {}:{}", file!(), line!(), column!()),
            )));

            return Type {
                kind: TypeKind::Void,
                span: left_ty.span,
                mut_token: None,
                id: Some(TypeId::VOID),
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
                id: Some(TypeId::VOID),
            };
        };

        // match &symbol.ty.kind {
        //     TypeKind::Enum(enum_type) => Type {
        //         mut_token: None,
        //         kind: (*enum_type.number_kind).clone(),
        //         span: expr.span.clone(),
        //     },
        //     _ => symbol.ty.clone(),
        // }
        symbol.ty.clone()
    }

    fn walk_expr_path(&mut self, path: &ast::ExprPath) -> ast::Type {
        let leaf = path.leaf();
        let head = path.head();
        let Some(symbol) = self
            .symbol_table
            .get_from_full_scope_path(&ScopePath::new(&[]), head.lexeme.as_str())
        else {
            return Type {
                kind: TypeKind::Void,
                span: leaf.span.clone(),
                mut_token: None,
                id: Some(TypeId::VOID),
            };
        };

        symbol.ty.clone()
    }

    fn walk_expr_if_else(&mut self, expr: &mut ast::ExprIfElse) -> ast::Type {
        let condition = self.walk_expr(&mut expr.condition);

        if !condition
            .id
            .map(|id| self.interner.same_type(id, TypeId::BOOL))
            .unwrap_or_default()
        {
            let error = ErrorMissMatchedType::new(
                condition.name(&self.interner),
                self.interner.name_of(TypeId::BOOL),
                condition.span,
                #[cfg(feature = "debug")]
                format!("{} {}:{}", file!(), line!(), column!()),
            )
            .alt_span(expr.condition.span());
            self.errors.push(Box::new(error));
        }
        let then_branch_type = self.walk_block(&mut expr.then_branch);
        if let Some(else_branch) = expr.else_branch.as_mut() {
            let else_branch_type = self.walk_expr(else_branch);
            if then_branch_type != else_branch_type {
                self.errors.push(Box::new(ErrorMissMatchedType::new(
                    then_branch_type.name(&self.interner),
                    else_branch_type.name(&self.interner),
                    then_branch_type.span.clone(),
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
            id: Some(TypeId::VOID),
        }
    }

    fn walk_expr_array(&mut self, expr: &mut ast::ExprArray) -> Type {
        let size = expr.elements.len();
        let ty = self.walk_expr(&mut expr.elements[0]);
        for element in expr.elements.iter_mut().skip(1) {
            let other = self.walk_expr(element);
            if ty != other {
                self.errors.push(Box::new(ErrorMissMatchedType::new(
                    ty.name(&self.interner),
                    other.name(&self.interner),
                    ty.span.clone(),
                    #[cfg(feature = "debug")]
                    format!("{} {}:{}", file!(), line!(), column!()),
                )));
            }
        }
        expr.ty = ty.clone();
        Type {
            kind: TypeKind::Array(size, Box::new(ty.clone())),
            span: expr.span(),
            mut_token: None,
            id: Some(
                self.interner
                    .array_of(size as u64, ty.id.expect("Should already have a type id")),
            ),
        }
    }

    fn walk_expr_array_index(&mut self, expr: &mut ast::ExprArrayIndex) -> Type {
        let array_type = self.walk_expr(&mut expr.expr);
        self.maybe_numeric_hint(&ast::Type {
            mut_token: None,
            kind: ast::TypeKind::UnsignedTargetPointerNumber,
            span: expr.index.span(),
            id: Some(TypeId::USIZE),
        });
        let index_type = self.walk_expr(&mut expr.index);
        if !index_type
            .id
            .map(|id| self.interner.same_type(id, TypeId::USIZE))
            .unwrap_or_default()
        {
            self.errors.push(Box::new(ErrorMissMatchedType::new(
                index_type.name(&self.interner),
                self.interner.name_of(TypeId::USIZE),
                index_type.span.clone(),
                #[cfg(feature = "debug")]
                format!("{} {}:{}", file!(), line!(), column!()),
            )));
        }

        expr.ty = array_type.clone();

        // Index one layer at a time so `*[T]` yields the slice element `T`
        // while a raw `*T` yields the pointee `T`. `de_ref` would strip every
        // pointer layer and break raw-pointer indexing.
        match &array_type.kind {
            TypeKind::Array(_, elem_ty) => *elem_ty.clone(),
            TypeKind::Slice(elem_ty) => *elem_ty.clone(),
            TypeKind::Pointer(inner) => match &inner.kind {
                TypeKind::Slice(elem_ty) => *elem_ty.clone(),
                TypeKind::Array(_, elem_ty) => *elem_ty.clone(),
                _ => (**inner).clone(),
            },
            _ => {
                self.errors.push(Box::new(ErrorMissMatchedType::new(
                    array_type.name(&self.interner),
                    // HACK: Seems like there should be a better way to express this.
                    format!("Array<{}>", array_type.name(&self.interner)),
                    array_type.span.clone(),
                    #[cfg(feature = "debug")]
                    format!("{} {}:{}", file!(), line!(), column!()),
                )));
                array_type
            }
        }
    }

    fn walk_expr_address_of(&mut self, expr: &mut ast::ExprAddressOf) -> Type {
        let inner_type = self.walk_expr(&mut expr.expr);
        Type {
            kind: TypeKind::Pointer(Box::new(inner_type.clone())),
            span: expr.span(),
            mut_token: None,
            id: inner_type.id.map(|id| self.interner.pointer_to(id)),
        }
    }

    fn walk_expr_deref(&mut self, expr: &mut ast::ExprDeref) -> Type {
        let base_type = self.walk_expr(&mut expr.base);
        match base_type.kind {
            TypeKind::Pointer(inner) => *inner,
            _ => {
                let Some(base_type_name) =
                    self.interner.lookup_name(&base_type.name(&self.interner))
                else {
                    panic!("Can ");
                };
                let expected_ptr = self.interner.pointer_to(base_type_name);
                let expected_type = self.interner.name_of(expected_ptr);
                self.errors.push(Box::new(ErrorMissMatchedType::new(
                    base_type.name(&self.interner),
                    expected_type,
                    base_type.span.clone(),
                    #[cfg(feature = "debug")]
                    format!("{} {}:{}", file!(), line!(), column!()),
                )));
                base_type
            }
        }
    }

    fn walk_expr_not(&mut self, expr: &mut ast::ExprNot) -> Type {
        self.walk_expr(&mut expr.expr);
        Type {
            kind: TypeKind::Bool,
            span: expr.span(),
            mut_token: None,
            id: Some(TypeId::BOOL),
        }
    }

    fn walk_expr_array_repeat(&mut self, expr: &mut ast::ExprArrayRepeat) -> Type {
        let ast::ExprArrayRepeat { value, count, .. } = expr;
        let value_type = self.walk_expr(value);
        let count_type = self.walk_expr(count);
        let Expr::Litral(ast::Litral::Integer(count_token)) = &**count else {
            self.errors.push(Box::new(ErrorMissMatchedType::new(
                count_type.name(&self.interner),
                self.interner.name_of(TypeId::S32),
                count_type.span.clone(),
                #[cfg(feature = "debug")]
                format!("{} {}:{}", file!(), line!(), column!()),
            )));
            return value_type;
        };

        // TODO: handle type here? Do we ignore here?
        let length = count_token.token.lexeme.parse::<u64>().unwrap();
        expr.ty = Type {
            kind: TypeKind::Array(
                count_token.token.lexeme.parse::<usize>().unwrap(),
                Box::new(value_type.clone()),
            ),
            span: expr.span(),
            mut_token: None,
            id: value_type.id.map(|id| self.interner.array_of(length, id)),
        };

        expr.ty.clone()
    }

    fn walk_expr_while(&mut self, expr: &mut ast::ExprWhile) -> Type {
        let condition = self.walk_expr(&mut expr.condition);
        if !matches!(condition.kind, TypeKind::Bool) {
            self.errors.push(Box::new(ErrorMissMatchedType::new(
                condition.name(&self.interner),
                self.interner.name_of(TypeId::BOOL),
                condition.span,
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

        let Some(base_type_id) = base_type.id else {
            self.errors.push(Box::new(ErrorMemberAccess::new(
                expr.span(),
                #[cfg(feature = "debug")]
                format!("{} {}:{}", file!(), line!(), column!()),
            )));
            return Type {
                span: expr.span(),
                ..Default::default()
            };
        };

        match self.interner.def(base_type_id) {
            crate::type_interner::TypeInfo::Array(array_def)
                if member_access.member.lexeme == "len" =>
            {
                let token = Token {
                    kind: TokenKind::Number,
                    lexeme: array_def.length.to_string(),
                    span: expr.span(),
                };
                let integer_litral = ast::IntegerLitral {
                    token,
                    ty: Type {
                        mut_token: None,
                        kind: ast::TypeKind::UnsignedTargetPointerNumber,
                        span: expr.span(),
                        id: Some(TypeId::USIZE),
                    },
                };
                *expr = ast::Expr::Litral(ast::Litral::Integer(Box::new(integer_litral)));
                Type {
                    kind: ast::TypeKind::UnsignedTargetPointerNumber,
                    span: expr.span(),
                    mut_token: None,
                    id: Some(TypeId::USIZE),
                }
            }
            crate::type_interner::TypeInfo::Struct(..) => {
                let member = member_access.member;

                let Some((_, field)) = self.interner.field(base_type_id, &member.lexeme) else {
                    self.errors.push(Box::new(ErrorMemberAccess::new(
                        member.span.clone(),
                        #[cfg(feature = "debug")]
                        format!("{} {}:{}", file!(), line!(), column!()),
                    )));
                    return Type {
                        span: member.span.clone(),
                        ..Default::default()
                    };
                };

                return Type {
                    span: member.span.clone(),
                    kind: TypeKind::Void,
                    id: Some(field.ty),
                    ..Default::default()
                };
            }
            crate::type_interner::TypeInfo::Slice(type_id)
                if member_access.member.lexeme == "len" =>
            {
                Type {
                    kind: ast::TypeKind::UnsignedTargetPointerNumber,
                    span: expr.span(),
                    mut_token: None,
                    id: Some(TypeId::USIZE),
                }
            }
            crate::type_interner::TypeInfo::Slice(type_id)
                if member_access.member.lexeme == "data" =>
            {
                let Some(ty) = self.interner.element_of(base_type_id) else {
                    self.errors.push(Box::new(ErrorMemberAccess::new(
                        expr.span(),
                        #[cfg(feature = "debug")]
                        format!("{} {}:{}", file!(), line!(), column!()),
                    )));
                    return Type {
                        span: expr.span(),
                        ..Default::default()
                    };
                };
                Type {
                    kind: TypeKind::Void,
                    span: expr.span(),
                    mut_token: None,
                    id: Some(ty),
                }
            }
            crate::type_interner::TypeInfo::Pointer(type_id) => match self.interner.def(*type_id) {
                crate::type_interner::TypeInfo::Struct(..) => {
                    let member = member_access.member;

                    let Some((_, field)) = self.interner.field(base_type_id, &member.lexeme) else {
                        self.errors.push(Box::new(ErrorMemberAccess::new(
                            member.span.clone(),
                            #[cfg(feature = "debug")]
                            format!("{} {}:{}", file!(), line!(), column!()),
                        )));
                        return Type {
                            span: member.span.clone(),
                            ..Default::default()
                        };
                    };

                    return Type {
                        span: member.span.clone(),
                        kind: TypeKind::Void,
                        id: Some(field.ty),
                        ..Default::default()
                    };
                }
                crate::type_interner::TypeInfo::Slice(type_id)
                    if member_access.member.lexeme == "len" =>
                {
                    Type {
                        kind: ast::TypeKind::UnsignedTargetPointerNumber,
                        span: expr.span(),
                        mut_token: None,
                        id: Some(TypeId::USIZE),
                    }
                }
                crate::type_interner::TypeInfo::Slice(type_id)
                    if member_access.member.lexeme == "data" =>
                {
                    let Some(ty) = self.interner.element_of(base_type_id) else {
                        self.errors.push(Box::new(ErrorMemberAccess::new(
                            expr.span(),
                            #[cfg(feature = "debug")]
                            format!("{} {}:{}", file!(), line!(), column!()),
                        )));
                        return Type {
                            span: expr.span(),
                            ..Default::default()
                        };
                    };
                    Type {
                        kind: TypeKind::Void,
                        span: expr.span(),
                        mut_token: None,
                        id: Some(ty),
                    }
                }
                _ => {
                    self.errors.push(Box::new(ErrorMemberAccess::new(
                        expr.span(),
                        #[cfg(feature = "debug")]
                        format!("{} {}:{}", file!(), line!(), column!()),
                    )));
                    Type {
                        span: expr.span(),
                        ..Default::default()
                    }
                }
            },
            _ => {
                self.errors.push(Box::new(ErrorMemberAccess::new(
                    expr.span(),
                    #[cfg(feature = "debug")]
                    format!("{} {}:{}", file!(), line!(), column!()),
                )));
                Type {
                    span: expr.span(),
                    ..Default::default()
                }
            }
        }
    }

    fn maybe_numeric_hint(&mut self, maybe: &Type) {
        match &maybe.kind {
            // Recurse to the innermost scalar so a nested array annotation like
            // `[2; [2; s32]]` hints `s32` (not `[2; s32]`). Hinting an array-typed
            // value to an integer literal would inflate its type by an array level.
            TypeKind::Array(_, elem_ty) | TypeKind::Slice(elem_ty) => {
                self.maybe_numeric_hint(elem_ty)
            }
            ty @ TypeKind::SignedNumber(bits) => {
                self.numeric_hint = Some(Type {
                    kind: ty.clone(),
                    span: maybe.span.clone(),
                    mut_token: None,
                    id: Some(self.interner.signed(*bits)),
                })
            }
            ty @ TypeKind::UnsignedNumber(bits) => {
                self.numeric_hint = Some(Type {
                    kind: ty.clone(),
                    span: maybe.span.clone(),
                    mut_token: None,
                    id: Some(self.interner.unsigned(*bits)),
                })
            }
            ty @ TypeKind::Float(bits) => {
                self.numeric_hint = Some(Type {
                    kind: ty.clone(),
                    span: maybe.span.clone(),
                    mut_token: None,
                    id: Some(self.interner.float(*bits)),
                })
            }
            ty @ TypeKind::SignedTargetPointerNumber => {
                self.numeric_hint = Some(Type {
                    kind: ty.clone(),
                    span: maybe.span.clone(),
                    mut_token: None,
                    id: Some(TypeId::SSIZE),
                })
            }
            ty @ TypeKind::UnsignedTargetPointerNumber => {
                self.numeric_hint = Some(Type {
                    kind: ty.clone(),
                    span: maybe.span.clone(),
                    mut_token: None,
                    id: Some(TypeId::USIZE),
                })
            }
            _ => {}
        }
    }

    fn lookup_struct_member(&mut self, struct_def: &StructType, member: &Token) -> Type {
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
        let Some(field) = members.iter().find(|f| f.name == member.lexeme) else {
            self.errors.push(Box::new(ErrorMemberAccess::new(
                member.span.clone(),
                #[cfg(feature = "debug")]
                format!("{} {}:{}", file!(), line!(), column!()),
            )));
            return Type {
                span: member.span.clone(),
                ..Default::default()
            };
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

// #[cfg(test)]
// mod tests {
//     use super::*;
//
//     #[test]
//     fn test_type_check() {
//         let src = r#"
//         fn add(a: s32, b: s32) s32 {
//             return a + b;
//         }
//         "#;
//
//         let tokens = crate::stage::lexer::lex("test_type_check", src);
//         let mut ast = crate::stage::parser::parse("test_type_check", tokens).unwrap();
//         let mut symbol_table =
//             crate::stage::semantic_analyzer::symbol_table::SymbolTableBuilder::default()
//                 .build(&mut ast)
//                 .unwrap();
//         let type_checker = TypeChecker::new(&mut symbol_table);
//         if let Err(errors) = type_checker.check(&mut ast) {
//             eprintln!("{}", errors.report(src));
//             assert!(false);
//         }
//         assert!(true);
//     }
// }
