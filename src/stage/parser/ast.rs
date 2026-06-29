#![allow(unused)]
use bitbox::{Target, text::semantic_analyzer::Symbol};
use std::fmt::Write;

use crate::stage::lexer::token::{Span, Token};

#[derive(Debug, Clone, Eq)]
pub struct Type {
    pub mut_token: Option<Token>,
    pub kind: TypeKind,
    pub span: Span,
}

impl Default for Type {
    fn default() -> Self {
        Self {
            mut_token: None,
            kind: TypeKind::default(),
            span: 0..1,
        }
    }
}

impl std::fmt::Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mutable = if self.mut_token.is_some() { "mut " } else { "" };
        write!(f, "{mutable}{}", self.kind)
    }
}

impl PartialEq<Type> for Type {
    fn eq(&self, other: &Type) -> bool {
        self.kind == other.kind
    }
}

impl Type {
    pub fn map_kind<F>(&self, f: F) -> Self
    where
        F: FnOnce(&TypeKind) -> TypeKind,
    {
        Self {
            mut_token: self.mut_token.clone(),
            kind: f(&self.kind),
            span: self.span.clone(),
        }
    }

    pub fn as_bitbox_type(&self, target: &Target) -> bitbox::ir::Type {
        self.kind.as_bitbox_type(target.target_pointer_size())
    }

    pub fn size(&self, target: &Target) -> usize {
        self.kind.size(target.target_pointer_size())
    }

    pub fn de_ref(&self) -> &Type {
        let mut ty = self;

        while let TypeKind::Pointer(inner) = &ty.kind {
            ty = inner;
        }

        ty
    }
}

#[derive(Debug, Default, Clone, PartialEq, Eq)]
pub enum TypeKind {
    Array(usize, Box<Type>),
    Bool,
    /// unimplemented!
    Enum(String),
    Float(u8),
    /// Simple Custom `Type` with no `TypeArgs`
    Name(Token),
    /// Any Custom `Type` that excepts `TypeArgs`
    NameWithParams(Token, TypeParams),
    Pointer(Box<Type>),
    SignedNumber(u8),
    /// isize
    SignedTargetPointerNumber,
    Slice(Box<Type>),
    Struct(StructType),
    Type,
    UnsignedNumber(u8),
    /// usize
    UnsignedTargetPointerNumber,
    #[default]
    Void,
}

impl TypeKind {
    fn as_bitbox_type(&self, target_pointer_size: u8) -> bitbox::ir::Type {
        match self {
            Self::Array(size, ty) => bitbox::ir::Type::Array(
                *size,
                Box::new(ty.kind.as_bitbox_type(target_pointer_size)),
            ),
            Self::Bool => bitbox::ir::Type::Unsigned(32),
            Self::Enum(name) => todo!("{name}"),
            Self::Float(bytes) => bitbox::ir::Type::Float(*bytes),
            Self::Name(name) => {
                unreachable!(
                    "Type::Name({}).as_bitbox_type() should be handled in type_resolver",
                    name.lexeme
                )
            }
            Self::NameWithParams(name, params) => {
                unreachable!(
                    "Type::Name({}, {params}).as_bitbox_type() should be handled in type_resolver",
                    name.lexeme
                )
            }
            Self::Pointer(inner) => {
                bitbox::ir::Type::Pointer(Box::new(inner.kind.as_bitbox_type(target_pointer_size)))
            }
            Self::SignedNumber(bytes) => bitbox::ir::Type::Signed(*bytes),
            Self::Slice(inner) => bitbox::ir::Type::Struct(bitbox::ir::StructType {
                name: format!("slice_{}", inner),
                packed: false,
                fields: vec![
                    (
                        "data".into(),
                        bitbox::ir::Type::Pointer(Box::new(
                            inner.kind.as_bitbox_type(target_pointer_size),
                        )),
                    ),
                    (
                        "len".into(),
                        bitbox::ir::Type::Unsigned(target_pointer_size),
                    ),
                ],
            }),
            Self::SignedTargetPointerNumber => bitbox::ir::Type::Signed(target_pointer_size),
            Self::Struct(struct_type) => bitbox::ir::Type::Struct(bitbox::ir::StructType {
                name: struct_type.name.clone(),
                fields: struct_type
                    .fields
                    .iter()
                    .map(|(name, ty)| (name.clone(), ty.kind.as_bitbox_type(target_pointer_size)))
                    .collect(),
                packed: struct_type.packed,
            }),
            Self::Type => unreachable!(
                // TODO: Replace/Update/Remove message once more understanding is reached.
                r#"NOTE:
I believe we will handle this before getting here.
I believe the idea is to replease Type with what ever the caller is using at comptime.
This means we may need to generate more then one X Type depending on how many Generic signitures are created.
                "#
            ),
            Self::UnsignedNumber(bytes) => bitbox::ir::Type::Unsigned(*bytes),
            Self::UnsignedTargetPointerNumber => bitbox::ir::Type::Unsigned(target_pointer_size),
            Self::Void => bitbox::ir::Type::Void,
        }
    }

    fn size(&self, target_pointer_size: u8) -> usize {
        match self {
            Self::Array(count, ty) => count * ty.kind.size(target_pointer_size),
            Self::Bool => 1,
            Self::Enum(_) => todo!("Size of enum"),
            Self::Name(name) => {
                unreachable!(
                    "Type::Name({}).size() should be handled in type_resolver",
                    name.lexeme
                )
            }
            Self::NameWithParams(name, params) => {
                unreachable!(
                    "Type::Name({}, {params}).size() should be handled in type_resolver",
                    name.lexeme
                )
            }
            Self::Pointer(_) => 64,
            Self::SignedTargetPointerNumber => unreachable!(
                "ssize or SignedTargetPointerNumber should be handled in type_resolver"
            ),
            Self::Slice(_) => {
                let ptr = target_pointer_size as usize / 8;
                ptr + ptr // data ptr + len
            }
            Self::Struct(struct_type) => {
                let mut size = 0;
                for (_, ty) in &struct_type.fields {
                    size += ty.kind.size(target_pointer_size);
                }
                size
            }
            Self::Type => unreachable!(
                // TODO: Replace/Update/Remove message once more understanding is reached.
                r#"NOTE:
I believe we will handle this before getting here.
I believe the idea is to replease Type with what ever the caller is using at comptime.
This means we may need to generate more then one X Type depending on how many Generic signitures are created.
                "#
            ),
            Self::UnsignedNumber(bytes) | Self::SignedNumber(bytes) | Self::Float(bytes) => {
                (*bytes as usize) / 8
            }
            Self::UnsignedTargetPointerNumber => unreachable!(
                "usize or UnsignedTargetPointerNumber should be handled in type_resolver"
            ),
            Self::Void => 0,
        }
    }
}

impl std::fmt::Display for TypeKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Array(size, ty) => write!(f, "[{}; {}]", size, ty),
            Self::Bool => write!(f, "bool"),
            Self::Float(n) => write!(f, "f{}", n),
            Self::Enum(name) => write!(f, "{}", name),
            Self::Name(name) => write!(f, "{}", name.lexeme),
            Self::NameWithParams(name, params) => write!(f, "{}({params})", name.lexeme),
            Self::Pointer(ty) => write!(f, "*{ty}"),
            Self::SignedNumber(n) => write!(f, "s{}", n),
            Self::SignedTargetPointerNumber => write!(f, "ssize"),
            Self::Slice(ty) => write!(f, "[{ty}]"),
            Self::Struct(symbol) => write!(f, "{}", symbol.name),
            Self::Type => write!(f, "type"),
            Self::UnsignedNumber(n) => write!(f, "u{}", n),
            Self::UnsignedTargetPointerNumber => write!(f, "usize"),
            Self::Void => write!(f, "void"),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TypeParams {
    pub params: Vec<Type>,
}

impl std::fmt::Display for TypeParams {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut string = String::new();
        for (idx, param) in self.params.iter().enumerate() {
            if idx == self.params.len().saturating_sub(1) {
                write!(&mut string, "{}", param);
                continue;
            }
            write!(&mut string, "{}, ", param);
        }

        write!(f, "{string}")
    }
}
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct StructType {
    pub name: String,
    pub type_params: Option<Vec<(Token, Type)>>,
    pub fields: Vec<(String, Type)>,
    pub packed: bool,
}

impl StructType {
    pub fn size(&self, target: &Target) -> usize {
        // TODO: alignment is needed
        // s32 -> 4 padding of 4
        // 264 -> 8
        // but
        // s32 -> 4
        // s32 -> 4
        // no padding is needed?
        // 264 -> 8
        // but
        // s32 -> 4
        // 264 -> 8
        // s32 -> 4
        // padding is needed cause of the order
        let mut size = 0;
        for (_, ty) in &self.fields {
            size += ty.size(target);
        }
        size
    }
}

impl std::fmt::Display for StructType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.name)
    }
}

#[derive(Debug, Default, Clone, Copy, PartialEq, Eq)]
pub enum Visibility {
    Public,
    #[default]
    Private,
}

#[derive(Debug)]
pub enum Item {
    Function(Function),
    Type(TypeDef),
    Use(Use),
    ExternFunction(ExternFunction),
}

#[derive(Debug)]
pub struct ExternFunction {
    pub visibility: Visibility,
    pub extern_token: Token,
    pub fn_token: Token,
    pub calling_convention: Token,
    pub binding_name: Token,
    pub local_name: Option<Token>,
    pub params: Vec<Type>,
    pub return_type: Type,
}

impl ExternFunction {
    pub fn span(&self) -> Span {
        let start = self.extern_token.span.start;
        let end = self
            .local_name
            .as_ref()
            .map(|n| n.span.end)
            .unwrap_or(self.binding_name.span.end);
        start..end
    }

    pub fn name(&self) -> &str {
        self.local_name
            .as_ref()
            .map(|n| n.lexeme.as_str())
            .unwrap_or(self.binding_name.lexeme.as_str())
    }
}

#[derive(Debug, Clone)]
pub struct Use {
    pub use_token: Token,
    pub path: Vec<Token>,
}

#[derive(Debug, Clone)]
pub struct Field {
    pub visibility: Visibility,
    pub name: Token,
    pub colon: Token,
    pub ty: Type,
    pub default_expr: Option<Box<Expr>>,
}

impl Field {
    pub fn span(&self) -> Span {
        let start = self.name.span.clone();
        let end = self
            .default_expr
            .as_ref()
            .map(|d| d.span().end)
            .unwrap_or(start.end);
        start.start..end
    }
}

#[derive(Debug, Clone)]
pub struct Struct {
    pub visibility: Visibility,
    pub type_token: Token,
    pub name: Token,
    pub type_params: Option<Vec<(Token, Type)>>,
    pub struct_token: Token,
    pub fields: Vec<Field>,
    pub open_brace: Token,
    pub close_brace: Token,
}

impl Struct {
    pub fn span(&self) -> Span {
        let start = self.type_token.span.start;
        let end = self.close_brace.span.end;
        start..end
    }
}

#[derive(Debug, Clone)]
pub enum TypeDef {
    Struct(Struct),
}

#[derive(Debug, Clone)]
pub struct Function {
    pub visibility: Visibility,
    pub fn_token: Token,
    pub name: Token,
    pub type_args: Option<Vec<(Token, Type)>>,
    pub params: Vec<Param>,
    pub return_type: Type,
    pub body: ExprBlock,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ExprBlock {
    pub open_brace: Token,
    pub statements: Vec<Statement>,
    pub close_brace: Token,
}

impl ExprBlock {
    pub fn span(&self) -> Span {
        let start = self.open_brace.span.start;
        let end = self.close_brace.span.end;
        start..end
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ExprAddressOf {
    pub ampersand: Token,
    pub expr: Box<Expr>,
}

impl ExprAddressOf {
    pub fn span(&self) -> Span {
        let start = self.ampersand.span.start;
        let end = self.expr.span().end;
        start..end
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ExprDeref {
    pub base: Box<Expr>,
    pub dot: Token,
    pub star: Token,
}

impl ExprDeref {
    pub fn span(&self) -> Span {
        let start = self.base.span().start;
        let end = self.star.span.end;
        start..end
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ExprNot {
    pub bang: Token,
    pub expr: Box<Expr>,
}

impl ExprNot {
    pub fn span(&self) -> Span {
        let start = self.bang.span.start;
        let end = self.expr.span().end;
        start..end
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Statement {
    pub expr: Box<Expr>,
    pub delem: Option<Token>,
}

impl Statement {
    pub fn span(&self) -> Span {
        let start = self.expr.span();
        let end = self.delem.as_ref().map(|d| d.span.end).unwrap_or(start.end);
        start.start..end
    }
}

#[derive(Debug, Clone)]
pub struct Param {
    pub name: Token,
    pub ty: Type,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Expr {
    Return(ExprReturn),
    Struct(ExprStruct),
    Declare(ExprDecl),
    Assignment(ExprAssignment),
    Litral(Litral),
    Call(ExprCall),
    Binary(ExprBinary),
    While(ExprWhile),
    Identifier(Token),
    Path(ExprPath),
    IfElse(ExprIfElse),
    MemberAccess(ExprMemberAccess),
    Array(ExprArray),
    ArrayIndex(ExprArrayIndex),
    ArrayRepeat(ExprArrayRepeat),
    Block(ExprBlock),
    AddressOf(ExprAddressOf),
    Deref(ExprDeref),
    Not(ExprNot),
    Grouping(ExprGrouping),
    TypeCast(ExprTypeCast),
}

impl Expr {
    pub fn span(&self) -> Span {
        match self {
            Self::Return(expr) => expr.span(),
            Self::Struct(expr) => expr.span(),
            Self::Declare(expr) => expr.span(),
            Self::Assignment(expr) => expr.span(),
            Self::Litral(litral) => litral.span(),
            Self::Call(expr) => expr.span(),
            Self::Binary(expr) => expr.span(),
            Self::While(expr) => expr.span(),
            Self::Identifier(token) => token.span.clone(),
            Self::Path(expr) => expr.span(),
            Self::IfElse(expr) => expr.span(),
            Self::MemberAccess(expr) => expr.span(),
            Self::Array(expr) => expr.span(),
            Self::ArrayIndex(expr) => expr.span(),
            Self::ArrayRepeat(expr) => expr.span(),
            Self::Block(expr) => expr.span(),
            Self::AddressOf(expr) => expr.span(),
            Self::Deref(expr) => expr.span(),
            Self::Not(expr) => expr.span(),
            Self::Grouping(expr) => expr.span(),
            Self::TypeCast(expr) => expr.span(),
        }
    }

    pub fn is_addressable(&self) -> bool {
        matches!(self, Expr::Identifier(..) | Expr::ArrayIndex(..))
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ExprTypeCast {
    pub as_token: Token,
    pub expr: Box<Expr>,
    pub target_type: Type,
}

impl ExprTypeCast {
    pub fn span(&self) -> Span {
        let start = self.as_token.span.start;
        let end = self.expr.span().end;
        start..end
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ExprGrouping {
    pub open_paren: Token,
    pub expr: Box<Expr>,
    pub close_paren: Token,
}

impl ExprGrouping {
    pub fn span(&self) -> Span {
        let start = self.open_paren.span.start;
        let end = self.close_paren.span.end;
        start..end
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ExprArray {
    pub open_bracket: Token,
    pub elements: Vec<Expr>,
    pub ty: Type,
    pub close_bracket: Token,
}

impl ExprArray {
    pub fn span(&self) -> Span {
        let start = self.open_bracket.span.start;
        let end = self.close_bracket.span.end;
        start..end
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ExprArrayIndex {
    pub expr: Box<Expr>,
    pub open_bracket: Token,
    pub index: Box<Expr>,
    pub close_bracket: Token,
    pub ty: Type,
}

impl ExprArrayIndex {
    pub fn span(&self) -> Span {
        let start = self.expr.span().start;
        let end = self.close_bracket.span.end;
        start..end
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ExprArrayRepeat {
    pub open_bracket: Token,
    pub count: Box<Expr>,
    pub semicolon: Token,
    pub value: Box<Expr>,
    pub close_bracket: Token,
    pub ty: Type,
}

impl ExprArrayRepeat {
    pub fn span(&self) -> Span {
        let start = self.open_bracket.span.start;
        let end = self.close_bracket.span.end;
        start..end
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ExprReturn {
    pub return_token: Token,
    pub expr: Option<Box<Expr>>,
}

impl ExprReturn {
    pub fn span(&self) -> Span {
        let start = self.return_token.span.start;
        let end = self.expr.as_ref().map(|e| e.span().end).unwrap_or(start);
        start..end
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct InitField {
    pub dot: Token,
    pub name: Token,
    pub equal: Token,
    pub expr: Box<Expr>,
}

impl InitField {
    pub fn span(&self) -> Span {
        let start = self.dot.span.start;
        let end = self.expr.span().end;
        start..end
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ExprStruct {
    pub name: Token,
    pub type_args: Option<Vec<Type>>,
    pub open_brace: Token,
    pub init_fields: Vec<InitField>,
    pub close_brace: Token,
}

impl ExprStruct {
    pub fn span(&self) -> Span {
        let start = self.name.span.start;
        let end = self.close_brace.span.end;
        start..end
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ExprDecl {
    pub let_token: Token,
    pub mutable: bool,
    pub ty: Option<Type>,
    pub ident: Token,
    pub expr: Box<Expr>,
}

impl ExprDecl {
    pub fn span(&self) -> Span {
        let start = self.let_token.span.start;
        let end = self.expr.span().end;
        start..end
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ExprAssignment {
    pub left: Box<Expr>,
    pub equal: Token,
    pub right: Box<Expr>,
}

impl ExprAssignment {
    pub fn span(&self) -> Span {
        let start = self.left.span();
        let end = self.right.span();
        start.start..end.end
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ExprCall {
    pub caller: Box<Expr>,
    pub type_args: Option<Vec<Type>>,
    pub left_paren: Token,
    pub args: Vec<Expr>,
    pub right_paren: Token,
}

impl ExprCall {
    pub fn span(&self) -> Span {
        let start = self.caller.span();
        let end = self.right_paren.span.end;
        start.start..end
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ExprBinary {
    pub left: Box<Expr>,
    pub op: Token,
    pub right: Box<Expr>,
}

impl ExprBinary {
    pub fn span(&self) -> Span {
        let start = self.left.span();
        let end = self.right.span();
        start.start..end.end
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ExprWhile {
    pub while_token: Token,
    pub condition: Box<Expr>,
    pub body: ExprBlock,
}

impl ExprWhile {
    pub fn span(&self) -> Span {
        let start = self.while_token.span.start;
        let end = self.body.close_brace.span.end;
        start..end
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ExprIfElse {
    pub if_token: Token,
    pub condition: Box<Expr>,
    pub then_branch: ExprBlock,
    pub else_token: Option<Token>,
    pub else_branch: Option<Box<Expr>>,
    pub ty: Type,
}

impl ExprIfElse {
    pub fn span(&self) -> Span {
        let start = self.if_token.span.start;
        let end = self
            .else_branch
            .as_ref()
            .map(|b| b.span().end)
            .unwrap_or(self.then_branch.close_brace.span.end);
        start..end
    }
}

/// A `::` qualified path used in expression position, `math::add` or
/// `foo::bar::baz`. The segments preserve their source tokens for spans and
/// error reporting.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ExprPath {
    pub segments: Vec<Token>,
}

impl ExprPath {
    pub fn span(&self) -> Span {
        let start = self.segments.first().map(|t| t.span.start).unwrap_or(0);
        let end = self.segments.last().map(|t| t.span.end).unwrap_or(start);
        start..end
    }

    /// The final segment, which names the item being referred to. v1 resolves a
    /// path to this segment within the flat program namespace.
    pub fn leaf(&self) -> &Token {
        self.segments
            .last()
            .expect("ExprPath must have at least one segment")
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ExprMemberAccess {
    pub base: Box<Expr>,
    pub dot: Token,
    pub member: Token,
}

impl ExprMemberAccess {
    pub fn span(&self) -> Span {
        let start = self.base.span().start;
        let end = self.member.span.end;
        start..end
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Litral {
    String(Token),
    Integer(Token),
    Float(Token),
    Char(Token),
    BoolTrue(Token),
    BoolFalse(Token),
}

impl Litral {
    pub fn span(&self) -> Span {
        match self {
            Litral::String(token) => token.span.clone(),
            Litral::Integer(token) => token.span.clone(),
            Litral::Float(token) => token.span.clone(),
            Litral::Char(token) => token.span.clone(),
            Litral::BoolTrue(token) => token.span.clone(),
            Litral::BoolFalse(token) => token.span.clone(),
        }
    }
}
