#![allow(unused)]
use bitbox::text::semantic_analyzer::Symbol;

use crate::stage::lexer::token::{Span, Token};

#[derive(Debug, Default, Clone, PartialEq, Eq)]
pub enum Type {
    Bool,
    UnsignedNumber(u8),
    SignedNumber(u8),
    Float(u8),
    Array(usize, Box<Self>),
    Pointer(Box<Self>),
    Struct(StructType),
    Enum(String),
    Name(String),
    #[default]
    Void,
}

impl Type {
    pub fn as_bitbox_type(&self) -> bitbox::ir::Type {
        match self {
            Self::Bool => bitbox::ir::Type::Unsigned(32),
            Self::UnsignedNumber(bytes) => bitbox::ir::Type::Unsigned(*bytes),
            Self::SignedNumber(bytes) => bitbox::ir::Type::Signed(*bytes),
            Self::Float(bytes) => bitbox::ir::Type::Float(*bytes),
            Self::Array(size, ty) => {
                bitbox::ir::Type::Array(*size, Box::new(ty.clone().as_bitbox_type()))
            }
            Self::Pointer(inner) => {
                bitbox::ir::Type::Pointer(Box::new(inner.clone().as_bitbox_type()))
            }
            Self::Struct(struct_type) => bitbox::ir::Type::Struct(bitbox::ir::StructType {
                name: struct_type.name.clone(),
                fields: struct_type
                    .fields
                    .iter()
                    .map(|(name, ty)| (name.clone(), ty.as_bitbox_type()))
                    .collect(),
                packed: struct_type.packed,
            }),
            Self::Enum(name) => todo!("{name}"),
            Self::Name(_) => unreachable!("Bro you screwed up"),
            Self::Void => bitbox::ir::Type::Void,
        }
    }

    pub(crate) fn size(&self) -> usize {
        match self {
            Type::Bool => 1,
            Type::UnsignedNumber(bytes) | Type::SignedNumber(bytes) | Type::Float(bytes) => {
                (*bytes as usize) / 8
            }
            Type::Array(count, ty) => count * ty.size(),
            Type::Pointer(_) => 64,
            Type::Struct(struct_type) => {
                let mut size = 0;
                for (_, ty) in &struct_type.fields {
                    size += ty.size();
                }
                size
            }
            Type::Enum(_) => todo!("Size of enum"),
            Type::Name(..) => unreachable!("Bro you screwed up"),
            Type::Void => 0,
        }
    }
}

impl std::fmt::Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Type::Bool => write!(f, "bool"),
            Type::UnsignedNumber(n) => write!(f, "u{}", n),
            Type::SignedNumber(n) => write!(f, "s{}", n),
            Type::Float(n) => write!(f, "f{}", n),
            Type::Array(size, ty) => write!(f, "[{}; {}]", size, ty),
            Type::Pointer(ty) => write!(f, "*{}", ty),
            Type::Struct(symbol) => write!(f, "{}", symbol.name),
            Type::Enum(name) => write!(f, "{}", name),
            Type::Name(name) => write!(f, "{}", name),
            Type::Void => write!(f, "void"),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct StructType {
    pub name: String,
    pub fields: Vec<(String, Type)>,
    pub packed: bool,
}

impl StructType {
    pub fn size(&self) -> usize {
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
            size += ty.size();
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

#[derive(Debug)]
pub struct Function {
    pub visibility: Visibility,
    pub fn_token: Token,
    pub name: Token,
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

#[derive(Debug)]
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
    IfElse(ExprIfElse),
    MemberAccess(ExprMemberAccess),
    Array(ExprArray),
    ArrayIndex(ExprArrayIndex),
    ArrayRepeat(ExprArrayRepeat),
    Block(ExprBlock),
    AddressOf(ExprAddressOf),
    Not(ExprNot),
}

impl Expr {
    pub fn span(&self) -> Span {
        match self {
            Self::Return(expr_return) => expr_return.span(),
            Self::Struct(expr_struct) => expr_struct.span(),
            Self::Declare(expr_decl) => expr_decl.span(),
            Self::Assignment(expr_assignment) => expr_assignment.span(),
            Self::Litral(litral) => litral.span(),
            Self::Call(expr_call) => expr_call.span(),
            Self::Binary(expr_binary) => expr_binary.span(),
            Self::While(expr_while) => expr_while.span(),
            Self::Identifier(token) => token.span.clone(),
            Self::IfElse(expr_if_else) => expr_if_else.span(),
            Self::MemberAccess(expr) => expr.span(),
            Self::Array(expr) => expr.span(),
            Self::ArrayIndex(expr) => expr.span(),
            Self::ArrayRepeat(expr) => expr.span(),
            Self::Block(block) => block.span(),
            Self::AddressOf(expr) => expr.span(),
            Self::Not(expr) => expr.span(),
        }
    }

    pub fn is_addressable(&self) -> bool {
        matches!(self, Expr::Identifier(..) | Expr::ArrayIndex(..))
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
