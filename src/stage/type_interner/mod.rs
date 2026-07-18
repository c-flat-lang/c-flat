#![allow(unused)]

use crate::stage::parser::ast;
use std::collections::HashMap;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct TypeId(u32);

impl TypeId {
    pub const UNRESOLVED: TypeId = TypeId(u32::MAX);
    pub const U8: TypeId = TypeId(0);
    pub const S8: TypeId = TypeId(1);
    pub const U16: TypeId = TypeId(2);
    pub const S16: TypeId = TypeId(3);
    pub const U32: TypeId = TypeId(4);
    pub const S32: TypeId = TypeId(5);
    pub const U64: TypeId = TypeId(6);
    pub const S64: TypeId = TypeId(7);
    pub const USIZE: TypeId = TypeId(8);
    pub const ISIZE: TypeId = TypeId(9);
    pub const U128: TypeId = TypeId(10);
    pub const S128: TypeId = TypeId(11);
    pub const F32: TypeId = TypeId(12);
    pub const F64: TypeId = TypeId(13);

    pub const fn new(id: u32) -> Self {
        Self(id)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ArrayDef {
    length: u64,
    type_id: TypeId,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct EnumDef {
    name: String,
    /// Option of the assigned type over the defualt value
    /// Example:
    /// type Value enum {
    ///     foo = 10, // other wise it would have been 0
    ///     bar = 20, // This one would have been 1
    /// }
    members: Vec<(String, Option<String>)>,
    /// This is the number type the enum values represent
    /// type Value enum(s32) { .. }
    /// this would override the default usage of u32 to s32
    type_id: TypeId,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct FunctionDef {
    name: String,
    type_param: Option<Vec<(String, TypeId)>>,
    params: Vec<(String, TypeId)>,
    return_type_id: TypeId,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct StructDef {
    name: String,
    type_param: Option<Vec<(String, TypeId)>>,
    fields: Vec<(String, TypeId)>,
    packed: bool,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct StructTemplate {
    pub name: String,
    pub type_params: Vec<(String, ast::Type)>,
    pub fields: Vec<(String, ast::Type)>,
    pub packed: bool,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct FunctionTemplate {
    pub name: String,
    pub type_params: Vec<String>,
    pub params: Vec<(String, ast::Type)>,
    pub return_type: ast::Type,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum TypeDef {
    Array(ArrayDef),
    Bool,
    Enum(EnumDef),
    Float(u8),
    Function(FunctionDef),
    Pointer(TypeId),
    SignedNumber(u8),
    Slice(TypeId),
    Ssize,
    Struct(StructDef),
    Type,
    UnsignedNumber(u8),
    Usize,
    Void,
}

#[derive(Debug)]
pub struct TypeInterner {
    defs: Vec<TypeDef>,
    by_name: HashMap<String, TypeId>,
    by_structure: HashMap<TypeDef, TypeId>,
    struct_templates: HashMap<String, StructTemplate>,
    fn_templates: HashMap<String, FunctionTemplate>,
    monomorphized: HashMap<(String, Vec<TypeId>), TypeId>,
}
