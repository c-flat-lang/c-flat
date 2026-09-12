#![allow(unused)]

use crate::stage::lexer::token::Span;
use crate::stage::parser::ast;
use crate::stage::{Stage, StageContext, StageOutput};
use std::cell::RefCell;
use std::collections::HashMap;
use std::collections::HashSet;
use std::fmt::Write;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct TypeId(u32);

impl TypeId {
    pub const UNRESOLVED: TypeId = TypeId(u32::MAX);
    pub const VOID: TypeId = TypeId(0);
    pub const BOOL: TypeId = TypeId(1);
    pub const U8: TypeId = TypeId(2);
    pub const S8: TypeId = TypeId(3);
    pub const U16: TypeId = TypeId(4);
    pub const S16: TypeId = TypeId(5);
    pub const U32: TypeId = TypeId(6);
    pub const S32: TypeId = TypeId(7);
    pub const U64: TypeId = TypeId(8);
    pub const S64: TypeId = TypeId(9);
    pub const USIZE: TypeId = TypeId(10);
    pub const SSIZE: TypeId = TypeId(11);
    pub const U128: TypeId = TypeId(12);
    pub const S128: TypeId = TypeId(13);
    pub const F32: TypeId = TypeId(14);
    pub const F64: TypeId = TypeId(15);

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

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct StructField<Type> {
    pub name: String,
    pub ty: Type,
    pub default: Option<Box<ast::Expr>>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct StructDef {
    name: String,
    type_param: Option<Vec<(String, TypeId)>>,
    fields: Vec<StructField<TypeId>>,
    packed: bool,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct StructTemplate {
    pub name: String,
    pub type_params: Vec<(String, ast::Type)>,
    pub fields: Vec<StructField<ast::Type>>,
    pub packed: bool,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FunctionTemplate {
    pub name: String,
    pub type_params: Vec<String>,
    pub params: Vec<(String, ast::Type)>,
    pub body: ast::ExprBlock,
    pub return_type: ast::Type,
}

#[derive(Debug, Clone, PartialEq, Eq)]
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

fn create_default_type_def_list() -> Vec<TypeDef> {
    vec![
        TypeDef::Void,                // TypeId::VOID,
        TypeDef::Bool,                // TypeId::BOOL,
        TypeDef::UnsignedNumber(8),   // TypeId::U8,
        TypeDef::SignedNumber(8),     // TypeId::S8,
        TypeDef::UnsignedNumber(16),  // TypeId::U16,
        TypeDef::SignedNumber(16),    // TypeId::S16,
        TypeDef::UnsignedNumber(32),  // TypeId::U32,
        TypeDef::SignedNumber(32),    // TypeId::S32,
        TypeDef::UnsignedNumber(64),  // TypeId::U64,
        TypeDef::SignedNumber(64),    // TypeId::S64,
        TypeDef::Usize,               // TypeId::USIZE,
        TypeDef::Ssize,               // TypeId::SSIZE,
        TypeDef::UnsignedNumber(128), // TypeId::U128,
        TypeDef::SignedNumber(128),   // TypeId::S128,
        TypeDef::Float(32),           // TypeId::F32,
        TypeDef::Float(64),           // TypeId::F64,
    ]
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
enum StructuralKey {
    Pointer(TypeId),
    Slice(TypeId),
    Array(u64, TypeId),
    Unsigned(u8),
    Signed(u8),
    Float(u8),
}

#[derive(Debug)]
pub struct TypeInterner {
    defs: Vec<TypeDef>,
    by_name: HashMap<String, TypeId>,
    by_structure: HashMap<StructuralKey, TypeId>,
    decl_span: HashMap<TypeId, Span>,
    unfilled: HashSet<TypeId>,
    struct_templates: HashMap<String, StructTemplate>,
    function_templates: HashMap<String, FunctionTemplate>,
    monomorphized: HashMap<(String, Vec<TypeId>), TypeId>,
    bitbox_cache: RefCell<HashMap<TypeId, bitbox::ir::Type>>,
    size_cache: RefCell<HashMap<TypeId, usize>>,
    in_progress: RefCell<HashSet<TypeId>>,
}

impl TypeInterner {
    /// Registers a generic struct's template. Unlike `declare_struct`,
    /// this doesn't reserve a `TypeId` in `defs` — templates aren't
    /// concrete types until `instantiate_struct` mangles a name and
    /// calls `declare_struct`/`fill_struct` for each instantiation.
    pub fn register_struct_template(&mut self, struct_def: &ast::Struct) -> Result<(), TypeId> {
        let name = struct_def.name.lexeme.clone();

        if self.by_name.contains_key(&name) || self.struct_templates.contains_key(&name) {
            // No TypeId exists for a template-name collision against a
            // concrete type, so surface it via existing decl_span lookup
            // where possible; otherwise there's nothing to return but the
            // fact of collision. Using by_name's id when present.
            if let Some(&existing) = self.by_name.get(&name) {
                return Err(existing);
            }
        }

        let type_params = struct_def
            .type_params
            .as_ref()
            .expect("register_struct_template called on a non-generic struct")
            .iter()
            .map(|tp| (tp.0.lexeme.clone(), tp.1.clone()))
            .collect();

        let fields = struct_def
            .fields
            .iter()
            .map(|f| StructField {
                name: f.name.lexeme.clone(),
                ty: f.ty.clone(),
                default: f.default_expr.clone(),
            })
            .collect();

        self.struct_templates.insert(
            name.clone(),
            StructTemplate {
                name,
                type_params,
                fields,
                packed: false,
            },
        );

        Ok(())
    }

    /// Reserves a `TypeId` for a not-yet-defined struct, hooking up its
    /// declaration span and marking it `unfilled` until `fill_struct` is
    /// called. Returns `Err(existing_id)` on a duplicate name so the caller
    /// can build `ErrorDuplicateType` with both spans.
    pub fn declare_struct(&mut self, name: &str, span: Span) -> Result<TypeId, TypeId> {
        if let Some(&existing) = self.by_name.get(name) {
            return Err(existing);
        }

        let id = TypeId::new(self.defs.len() as u32);
        self.defs.push(TypeDef::Struct(StructDef {
            name: name.to_string(),
            type_param: None, // generics live in struct_templates, not here
            fields: Vec::new(),
            packed: false,
        }));

        self.by_name.insert(name.to_string(), id);
        self.decl_span.insert(id, span);
        self.unfilled.insert(id);

        Ok(id)
    }

    /// Same contract as `declare_struct`, for enums. The enum's underlying
    /// representation type isn't known until `fill_enum`, so it's stubbed
    /// with `TypeId::UNRESOLVED` here.
    pub fn declare_enum(&mut self, name: &str, span: Span) -> Result<TypeId, TypeId> {
        if let Some(&existing) = self.by_name.get(name) {
            return Err(existing);
        }

        let id = TypeId::new(self.defs.len() as u32);
        self.defs.push(TypeDef::Enum(EnumDef {
            name: name.to_string(),
            members: Vec::new(),
            type_id: TypeId::UNRESOLVED,
        }));

        self.by_name.insert(name.to_string(), id);
        self.decl_span.insert(id, span);
        self.unfilled.insert(id);

        Ok(id)
    }

    /// Fills in a previously-declared struct's fields and clears it from
    /// `unfilled`. Panics if `id` wasn't produced by `declare_struct` —
    /// that would mean a stage bug, not a user error.
    pub fn fill_struct(&mut self, id: TypeId, fields: Vec<StructField<TypeId>>) {
        match self.defs.get_mut(id.0 as usize) {
            Some(TypeDef::Struct(def)) => def.fields = fields,
            _ => unreachable!("fill_struct called on a non-struct TypeId"),
        }
        self.unfilled.remove(&id);
    }

    /// Fills in a previously-declared enum's members and representation
    /// type, and clears it from `unfilled`.
    pub fn fill_enum(&mut self, id: TypeId, members: Vec<(String, Option<String>)>, repr: TypeId) {
        match self.defs.get_mut(id.0 as usize) {
            Some(TypeDef::Enum(def)) => {
                def.members = members;
                def.type_id = repr;
            }
            _ => unreachable!("fill_enum called on a non-enum TypeId"),
        }
        self.unfilled.remove(&id);
    }

    /// Ids declared but not yet filled — `ResolveTypesStage` asserts this
    /// is empty once it's done with definitions and signatures.
    pub fn unfilled(&self) -> impl Iterator<Item = TypeId> + '_ {
        self.unfilled.iter().copied()
    }
}

impl Default for TypeInterner {
    fn default() -> Self {
        Self {
            defs: create_default_type_def_list(),
            by_name: HashMap::default(),
            by_structure: HashMap::default(),
            decl_span: HashMap::default(),
            unfilled: HashSet::default(),
            struct_templates: HashMap::default(),
            function_templates: HashMap::default(),
            monomorphized: HashMap::default(),
            bitbox_cache: RefCell::default(),
            size_cache: RefCell::default(),
            in_progress: RefCell::default(),
        }
    }
}
