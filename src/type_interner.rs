use crate::stage::lexer::token::{Keyword as Kw, Span, TokenKind};
use crate::stage::parser::ast;
use std::cell::RefCell;
use std::collections::HashMap;
use std::collections::HashSet;

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
    pub const TYPE: TypeId = TypeId(16);

    pub const fn new(id: u32) -> Self {
        Self(id)
    }

    pub const fn index(self) -> usize {
        self.0 as usize
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
    pub return_type: ast::Type,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TypeInfo {
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

fn create_default_type_info_list() -> Vec<TypeInfo> {
    vec![
        TypeInfo::Void,                // TypeId::VOID,
        TypeInfo::Bool,                // TypeId::BOOL,
        TypeInfo::UnsignedNumber(8),   // TypeId::U8,
        TypeInfo::SignedNumber(8),     // TypeId::S8,
        TypeInfo::UnsignedNumber(16),  // TypeId::U16,
        TypeInfo::SignedNumber(16),    // TypeId::S16,
        TypeInfo::UnsignedNumber(32),  // TypeId::U32,
        TypeInfo::SignedNumber(32),    // TypeId::S32,
        TypeInfo::UnsignedNumber(64),  // TypeId::U64,
        TypeInfo::SignedNumber(64),    // TypeId::S64,
        TypeInfo::Usize,               // TypeId::USIZE,
        TypeInfo::Ssize,               // TypeId::SSIZE,
        TypeInfo::UnsignedNumber(128), // TypeId::U128,
        TypeInfo::SignedNumber(128),   // TypeId::S128,
        TypeInfo::Float(32),           // TypeId::F32,
        TypeInfo::Float(64),           // TypeId::F64,
        TypeInfo::Type,                // TypeId::TYPE,
    ]
}

pub const GENERIC_SEPARATOR: &str = "__";

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum DeclareError {
    ExistingType(TypeId),
    ExistingTemplate,
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

/// TODO: Remove Clone once we can pass the interner to the `TypeChecker` as a borrower value.
/// Check if `SymbolTable` should be passed as mutable to `TypeChecker`.
#[derive(Debug, Clone)]
pub struct TypeInterner {
    target: bitbox::Target,
    defs: Vec<TypeInfo>,
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
    pub fn new(target: bitbox::Target) -> Self {
        Self {
            target,
            ..Default::default()
        }
    }

    pub fn target(&self) -> bitbox::Target {
        self.target
    }

    pub fn register_struct_template(
        &mut self,
        struct_def: &ast::Struct,
    ) -> Result<(), DeclareError> {
        let name = struct_def.name.lexeme.clone();

        if let Some(&existing) = self.by_name.get(&name) {
            return Err(DeclareError::ExistingType(existing));
        }
        if self.struct_templates.contains_key(&name) {
            return Err(DeclareError::ExistingTemplate);
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
        self.defs.push(TypeInfo::Struct(StructDef {
            name: name.to_string(),
            type_param: None,
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
        self.defs.push(TypeInfo::Enum(EnumDef {
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
            Some(TypeInfo::Struct(def)) => def.fields = fields,
            _ => unreachable!("fill_struct called on a non-struct TypeId"),
        }
        self.unfilled.remove(&id);
    }

    /// Fills in a previously-declared enum's members and representation
    /// type, and clears it from `unfilled`.
    pub fn fill_enum(&mut self, id: TypeId, members: Vec<(String, Option<String>)>, repr: TypeId) {
        match self.defs.get_mut(id.0 as usize) {
            Some(TypeInfo::Enum(def)) => {
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

    pub fn def(&self, id: TypeId) -> &TypeInfo {
        &self.defs[id.index()]
    }

    pub fn lookup_name(&self, name: &str) -> Option<TypeId> {
        self.by_name.get(name).copied()
    }

    pub fn decl_span(&self, id: TypeId) -> Option<&Span> {
        self.decl_span.get(&id)
    }

    pub fn nominal_ids(&self) -> Vec<TypeId> {
        self.defs
            .iter()
            .enumerate()
            .filter(|(_, info)| matches!(info, TypeInfo::Struct(_) | TypeInfo::Enum(_)))
            .map(|(index, _)| TypeId::new(index as u32))
            .collect()
    }

    fn intern_structural(&mut self, key: StructuralKey, info: TypeInfo) -> TypeId {
        if let Some(&id) = self.by_structure.get(&key) {
            return id;
        }
        let id = TypeId::new(self.defs.len() as u32);
        self.defs.push(info);
        self.by_structure.insert(key, id);
        id
    }

    pub fn pointer_to(&mut self, inner: TypeId) -> TypeId {
        self.intern_structural(StructuralKey::Pointer(inner), TypeInfo::Pointer(inner))
    }

    pub fn slice_of(&mut self, inner: TypeId) -> TypeId {
        self.intern_structural(StructuralKey::Slice(inner), TypeInfo::Slice(inner))
    }

    pub fn array_of(&mut self, length: u64, inner: TypeId) -> TypeId {
        self.intern_structural(
            StructuralKey::Array(length, inner),
            TypeInfo::Array(ArrayDef {
                length,
                type_id: inner,
            }),
        )
    }

    pub fn unsigned(&mut self, bits: u8) -> TypeId {
        match bits {
            8 => TypeId::U8,
            16 => TypeId::U16,
            32 => TypeId::U32,
            64 => TypeId::U64,
            128 => TypeId::U128,
            _ => self.intern_structural(
                StructuralKey::Unsigned(bits),
                TypeInfo::UnsignedNumber(bits),
            ),
        }
    }

    pub fn signed(&mut self, bits: u8) -> TypeId {
        match bits {
            8 => TypeId::S8,
            16 => TypeId::S16,
            32 => TypeId::S32,
            64 => TypeId::S64,
            128 => TypeId::S128,
            _ => self.intern_structural(StructuralKey::Signed(bits), TypeInfo::SignedNumber(bits)),
        }
    }

    pub fn float(&mut self, bits: u8) -> TypeId {
        match bits {
            32 => TypeId::F32,
            64 => TypeId::F64,
            _ => self.intern_structural(StructuralKey::Float(bits), TypeInfo::Float(bits)),
        }
    }

    pub fn name_of(&self, id: TypeId) -> String {
        match self.def(id) {
            TypeInfo::Array(def) => {
                format!("[{}; {}]", def.length, self.name_of(def.type_id))
            }
            TypeInfo::Bool => "bool".to_string(),
            TypeInfo::Enum(def) => def.name.clone(),
            TypeInfo::Float(bits) => format!("f{bits}"),
            TypeInfo::Function(def) => def.name.clone(),
            TypeInfo::Pointer(inner) => format!("*{}", self.name_of(*inner)),
            TypeInfo::SignedNumber(bits) => format!("s{bits}"),
            TypeInfo::Slice(inner) => format!("[{}]", self.name_of(*inner)),
            TypeInfo::Ssize => "ssize".to_string(),
            TypeInfo::Struct(def) => def.name.clone(),
            TypeInfo::Type => "type".to_string(),
            TypeInfo::UnsignedNumber(bits) => format!("u{bits}"),
            TypeInfo::Usize => "usize".to_string(),
            TypeInfo::Void => "void".to_string(),
        }
    }

    pub fn mangled_name_of(&self, id: TypeId) -> String {
        match self.def(id) {
            TypeInfo::Array(def) => {
                format!("arr{}_{}", def.length, self.mangled_name_of(def.type_id))
            }
            TypeInfo::Bool => "bool".to_string(),
            TypeInfo::Enum(def) => def.name.clone(),
            TypeInfo::Float(bits) => format!("f{bits}"),
            TypeInfo::Function(def) => def.name.clone(),
            TypeInfo::Pointer(inner) => format!("ptr_{}", self.mangled_name_of(*inner)),
            TypeInfo::SignedNumber(bits) => format!("s{bits}"),
            TypeInfo::Slice(inner) => format!("slice_{}", self.mangled_name_of(*inner)),
            TypeInfo::Ssize => "ssize".to_string(),
            TypeInfo::Struct(def) => def.name.clone(),
            TypeInfo::Type => "type".to_string(),
            TypeInfo::UnsignedNumber(bits) => format!("u{bits}"),
            TypeInfo::Usize => "usize".to_string(),
            TypeInfo::Void => "void".to_string(),
        }
    }

    pub fn mangle(&self, base: &str, args: &[TypeId]) -> String {
        let mut out = base.to_string();
        for arg in args {
            out.push_str(GENERIC_SEPARATOR);
            out.push_str(&self.mangled_name_of(*arg));
        }
        out
    }

    pub fn as_bitbox_type(&self, id: TypeId) -> bitbox::ir::Type {
        if let Some(cached) = self.bitbox_cache.borrow().get(&id) {
            return cached.clone();
        }
        if !self.in_progress.borrow_mut().insert(id) {
            return bitbox::ir::Type::Void;
        }
        let lowered = self.compute_bitbox_type(id);
        self.in_progress.borrow_mut().remove(&id);
        self.bitbox_cache.borrow_mut().insert(id, lowered.clone());
        lowered
    }

    fn compute_bitbox_type(&self, id: TypeId) -> bitbox::ir::Type {
        let ptr = self.target.target_pointer_size();
        match self.def(id) {
            TypeInfo::Array(def) => bitbox::ir::Type::Array(
                def.length as usize,
                Box::new(self.as_bitbox_type(def.type_id)),
            ),
            TypeInfo::Bool => bitbox::ir::Type::Unsigned(32),
            TypeInfo::Enum(_) => bitbox::ir::Type::Unsigned(32),
            TypeInfo::Float(bits) => bitbox::ir::Type::Float(*bits),
            TypeInfo::Function(_) => unreachable!("function types do not lower to bitbox types"),
            TypeInfo::Pointer(inner) => {
                bitbox::ir::Type::Pointer(Box::new(self.as_bitbox_type(*inner)))
            }
            TypeInfo::SignedNumber(bits) => bitbox::ir::Type::Signed(*bits),
            TypeInfo::Slice(inner) => bitbox::ir::Type::Struct(bitbox::ir::StructType {
                name: format!("slice_{}", self.name_of(*inner)),
                packed: false,
                fields: vec![
                    (
                        "data".into(),
                        bitbox::ir::Type::Pointer(Box::new(self.as_bitbox_type(*inner))),
                    ),
                    ("len".into(), bitbox::ir::Type::Unsigned(ptr)),
                ],
            }),
            TypeInfo::Ssize => bitbox::ir::Type::Signed(ptr),
            TypeInfo::Struct(def) => bitbox::ir::Type::Struct(bitbox::ir::StructType {
                name: def.name.clone(),
                fields: def
                    .fields
                    .iter()
                    .map(|field| (field.name.clone(), self.as_bitbox_type(field.ty)))
                    .collect(),
                packed: def.packed,
            }),
            TypeInfo::Type => unreachable!("`type` should be resolved away before lowering"),
            TypeInfo::UnsignedNumber(bits) => bitbox::ir::Type::Unsigned(*bits),
            TypeInfo::Usize => bitbox::ir::Type::Unsigned(ptr),
            TypeInfo::Void => bitbox::ir::Type::Void,
        }
    }

    pub fn size_of(&self, id: TypeId) -> usize {
        if let Some(&cached) = self.size_cache.borrow().get(&id) {
            return cached;
        }
        if !self.in_progress.borrow_mut().insert(id) {
            return 0;
        }
        let size = self.compute_size_of(id);
        self.in_progress.borrow_mut().remove(&id);
        self.size_cache.borrow_mut().insert(id, size);
        size
    }

    pub fn register_function_template(&mut self, template: FunctionTemplate) {
        self.function_templates
            .insert(template.name.clone(), template);
    }

    pub fn function_template(&self, name: &str) -> Option<&FunctionTemplate> {
        self.function_templates.get(name)
    }

    pub fn struct_template(&self, name: &str) -> Option<&StructTemplate> {
        self.struct_templates.get(name)
    }

    pub fn template_arity(&self, name: &str) -> Option<usize> {
        self.struct_templates
            .get(name)
            .map(|template| template.type_params.len())
    }

    pub fn instantiated(&self, base: &str, args: &[TypeId]) -> Option<TypeId> {
        self.monomorphized
            .get(&(base.to_string(), args.to_vec()))
            .copied()
    }

    pub fn record_instantiation(&mut self, base: String, args: Vec<TypeId>, id: TypeId) {
        self.monomorphized.insert((base, args), id);
    }

    pub fn de_ref(&self, id: TypeId) -> TypeId {
        let mut current = id;
        while let TypeInfo::Pointer(inner) = self.def(current) {
            current = *inner;
        }
        current
    }

    pub fn pointee(&self, id: TypeId) -> Option<TypeId> {
        match self.def(id) {
            TypeInfo::Pointer(inner) => Some(*inner),
            _ => None,
        }
    }

    pub fn element_of(&self, id: TypeId) -> Option<TypeId> {
        match self.def(id) {
            TypeInfo::Array(def) => Some(def.type_id),
            TypeInfo::Slice(inner) => Some(*inner),
            _ => None,
        }
    }

    pub fn array_len(&self, id: TypeId) -> Option<u64> {
        match self.def(id) {
            TypeInfo::Array(def) => Some(def.length),
            _ => None,
        }
    }

    pub fn is_indexable(&self, id: TypeId) -> bool {
        matches!(
            self.def(id),
            TypeInfo::Array(_) | TypeInfo::Slice(_) | TypeInfo::Pointer(_)
        )
    }

    pub fn is_slice(&self, id: TypeId) -> bool {
        matches!(self.def(id), TypeInfo::Slice(_))
    }

    pub fn unsigned_bits(&self, id: TypeId) -> Option<u8> {
        match self.def(id) {
            TypeInfo::UnsignedNumber(bits) => Some(*bits),
            TypeInfo::Enum(def) if def.type_id != TypeId::UNRESOLVED => {
                match self.def(def.type_id) {
                    TypeInfo::UnsignedNumber(bits) => Some(*bits),
                    _ => None,
                }
            }
            _ => None,
        }
    }

    pub fn struct_fields(&self, id: TypeId) -> Option<&[StructField<TypeId>]> {
        match self.def(id) {
            TypeInfo::Struct(def) => Some(&def.fields),
            _ => None,
        }
    }

    pub fn field(&self, id: TypeId, name: &str) -> Option<(usize, &StructField<TypeId>)> {
        self.struct_fields(id)?
            .iter()
            .enumerate()
            .find(|(_, field)| field.name == name)
    }

    pub fn field_through_ptr(
        &self,
        id: TypeId,
        name: &str,
    ) -> Option<(usize, &StructField<TypeId>)> {
        self.field(self.de_ref(id), name)
    }

    pub fn enum_repr(&self, id: TypeId) -> Option<TypeId> {
        match self.def(id) {
            TypeInfo::Enum(def) if def.type_id != TypeId::UNRESOLVED => Some(def.type_id),
            _ => None,
        }
    }

    pub fn enum_variant_value(&self, id: TypeId, variant: &str) -> Option<String> {
        let TypeInfo::Enum(def) = self.def(id) else {
            return None;
        };
        def.members
            .iter()
            .enumerate()
            .find(|(_, (name, _))| name == variant)
            .map(|(index, (_, value))| value.clone().unwrap_or_else(|| index.to_string()))
    }

    pub fn same_type(&self, lhs: TypeId, rhs: TypeId) -> bool {
        lhs == rhs
    }

    pub fn compatible(&self, lhs: TypeId, rhs: TypeId) -> bool {
        if lhs == rhs {
            return true;
        }
        match (self.unsigned_bits(lhs), self.unsigned_bits(rhs)) {
            (Some(a), Some(b)) => a == b,
            _ => false,
        }
    }

    pub fn numeric_hint(&self, id: TypeId) -> Option<TypeId> {
        let mut current = id;
        loop {
            match self.def(current) {
                TypeInfo::Array(def) => current = def.type_id,
                TypeInfo::Slice(inner) => current = *inner,
                TypeInfo::SignedNumber(_)
                | TypeInfo::UnsignedNumber(_)
                | TypeInfo::Float(_)
                | TypeInfo::Ssize
                | TypeInfo::Usize => return Some(current),
                _ => return None,
            }
        }
    }

    pub fn binary_op_result(&self, lhs: TypeId, op: &TokenKind, rhs: TypeId) -> Option<TypeId> {
        use TokenKind::*;

        let arithmetic = matches!(
            op,
            Plus | Minus | Star | Slash | Percent | BitShiftRight | Ampersand
        );
        let comparison = matches!(op, EqualEqual | Greater | GreaterEqual | Less | LessEqual);

        match (self.def(lhs), self.def(rhs)) {
            (TypeInfo::Ssize, TypeInfo::Ssize) if arithmetic => Some(lhs),
            (TypeInfo::Usize, TypeInfo::Usize) if arithmetic => Some(lhs),
            (TypeInfo::UnsignedNumber(a), TypeInfo::UnsignedNumber(b)) if arithmetic && a == b => {
                Some(lhs)
            }
            (TypeInfo::SignedNumber(a), TypeInfo::SignedNumber(b)) if arithmetic && a == b => {
                Some(lhs)
            }
            (TypeInfo::Float(a), TypeInfo::Float(b)) if arithmetic && a == b => Some(lhs),

            (TypeInfo::Usize, TypeInfo::Usize) if comparison => Some(TypeId::BOOL),
            (TypeInfo::Ssize, TypeInfo::Ssize) if comparison => Some(TypeId::BOOL),
            (
                TypeInfo::Enum(_) | TypeInfo::UnsignedNumber(_),
                TypeInfo::Enum(_) | TypeInfo::UnsignedNumber(_),
            ) if comparison => match (self.unsigned_bits(lhs), self.unsigned_bits(rhs)) {
                (Some(a), Some(b)) if a == b => Some(TypeId::BOOL),
                _ => None,
            },
            (TypeInfo::SignedNumber(a), TypeInfo::SignedNumber(b)) if comparison && a == b => {
                Some(TypeId::BOOL)
            }
            (TypeInfo::Float(a), TypeInfo::Float(b)) if comparison && a == b => Some(TypeId::BOOL),

            (TypeInfo::Bool, TypeInfo::Bool)
                if matches!(op, EqualEqual | Keyword(Kw::And) | Keyword(Kw::Or)) =>
            {
                Some(TypeId::BOOL)
            }
            _ => None,
        }
    }

    fn size_descendants(&self, id: TypeId) -> Vec<TypeId> {
        match self.def(id) {
            TypeInfo::Struct(def) => def.fields.iter().map(|field| field.ty).collect(),
            TypeInfo::Array(def) => vec![def.type_id],
            _ => Vec::new(),
        }
    }

    fn cycle_back_to(
        &self,
        root: TypeId,
        current: TypeId,
        path: &mut Vec<TypeId>,
        seen: &mut HashSet<TypeId>,
    ) -> bool {
        for next in self.size_descendants(current) {
            path.push(next);
            if next == root {
                return true;
            }
            if seen.insert(next) && self.cycle_back_to(root, next, path, seen) {
                return true;
            }
            path.pop();
        }
        false
    }

    pub fn check_finite_sizes(&self) -> Vec<(TypeId, Vec<TypeId>)> {
        let mut offenders = Vec::new();
        for root in self.nominal_ids() {
            if !matches!(self.def(root), TypeInfo::Struct(_)) {
                continue;
            }
            let mut path = Vec::new();
            let mut seen = HashSet::new();
            if self.cycle_back_to(root, root, &mut path, &mut seen) {
                offenders.push((root, path));
            }
        }
        offenders
    }

    pub fn try_size_of(&self, id: TypeId) -> Option<usize> {
        if !self.in_progress.borrow_mut().insert(id) {
            return Some(0);
        }
        let size = self.compute_try_size_of(id);
        self.in_progress.borrow_mut().remove(&id);
        size
    }

    fn compute_try_size_of(&self, id: TypeId) -> Option<usize> {
        let ptr = self.target.target_pointer_size() as usize;
        match self.def(id) {
            TypeInfo::Array(def) => Some(def.length as usize * self.try_size_of(def.type_id)?),
            TypeInfo::Bool => Some(1),
            TypeInfo::Enum(_) => Some(4),
            TypeInfo::Float(bits)
            | TypeInfo::SignedNumber(bits)
            | TypeInfo::UnsignedNumber(bits) => Some((*bits as usize) / 8),
            TypeInfo::Pointer(_) => Some(64),
            TypeInfo::Slice(_) => Some(ptr / 8 + ptr / 8),
            TypeInfo::Struct(def) => def
                .fields
                .iter()
                .map(|field| self.try_size_of(field.ty))
                .sum::<Option<usize>>(),
            TypeInfo::Void => Some(0),
            TypeInfo::Usize | TypeInfo::Ssize | TypeInfo::Type | TypeInfo::Function(_) => None,
        }
    }

    fn compute_size_of(&self, id: TypeId) -> usize {
        let ptr = self.target.target_pointer_size() as usize;
        match self.def(id) {
            TypeInfo::Array(def) => def.length as usize * self.size_of(def.type_id),
            TypeInfo::Bool => 1,
            TypeInfo::Enum(_) => 4,
            TypeInfo::Float(bits)
            | TypeInfo::SignedNumber(bits)
            | TypeInfo::UnsignedNumber(bits) => (*bits as usize) / 8,
            TypeInfo::Function(_) => unreachable!("function types have no size"),
            TypeInfo::Pointer(_) => 64,
            TypeInfo::Slice(_) => ptr / 8 + ptr / 8,
            TypeInfo::Ssize => {
                unreachable!(
                    "ssize or SignedTargetPointerNumber should be handled in type_resolver"
                )
            }
            TypeInfo::Struct(def) => def.fields.iter().map(|f| self.size_of(f.ty)).sum(),
            TypeInfo::Type => unreachable!("`type` has no size"),
            TypeInfo::Usize => {
                unreachable!(
                    "usize or UnsignedTargetPointerNumber should be handled in type_resolver"
                )
            }
            TypeInfo::Void => 0,
        }
    }
}

impl Default for TypeInterner {
    fn default() -> Self {
        Self {
            target: bitbox::Target::default(),
            defs: create_default_type_info_list(),
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

#[cfg(test)]
mod tests {
    use super::*;
    use crate::stage::lexer::token::{Keyword as Kw, Span, TokenKind};
    use crate::stage::parser::ast::TypeKind;
    use bitbox::Target;

    fn ty(kind: TypeKind) -> ast::Type {
        ast::Type {
            mut_token: None,
            kind,
            span: Span::default(),
        }
    }

    fn boxed(kind: TypeKind) -> Box<ast::Type> {
        Box::new(ty(kind))
    }

    fn scalar_pairs(interner: &mut TypeInterner) -> Vec<(TypeKind, TypeId)> {
        vec![
            (TypeKind::Void, TypeId::VOID),
            (TypeKind::Bool, TypeId::BOOL),
            (TypeKind::UnsignedNumber(8), interner.unsigned(8)),
            (TypeKind::UnsignedNumber(16), interner.unsigned(16)),
            (TypeKind::UnsignedNumber(32), interner.unsigned(32)),
            (TypeKind::UnsignedNumber(64), interner.unsigned(64)),
            (TypeKind::UnsignedNumber(128), interner.unsigned(128)),
            (TypeKind::SignedNumber(8), interner.signed(8)),
            (TypeKind::SignedNumber(16), interner.signed(16)),
            (TypeKind::SignedNumber(32), interner.signed(32)),
            (TypeKind::SignedNumber(64), interner.signed(64)),
            (TypeKind::SignedNumber(128), interner.signed(128)),
            (TypeKind::Float(32), interner.float(32)),
            (TypeKind::Float(64), interner.float(64)),
            (TypeKind::UnsignedTargetPointerNumber, TypeId::USIZE),
            (TypeKind::SignedTargetPointerNumber, TypeId::SSIZE),
        ]
    }

    fn composite_pairs(interner: &mut TypeInterner) -> Vec<(TypeKind, TypeId)> {
        let u8_id = TypeId::U8;
        let s32_id = TypeId::S32;

        let ptr_u8 = interner.pointer_to(u8_id);
        let slice_u8 = interner.slice_of(u8_id);
        let ptr_slice_u8 = interner.pointer_to(slice_u8);
        let arr2_s32 = interner.array_of(2, s32_id);
        let arr2_arr2_s32 = interner.array_of(2, arr2_s32);

        vec![
            (
                TypeKind::Pointer(boxed(TypeKind::UnsignedNumber(8))),
                ptr_u8,
            ),
            (
                TypeKind::Slice(boxed(TypeKind::UnsignedNumber(8))),
                slice_u8,
            ),
            (
                TypeKind::Pointer(boxed(TypeKind::Slice(boxed(TypeKind::UnsignedNumber(8))))),
                ptr_slice_u8,
            ),
            (
                TypeKind::Array(2, boxed(TypeKind::SignedNumber(32))),
                arr2_s32,
            ),
            (
                TypeKind::Array(
                    2,
                    boxed(TypeKind::Array(2, boxed(TypeKind::SignedNumber(32)))),
                ),
                arr2_arr2_s32,
            ),
        ]
    }

    #[test]
    fn seeded_ids_match_their_consts() {
        let interner = TypeInterner::default();
        let expected = [
            (TypeId::VOID, TypeInfo::Void),
            (TypeId::BOOL, TypeInfo::Bool),
            (TypeId::U8, TypeInfo::UnsignedNumber(8)),
            (TypeId::S8, TypeInfo::SignedNumber(8)),
            (TypeId::U16, TypeInfo::UnsignedNumber(16)),
            (TypeId::S16, TypeInfo::SignedNumber(16)),
            (TypeId::U32, TypeInfo::UnsignedNumber(32)),
            (TypeId::S32, TypeInfo::SignedNumber(32)),
            (TypeId::U64, TypeInfo::UnsignedNumber(64)),
            (TypeId::S64, TypeInfo::SignedNumber(64)),
            (TypeId::USIZE, TypeInfo::Usize),
            (TypeId::SSIZE, TypeInfo::Ssize),
            (TypeId::U128, TypeInfo::UnsignedNumber(128)),
            (TypeId::S128, TypeInfo::SignedNumber(128)),
            (TypeId::F32, TypeInfo::Float(32)),
            (TypeId::F64, TypeInfo::Float(64)),
            (TypeId::TYPE, TypeInfo::Type),
        ];

        for (id, info) in expected {
            assert_eq!(interner.def(id), &info, "TypeId({})", id.index());
        }
    }

    #[test]
    fn structural_types_are_deduplicated() {
        let mut interner = TypeInterner::default();

        assert_eq!(
            interner.pointer_to(TypeId::U8),
            interner.pointer_to(TypeId::U8)
        );
        assert_eq!(interner.slice_of(TypeId::U8), interner.slice_of(TypeId::U8));
        assert_eq!(
            interner.array_of(2, TypeId::S32),
            interner.array_of(2, TypeId::S32)
        );

        assert_ne!(
            interner.pointer_to(TypeId::U8),
            interner.slice_of(TypeId::U8)
        );
        assert_ne!(
            interner.array_of(2, TypeId::S32),
            interner.array_of(3, TypeId::S32)
        );
    }

    #[test]
    fn canonical_widths_reuse_the_consts() {
        let mut interner = TypeInterner::default();

        assert_eq!(interner.unsigned(32), TypeId::U32);
        assert_eq!(interner.signed(32), TypeId::S32);
        assert_eq!(interner.float(64), TypeId::F64);
    }

    #[test]
    fn usize_is_distinct_from_u32_but_lowers_the_same_on_wasm32() {
        let mut interner = TypeInterner::new(Target::Wasm32);

        assert_ne!(interner.unsigned(32), TypeId::USIZE);
        assert_eq!(
            interner.as_bitbox_type(TypeId::USIZE),
            interner.as_bitbox_type(TypeId::U32)
        );
    }

    #[test]
    fn usize_lowers_to_the_target_pointer_width() {
        assert_eq!(
            TypeInterner::new(Target::Wasm32).as_bitbox_type(TypeId::USIZE),
            bitbox::ir::Type::Unsigned(32)
        );
        assert_eq!(
            TypeInterner::new(Target::X86_64Linux).as_bitbox_type(TypeId::USIZE),
            bitbox::ir::Type::Unsigned(64)
        );
    }

    #[test]
    fn as_bitbox_type_matches_the_ast_lowering() {
        for target in [Target::Wasm32, Target::X86_64Linux] {
            let mut interner = TypeInterner::new(target);
            let mut pairs = scalar_pairs(&mut interner);
            pairs.extend(composite_pairs(&mut interner));

            for (kind, id) in pairs {
                let from_ast = ty(kind.clone()).as_bitbox_type(&target);
                let from_interner = interner.as_bitbox_type(id);
                assert_eq!(
                    from_ast, from_interner,
                    "lowering mismatch for `{kind}` on {target:?}"
                );
            }
        }
    }

    #[test]
    fn size_of_matches_the_ast_sizes() {
        for target in [Target::Wasm32, Target::X86_64Linux] {
            let mut interner = TypeInterner::new(target);
            let mut pairs: Vec<(TypeKind, TypeId)> = scalar_pairs(&mut interner)
                .into_iter()
                .filter(|(kind, _)| {
                    !matches!(
                        kind,
                        TypeKind::UnsignedTargetPointerNumber | TypeKind::SignedTargetPointerNumber
                    )
                })
                .collect();
            pairs.extend(composite_pairs(&mut interner));

            for (kind, id) in pairs {
                let from_ast = ty(kind.clone()).size(&target);
                let from_interner = interner.size_of(id);
                assert_eq!(
                    from_ast, from_interner,
                    "size mismatch for `{kind}` on {target:?}"
                );
            }
        }
    }

    #[test]
    fn size_of_preserves_the_pointer_size_quirk() {
        let interner = TypeInterner::new(Target::X86_64Linux);
        let mut interner = interner;
        let ptr = interner.pointer_to(TypeId::U8);

        assert_eq!(interner.size_of(ptr), 64);
    }

    #[test]
    fn name_of_matches_the_ast_display() {
        let mut interner = TypeInterner::default();
        let mut pairs = scalar_pairs(&mut interner);
        pairs.extend(composite_pairs(&mut interner));

        for (kind, id) in pairs {
            assert_eq!(kind.to_string(), interner.name_of(id), "name mismatch");
        }
    }

    #[test]
    fn mangled_names_match_the_monomorphizer_scheme() {
        let mut interner = TypeInterner::default();
        let ptr_u8 = interner.pointer_to(TypeId::U8);
        let slice_u8 = interner.slice_of(TypeId::U8);
        let arr2_s32 = interner.array_of(2, TypeId::S32);

        assert_eq!(interner.mangled_name_of(TypeId::S32), "s32");
        assert_eq!(interner.mangled_name_of(TypeId::USIZE), "usize");
        assert_eq!(interner.mangled_name_of(TypeId::BOOL), "bool");
        assert_eq!(interner.mangled_name_of(ptr_u8), "ptr_u8");
        assert_eq!(interner.mangled_name_of(slice_u8), "slice_u8");
        assert_eq!(interner.mangled_name_of(arr2_s32), "arr2_s32");

        assert_eq!(interner.mangle("Pair", &[TypeId::S32]), "Pair__s32");
        assert_eq!(
            interner.mangle("Foo", &[TypeId::S32, TypeId::BOOL]),
            "Foo__s32__bool"
        );
    }

    #[test]
    fn slice_lowers_with_the_load_bearing_name_prefix() {
        let mut interner = TypeInterner::new(Target::Wasm32);
        let slice_u8 = interner.slice_of(TypeId::U8);

        let bitbox::ir::Type::Struct(lowered) = interner.as_bitbox_type(slice_u8) else {
            panic!("a slice must lower to a struct");
        };

        assert_eq!(lowered.name, "slice_u8");
        assert_eq!(lowered.fields[0].0, "data");
        assert_eq!(lowered.fields[1].0, "len");
        assert_eq!(lowered.fields[1].1, bitbox::ir::Type::Unsigned(32));
    }

    #[test]
    fn recursive_struct_lowering_terminates() {
        let mut interner = TypeInterner::default();
        let node = interner
            .declare_struct("Node", Span::default())
            .expect("fresh name");
        let next = interner.pointer_to(node);
        interner.fill_struct(
            node,
            vec![
                StructField {
                    name: "next".to_string(),
                    ty: next,
                    default: None,
                },
                StructField {
                    name: "value".to_string(),
                    ty: TypeId::S32,
                    default: None,
                },
            ],
        );

        let lowered = interner.as_bitbox_type(node);
        assert!(matches!(lowered, bitbox::ir::Type::Struct(_)));
        assert_eq!(interner.name_of(next), "*Node");
    }

    #[test]
    fn declaring_a_duplicate_name_reports_the_existing_id() {
        let mut interner = TypeInterner::default();
        let first = interner
            .declare_struct("List", Span::default())
            .expect("fresh name");

        assert_eq!(interner.declare_struct("List", Span::default()), Err(first));
        assert_eq!(interner.declare_enum("List", Span::default()), Err(first));
    }

    fn struct_with_fields(
        interner: &mut TypeInterner,
        name: &str,
        fields: Vec<(&str, TypeId)>,
    ) -> TypeId {
        let id = interner
            .declare_struct(name, Span::default())
            .expect("fresh name");
        let fields = fields
            .into_iter()
            .map(|(field_name, ty)| StructField {
                name: field_name.to_string(),
                ty,
                default: None,
            })
            .collect();
        interner.fill_struct(id, fields);
        id
    }

    #[test]
    fn a_struct_containing_itself_by_value_has_no_finite_size() {
        let mut interner = TypeInterner::default();
        let bad = interner
            .declare_struct("Bad", Span::default())
            .expect("fresh name");
        interner.fill_struct(
            bad,
            vec![StructField {
                name: "me".to_string(),
                ty: bad,
                default: None,
            }],
        );

        let offenders = interner.check_finite_sizes();
        assert_eq!(offenders.len(), 1);
        assert_eq!(offenders[0].0, bad);
    }

    #[test]
    fn indirection_through_a_pointer_makes_a_recursive_struct_finite() {
        let mut interner = TypeInterner::default();
        let node = interner
            .declare_struct("Node", Span::default())
            .expect("fresh name");
        let next = interner.pointer_to(node);
        interner.fill_struct(
            node,
            vec![StructField {
                name: "next".to_string(),
                ty: next,
                default: None,
            }],
        );

        assert!(interner.check_finite_sizes().is_empty());
    }

    #[test]
    fn mutually_recursive_structs_are_caught() {
        let mut interner = TypeInterner::default();
        let a = interner.declare_struct("A", Span::default()).unwrap();
        let b = interner.declare_struct("B", Span::default()).unwrap();
        interner.fill_struct(
            a,
            vec![StructField {
                name: "b".to_string(),
                ty: b,
                default: None,
            }],
        );
        interner.fill_struct(
            b,
            vec![StructField {
                name: "a".to_string(),
                ty: a,
                default: None,
            }],
        );

        assert_eq!(interner.check_finite_sizes().len(), 2);
    }

    #[test]
    fn a_struct_containing_an_array_of_itself_is_caught() {
        let mut interner = TypeInterner::default();
        let bad = interner.declare_struct("Bad", Span::default()).unwrap();
        let array = interner.array_of(2, bad);
        interner.fill_struct(
            bad,
            vec![StructField {
                name: "kids".to_string(),
                ty: array,
                default: None,
            }],
        );

        assert_eq!(interner.check_finite_sizes().len(), 1);
    }

    #[test]
    fn plain_structs_have_finite_size() {
        let mut interner = TypeInterner::default();
        struct_with_fields(
            &mut interner,
            "Point",
            vec![("x", TypeId::S32), ("y", TypeId::S32)],
        );

        assert!(interner.check_finite_sizes().is_empty());
    }

    #[test]
    fn enum_variant_values_fall_back_to_the_ordinal_index() {
        let mut interner = TypeInterner::default();
        let id = interner.declare_enum("E", Span::default()).unwrap();
        interner.fill_enum(
            id,
            vec![
                ("a".to_string(), None),
                ("b".to_string(), Some("10".to_string())),
                ("c".to_string(), None),
            ],
            TypeId::U32,
        );

        assert_eq!(interner.enum_variant_value(id, "a").as_deref(), Some("0"));
        assert_eq!(interner.enum_variant_value(id, "b").as_deref(), Some("10"));
        assert_eq!(interner.enum_variant_value(id, "c").as_deref(), Some("2"));
        assert_eq!(interner.enum_variant_value(id, "missing"), None);
    }

    #[test]
    fn an_enum_is_compatible_with_its_unsigned_representation() {
        let mut interner = TypeInterner::default();
        let id = interner.declare_enum("E", Span::default()).unwrap();
        interner.fill_enum(id, vec![("a".to_string(), None)], TypeId::U32);

        assert!(interner.compatible(id, TypeId::U32));
        assert!(interner.compatible(TypeId::U32, id));
        assert!(!interner.same_type(id, TypeId::U32));
        assert!(!interner.compatible(id, TypeId::S32));
    }

    #[test]
    fn usize_is_never_treated_as_an_unsigned_number() {
        let interner = TypeInterner::new(Target::Wasm32);

        assert_eq!(interner.unsigned_bits(TypeId::USIZE), None);
        assert!(!interner.compatible(TypeId::USIZE, TypeId::U32));
    }

    #[test]
    fn structural_queries_walk_the_expected_shapes() {
        let mut interner = TypeInterner::default();
        let ptr_u8 = interner.pointer_to(TypeId::U8);
        let ptr_ptr_u8 = interner.pointer_to(ptr_u8);
        let slice_u8 = interner.slice_of(TypeId::U8);
        let arr = interner.array_of(4, TypeId::U16);

        assert_eq!(interner.de_ref(ptr_ptr_u8), TypeId::U8);
        assert_eq!(interner.pointee(ptr_ptr_u8), Some(ptr_u8));
        assert_eq!(interner.element_of(slice_u8), Some(TypeId::U8));
        assert_eq!(interner.element_of(arr), Some(TypeId::U16));
        assert_eq!(interner.array_len(arr), Some(4));
        assert_eq!(interner.array_len(slice_u8), None);
        assert!(interner.is_slice(slice_u8));
        assert!(!interner.is_slice(arr));
        assert!(interner.is_indexable(arr));
        assert!(interner.is_indexable(ptr_ptr_u8));
        assert!(!interner.is_indexable(TypeId::S32));
    }

    #[test]
    fn field_lookup_returns_the_declaration_index() {
        let mut interner = TypeInterner::default();
        let point = struct_with_fields(
            &mut interner,
            "Point",
            vec![("x", TypeId::S32), ("y", TypeId::F32)],
        );
        let ptr = interner.pointer_to(point);

        assert_eq!(interner.field(point, "x").map(|(i, _)| i), Some(0));
        assert_eq!(interner.field(point, "y").map(|(i, _)| i), Some(1));
        assert_eq!(interner.field(point, "z").map(|(i, _)| i), None);
        assert_eq!(
            interner.field_through_ptr(ptr, "y").map(|(i, _)| i),
            Some(1)
        );
        assert_eq!(interner.struct_fields(point).map(|f| f.len()), Some(2));
        assert_eq!(interner.struct_fields(TypeId::S32), None);
    }

    fn binary_ops() -> Vec<TokenKind> {
        vec![
            TokenKind::Plus,
            TokenKind::Minus,
            TokenKind::Star,
            TokenKind::Slash,
            TokenKind::Percent,
            TokenKind::BitShiftRight,
            TokenKind::Ampersand,
            TokenKind::EqualEqual,
            TokenKind::Greater,
            TokenKind::GreaterEqual,
            TokenKind::Less,
            TokenKind::LessEqual,
            TokenKind::Keyword(Kw::And),
            TokenKind::Keyword(Kw::Or),
        ]
    }

    #[test]
    fn binary_op_result_matches_supports_binary_op() {
        let mut interner = TypeInterner::new(Target::Wasm32);

        let enum_id = interner.declare_enum("E", Span::default()).unwrap();
        interner.fill_enum(enum_id, vec![("a".to_string(), None)], TypeId::U32);
        let enum_kind = TypeKind::Enum(ast::EnumType {
            name: "E".to_string(),
            type_params: None,
            variants: vec![],
            number_kind: Box::new(TypeKind::UnsignedNumber(32)),
        });

        let mut operands = scalar_pairs(&mut interner);
        operands.push((enum_kind, enum_id));

        for (lhs_kind, lhs_id) in &operands {
            for (rhs_kind, rhs_id) in &operands {
                for op in binary_ops() {
                    let from_ast = ty(lhs_kind.clone())
                        .supports_binary_op(&op, &ty(rhs_kind.clone()), Span::default())
                        .map(|result| result.kind.to_string());
                    let from_interner = interner
                        .binary_op_result(*lhs_id, &op, *rhs_id)
                        .map(|id| interner.name_of(id));

                    assert_eq!(
                        from_ast, from_interner,
                        "`{lhs_kind}` {op:?} `{rhs_kind}` disagreed"
                    );
                }
            }
        }
    }

    #[test]
    fn usize_and_u32_do_not_mix_in_binary_ops() {
        let interner = TypeInterner::new(Target::Wasm32);

        assert_eq!(
            interner.binary_op_result(TypeId::USIZE, &TokenKind::EqualEqual, TypeId::U32),
            None
        );
        assert_eq!(
            interner.binary_op_result(TypeId::USIZE, &TokenKind::Plus, TypeId::USIZE),
            Some(TypeId::USIZE)
        );
    }

    #[test]
    fn try_size_of_reports_unsized_types_instead_of_panicking() {
        let mut interner = TypeInterner::new(Target::X86_64Linux);
        let with_usize = struct_with_fields(
            &mut interner,
            "ArrayList",
            vec![("capacity", TypeId::USIZE), ("length", TypeId::USIZE)],
        );
        let plain = struct_with_fields(
            &mut interner,
            "Point",
            vec![("x", TypeId::S32), ("y", TypeId::S32)],
        );

        assert_eq!(interner.try_size_of(TypeId::USIZE), None);
        assert_eq!(interner.try_size_of(with_usize), None);
        assert_eq!(interner.try_size_of(plain), Some(8));
        assert_eq!(interner.try_size_of(TypeId::BOOL), Some(1));
    }

    #[test]
    fn numeric_hint_walks_to_the_innermost_scalar() {
        let mut interner = TypeInterner::default();
        let inner = interner.array_of(2, TypeId::S32);
        let nested = interner.array_of(2, inner);
        let slice = interner.slice_of(TypeId::F32);
        let ptr = interner.pointer_to(TypeId::S32);

        assert_eq!(interner.numeric_hint(nested), Some(TypeId::S32));
        assert_eq!(interner.numeric_hint(slice), Some(TypeId::F32));
        assert_eq!(interner.numeric_hint(TypeId::USIZE), Some(TypeId::USIZE));
        assert_eq!(interner.numeric_hint(TypeId::BOOL), None);
        assert_eq!(interner.numeric_hint(ptr), None);
    }

    #[test]
    fn nominal_ids_are_returned_in_declaration_order() {
        let mut interner = TypeInterner::default();
        let a = interner.declare_struct("A", Span::default()).unwrap();
        let b = interner.declare_enum("B", Span::default()).unwrap();
        let c = interner.declare_struct("C", Span::default()).unwrap();

        assert_eq!(interner.nominal_ids(), vec![a, b, c]);
    }
}
