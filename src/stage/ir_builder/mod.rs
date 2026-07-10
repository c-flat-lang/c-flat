use super::Stage;
use crate::stage::lexer::token::{Keyword, TokenKind};
use crate::stage::semantic_analyzer::symbol_table::SymbolTable;
use crate::{error::Result, stage::parser::ast, stage::parser::ast::Item};
use bitbox::Target;
use bitbox::ir::builder::{AssemblerBuilder, FunctionBuilder, ModuleBuilder};
use bitbox::ir::{self, ConstantInt, Module, Operand, StructType, Type, Variable, Visibility};
use std::str::FromStr;

use crate::stage::parser::ast::{
    Expr, ExprAddressOf, ExprArray, ExprArrayIndex, ExprArrayRepeat, ExprAssignment, ExprBinary,
    ExprBlock, ExprCall, ExprDecl, ExprDeref, ExprGrouping, ExprIfElse, ExprMemberAccess, ExprNot,
    ExprPath, ExprReturn, ExprStruct, ExprTypeCast, ExprWhile, Litral, TypeKind,
};

#[derive(Debug)]
pub struct IRBuilder {
    symbol_table: SymbolTable,
    target: Target,
}

impl IRBuilder {
    pub fn new(target: Target) -> Self {
        Self {
            symbol_table: SymbolTable::default(),
            target,
        }
    }

    fn build_function(&mut self, function: ast::Function, mb: &mut ModuleBuilder) {
        let ast::Function {
            visibility,
            fn_token: _,
            name,
            type_args: _,
            params,
            return_type,
            body,
        } = function;

        self.symbol_table.enter_scope(&name.lexeme);

        let mut function_builder = FunctionBuilder::new(name.lexeme.clone())
            .with_visibility(match visibility {
                ast::Visibility::Public => Visibility::Public,
                ast::Visibility::Private => Visibility::Private,
            })
            .with_params(
                params
                    .iter()
                    .map(|param| {
                        Variable::new(
                            param.name.lexeme.clone(),
                            param.ty.as_bitbox_type(&self.target),
                        )
                    })
                    .collect(),
            )
            .with_return_type(return_type.as_bitbox_type(&self.target));

        let mut ctx = LoweringContext::new(&mut self.symbol_table, self.target);

        for param in function_builder.params.iter() {
            ctx.push(param.clone());
        }

        let mut assembler = function_builder.assembler();
        assembler.create_block("entry");

        body.lower(&mut assembler, &mut ctx);

        self.symbol_table.exit_scope();
        mb.function(function_builder.build());
    }
}

impl Stage<(SymbolTable, Vec<Item>), Result<Module>> for IRBuilder {
    fn run(&mut self, (symbol_table, ast): (SymbolTable, Vec<Item>)) -> Result<Module> {
        eprintln!("{: >30}", "IR Code Generation");
        self.symbol_table = symbol_table;
        let mut mb = ModuleBuilder::default();
        for item in ast {
            match item {
                Item::Function(function) => self.build_function(function, &mut mb),
                Item::Type(_) => {}
                Item::Use(_) => {}
                Item::ExternFunction(extern_function) => {
                    let ast::ExternFunction {
                        binding_name,
                        params,
                        return_type,
                        ..
                    } = extern_function;

                    let declaration = ir::ExternDecl {
                        name: binding_name.lexeme.clone(),
                        params: params
                            .iter()
                            .map(|ty| ty.as_bitbox_type(&self.target))
                            .collect(),
                        return_type: return_type.as_bitbox_type(&self.target),
                    };

                    mb.extern_decl(declaration);
                }
            }
        }
        Ok(mb.build())
    }
}

pub struct LoweringContext<'a> {
    symbol_table: &'a mut SymbolTable,
    variable_stack: Vec<Variable>,
    target: Target,
}

impl<'a> LoweringContext<'a> {
    pub fn new(symbol_table: &'a mut SymbolTable, target: Target) -> Self {
        Self {
            symbol_table,
            variable_stack: Vec::new(),
            target,
        }
    }

    pub fn push(&mut self, var: Variable) {
        self.variable_stack.push(var);
    }

    fn get_variable(&self, lexeme: &str) -> Option<Variable> {
        self.variable_stack
            .iter()
            .find(|var| var.name == lexeme)
            .cloned()
    }
}

#[derive(Debug, Clone)]
pub struct Address {
    variable: Variable,
    offset: Operand,
}

#[derive(Debug, Clone)]
pub enum AddressableVar {
    Var(Variable),
    Address(Address),
}

pub trait Lowerable {
    /// Lowers this AST node into IR using the provided assembler.
    /// Returns an optional variable representing the result of this expression.
    fn lower(
        &self,
        assembler: &mut AssemblerBuilder,
        ctx: &mut LoweringContext,
    ) -> Option<Variable>;
}

pub trait Addressable {
    fn lower_to_address(
        &self,
        assembler: &mut AssemblerBuilder,
        ctx: &mut LoweringContext,
    ) -> Option<AddressableVar>;
}

impl Lowerable for Expr {
    fn lower(
        &self,
        assembler: &mut AssemblerBuilder,
        ctx: &mut LoweringContext,
    ) -> Option<Variable> {
        match self {
            Expr::Return(expr) => expr.lower(assembler, ctx),
            Expr::Binary(expr) => expr.lower(assembler, ctx),
            Expr::IfElse(expr) => expr.lower(assembler, ctx),
            Expr::Litral(lit) => lit.lower(assembler, ctx),
            Expr::Assignment(assign) => assign.lower(assembler, ctx),
            Expr::Declare(declare) => declare.lower(assembler, ctx),
            Expr::Builtin(call) => call.lower(assembler, ctx),
            Expr::Call(call) => call.lower(assembler, ctx),
            Expr::Identifier(ident) => {
                let Some(_) = ctx.symbol_table.get(&ident.lexeme) else {
                    panic!("Symbol not found {}", ident.lexeme);
                };
                ctx.get_variable(&ident.lexeme)
            }
            Expr::Path(expr) => expr.lower(assembler, ctx),
            Expr::Struct(expr) => expr.lower(assembler, ctx),
            Expr::MemberAccess(expr) => expr.lower(assembler, ctx),
            Expr::Array(expr) => expr.lower(assembler, ctx),
            Expr::ArrayIndex(expr) => expr.lower(assembler, ctx),
            Expr::ArrayRepeat(expr) => expr.lower(assembler, ctx),
            Expr::While(expr) => expr.lower(assembler, ctx),
            Expr::Block(block) => block.lower(assembler, ctx),
            Expr::AddressOf(expr) => expr.lower(assembler, ctx),
            Expr::Deref(expr) => expr.lower(assembler, ctx),
            Expr::Not(expr) => expr.lower(assembler, ctx),
            Expr::Grouping(expr) => expr.lower(assembler, ctx),
            Expr::TypeCast(expr) => expr.lower(assembler, ctx),
        }
    }
}

impl Addressable for Expr {
    fn lower_to_address(
        &self,
        assembler: &mut AssemblerBuilder,
        ctx: &mut LoweringContext,
    ) -> Option<AddressableVar> {
        match self {
            Expr::Identifier(ident) => {
                let Some(_) = ctx.symbol_table.get(&ident.lexeme) else {
                    panic!("Symbol not found {}", ident.lexeme);
                };

                ctx.get_variable(&ident.lexeme).map(AddressableVar::Var)
            }
            Expr::ArrayIndex(expr) => expr.lower_to_address(assembler, ctx),
            // `base.field = value` — address is the base (struct value or
            // pointer-to-struct) indexed by the field position.
            Expr::MemberAccess(member) => {
                let Some(base) = member.base.lower(assembler, ctx) else {
                    panic!("Failed to return variable from MemberAccess base lowering");
                };
                let fields = match &base.ty {
                    ir::Type::Struct(ir::StructType { fields, .. }) => fields.clone(),
                    ir::Type::Pointer(inner) => match inner.as_ref() {
                        ir::Type::Struct(ir::StructType { fields, .. }) => fields.clone(),
                        _ => panic!("Expected pointer to struct type in MemberAccess assignment"),
                    },
                    _ => panic!("Expected struct type in MemberAccess assignment"),
                };
                let Some(idx) = fields
                    .iter()
                    .position(|(name, _)| name == &member.member.lexeme)
                else {
                    panic!("Field not found {}", member.member.lexeme);
                };
                let address = Address {
                    variable: base,
                    offset: Operand::ConstantInt(ConstantInt::new(
                        idx.to_string(),
                        Type::Signed(32),
                    )),
                };
                Some(AddressableVar::Address(address))
            }
            _ => unreachable!(),
        }
    }
}
impl Lowerable for ExprPath {
    fn lower(
        &self,
        assembler: &mut AssemblerBuilder,
        ctx: &mut LoweringContext,
    ) -> Option<Variable> {
        let leaf = self.leaf();
        let head = self.head();
        let Some(symbol) = ctx.symbol_table.get(&head.lexeme) else {
            panic!("Undefind symbol {:?} or {:?}", self.head(), leaf);
        };
        match &symbol.kind {
            super::semantic_analyzer::symbol_table::SymbolKind::Struct => {
                todo!("Path Struct")
            }
            super::semantic_analyzer::symbol_table::SymbolKind::Enum => {
                let ast::TypeKind::Enum(ty) = &symbol.ty.kind else {
                    panic!("incorrect SymbolKind matched with a Symbol");
                };
                let Some((_, value)) = ty.variants.iter().find(|v| v.0.lexeme == leaf.lexeme)
                else {
                    panic!("Unknown variant on {}", head.lexeme);
                };
                let tmp = assembler.var(Type::Unsigned(32));
                assembler.assign(tmp.clone(), Operand::const_unsigned(value, 32));
                Some(tmp)
            }
            kind => {
                if ctx.symbol_table.get(&leaf.lexeme).is_some() {
                    return ctx.get_variable(&leaf.lexeme);
                }
                unreachable!("PATH {kind:?}");
            }
        }
    }
}

impl Lowerable for ExprStruct {
    fn lower(
        &self,
        assembler: &mut AssemblerBuilder,
        ctx: &mut LoweringContext,
    ) -> Option<Variable> {
        let Some(symbol) = ctx.symbol_table.get(&self.name.lexeme).cloned() else {
            panic!("Symbol not found {}", self.name.lexeme);
        };

        let Some(fields) = symbol.fields.as_ref() else {
            panic!("Symbol not found {}", self.name.lexeme);
        };

        let ty = symbol.ty.as_bitbox_type(&ctx.target);

        let ptr = assembler.var(ty.clone());

        assembler.alloc(
            ty.clone(),
            ptr.clone(),
            Operand::ConstantInt(ir::ConstantInt::new("1".to_string(), Type::Signed(32))),
        );

        for (index, symbol_field) in fields.iter().enumerate() {
            let value = self
                .init_fields
                .iter()
                .find_map(|init_field| {
                    if init_field.name.lexeme == *symbol_field.name {
                        init_field.expr.lower(assembler, ctx)
                    } else {
                        None
                    }
                })
                .unwrap_or_else(|| {
                    let Some(default) = &symbol_field.default_value else {
                        panic!(
                            "Failed to return variable from expr lowering in Struct field {:#?}",
                            symbol_field.ty.span.clone()
                        );
                    };
                    default
                        .lower(assembler, ctx)
                        .expect("Failed to return variable from expr lowering in Struct field")
                });

            assembler.elemset(
                ptr.clone(),
                Operand::ConstantInt(ConstantInt::new(index.to_string(), Type::Signed(32))),
                value,
            );
        }

        Some(ptr)
    }
}

impl Lowerable for ExprMemberAccess {
    fn lower(
        &self,
        assembler: &mut AssemblerBuilder,
        ctx: &mut LoweringContext,
    ) -> Option<Variable> {
        let Some(ptr) = self.base.lower(assembler, ctx) else {
            panic!("Failed to return variable from expr lowering in MemberAccess");
        };
        let fields = match &ptr.ty {
            ir::Type::Struct(ir::StructType { fields, .. }) => fields.clone(),
            ir::Type::Pointer(inner) => match inner.as_ref() {
                ir::Type::Struct(ir::StructType { fields, .. }) => fields.clone(),
                _ => panic!("Expected pointer to struct type in MemberAccess"),
            },
            ir::Type::Array(size, _) => {
                // HACK: I think we could maybe handle this better.
                let ty = Type::Unsigned(ctx.target.target_pointer_size());
                let des = assembler.var(ty.clone());
                let index = Operand::ConstantInt(ConstantInt::new(size.to_string(), ty));
                assembler.assign(des.clone(), index);
                return Some(des);
            }
            ty => panic!("Expected struct type in MemberAccess but found {ty}"),
        };
        let Some((idx, (_, field_ty))) = fields
            .iter()
            .enumerate()
            .find(|(_, (name, _))| name == &self.member.lexeme)
        else {
            panic!("Field not found {}", self.member.lexeme);
        };

        let des = assembler.var(field_ty.clone());
        let index = Operand::ConstantInt(ConstantInt::new(idx.to_string(), Type::Signed(32)));

        assembler.elemget(des.clone(), ptr, index);

        Some(des)
    }
}

impl Lowerable for ExprArray {
    fn lower(
        &self,
        assembler: &mut AssemblerBuilder,
        ctx: &mut LoweringContext,
    ) -> Option<Variable> {
        let elem_ty = self.ty.as_bitbox_type(&ctx.target);
        let full_ty = Type::Array(self.elements.len(), Box::new(elem_ty));
        let ptr = assembler.var(full_ty.clone());
        assembler.alloc(
            full_ty.clone(),
            ptr.clone(),
            Operand::ConstantInt(ir::ConstantInt::new("1".to_string(), Type::Signed(32))),
        );
        for (index, element) in self.elements.iter().enumerate() {
            let Some(value) = element.lower(assembler, ctx) else {
                panic!("Failed to return variable from expr lowering in Array");
            };
            assembler.elemset(
                ptr.clone(),
                Operand::ConstantInt(ConstantInt::new(index.to_string(), Type::Signed(32))),
                value,
            );
        }
        Some(ptr)
    }
}

impl Lowerable for ExprBinary {
    fn lower(
        &self,
        assembler: &mut AssemblerBuilder,
        ctx: &mut LoweringContext,
    ) -> Option<Variable> {
        let lhs = self
            .left
            .lower(assembler, ctx)
            .unwrap_or_else(|| panic!("lhs {:?} should produce a variable", self.left));
        let rhs = self
            .right
            .lower(assembler, ctx)
            .unwrap_or_else(|| panic!("rhs {:?} should produce a variable", self.right));

        let des = assembler.var(lhs.ty.clone()); // Or infer type
        match self.op.kind {
            TokenKind::Ampersand => assembler.bwand(des.clone(), lhs, rhs),
            TokenKind::BitShiftRight => assembler.bsr(des.clone(), lhs, rhs),
            TokenKind::EqualEqual => assembler.eq(des.clone(), lhs, rhs),
            TokenKind::Greater => assembler.gt(des.clone(), lhs, rhs),
            TokenKind::GreaterEqual => assembler.gte(des.clone(), lhs, rhs),
            TokenKind::Keyword(Keyword::And) => assembler.and(des.clone(), lhs, rhs),
            TokenKind::Keyword(Keyword::Or) => assembler.or(des.clone(), lhs, rhs),
            TokenKind::Less => assembler.lt(des.clone(), lhs, rhs),
            TokenKind::LessEqual => assembler.lte(des.clone(), lhs, rhs),
            TokenKind::Minus => assembler.sub(des.clone(), lhs, rhs),
            TokenKind::Percent => assembler.rem(des.clone(), lhs, rhs),
            TokenKind::Plus => assembler.add(des.clone(), lhs, rhs),
            TokenKind::Slash => assembler.div(des.clone(), lhs, rhs),
            TokenKind::Star => assembler.mul(des.clone(), lhs, rhs),
            op => unimplemented!("Operator not implemented {op:?}"),
        };
        Some(des)
    }
}

impl Lowerable for ExprIfElse {
    fn lower(
        &self,
        assembler: &mut AssemblerBuilder,
        ctx: &mut LoweringContext,
    ) -> Option<Variable> {
        let result_var = if self.ty.kind == ast::TypeKind::Void {
            None
        } else {
            Some(assembler.var(self.ty.as_bitbox_type(&ctx.target)))
        };

        let mut var = vec![];

        #[cfg(not(feature = "uuids"))]
        let counter = assembler.counter();
        #[cfg(not(feature = "uuids"))]
        let label_counter = assembler.label_counter();

        // Condition
        let mut cond_blocks = vec![];
        let mut cond_assembler = AssemblerBuilder::new(&mut cond_blocks, &mut var);
        #[cfg(not(feature = "uuids"))]
        cond_assembler
            .with_counter(counter)
            .with_label_counter(label_counter);
        cond_assembler.create_block("cond");
        let cond_result = self
            .condition
            .lower(&mut cond_assembler, ctx)
            .expect("condition must produce a value");
        #[cfg(not(feature = "uuids"))]
        let counter = cond_assembler.counter();
        #[cfg(not(feature = "uuids"))]
        let label_counter = cond_assembler.label_counter();

        // Then branch
        var.clear();
        let mut then_blocks = vec![];
        let mut then_assembler = AssemblerBuilder::new(&mut then_blocks, &mut var);
        #[cfg(not(feature = "uuids"))]
        then_assembler
            .with_counter(counter)
            .with_label_counter(label_counter);
        then_assembler.create_block("then");
        if let Some(val) = self.then_branch.lower(&mut then_assembler, ctx)
            && let Some(res) = &result_var
        {
            then_assembler.assign(res.clone(), val);
        }
        #[cfg(not(feature = "uuids"))]
        let counter = then_assembler.counter();
        #[cfg(not(feature = "uuids"))]
        let label_counter = then_assembler.label_counter();

        // Else branch
        var.clear();
        let mut else_blocks = vec![];
        if let Some(else_b) = &self.else_branch {
            let mut else_assembler = AssemblerBuilder::new(&mut else_blocks, &mut var);
            #[cfg(not(feature = "uuids"))]
            else_assembler
                .with_counter(counter)
                .with_label_counter(label_counter);
            else_assembler.create_block("else");
            if let Some(val) = else_b.lower(&mut else_assembler, ctx)
                && let Some(res) = &result_var
            {
                else_assembler.assign(res.clone(), val);
            }
            #[cfg(not(feature = "uuids"))]
            assembler
                .with_counter(else_assembler.counter())
                .with_label_counter(else_assembler.label_counter());
        } else {
            #[cfg(not(feature = "uuids"))]
            assembler
                .with_counter(counter)
                .with_label_counter(label_counter);
        }

        assembler.if_else(
            cond_blocks,
            cond_result,
            then_blocks,
            else_blocks,
            result_var.clone(),
        );
        result_var
    }
}

impl Lowerable for ExprBlock {
    fn lower(
        &self,
        assembler: &mut AssemblerBuilder,
        ctx: &mut LoweringContext,
    ) -> Option<Variable> {
        let mut result = None;
        for stmt in &self.statements {
            result = stmt.expr.lower(assembler, ctx);
        }
        result
    }
}

impl Lowerable for ExprReturn {
    fn lower(
        &self,
        assembler: &mut AssemblerBuilder,
        ctx: &mut LoweringContext,
    ) -> Option<Variable> {
        match &self.expr {
            Some(expr) => {
                let Some(return_variable) = expr.lower(assembler, ctx) else {
                    panic!("Failed to pop variable");
                };
                assembler.ret(return_variable.ty.clone(), return_variable.clone());
                Some(return_variable)
            }
            None => {
                assembler.void_ret();
                None
            }
        }
    }
}

impl Lowerable for Litral {
    fn lower(
        &self,
        assembler: &mut AssemblerBuilder,
        ctx: &mut LoweringContext,
    ) -> Option<Variable> {
        match self {
            ast::Litral::String(token) => {
                let bytes = token.lexeme.as_bytes();
                let elem_ty = Type::Unsigned(8);
                let arr_ty = Type::Array(bytes.len(), Box::new(elem_ty.clone()));
                // Count is 1: the alloc contract reserves `count * arr_ty.size()` = `len` bytes.
                let size =
                    Operand::ConstantInt(ir::ConstantInt::new("1".to_string(), Type::Signed(32)));
                let ptr = assembler.var(arr_ty.clone());
                assembler.alloc(arr_ty, ptr.clone(), size);
                for (index, &byte) in bytes.iter().enumerate() {
                    let elem = assembler.var(elem_ty.clone());
                    assembler.assign(
                        elem.clone(),
                        Operand::ConstantInt(ir::ConstantInt::new(
                            byte.to_string(),
                            elem_ty.clone(),
                        )),
                    );
                    assembler.elemset(
                        ptr.clone(),
                        Operand::ConstantInt(ir::ConstantInt::new(
                            index.to_string(),
                            Type::Signed(32),
                        )),
                        elem,
                    );
                }
                Some(ptr)
            }
            ast::Litral::Integer(integer_litral) => {
                let ty = integer_litral.ty.as_bitbox_type(&ctx.target);
                let bits: u8 = (ty.size(&ctx.target) * 4) as u8;
                let var = assembler.var(ty);
                assembler.assign(
                    var.clone(),
                    Operand::const_unsigned(&integer_litral.token.lexeme, bits),
                );
                Some(var)
            }
            ast::Litral::Float(token) => {
                let ty = Type::Float(32);
                let var = assembler.var(ty);
                assembler.assign(var.clone(), Operand::const_float(&token.lexeme, 32));
                Some(var)
            }
            ast::Litral::Char(token) => {
                let Ok(c) = crate::stage::semantic_analyzer::type_check::SmallestCharInt::from_str(
                    &token.lexeme,
                ) else {
                    panic!("failed to get the SmallestCharInt for {}", token.lexeme)
                };
                let bytes = match c {
                    super::semantic_analyzer::type_check::SmallestCharInt::U8(_) => 8,
                    super::semantic_analyzer::type_check::SmallestCharInt::U16(_) => 16,
                    super::semantic_analyzer::type_check::SmallestCharInt::U32(_) => 32,
                };
                let ty = Type::Unsigned(bytes);
                let var = assembler.var(ty);
                assembler.assign(
                    var.clone(),
                    Operand::const_unsigned(c.value().to_string(), bytes),
                );
                Some(var)
            }
            ast::Litral::BoolTrue(_) => {
                let ty = Type::Unsigned(32);
                let var = assembler.var(ty);
                assembler.assign(var.clone(), Operand::const_bool(true));
                Some(var)
            }
            ast::Litral::BoolFalse(_) => {
                let ty = Type::Signed(32);
                let var = assembler.var(ty);
                assembler.assign(var.clone(), Operand::const_bool(false));
                Some(var)
            }
        }
    }
}

impl Lowerable for ExprAssignment {
    fn lower(
        &self,
        assembler: &mut AssemblerBuilder,
        ctx: &mut LoweringContext,
    ) -> Option<Variable> {
        let lhs = if self.left.is_addressable() {
            let Some(addr) = self.left.lower_to_address(assembler, ctx) else {
                panic!("Failed to return variable from expr lowering on assignment for left side");
            };

            match addr {
                AddressableVar::Var(variable) => variable,
                AddressableVar::Address(address) => {
                    let Some(rhs) = self.right.lower(assembler, ctx) else {
                        panic!(
                            "Failed to return variable from expr lowering on assignment for right side"
                        );
                    };

                    assembler.elemset(address.variable.clone(), address.offset, rhs);

                    return Some(address.variable);
                }
            }
        } else {
            let Some(lhs) = self.left.lower(assembler, ctx) else {
                panic!("Failed to return variable from expr lowering on assignment for left side");
            };
            lhs
        };

        let Some(rhs) = self.right.lower(assembler, ctx) else {
            panic!("Failed to return variable from expr lowering on assignment for right side");
        };

        assembler.assign(lhs.clone(), rhs);

        Some(lhs)
    }
}

impl Lowerable for ExprDecl {
    fn lower(
        &self,
        assembler: &mut AssemblerBuilder,
        ctx: &mut LoweringContext,
    ) -> Option<Variable> {
        let ast::ExprDecl {
            let_token: _,
            mutable: _,
            ident,
            expr,
            ty,
        } = self;

        let ty = if let Some(symbol) = ctx.symbol_table.get(&ident.lexeme) {
            symbol.ty.as_bitbox_type(&ctx.target)
        } else {
            ty.as_ref()
                .unwrap_or(&ast::Type {
                    kind: ast::TypeKind::default(),
                    span: expr.span(),
                    ..Default::default()
                })
                .as_bitbox_type(&ctx.target)
        };
        let Some(src) = expr.lower(assembler, ctx) else {
            panic!("Failed to return variable from expr lowering\n{self:#?}");
        };

        let des = Variable::new(ident.lexeme.clone(), ty);

        assembler.assign(des.clone(), src);

        ctx.push(des.clone());
        Some(des)
    }
}

impl Lowerable for ExprCall {
    fn lower(
        &self,
        assembler: &mut AssemblerBuilder,
        ctx: &mut LoweringContext,
    ) -> Option<Variable> {
        let name = match self.caller.as_ref() {
            ast::Expr::Identifier(ident) => match &ident.kind {
                TokenKind::Builtin(builtin) => match builtin {
                    super::lexer::token::Builtin::SizeOf => {
                        let number_bytes = ctx.target.target_pointer_size();
                        let var = assembler.var(Type::Unsigned(number_bytes));
                        let ty = &self.type_args.as_ref().unwrap()[0];
                        let size_of_type = match &ty.kind {
                            TypeKind::Name(name) => {
                                let Some(symbol) = ctx.symbol_table.get(&name.lexeme) else {
                                    panic!("unknown symbol {}", name.lexeme);
                                };

                                symbol.ty.size(&ctx.target)
                            }
                            _ => ty.size(&ctx.target),
                        };

                        assembler.assign(
                            var.clone(),
                            Operand::const_unsigned(size_of_type.to_string(), number_bytes),
                        );
                        return Some(var);
                    }
                },
                _ => &ident.lexeme,
            },
            ast::Expr::Path(path) => &path.leaf().lexeme,
            _ => panic!("Caller must be an identifier or path"),
        };

        let Some(symbol) = ctx.symbol_table.get(name).cloned() else {
            panic!("Symbol not found {}", name);
        };

        let callee = symbol.binding_name.as_ref().unwrap_or(name).clone();
        let ty = symbol.ty.as_bitbox_type(&ctx.target);
        let args: Vec<Operand> = self
            .args
            .iter()
            .zip(symbol.params.unwrap_or_default().iter())
            .filter_map(|(arg, param_ty)| {
                let var = arg.lower(assembler, ctx)?;

                match (&var.ty.de_ref(), &param_ty.de_ref().kind) {
                    (Type::Array(len, elem), ast::TypeKind::Slice(_)) => {
                        // Pass pointer to the first element and length for array-to-slice coercion
                        let data_ptr = assembler.var(Type::Pointer(elem.clone()));
                        assembler.ref_of(data_ptr.clone(), var.clone());

                        // HACK: Probably should check the calling convention and only do this for extern functions, but for now we just check if the symbol is an extern function
                        if symbol.kind == crate::stage::semantic_analyzer::symbol_table::SymbolKind::ExternFunction {
                            return Some(data_ptr.into());
                        }

                        let len_var = assembler.var(Type::Signed(32));
                        assembler.assign(
                            len_var.clone(),
                            Operand::ConstantInt(ConstantInt::new(
                                len.to_string(),
                                Type::Signed(32),
                            )),
                        );

                        let slice_struct_ty = Type::Struct(StructType {
                            name: format!("slice_{elem:?}"),
                            packed: false,
                            fields: vec![
                                ("data".into(), Type::Pointer(elem.clone())),
                                ("len".into(), Type::Signed(ctx.target.target_pointer_size())),
                            ],
                        });

                        let slice_val = assembler.var(slice_struct_ty.clone());

                        assembler.alloc(
                            slice_struct_ty.clone(),
                            slice_val.clone(),
                            Operand::const_signed(slice_struct_ty.size(&ctx.target).to_string(), ctx.target.target_pointer_size()),
                        );

                        assembler.elemset(
                            slice_val.clone(),
                            Operand::const_signed("0", 32),
                            Operand::from(data_ptr),
                        );
                        assembler.elemset(
                            slice_val.clone(),
                            Operand::const_signed("1", 32),
                            Operand::from(len_var),
                        );

                        let slice_ptr = assembler.var(Type::Pointer(Box::new(slice_struct_ty)));
                        assembler.ref_of(slice_ptr.clone(), slice_val);
                        Some(slice_ptr.into())
                    }
                    _ => Some(var.into()),
                }
            })
            .collect();

        let des_var = if ty == Type::Void {
            None
        } else {
            let des = assembler.var(ty.clone());
            Some(des)
        };

        if [
            "syscall1", "syscall2", "syscall3", "syscall4", "syscall5", "syscall6",
        ]
        .contains(&callee.as_str())
        {
            assembler.syscall(des_var.clone(), args);
            return des_var;
        }

        assembler.call(des_var.clone(), callee, &args);

        des_var
    }
}

impl Lowerable for ExprArrayIndex {
    fn lower(
        &self,
        assembler: &mut AssemblerBuilder,
        ctx: &mut LoweringContext,
    ) -> Option<Variable> {
        let ptr = self.expr.lower(assembler, ctx)?;
        let index = self.index.lower(assembler, ctx)?;

        match &ptr.ty {
            // [T; N]
            Type::Array(_, elem_ty) => {
                let dst = assembler.var(*elem_ty.clone());
                assembler.elemget(dst.clone(), ptr, index);
                Some(dst)
            }

            // &[T]
            Type::Pointer(inner)
                if matches!(
                    **inner,
                    Type::Struct(ref s)
                    if s.name.starts_with("slice_")
                ) =>
            {
                let Type::Struct(slice_struct) = inner.de_ref() else {
                    unreachable!()
                };
                let (_, data_ty) = slice_struct.fields[0].clone();
                let (_, len_ty) = slice_struct.fields[1].clone();

                // load .data
                let data_index =
                    Operand::ConstantInt(ConstantInt::new(0.to_string(), Type::Signed(32)));
                let data = assembler.var(data_ty);
                assembler.elemget(data.clone(), ptr.clone(), data_index);

                // load .len
                let len_index =
                    Operand::ConstantInt(ConstantInt::new(1.to_string(), Type::Signed(32)));
                let len = assembler.var(len_ty);
                assembler.elemget(len, ptr.clone(), len_index);

                // assembler.bounds_check(index.clone(), len);

                let elem_ty = match data.ty {
                    Type::Pointer(ref elem) => (**elem).clone(),
                    ty => unreachable!("{ty:#?}"),
                };

                let dst = assembler.var(elem_ty);

                assembler.elemget(dst.clone(), data, index);

                Some(dst)
            }

            // *T — raw pointer to elements. Index directly off the pointer.
            Type::Pointer(elem_ty) => {
                let dst = assembler.var((**elem_ty).clone());
                assembler.elemget(dst.clone(), ptr, index);
                Some(dst)
            }

            other => {
                panic!("Cannot index {:?}", other)
            }
        }
    }
}

impl Addressable for ExprArrayIndex {
    fn lower_to_address(
        &self,
        assembler: &mut AssemblerBuilder,
        ctx: &mut LoweringContext,
    ) -> Option<AddressableVar> {
        let Some(variable) = self.expr.lower(assembler, ctx) else {
            panic!("Failed to return variable from ExprArrayIndex expr lowering");
        };

        let Some(offset) = self.index.lower(assembler, ctx) else {
            panic!("Failed to return variable from ExprArrayIndex index lowering");
        };

        // Assignable containers: fixed arrays, slices, and raw pointers (`*T`).
        match &self.ty.kind {
            ast::TypeKind::Array(_, _) | ast::TypeKind::Slice(_) | ast::TypeKind::Pointer(_) => {}
            other => panic!("Expected indexable type but got {other:?}"),
        };
        let address = Address {
            variable,
            offset: offset.into(),
        };
        let var = AddressableVar::Address(address);
        Some(var)
    }
}

impl Lowerable for ExprArrayRepeat {
    fn lower(
        &self,
        assembler: &mut AssemblerBuilder,
        ctx: &mut LoweringContext,
    ) -> Option<Variable> {
        let ast::TypeKind::Array(count, _) = &self.ty.kind else {
            panic!("Expected array type but got {}", self.ty);
        };
        let count = *count;
        // Allocate the full array type inline (count=1 under the `count * ty.size()`
        // alloc contract); each iteration copies a freshly-lowered element in.
        let full_ty = self.ty.as_bitbox_type(&ctx.target);
        let ptr = assembler.var(full_ty.clone());
        assembler.alloc(
            full_ty.clone(),
            ptr.clone(),
            Operand::from(ConstantInt::new("1".to_string(), Type::Signed(32))),
        );
        for index in 0..count {
            let Some(value) = self.value.lower(assembler, ctx) else {
                panic!("Failed to return variable from expr lowering in Array");
            };
            assembler.elemset(
                ptr.clone(),
                Operand::from(ConstantInt::new(index.to_string(), Type::Signed(32))),
                value,
            );
        }

        Some(ptr)
    }
}

impl Lowerable for ExprWhile {
    fn lower(
        &self,
        assembler: &mut AssemblerBuilder,
        ctx: &mut LoweringContext,
    ) -> Option<Variable> {
        let mut var = vec![];

        #[cfg(not(feature = "uuids"))]
        let counter = assembler.counter();
        #[cfg(not(feature = "uuids"))]
        let label_counter = assembler.label_counter();

        let mut cond_blocks = vec![];
        let mut cond_assembler = AssemblerBuilder::new(&mut cond_blocks, &mut var);

        #[cfg(not(feature = "uuids"))]
        cond_assembler
            .with_counter(counter)
            .with_label_counter(label_counter);

        cond_assembler.create_block("cond");
        let Some(cond_result) = self.condition.lower(&mut cond_assembler, ctx) else {
            panic!("Condition should produce a variable");
        };

        #[cfg(not(feature = "uuids"))]
        let counter = cond_assembler.counter();
        #[cfg(not(feature = "uuids"))]
        let label_counter = cond_assembler.label_counter();

        var.clear();
        let mut loop_blocks = vec![];
        let mut loop_assembler = AssemblerBuilder::new(&mut loop_blocks, &mut var);

        #[cfg(not(feature = "uuids"))]
        loop_assembler
            .with_counter(counter)
            .with_label_counter(label_counter);

        loop_assembler.create_block("loop_body");
        let _then_block_result = self.body.lower(&mut loop_assembler, ctx);

        #[cfg(not(feature = "uuids"))]
        assembler
            .with_counter(loop_assembler.counter())
            .with_label_counter(loop_assembler.label_counter());

        assembler.loop_(cond_blocks, cond_result, loop_blocks);
        None
    }
}

impl Lowerable for ExprAddressOf {
    fn lower(
        &self,
        assembler: &mut AssemblerBuilder,
        ctx: &mut LoweringContext,
    ) -> Option<Variable> {
        let src_var = self.expr.lower(assembler, ctx)?;
        let ptr_ty = ir::Type::Pointer(Box::new(src_var.ty.clone()));
        let des = assembler.var(ptr_ty);
        assembler.ref_of(des.clone(), src_var);
        Some(des)
    }
}

impl Lowerable for ExprDeref {
    fn lower(
        &self,
        assembler: &mut AssemblerBuilder,
        ctx: &mut LoweringContext,
    ) -> Option<Variable> {
        let ptr = self.base.lower(assembler, ctx)?;
        let ir::Type::Pointer(inner) = &ptr.ty else {
            panic!("Expected pointer type in Deref, found {}", ptr.ty);
        };
        let des = assembler.var(inner.as_ref().clone());
        assembler.load(des.clone(), ptr);
        Some(des)
    }
}

impl Lowerable for ExprNot {
    fn lower(
        &self,
        assembler: &mut AssemblerBuilder,
        ctx: &mut LoweringContext,
    ) -> Option<Variable> {
        let src_var = self.expr.lower(assembler, ctx)?;
        let des = assembler.var(Type::Unsigned(32));
        assembler.not(des.clone(), src_var);
        Some(des)
    }
}

impl Lowerable for ExprGrouping {
    fn lower(
        &self,
        assembler: &mut AssemblerBuilder,
        ctx: &mut LoweringContext,
    ) -> Option<Variable> {
        let des = self.expr.lower(assembler, ctx)?;
        Some(des)
    }
}

impl Lowerable for ExprTypeCast {
    fn lower(
        &self,
        assembler: &mut AssemblerBuilder,
        ctx: &mut LoweringContext,
    ) -> Option<Variable> {
        use std::cmp::Ordering;
        let src_var = self.expr.lower(assembler, ctx)?;
        let des_ty = self.target_type.as_bitbox_type(&ctx.target);
        let src_ty = src_var.ty.clone();
        let des = assembler.var(des_ty.clone());
        let cast_kind = match (des_ty, src_ty) {
            (Type::Signed(des_bits), Type::Unsigned(src_bits)) => match des_bits.cmp(&src_bits) {
                Ordering::Equal => {
                    assembler.cast(des.clone(), src_var, ir::CastKind::UnsignedToSigned);
                    return Some(des);
                }
                Ordering::Greater => {
                    let mid = assembler.var(Type::Unsigned(des_bits));
                    assembler.cast(mid.clone(), src_var, ir::CastKind::ZeroExtend);
                    assembler.cast(des.clone(), mid, ir::CastKind::UnsignedToSigned);
                    return Some(des);
                }
                Ordering::Less => {
                    let mid = assembler.var(Type::Signed(des_bits));
                    assembler.cast(mid.clone(), src_var, ir::CastKind::Truncate);
                    assembler.cast(des.clone(), mid, ir::CastKind::UnsignedToSigned);
                    return Some(des);
                }
            },
            (Type::Unsigned(des_bits), Type::Signed(src_bits)) => match des_bits.cmp(&src_bits) {
                Ordering::Equal => {
                    assembler.cast(des.clone(), src_var, ir::CastKind::SignedToUnsigned);
                    return Some(des);
                }
                Ordering::Greater => {
                    let mid = assembler.var(Type::Signed(des_bits));
                    assembler.cast(mid.clone(), src_var, ir::CastKind::SignExtend);
                    assembler.cast(des.clone(), mid, ir::CastKind::SignedToUnsigned);
                    return Some(des);
                }
                Ordering::Less => {
                    let mid = assembler.var(Type::Signed(des_bits));
                    assembler.cast(mid.clone(), src_var, ir::CastKind::Truncate);
                    assembler.cast(des.clone(), mid, ir::CastKind::SignedToUnsigned);
                    return Some(des);
                }
            },
            (Type::Float(_), Type::Signed(_)) => ir::CastKind::IntToFloat,
            (Type::Float(_), Type::Unsigned(_)) => ir::CastKind::IntToFloat,
            (Type::Signed(_), Type::Float(_)) => ir::CastKind::FloatToInt,
            (Type::Unsigned(_), Type::Float(_)) => ir::CastKind::FloatToInt,
            (Type::Unsigned(src_bytes), Type::Unsigned(des_bytes)) if des_bytes < src_bytes => {
                ir::CastKind::Truncate
            }
            (Type::Unsigned(src_bytes), Type::Unsigned(des_bytes)) if des_bytes > src_bytes => {
                ir::CastKind::ZeroExtend
            }
            (Type::Signed(des_bits), Type::Signed(src_bits)) => match des_bits.cmp(&src_bits) {
                Ordering::Equal => ir::CastKind::NoOp,
                Ordering::Greater => ir::CastKind::SignExtend,
                Ordering::Less => ir::CastKind::Truncate,
            },
            (Type::Pointer(_), Type::Unsigned(_))
            | (Type::Pointer(_), Type::Signed(_))
            | (Type::Unsigned(_), Type::Pointer(_))
            | (Type::Signed(_), Type::Pointer(_)) => ir::CastKind::BitCast,
            (dst, src) if dst == src => ir::CastKind::NoOp,
            _ => unimplemented!("Unsupported cast from {:?} to {:?}", src_var.ty, des.ty),
        };
        assembler.cast(des.clone(), src_var, cast_kind);
        Some(des)
    }
}
