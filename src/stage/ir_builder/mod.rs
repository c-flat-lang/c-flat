#![allow(unused)]
use super::Stage;
use crate::stage::lexer::token::{Token, TokenKind};
use crate::stage::semantic_analyzer::symbol_table::SymbolTable;
use crate::{error::CompilerError, stage::parser::ast, stage::parser::ast::Item};
use bitbeat::Instruction;
use bitbox::ir::builder::{AssemblerBuilder, FunctionBuilder, ModuleBuilder};
use bitbox::ir::{Constant, Module, Operand, Type, Variable, Visibility};

#[derive(Debug, Default)]
pub struct IRBuilder {
    symbol_table: SymbolTable,
    variable_stack: Vec<Variable>,
}

impl IRBuilder {
    fn push(&mut self, var: Variable) {
        self.variable_stack.push(var);
    }

    fn pop(&mut self) -> Option<Variable> {
        self.variable_stack.pop()
    }

    fn build_function(&mut self, function: ast::Function, mb: &mut ModuleBuilder) {
        let ast::Function {
            visibility,
            fn_token: _,
            name,
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
                    .map(|param| Variable {
                        name: param.name.lexeme.clone(),
                        ty: param.ty.as_bitbox_type(),
                        version: 0,
                    })
                    .collect(),
            )
            .with_return_type(return_type.as_bitbox_type());

        let mut ctx = LoweringContext::new(&mut self.symbol_table);

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

impl Stage<(SymbolTable, Vec<Item>), Result<Module, CompilerError>> for IRBuilder {
    fn run(
        &mut self,
        (symbol_table, ast): (SymbolTable, Vec<Item>),
    ) -> Result<Module, CompilerError> {
        self.symbol_table = symbol_table;
        let mut mb = ModuleBuilder::default();
        for item in ast {
            match item {
                Item::Function(function) => self.build_function(function, &mut mb),
                Item::Type(_) => todo!("type def"),
                Item::Use(_) => todo!("use"),
            }
        }
        Ok(mb.build())
    }
}

pub struct LoweringContext<'a> {
    symbol_table: &'a mut SymbolTable,
    variable_stack: Vec<Variable>,
}

impl<'a> LoweringContext<'a> {
    pub fn new(symbol_table: &'a mut SymbolTable) -> Self {
        Self {
            symbol_table,
            variable_stack: Vec::new(),
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
    offset: Variable,
    size: usize,
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

use crate::stage::parser::ast::{
    Block, Expr, ExprArray, ExprArrayIndex, ExprAssignment, ExprBinary, ExprCall, ExprDecl,
    ExprIfElse, ExprReturn, Litral,
};

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
            Expr::Call(call) => call.lower(assembler, ctx),
            Expr::Identifier(ident) => {
                let Some(_) = ctx.symbol_table.get(&ident.lexeme) else {
                    panic!("Symbol not found {}", ident.lexeme);
                };
                ctx.get_variable(&ident.lexeme)
            }
            Expr::Struct(_) => todo!("Struct expressions"),
            Expr::Array(expr) => expr.lower(assembler, ctx),
            Expr::ArrayIndex(expr) => expr.lower(assembler, ctx),
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
            _ => unreachable!(),
        }
    }
}

impl Lowerable for ExprArray {
    fn lower(
        &self,
        assembler: &mut AssemblerBuilder,
        ctx: &mut LoweringContext,
    ) -> Option<Variable> {
        let ty = self.ty.clone().as_bitbox_type();
        let size = Operand::ConstantInt {
            value: format!("{}", self.elements.len() * (ty.size() as usize)),
            ty: Type::Signed(32),
        };
        let ptr = assembler.var(ty.clone());
        assembler.alloc(ty.clone(), ptr.clone(), size);
        for (index, element) in self.elements.iter().enumerate() {
            let Some(value) = element.lower(assembler, ctx) else {
                panic!("Failed to return variable from expr lowering in Array");
            };
            assembler.elemset(
                ptr.clone(),
                Operand::ConstantInt {
                    value: format!("{index}"),
                    ty: Type::Signed(32),
                },
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
            TokenKind::Plus => assembler.add(des.clone(), lhs, rhs),
            TokenKind::Minus => assembler.sub(des.clone(), lhs, rhs),
            TokenKind::Star => assembler.mul(des.clone(), lhs, rhs),
            TokenKind::Slash => assembler.div(des.clone(), lhs, rhs),
            TokenKind::Greater => assembler.gt(des.clone(), lhs, rhs),
            TokenKind::Less => assembler.lt(des.clone(), lhs, rhs),
            TokenKind::EqualEqual => assembler.eq(des.clone(), lhs, rhs),
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
        let ExprIfElse {
            condition,
            then_branch,
            else_branch,
            ty,
        } = self;

        let result = if ty == &ast::Type::Void {
            None
        } else {
            let ty = ty.as_bitbox_type();
            let return_value = assembler.var(ty.clone());
            // assembler.alloc(ty, return_value.clone());
            Some(return_value)
        };

        let mut var = vec![];

        let mut cond_blocks = vec![];
        let mut cond_assembler = AssemblerBuilder::new(&mut cond_blocks, &mut var);
        cond_assembler.create_block("cond");
        let Some(cond_result) = condition.lower(&mut cond_assembler, ctx) else {
            panic!("Condition should produce a variable");
        };

        var.clear();
        let mut then_blocks = vec![];
        let mut then_assembler = AssemblerBuilder::new(&mut then_blocks, &mut var);
        then_assembler.create_block("then_branch");
        let then_block_result = then_branch.lower(&mut then_assembler, ctx);

        if let (Some(then_block_result), Some(result)) = (then_block_result, result.clone()) {
            then_assembler.assign(result, then_block_result);
        }

        let mut else_blocks = vec![];
        if let Some(else_branch) = else_branch {
            var.clear();
            let mut else_assembler = AssemblerBuilder::new(&mut else_blocks, &mut var);
            else_assembler.create_block("else_branch");
            let else_block_result = else_branch.lower(&mut else_assembler, ctx);

            if let Some(else_block_result) = else_block_result {
                else_assembler.assign(result.clone().unwrap(), else_block_result);
            }
        }

        assembler.if_else(
            cond_blocks,
            cond_result,
            then_blocks,
            else_blocks,
            result.clone(),
        );

        result
    }
}

impl Lowerable for Block {
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
            ast::Litral::String(_) => todo!("STRING"),
            ast::Litral::Integer(token) => {
                let ty = Type::Unsigned(32);
                let var = assembler.var(ty);
                assembler.assign(var.clone(), Operand::const_unsigned(&token.lexeme, 32));
                Some(var)
            }
            ast::Litral::Float(token) => {
                let ty = Type::Float(32);
                let var = assembler.var(ty);
                assembler.assign(var.clone(), Operand::const_float(&token.lexeme, 32));
                Some(var)
            }
            ast::Litral::Char(token) => {
                let ty = Type::Unsigned(8);
                let var = assembler.var(ty);
                assembler.assign(var.clone(), Operand::const_unsigned(&token.lexeme, 8));
                Some(var)
            }
            ast::Litral::BoolTrue(_) => {
                let ty = Type::Unsigned(32);
                let var = assembler.var(ty);
                assembler.assign(var.clone(), Operand::const_bool(true));
                Some(var)
            }
            ast::Litral::BoolFalse(_) => {
                let ty = Type::Unsigned(32);
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
            symbol.ty.clone().as_bitbox_type()
        } else {
            ty.clone().unwrap_or_default().as_bitbox_type()
        };
        let Some(src) = expr.lower(assembler, ctx) else {
            panic!("Failed to return variable from expr lowering");
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
        // TODO: We may need to reverse the args
        let args: Vec<Operand> = self
            .args
            .iter()
            .filter_map(|arg| arg.lower(assembler, ctx).map(|var| var.into()))
            .collect();

        // HACK: This works for now but later you probably would want to be able to call
        // (lambda x: x + 1)(2)
        // but thats if we want to support lambdas and I dont think we do
        let ast::Expr::Identifier(ident) = self.caller.as_ref() else {
            panic!("Caller must be an identifier");
        };

        let ty = if let Some(symbol) = ctx.symbol_table.get(&ident.lexeme) {
            symbol.ty.as_bitbox_type()
        } else if ident.lexeme == "print" {
            Type::Void
        } else {
            panic!("Symbol not found {}", ident.lexeme);
        };
        let des_var = if ty == Type::Void {
            None
        } else {
            let des = assembler.var(ty.clone());
            Some(des)
        };
        assembler.call(des_var.clone(), ident.lexeme.clone(), &args);

        des_var
    }
}

impl Lowerable for ExprArrayIndex {
    fn lower(
        &self,
        assembler: &mut AssemblerBuilder,
        ctx: &mut LoweringContext,
    ) -> Option<Variable> {
        let Some(ptr) = self.expr.lower(assembler, ctx) else {
            panic!("Failed to return variable from expr lowering");
        };
        let Some(index) = self.index.lower(assembler, ctx) else {
            panic!("Failed to return variable from expr lowering");
        };
        let Type::Array(_, ty) = ptr.ty.clone() else {
            panic!("Expected array type");
        };
        let des = assembler.var(*ty);
        assembler.elemget(des.clone(), ptr, index);
        Some(des)
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

        let ast::Type::Array(_, ty) = self.ty.clone() else {
            panic!("Expected array type but got {}", self.ty);
        };
        let address = Address {
            variable,
            offset,
            size: ty.size() as usize,
        };
        let var = AddressableVar::Address(address);
        Some(var)
    }
}
