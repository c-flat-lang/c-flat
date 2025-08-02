use crate::lexer::token::TokenKind;
use crate::semantic_analysis::SymbolTable;
use crate::semantic_analysis::symbol_table::SymbolTableBuilder;
use crate::{ast, semantic_analysis::type_check::TypeChecker};
use bitbox::ir::{
    FunctionBuilder, Instruction, InstructionBuilder, LabeledInstruction, Module, ModuleBuilder,
    Operand, Type, Variable, Visibility,
};

pub fn emit(ast: &mut Vec<ast::Item>) -> Module {
    let mut symbol_table = match SymbolTableBuilder::new().build(ast) {
        Ok(table) => table,
        Err(errors) => {
            for error in errors {
                eprintln!("{}", error);
            }
            std::process::exit(1);
        }
    };

    if let Err(errors) = TypeChecker::new(&mut symbol_table).check(ast) {
        for error in errors {
            eprintln!("{}", error);
        }
        std::process::exit(1);
    }
    Emitter::new(symbol_table).build(ast)
}

#[derive(Default)]
pub struct Emitter {
    symbol_table: SymbolTable,
    variable_counter: usize,
    label_counter: usize,
    variable_stack: Vec<Variable>,
}

impl Emitter {
    pub fn new(symbol_table: SymbolTable) -> Self {
        Self {
            symbol_table,
            ..Default::default()
        }
    }

    fn push(&mut self, var: Variable) {
        self.variable_stack.push(var);
    }

    fn pop(&mut self) -> Option<Variable> {
        self.variable_stack.pop()
    }

    pub fn build(mut self, ast: &[ast::Item]) -> Module {
        let mut module = ModuleBuilder::default();
        for item in ast {
            self.walk_item(&mut module, &item);
        }
        module.build()
    }

    fn walk_item(&mut self, mb: &mut ModuleBuilder, item: &ast::Item) {
        match item {
            ast::Item::Function(function) => self.walk_function(mb, function),
            ast::Item::Type(type_def) => self.walk_type_def(mb, type_def),
            ast::Item::Use(r#use) => self.walk_use(mb, r#use),
        }
    }

    fn walk_use(&mut self, mb: &mut ModuleBuilder, item: &ast::Use) {
        todo!()
    }

    fn walk_function(&mut self, mb: &mut ModuleBuilder, function: &ast::Function) {
        let ast::Function {
            visibility,
            fn_token,
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
                        ty: param.ty.into_bitbox_type(),
                        version: 0,
                    })
                    .collect(),
            )
            .with_return_type(return_type.into_bitbox_type());

        let mut assembler = function_builder.instructions();

        self.walk_block(&mut assembler, &function.body);

        self.symbol_table.exit_scope();
        mb.push_function(function_builder.build());
    }

    fn walk_type_def(&mut self, ctx: &mut ModuleBuilder, type_def: &ast::TypeDef) {
        todo!()
    }

    fn walk_struct(&mut self, ctx: &mut ModuleBuilder, struct_def: &ast::Struct) {
        todo!()
    }

    fn walk_block(&mut self, assembler: &mut InstructionBuilder, block: &ast::Block) {
        for stmt in block.statements.iter() {
            self.walk_stmt(assembler, stmt);
        }
    }

    fn walk_stmt(&mut self, assembler: &mut InstructionBuilder, stmt: &ast::Statement) {
        self.walk_expr(assembler, &stmt.expr)
    }

    fn walk_expr(&mut self, assembler: &mut InstructionBuilder, expr: &ast::Expr) {
        match expr {
            ast::Expr::Return(expr) => self.walk_expr_return(assembler, expr),
            ast::Expr::Struct(expr) => self.walk_expr_struct(assembler, expr),
            ast::Expr::Assignment(expr) => self.walk_expr_assignment(assembler, expr),
            ast::Expr::Litral(expr) => self.walk_expr_litral(assembler, expr),
            ast::Expr::Call(expr) => self.walk_expr_call(assembler, expr),
            ast::Expr::Binary(expr) => self.walk_expr_binary(assembler, expr),
            ast::Expr::Identifier(expr) => self.walk_expr_identifier(assembler, expr),
            ast::Expr::IfElse(expr) => self.walk_expr_if_else(assembler, expr),
        }
    }

    fn walk_expr_return(&mut self, assember: &mut InstructionBuilder, expr: &ast::ExprReturn) {
        match &expr.expr {
            Some(expr) => {
                self.walk_expr(assember, &expr);
                let Some(return_variable) = self.pop() else {
                    panic!("Failed to pop variable");
                };
                assember.ret(return_variable.clone());
            }
            None => {
                assember.void_ret();
            }
        }
    }

    fn walk_expr_struct(&mut self, assembler: &mut InstructionBuilder, expr: &ast::ExprStruct) {
        todo!()
    }

    fn walk_expr_assignment(
        &mut self,
        assembler: &mut InstructionBuilder,
        expr: &ast::ExprAssignment,
    ) {
        let ty = expr.ty.clone().unwrap_or_default().into_bitbox_type();
        self.walk_expr(assembler, &expr.expr);
        let Some(src) = self.pop() else {
            panic!("Failed to pop variable");
        };
        let des = Variable::new(expr.ident.lexeme.clone(), ty);

        assembler.assign(des.clone(), src);

        self.push(des);
    }

    fn walk_expr_litral(&mut self, assembler: &mut InstructionBuilder, expr: &ast::Litral) {
        match expr {
            ast::Litral::String(token) => todo!("STRING"),
            ast::Litral::Integer(token) => {
                let ty = Type::Unsigned(32);
                let var = assembler.var(ty);
                assembler.assign(var.clone(), Operand::const_unsigned(&token.lexeme, 32));
                self.push(var);
            }
            ast::Litral::Float(token) => {
                let ty = Type::Float(32);
                let var = assembler.var(ty);
                assembler.assign(var.clone(), Operand::const_float(&token.lexeme, 32));
                self.push(var);
            }
            ast::Litral::Char(token) => {
                let ty = Type::Unsigned(8);
                let var = assembler.var(ty);
                assembler.assign(var.clone(), Operand::const_unsigned(&token.lexeme, 8));
                self.push(var);
            }
            ast::Litral::BoolTrue(_) => {
                let ty = Type::Unsigned(32);
                let var = assembler.var(ty);
                assembler.assign(var.clone(), Operand::const_bool(true));
                self.push(var);
            }
            ast::Litral::BoolFalse(token) => {
                let ty = Type::Unsigned(32);
                let var = assembler.var(ty);
                assembler.assign(var.clone(), Operand::const_bool(false));
                self.push(var);
            }
        }
    }

    fn walk_expr_call(&mut self, assembler: &mut InstructionBuilder, expr: &ast::ExprCall) {
        let mut args = vec![];
        // TODO: We may need to reverse the args
        for arg in expr.args.iter() {
            self.walk_expr(assembler, arg);
            let Some(arg_var) = self.pop() else {
                panic!("Failed to pop variable");
            };
            args.push(Operand::from(arg_var));
        }

        // HACK: This works for now but later you probably would want to be able to call
        // (lambda x: x + 1)(2)
        // but thats if we want to support lambdas and I dont think we do
        let ast::Expr::Identifier(ident) = expr.caller.as_ref() else {
            panic!("Caller must be an identifier");
        };

        let ty = if let Some(symbol) = self.symbol_table.get(&ident.lexeme) {
            symbol.ty.into_bitbox_type()
        } else if ident.lexeme == "print" {
            Type::Void
        } else {
            panic!("Symbol not found {}", ident.lexeme);
        };
        let des = assembler.var(ty);
        assembler.call(des.clone(), ident.lexeme.clone(), &args);
        self.push(des);
    }

    fn walk_expr_binary(&mut self, assembler: &mut InstructionBuilder, expr: &ast::ExprBinary) {
        let ast::ExprBinary { left, op, right } = &expr;

        self.walk_expr(assembler, left);
        let Some(lhs) = self.pop() else {
            panic!("Failed to pop variable");
        };
        self.walk_expr(assembler, right);
        let Some(rhs) = self.pop() else {
            panic!("Failed to pop variable");
        };

        // TODO: the type is not known at this point which is not good.
        let des = assembler.var(Type::Unsigned(32));
        match &op.kind {
            TokenKind::Plus => assembler.add(des.clone(), lhs, rhs),
            TokenKind::Minus => assembler.sub(des.clone(), lhs, rhs),
            TokenKind::Star => assembler.mul(des.clone(), lhs, rhs),
            TokenKind::Slash => assembler.div(des.clone(), lhs, rhs),
            TokenKind::EqualEqual => assembler.cmp(des.clone(), lhs, rhs),
            _ => unreachable!("{:?}", op.kind),
        };

        self.push(des);
    }

    fn walk_expr_identifier(
        &mut self,
        assembler: &mut InstructionBuilder,
        expr: &crate::lexer::token::Token,
    ) {
        let Some(symbol) = self.symbol_table.get(&expr.lexeme) else {
            panic!("Symbol not found {}", expr.lexeme);
        };

        let var = Variable::new(&expr.lexeme, symbol.ty.into_bitbox_type());
        self.push(var);
    }

    fn walk_expr_if_else(&mut self, assembler: &mut InstructionBuilder, expr: &ast::ExprIfElse) {
        let ast::ExprIfElse {
            condition,
            then_branch,
            else_branch,
            ty,
        } = expr;
        match ty {
            ast::Type::Void => self.walk_expr_if_else_as_statement(assembler, expr),
            _ => self.walk_expr_if_else_as_expression(assembler, expr),
        }
    }

    fn walk_expr_if_else_as_statement(
        &mut self,
        assembler: &mut InstructionBuilder,
        expr: &ast::ExprIfElse,
    ) {
        let ast::ExprIfElse {
            condition,
            then_branch,
            else_branch,
            ty,
        } = expr;

        let then_label = assembler.new_label(Some("then"));
        let else_label = assembler.new_label(Some("else"));
        let exit_label = assembler.new_label(Some("exit"));

        self.walk_expr(assembler, condition);
        let Some(condition_var) = self.pop() else {
            panic!("Failed to pop condition variable");
        };

        let jump_condition = assembler.var(Type::Unsigned(32));
        assembler.cmp(
            jump_condition.clone(),
            condition_var,
            Operand::const_bool(true),
        );
        assembler.jump_if(jump_condition, then_label.clone());

        if else_branch.is_some() {
            assembler.jump(else_label.clone());
        } else {
            assembler.jump(exit_label.clone());
        }

        assembler.label(then_label.clone());
        self.walk_block(assembler, then_branch);
        assembler.jump(exit_label.clone());

        let Some(else_branch) = else_branch else {
            assembler.label(exit_label.clone());
            return;
        };
        assembler.label(else_label.clone());
        self.walk_block(assembler, else_branch);
        assembler.jump(exit_label.clone());
        assembler.label(exit_label.clone());
    }

    fn walk_expr_if_else_as_expression(
        &mut self,
        assembler: &mut InstructionBuilder,
        expr: &ast::ExprIfElse,
    ) {
        let ast::ExprIfElse {
            condition,
            then_branch,
            else_branch,
            ty,
        } = expr;

        // Walk the condition expression
        self.walk_expr(assembler, condition);
        let Some(condition_var) = self.pop() else {
            panic!("Failed to pop condition variable");
        };

        let then_label = assembler.new_label(Some("then"));
        let else_label = assembler.new_label(Some("else"));
        let exit_label = assembler.new_label(Some("exit"));

        let jump_condition = assembler.var(Type::Unsigned(32));
        assembler.cmp(
            jump_condition.clone(),
            condition_var,
            Operand::const_bool(true),
        );
        assembler.jump_if(jump_condition, then_label.clone());

        if else_branch.is_some() {
            assembler.jump(else_label.clone());
        } else {
            assembler.jump(exit_label.clone());
        }

        // THEN branch
        assembler.label(then_label.clone());
        self.walk_block(assembler, then_branch);
        let Some(then_val) = self.pop() else {
            panic!(
                "Failed to pop variable from then branch {:#?}",
                self.variable_stack
            );
        };
        assembler.jump(exit_label.clone());

        // ELSE branch (optional)
        let else_val = if let Some(else_branch) = else_branch {
            assembler.label(else_label.clone());
            self.walk_block(assembler, else_branch);
            let Some(value) = self.pop() else {
                panic!("Failed to pop variable from else branch");
            };
            assembler.jump(exit_label.clone());
            Some((value, else_label))
        } else {
            None
        };

        // Merge block and PHI
        assembler.label(exit_label.clone());
        let result_var = assembler.var(ty.into_bitbox_type());

        let mut phi_inputs = vec![(then_val.clone(), then_label)];
        if let Some((else_val, else_lbl)) = else_val {
            phi_inputs.push((else_val, else_lbl));
        }

        if phi_inputs.len() == 1 {
            assembler.assign(result_var.clone(), Operand::Variable(then_val));
        } else {
            assembler.phi(result_var.clone(), phi_inputs);
        }

        self.push(result_var);
    }
}
