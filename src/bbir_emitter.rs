use crate::lexer::token::TokenKind;
use crate::semantic_analysis::SymbolTable;
use crate::semantic_analysis::symbol_table::SymbolTableBuilder;
use crate::{ast, semantic_analysis::type_check::TypeChecker};
use bitbox::{
    BlockBuilder, FunctionBuilder, Instruction, LabeledInstruction, ModuleBuilder, Operand, Type,
    Variable,
};

pub fn emit(ast: &mut Vec<ast::Item>) -> bitbox::Module {
    let mut symbol_table = SymbolTableBuilder::new().build(ast);
    // TODO: make this a mutable reference
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
}

impl Emitter {
    pub fn new(symbol_table: SymbolTable) -> Self {
        Self {
            symbol_table,
            ..Default::default()
        }
    }

    pub fn build(mut self, ast: &[ast::Item]) -> bitbox::Module {
        let mut module = bitbox::ModuleBuilder::default();
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

        let mut function_builder = bitbox::FunctionBuilder::new(name.lexeme.clone())
            .with_visibility(match visibility {
                ast::Visibility::Public => bitbox::Visibility::Public,
                ast::Visibility::Private => bitbox::Visibility::Private,
            })
            .with_params(
                params
                    .iter()
                    .map(|param| bitbox::Variable {
                        name: param.name.lexeme.clone(),
                        ty: param.ty.into_bitbox_type(),
                        version: 0,
                    })
                    .collect(),
            )
            .with_return_type(return_type.into_bitbox_type());

        for stmt in body.statements.iter() {
            self.walk_stmt(&mut function_builder, stmt);
        }

        self.symbol_table.exit_scope();
        mb.push_function(function_builder.build());
    }

    fn walk_type_def(&mut self, ctx: &mut ModuleBuilder, type_def: &ast::TypeDef) {
        todo!()
    }

    fn walk_struct(&mut self, ctx: &mut ModuleBuilder, struct_def: &ast::Struct) {
        todo!()
    }

    fn walk_block(&mut self, fb: &mut FunctionBuilder, block: &ast::Block) -> Option<Variable> {
        let mut return_variable: Option<Variable> = None;
        for stmt in block.statements.iter() {
            return_variable = self.walk_stmt(fb, stmt);
        }
        return_variable
    }

    fn walk_stmt(&mut self, fb: &mut FunctionBuilder, stmt: &ast::Statement) -> Option<Variable> {
        self.walk_expr(fb, &stmt.expr)
    }

    fn walk_expr(&mut self, fb: &mut FunctionBuilder, expr: &ast::Expr) -> Option<Variable> {
        match expr {
            ast::Expr::Return(expr) => self.walk_expr_return(fb, expr),
            ast::Expr::Struct(expr) => self.walk_expr_struct(fb, expr),
            ast::Expr::Assignment(expr) => self.walk_expr_assignment(fb, expr),
            ast::Expr::Litral(expr) => self.walk_expr_litral(fb, expr),
            ast::Expr::Call(expr) => self.walk_expr_call(fb, expr),
            ast::Expr::Binary(expr) => self.walk_expr_binary(fb, expr),
            ast::Expr::Identifier(expr) => self.walk_expr_identifier(fb, expr),
            ast::Expr::IfElse(expr) => self.walk_expr_if_else(fb, expr),
        }
    }

    fn walk_expr_return(
        &mut self,
        fb: &mut FunctionBuilder,
        expr: &ast::ExprReturn,
    ) -> Option<Variable> {
        match &expr.expr {
            Some(expr) => {
                return self.walk_expr(fb, &expr);
            }
            None => {
                fb.block(|bb| {
                    bb.r#return_void();
                });
                None
            }
        }
    }

    fn walk_expr_struct(
        &mut self,
        ctx: &mut FunctionBuilder,
        expr: &ast::ExprStruct,
    ) -> Option<Variable> {
        todo!()
    }

    fn walk_expr_assignment(
        &mut self,
        fb: &mut FunctionBuilder,
        expr: &ast::ExprAssignment,
    ) -> Option<Variable> {
        let ty = expr
            .ty
            .as_ref()
            .map_or(Type::Void, |ty| ty.into_bitbox_type());
        self.walk_expr(fb, &expr.expr);

        let mut return_variable: Option<Variable> = None;
        fb.block(|bb| {
            let src_var = bb
                .get_temp_var()
                .expect("Failed to get temp var in walk_expr_assignment");
            let dest_var = bb.temp_var(ty);
            bb.assign(dest_var.clone(), Operand::Variable(src_var.clone()));
            return_variable = Some(dest_var);
        });

        return_variable
    }

    fn walk_expr_litral(
        &mut self,
        fb: &mut FunctionBuilder,
        expr: &ast::Litral,
    ) -> Option<Variable> {
        let mut return_variable: Option<Variable> = None;
        match expr {
            ast::Litral::String(token) => todo!("STRING"),
            ast::Litral::Integer(token) => {
                fb.block(|bb| {
                    let ty = Type::Unsigned(32);
                    let var = bb.temp_var(ty.clone());
                    let value = token.lexeme.clone();
                    bb.assign(var.clone(), Operand::ConstantInt { value, ty });
                    return_variable = Some(var);
                });
            }
            ast::Litral::Float(token) => {
                fb.block(|bb| {
                    let ty = Type::Float(32);
                    let var = bb.temp_var(ty.clone());
                    let value = token.lexeme.clone();
                    bb.assign(var.clone(), Operand::ConstantInt { value, ty });
                    return_variable = Some(var);
                });
            }
            ast::Litral::Char(token) => {
                fb.block(|bb| {
                    let ty = Type::Unsigned(8);
                    let var = bb.temp_var(ty.clone());
                    let value = token.lexeme.clone();
                    bb.assign(var.clone(), Operand::ConstantInt { value, ty });
                    return_variable = Some(var);
                });
            }
            ast::Litral::BoolTrue(_) => {
                fb.block(|bb| {
                    let ty = Type::Unsigned(32);
                    let var = bb.temp_var(ty.clone());
                    bb.assign(var.clone(), Operand::const_bool(true));
                    return_variable = Some(var);
                });
            }
            ast::Litral::BoolFalse(token) => {
                fb.block(|bb| {
                    let ty = Type::Unsigned(32);
                    let var = bb.temp_var(ty.clone());
                    bb.assign(var.clone(), Operand::const_bool(false));
                    return_variable = Some(var);
                });
            }
        }

        return_variable
    }

    fn walk_expr_call(
        &mut self,
        fb: &mut FunctionBuilder,
        expr: &ast::ExprCall,
    ) -> Option<Variable> {
        let args = expr
            .args
            .iter()
            .map(|arg| {
                let Some(arg_var) = self.walk_expr(fb, arg) else {
                    panic!("Failed to walk expr in walk_expr_call");
                };
                Operand::Variable(arg_var)
            })
            .collect();

        // HACK: This works for now but later you probably would want to be able to call
        // (lambda x: x + 1)(2)
        // but thats if we want to support lambdas and I dont think we do
        let ast::Expr::Identifier(ident) = expr.caller.as_ref() else {
            panic!("Caller must be an identifier");
        };
        let Some(symbol) = self.symbol_table.get(&ident.lexeme) else {
            panic!("Symbol not found {}", ident.lexeme);
        };
        let ty = symbol.ty.into_bitbox_type();
        let mut return_variable: Option<Variable> = None;
        fb.block(|bb| {
            let dest_var = bb.temp_var(ty);
            bb.call(dest_var, ident.lexeme.clone(), args);
            return_variable = bb.get_temp_var();
        });

        return_variable
    }

    fn walk_expr_binary(
        &mut self,
        bb: &mut FunctionBuilder,
        expr: &ast::ExprBinary,
    ) -> Option<Variable> {
        todo!()
        // let ast::ExprBinary { left, op, right } = &expr;
        //
        // let Some(left_var) = self.walk_expr(bb, left) else {
        //     panic!("Failed to walk expr in walk_expr_binary");
        // };
        // let Some(right_var) = self.walk_expr(bb, right) else {
        //     panic!("Failed to walk expr in walk_expr_binary");
        // };
        //
        // let dest_var = self.fresh_variable(None, Type::Unsigned(32));
        //
        // match &op.kind {
        //     TokenKind::Plus => {
        //         bb.push(
        //             Instruction::Add(
        //                 dest_var.clone(),
        //                 Operand::Variable(left_var),
        //                 Operand::Variable(right_var),
        //             )
        //             .into(),
        //         );
        //         Some(dest_var)
        //     }
        //     TokenKind::Minus => {
        //         bb.push(
        //             Instruction::Sub(
        //                 dest_var.clone(),
        //                 Operand::Variable(left_var),
        //                 Operand::Variable(right_var),
        //             )
        //             .into(),
        //         );
        //         Some(dest_var)
        //     }
        //     TokenKind::Star => {
        //         bb.push(
        //             Instruction::Mul(
        //                 dest_var.clone(),
        //                 Operand::Variable(left_var),
        //                 Operand::Variable(right_var),
        //             )
        //             .into(),
        //         );
        //         Some(dest_var)
        //     }
        //     TokenKind::Slash => {
        //         bb.push(
        //             Instruction::Div(
        //                 dest_var.clone(),
        //                 Operand::Variable(left_var),
        //                 Operand::Variable(right_var),
        //             )
        //             .into(),
        //         );
        //         Some(dest_var)
        //     }
        //     _ => unreachable!("{:?}", op.kind),
        // }
    }

    fn walk_expr_identifier(
        &mut self,
        fb: &mut FunctionBuilder,
        expr: &crate::lexer::token::Token,
    ) -> Option<Variable> {
        let Some(symbol) = self.symbol_table.get(&expr.lexeme) else {
            panic!("Symbol not found {}", expr.lexeme);
        };

        Some(Variable {
            name: expr.lexeme.clone(),
            ty: symbol.ty.into_bitbox_type(),
            version: 0,
        })
    }

    fn walk_expr_if_else(
        &mut self,
        fb: &mut FunctionBuilder,
        expr: &ast::ExprIfElse,
    ) -> Option<Variable> {
        let ast::ExprIfElse {
            condition,
            then_branch,
            else_branch,
            ty,
        } = expr;

        let Some(condition_var) = self.walk_expr(fb, condition) else {
            panic!("Failed to walk condition in if expression");
        };

        let then_label = fb.new_label(Some("then"));
        let else_label = fb.new_label(Some("esle"));
        let exit_label = fb.new_label(Some("exit"));

        let result_var = fb.var(ty.into_bitbox_type());

        fb.block(|bb| {
            let jump_condition = bb.temp_var(Type::Unsigned(32));
            bb.cmp(
                jump_condition.clone(),
                condition_var,
                Operand::const_bool(true),
            );
            bb.jump_if(jump_condition, then_label.clone());
            bb.jump(else_label.clone());
            bb.label(then_label);
        });

        if let Some(value) = self.walk_block(fb, then_branch) {
            fb.block(|bb| {
                bb.assign(result_var.clone(), Operand::Variable(value));
                bb.jump(exit_label.clone());
            });
        }

        fb.block(|bb| {
            bb.label(else_label);
        });

        if let Some(else_branch) = else_branch {
            if let Some(value) = self.walk_block(fb, else_branch) {
                fb.block(|bb| {
                    bb.assign(result_var.clone(), Operand::Variable(value));
                });
            }
        }

        fb.block(|bb| {
            bb.label(exit_label);
        });

        Some(result_var)
    }
}
