use crate::lexer::token::TokenKind;
use crate::semantic_analysis::SymbolTable;
use crate::semantic_analysis::symbol_table::SymbolTableBuilder;
use crate::{ast, semantic_analysis::type_check::TypeChecker};
use bitbox::{
    BlockBuilder, Instruction, LabeledInstruction, ModuleBuilder, Operand, Type, Variable,
};

pub fn emit(ast: &[ast::Item]) -> bitbox::Module {
    let symbol_table = SymbolTableBuilder::new().build(ast);
    // TODO: make this a mutable reference
    if let Err(errors) = TypeChecker::new(symbol_table.clone()).check(ast) {
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

    fn fresh_variable(&mut self, name: Option<&str>, ty: bitbox::Type) -> Variable {
        let var = Variable {
            name: name.map_or("_tmp_".to_string(), |name| name.to_string()),
            ty,
            version: self.variable_counter,
        };
        self.variable_counter += 1;
        var
    }

    fn fresh_label(&mut self) -> String {
        let label = self.label_counter;
        self.label_counter += 1;
        format!("%L{}:", label)
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
        //let ast::Function {
        //    visibility,
        //    fn_token,
        //    name,
        //    params,
        //    return_type,
        //    body,
        //} = function;
        //
        //self.symbol_table.enter_scope(&name.lexeme);
        //
        //let mut instructions = Vec::new();
        //for stmt in body.statements.iter() {
        //    self.walk_stmt(&mut instructions, stmt);
        //}
        //
        //let function_builder = bitbox::FunctionBuilder::new(name.lexeme.clone())
        //    .visibility(match visibility {
        //        ast::Visibility::Public => bitbox::Visibility::Public,
        //        ast::Visibility::Private => bitbox::Visibility::Private,
        //    })
        //    .params(
        //        params
        //            .iter()
        //            .map(|param| bitbox::Variable {
        //                name: param.name.lexeme.clone(),
        //                ty: param.ty.into_bitbox_type(),
        //                version: 0,
        //            })
        //            .collect(),
        //    )
        //    .return_type(return_type.into_bitbox_type())
        //    .block("entry", instructions);
        //
        //self.symbol_table.exit_scope();
        //mb.push_function(function_builder.build());
    }

    fn walk_type_def(&mut self, ctx: &mut ModuleBuilder, type_def: &ast::TypeDef) {
        todo!()
    }

    fn walk_struct(&mut self, ctx: &mut ModuleBuilder, struct_def: &ast::Struct) {
        todo!()
    }

    fn walk_block(&mut self, bb: &mut Vec<LabeledInstruction>, block: &ast::Block) {
        for stmt in block.statements.iter() {
            self.walk_stmt(bb, stmt);
        }
    }

    fn walk_stmt(&mut self, bb: &mut Vec<LabeledInstruction>, stmt: &ast::Statement) {
        self.walk_expr(bb, &stmt.expr);
    }

    fn walk_expr(
        &mut self,
        bb: &mut Vec<LabeledInstruction>,
        expr: &ast::Expr,
    ) -> Option<Variable> {
        match expr {
            ast::Expr::Return(expr) => self.walk_expr_return(bb, expr),
            ast::Expr::Struct(expr) => self.walk_expr_struct(bb, expr),
            ast::Expr::Assignment(expr) => self.walk_expr_assignment(bb, expr),
            ast::Expr::Litral(expr) => self.walk_expr_litral(bb, expr),
            ast::Expr::Call(expr) => self.walk_expr_call(bb, expr),
            ast::Expr::Binary(expr) => self.walk_expr_binary(bb, expr),
            ast::Expr::Identifier(expr) => self.walk_expr_identifier(bb, expr),
            ast::Expr::IfElse(expr) => self.walk_expr_if_else(bb, expr),
        }
    }

    fn walk_expr_return(
        &mut self,
        bb: &mut Vec<LabeledInstruction>,
        expr: &ast::ExprReturn,
    ) -> Option<Variable> {
        match &expr.expr {
            Some(expr) => {
                self.walk_expr(bb, &expr);
            }
            None => bb.push(
                bitbox::Instruction::Return(bitbox::Type::Void, bitbox::Operand::None).into(),
            ),
        }
        None
    }

    fn walk_expr_struct(
        &mut self,
        ctx: &mut Vec<LabeledInstruction>,
        expr: &ast::ExprStruct,
    ) -> Option<Variable> {
        todo!()
    }

    fn walk_expr_assignment(
        &mut self,
        bb: &mut Vec<LabeledInstruction>,
        expr: &ast::ExprAssignment,
    ) -> Option<Variable> {
        let ty = expr
            .ty
            .as_ref()
            .map_or(Type::Void, |ty| ty.into_bitbox_type());
        let dest_var = self.fresh_variable(Some(&expr.ident.lexeme), ty);

        let Some(src_var) = self.walk_expr(bb, &expr.expr) else {
            panic!(
                "Failed to walk expr in walk_expr_assignment {}",
                expr.ident.lexeme
            );
        };

        bb.push(Instruction::Assign(dest_var.clone(), Operand::Variable(src_var)).into());

        Some(dest_var)
    }

    fn walk_expr_litral(
        &mut self,
        bb: &mut Vec<LabeledInstruction>,
        expr: &ast::Litral,
    ) -> Option<Variable> {
        match expr {
            ast::Litral::String(token) => todo!("STRING"),
            ast::Litral::Integer(token) => {
                let var = self.fresh_variable(None, Type::Unsigned(32));
                bb.push(Instruction::Assign(var.clone(), Operand::ConstantInt(1)).into());
                Some(var)
            }
            ast::Litral::Float(token) => todo!("FLOAT"),
            ast::Litral::Char(token) => todo!("CHAR"),
            ast::Litral::BoolTrue(_) => {
                let var = self.fresh_variable(None, Type::Unsigned(32));
                bb.push(Instruction::Assign(var.clone(), Operand::ConstantInt(1)).into());
                Some(var)
            }
            ast::Litral::BoolFalse(token) => {
                let var = self.fresh_variable(None, Type::Unsigned(32));
                bb.push(Instruction::Assign(var.clone(), Operand::ConstantInt(0)).into());
                Some(var)
            }
        }
    }

    fn walk_expr_call(
        &mut self,
        bb: &mut Vec<LabeledInstruction>,
        expr: &ast::ExprCall,
    ) -> Option<Variable> {
        let args = expr
            .args
            .iter()
            .map(|arg| {
                let Some(arg_var) = self.walk_expr(bb, arg) else {
                    panic!("Failed to walk expr in walk_expr_call");
                };
                Operand::Variable(arg_var)
            })
            .collect();

        let ast::Expr::Identifier(ident) = expr.caller.as_ref() else {
            panic!("Caller must be an identifier");
        };
        let Some(symbol) = self.symbol_table.get(&ident.lexeme) else {
            eprintln!("{:#?}", self.symbol_table);
            panic!("Symbol not found {}", ident.lexeme);
        };
        let ty = symbol.ty.into_bitbox_type();
        let dest_var = self.fresh_variable(None, ty);

        bb.push(Instruction::Call(dest_var.clone(), ident.lexeme.clone(), args).into());

        Some(dest_var)
    }

    fn walk_expr_binary(
        &mut self,
        bb: &mut Vec<LabeledInstruction>,
        expr: &ast::ExprBinary,
    ) -> Option<Variable> {
        let ast::ExprBinary { left, op, right } = &expr;

        let Some(left_var) = self.walk_expr(bb, left) else {
            panic!("Failed to walk expr in walk_expr_binary");
        };
        let Some(right_var) = self.walk_expr(bb, right) else {
            panic!("Failed to walk expr in walk_expr_binary");
        };

        let dest_var = self.fresh_variable(None, Type::Unsigned(32));

        match &op.kind {
            TokenKind::Plus => {
                bb.push(
                    Instruction::Add(
                        dest_var.clone(),
                        Operand::Variable(left_var),
                        Operand::Variable(right_var),
                    )
                    .into(),
                );
                Some(dest_var)
            }
            TokenKind::Minus => {
                bb.push(
                    Instruction::Sub(
                        dest_var.clone(),
                        Operand::Variable(left_var),
                        Operand::Variable(right_var),
                    )
                    .into(),
                );
                Some(dest_var)
            }
            TokenKind::Star => {
                bb.push(
                    Instruction::Mul(
                        dest_var.clone(),
                        Operand::Variable(left_var),
                        Operand::Variable(right_var),
                    )
                    .into(),
                );
                Some(dest_var)
            }
            TokenKind::Slash => {
                bb.push(
                    Instruction::Div(
                        dest_var.clone(),
                        Operand::Variable(left_var),
                        Operand::Variable(right_var),
                    )
                    .into(),
                );
                Some(dest_var)
            }
            _ => unreachable!("{:?}", op.kind),
        }
    }

    fn walk_expr_identifier(
        &mut self,
        bb: &mut Vec<LabeledInstruction>,
        expr: &crate::lexer::token::Token,
    ) -> Option<Variable> {
        let Some(symbol) = self.symbol_table.get(&expr.lexeme) else {
            panic!("Symbol not found {}", expr.lexeme);
        };

        Some(Variable {
            name: expr.lexeme.clone(),
            ty: symbol.ty.into_bitbox_type(),
            version: 1,
        })
    }

    fn walk_expr_if_else(
        &mut self,
        bb: &mut Vec<LabeledInstruction>,
        expr: &ast::ExprIfElse,
    ) -> Option<Variable> {
        let ast::ExprIfElse {
            condition,
            then_branch,
            else_branch,
        } = expr;

        let Some(condition_var) = self.walk_expr(bb, condition) else {
            panic!("Failed to walk condition in if expression");
        };

        let output_var = self.fresh_variable(Some("output"), Type::Void); // TODO: infer actual type

        let then_label = self.fresh_label();
        let else_label = self.fresh_label();
        let exit_label = self.fresh_label();

        // Compare the condition to 1
        let cond_bool_var = self.fresh_variable(Some("cond_bool"), Type::Unsigned(32));
        bb.push(
            Instruction::Cmp(
                cond_bool_var.clone(),
                Operand::Variable(condition_var),
                Operand::ConstantInt(1),
            )
            .into(),
        );

        // JumpIf true -> then, else -> else
        bb.push(Instruction::JumpIf(Operand::Variable(cond_bool_var), then_label.clone()).into());

        bb.push(Instruction::Jump(else_label.clone()).into());

        // --- Then branch
        bb.push(LabeledInstruction {
            label: Some(then_label.clone()),
            instruction: Instruction::NoOp, // Optional, or just start pushing real instrs
        });
        self.walk_block(bb, then_branch);
        bb.push(Instruction::Jump(exit_label.clone()).into());

        // --- Else branch
        bb.push(LabeledInstruction {
            label: Some(else_label.clone()),
            instruction: Instruction::NoOp,
        });
        if let Some(else_blk) = else_branch {
            self.walk_block(bb, else_blk);
        }
        bb.push(Instruction::Jump(exit_label.clone()).into());

        // --- Exit label
        bb.push(LabeledInstruction {
            label: Some(exit_label),
            instruction: Instruction::NoOp,
        });

        Some(output_var)
    }
}
