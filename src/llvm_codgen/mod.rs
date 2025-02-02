mod codegen;

use crate::ast::{Block, Function, Statement};
pub use codegen::Codegen;
use inkwell::{builder::Builder, context::Context, module::Module};

impl Codegen for Function {
    fn codegen<'ctx>(
        &self,
        context: &'ctx Context,
        module: &Module<'ctx>,
        builder: &Builder<'ctx>,
    ) {
        // TODO:
        // 1. gen return type
        // 2. gen arguments
        // 3. gen body
        let i32_type = context.i32_type();
        let function_type = i32_type.fn_type(&[], false);
        let func = module.add_function(&self.name.lexeme, function_type, None);
        let entry = context.append_basic_block(func, "entry");
        builder.position_at_end(entry);

        self.body.codegen(context, module, builder);

        // Ensure function always returns something in case it's missing
        if builder
            .get_insert_block()
            .unwrap()
            .get_terminator()
            .is_none()
        {
            let return_value = i32_type.const_int(0, false);
            builder.build_return(Some(&return_value));
        }
    }
}

impl Codegen for Block {
    fn codegen<'ctx>(
        &self,
        context: &'ctx Context,
        module: &Module<'ctx>,
        builder: &Builder<'ctx>,
    ) {
        for statement in &self.statements {
            statement.codegen(context, module, builder);
        }
    }
}

impl Codegen for Statement {
    fn codegen<'ctx>(
        &self,
        context: &'ctx Context,
        module: &Module<'ctx>,
        builder: &Builder<'ctx>,
    ) {
        match &*self.expr {
            crate::ast::Expr::Return(expr_return) => {
                let Some(expr) = expr_return.expr.as_ref() else {
                    builder.build_return(None);
                    return;
                };

                match &**expr {
                    crate::ast::Expr::Litral(crate::ast::Litral::Integer(token)) => {
                        let int_value = token.lexeme.parse::<u64>().unwrap(); // Convert token to u64
                        let i32_type = context.i32_type();
                        let const_int = i32_type.const_int(int_value, false);
                        builder.build_return(Some(&const_int));
                    }
                    _ => panic!("Unsupported return expression"), // Handle other return types later
                }
            }
            crate::ast::Expr::Struct(expr_struct) => todo!("struct {:#?}", expr_struct),
            crate::ast::Expr::Assignment(expr_assignment) => {
                todo!("assignment {:#?}", expr_assignment)
            }
            crate::ast::Expr::Litral(litral) => todo!("litral {:#?}", litral),
            crate::ast::Expr::Call(expr_call) => todo!("call {:#?}", expr_call),
            crate::ast::Expr::Binary(expr_binary) => todo!("binary {:#?}", expr_binary),
            crate::ast::Expr::Identifier(token) => todo!("identifier {:#?}", token),
            crate::ast::Expr::IfElse(expr_if_else) => todo!("if else {:#?}", expr_if_else),
        }
    }
}
