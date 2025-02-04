mod codegen;

use crate::{
    ast::{Block, Expr, ExprCall, Function, Litral, Statement},
    lexer::token::TokenKind,
};
pub use codegen::{Codegen, CodegenResult};
use inkwell::{
    builder::Builder, context::Context, module::Module, types::BasicMetadataTypeEnum,
    values::BasicValueEnum,
};

use std::collections::HashMap;

impl Codegen for Function {
    type Output<'ctx> = ();
    fn codegen<'ctx>(
        &self,
        context: &'ctx Context,
        module: &Module<'ctx>,
        builder: &Builder<'ctx>,
    ) -> CodegenResult<Self::Output<'ctx>> {
        let i32_type = match self.return_type {
            crate::ast::Type::UnsignedNumber(_) => todo!(),
            crate::ast::Type::SignedNumber(32) => context.i32_type(),
            crate::ast::Type::SignedNumber(_) => todo!(),
            crate::ast::Type::Float(_) => todo!(),
            crate::ast::Type::Array(_, _) => todo!(),
            crate::ast::Type::Pointer(_) => todo!(),
            crate::ast::Type::Struct(_) => todo!(),
            crate::ast::Type::Enum(_) => todo!(),
            crate::ast::Type::Void => todo!(),
        };

        let param_types: Vec<BasicMetadataTypeEnum> =
            self.params.iter().map(|_| i32_type.into()).collect();

        let function_type = i32_type.fn_type(&param_types, false);
        let func = module.add_function(&self.name.lexeme, function_type, None);
        let entry = context.append_basic_block(func, "entry");
        builder.position_at_end(entry);

        let mut variables = HashMap::new();
        for (i, param) in self.params.iter().enumerate() {
            let param_value = func.get_nth_param(i as u32).unwrap();
            param_value.set_name(&param.name.lexeme);
            variables.insert(param.name.lexeme.clone(), param_value);
        }

        self.body.codegen(context, module, builder)?;

        if builder
            .get_insert_block()
            .unwrap()
            .get_terminator()
            .is_none()
        {
            let return_value = i32_type.const_int(0, false);
            builder.build_return(Some(&return_value))?;
        }

        Ok(())
    }
}

impl Codegen for Block {
    type Output<'ctx> = ();
    fn codegen<'ctx>(
        &self,
        context: &'ctx Context,
        module: &Module<'ctx>,
        builder: &Builder<'ctx>,
    ) -> CodegenResult<Self::Output<'ctx>> {
        for statement in &self.statements {
            statement.codegen(context, module, builder)?;
        }
        Ok(())
    }
}

impl Codegen for Statement {
    type Output<'ctx> = ();
    fn codegen<'ctx>(
        &self,
        context: &'ctx Context,
        module: &Module<'ctx>,
        builder: &Builder<'ctx>,
    ) -> CodegenResult<Self::Output<'ctx>> {
        let None = self.expr.codegen(context, module, builder)? else {
            panic!("Unsupported statement");
        };
        Ok(())
    }
}

impl Codegen for ExprCall {
    type Output<'ctx> = inkwell::values::BasicValueEnum<'ctx>;
    fn codegen<'ctx>(
        &self,
        context: &'ctx Context,
        module: &Module<'ctx>,
        builder: &Builder<'ctx>,
    ) -> CodegenResult<Self::Output<'ctx>> {
        let Expr::Identifier(caller) = &*self.caller else {
            unreachable!();
        };
        let function_name = &caller.lexeme;
        let function = module.get_function(function_name).unwrap_or_else(|| {
            panic!("Function `{}` not found in module", function_name);
        });

        let mut args = Vec::new();
        let i32_type = context.i32_type();

        for arg in &self.args {
            match arg {
                crate::ast::Expr::Litral(crate::ast::Litral::Integer(token)) => {
                    let int_value = token.lexeme.parse::<u64>().unwrap();
                    let const_int = i32_type.const_int(int_value, false);
                    args.push(const_int.into()); // Convert to BasicValueEnum
                }
                _ => panic!("Unsupported argument expression"),
            }
        }

        let call_site = builder.build_call(function, &args, "call_tmp")?;

        Ok(call_site.try_as_basic_value().left().unwrap())
    }
}

impl Codegen for Expr {
    type Output<'ctx> = Option<BasicValueEnum<'ctx>>;
    fn codegen<'ctx>(
        &self,
        context: &'ctx Context,
        module: &Module<'ctx>,
        builder: &Builder<'ctx>,
    ) -> CodegenResult<Self::Output<'ctx>> {
        match self {
            Expr::Return(e) => {
                let Some(expr) = e.expr.as_ref() else {
                    builder.build_return(None)?;
                    return Ok(None);
                };
                let const_int = expr.codegen(context, module, builder)?;

                builder.build_return(
                    const_int
                        .as_ref()
                        .map(|v| v as &dyn inkwell::values::BasicValue),
                )?;
                Ok(None)
            }
            Expr::Struct(e) => todo!("{:#?}", e),
            Expr::Assignment(e) => todo!("{:#?}", e),
            Expr::Litral(Litral::Integer(token)) => {
                let int_value = token.lexeme.parse::<u64>().unwrap(); // Convert token to u64
                let i32_type = context.i32_type();
                let value = i32_type.const_int(int_value, false);
                Ok(Some(value.into()))
            }
            Expr::Litral(e) => todo!("{:#?}", e),
            Expr::Call(e) => {
                let call_value = e.codegen(context, module, builder)?;
                Ok(Some(call_value))
            }
            Expr::Binary(e) => {
                let Some(BasicValueEnum::IntValue(left)) =
                    e.left.codegen(context, module, builder)?
                else {
                    unreachable!();
                };

                let Some(BasicValueEnum::IntValue(right)) =
                    e.right.codegen(context, module, builder)?
                else {
                    unreachable!();
                };

                let result = match e.op.kind {
                    TokenKind::Plus => builder.build_int_add(left, right, "add_tmp")?,
                    TokenKind::Minus => builder.build_int_sub(left, right, "sub_tmp")?,
                    TokenKind::Star => builder.build_int_mul(left, right, "mul_tmp")?,
                    TokenKind::Slash => builder.build_int_signed_div(left, right, "div_tmp")?,
                    TokenKind::Percent => builder.build_int_signed_rem(left, right, "rem_tmp")?,
                    TokenKind::EqualEqual => builder.build_int_compare(
                        inkwell::IntPredicate::EQ,
                        left,
                        right,
                        "eq_tmp",
                    )?,
                    TokenKind::BangEqual => builder.build_int_compare(
                        inkwell::IntPredicate::NE,
                        left,
                        right,
                        "ne_tmp",
                    )?,
                    TokenKind::Greater => builder.build_int_compare(
                        inkwell::IntPredicate::SGT,
                        left,
                        right,
                        "gt_tmp",
                    )?,
                    TokenKind::GreaterEqual => builder.build_int_compare(
                        inkwell::IntPredicate::SGE,
                        left,
                        right,
                        "ge_tmp",
                    )?,
                    TokenKind::Less => builder.build_int_compare(
                        inkwell::IntPredicate::SLT,
                        left,
                        right,
                        "lt_tmp",
                    )?,
                    TokenKind::LessEqual => builder.build_int_compare(
                        inkwell::IntPredicate::SLE,
                        left,
                        right,
                        "le_tmp",
                    )?,
                    _ => unreachable!(),
                };

                Ok(Some(result.into()))
            }
            Expr::Identifier(e) => {
                let var_name = &e.lexeme;

                if let Some(global) = module.get_global(var_name) {
                    return Ok(Some(global.as_pointer_value().into()));
                }

                let parent = builder
                    .get_insert_block()
                    .ok_or("No insert block found")?
                    .get_parent()
                    .ok_or("No parent function found")?;

                for (i, param) in parent.get_param_iter().enumerate() {
                    if let Some(name) = parent
                        .get_nth_param(i as u32)
                        .map(|p| p.get_name().to_str().unwrap().to_string())
                    {
                        if name.as_str() == var_name {
                            return Ok(Some(param.into()));
                        }
                    }
                }

                panic!("Undefined variable `{}`", var_name);
            }
            Expr::IfElse(e) => todo!("{:#?}", e),
        }
    }
}
