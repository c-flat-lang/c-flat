mod codegen;

use crate::{
    ast::{Block, Expr, ExprCall, Function, Litral, Statement},
    lexer::token::TokenKind,
};
pub use codegen::{Codegen, CodegenResult, VariableContext};
use inkwell::{
    builder::Builder, context::Context, module::Module, types::BasicMetadataTypeEnum,
    values::BasicValueEnum,
};

use std::collections::HashMap;

fn get_type<'ctx>(
    ty: &crate::ast::Type,
    context: &'ctx Context,
) -> inkwell::types::BasicTypeEnum<'ctx> {
    inkwell::types::BasicTypeEnum::from(match ty {
        crate::ast::Type::Bool => context.bool_type(),
        crate::ast::Type::UnsignedNumber(_) => todo!(),
        crate::ast::Type::SignedNumber(32) => context.i32_type(),
        crate::ast::Type::SignedNumber(_) => todo!(),
        crate::ast::Type::Float(_) => todo!(),
        crate::ast::Type::Array(_, _) => todo!(),
        crate::ast::Type::Pointer(_) => todo!(),
        crate::ast::Type::Struct(_) => todo!(),
        crate::ast::Type::Enum(_) => todo!(),
        crate::ast::Type::Void => todo!(),
    })
}

impl Codegen for Function {
    type Output<'ctx> = ();
    fn codegen<'ctx>(
        &self,
        variable_context: &mut VariableContext<'ctx>,
        context: &'ctx Context,
        module: &Module<'ctx>,
        builder: &Builder<'ctx>,
    ) -> CodegenResult<Self::Output<'ctx>> {
        let i32_type = match self.return_type {
            crate::ast::Type::Bool => context.bool_type(),
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

        self.body
            .codegen(variable_context, context, module, builder)?;

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
    type Output<'ctx> = Option<BasicValueEnum<'ctx>>;
    fn codegen<'ctx>(
        &self,
        variable_context: &mut VariableContext<'ctx>,
        context: &'ctx Context,
        module: &Module<'ctx>,
        builder: &Builder<'ctx>,
    ) -> CodegenResult<Self::Output<'ctx>> {
        let mut last_value: Option<BasicValueEnum> = None;

        for statement in &self.statements {
            last_value = statement.codegen(variable_context, context, module, builder)?;
        }
        Ok(last_value)
    }
}

impl Codegen for Statement {
    type Output<'ctx> = Option<BasicValueEnum<'ctx>>;
    fn codegen<'ctx>(
        &self,
        variable_context: &mut VariableContext<'ctx>,
        context: &'ctx Context,
        module: &Module<'ctx>,
        builder: &Builder<'ctx>,
    ) -> CodegenResult<Self::Output<'ctx>> {
        let Some(expr) = self
            .expr
            .codegen(variable_context, context, module, builder)?
        else {
            return Ok(None);
        };
        builder.build_return(Some(&expr))?;
        Ok(Some(expr))
    }
}

impl Codegen for ExprCall {
    type Output<'ctx> = inkwell::values::BasicValueEnum<'ctx>;
    fn codegen<'ctx>(
        &self,
        _: &mut VariableContext<'ctx>,
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
        variable_context: &mut VariableContext<'ctx>,
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
                let const_int = expr.codegen(variable_context, context, module, builder)?;

                builder.build_return(
                    const_int
                        .as_ref()
                        .map(|v| v as &dyn inkwell::values::BasicValue),
                )?;
                Ok(None)
            }
            Expr::Struct(e) => todo!("{:#?}", e),
            Expr::Assignment(e) => {
                let var_name = &e.ident.lexeme;

                let Some(rhs_value) = e.expr.codegen(variable_context, context, module, builder)?
                else {
                    panic!("Right-hand side of assignment must produce a value");
                };

                let llvm_ty = get_type(&e.ty.clone().unwrap(), context);
                assert_eq!(
                    llvm_ty,
                    rhs_value.get_type(),
                    "Type mismatch when assigning"
                );

                let var_ptr = builder.build_alloca(rhs_value.get_type(), var_name)?;

                builder.build_store(var_ptr, rhs_value)?;

                variable_context.insert(var_name.clone(), (var_ptr, e.ty.clone().unwrap()));

                Ok(Some(rhs_value))
            }
            Expr::Litral(e) => {
                match e {
                    Litral::String(token) => todo!("{:#?}", token),
                    Litral::Integer(token) => {
                        let int_value = token.lexeme.parse::<u64>().unwrap(); // Convert token to u64
                        let i32_type = context.i32_type();
                        let value = i32_type.const_int(int_value, false);
                        Ok(Some(value.into()))
                    }
                    Litral::Float(token) => {
                        let float_value = token.lexeme.parse::<f64>().unwrap(); // Convert token to f64
                        let float_type = context.f64_type();
                        let value = float_type.const_float(float_value);
                        Ok(Some(value.into()))
                    }
                    Litral::Char(token) => {
                        let char_value = token.lexeme.chars().next().unwrap() as u64; // Convert char to u64
                        let i8_type = context.i8_type();
                        let value = i8_type.const_int(char_value, false);
                        Ok(Some(value.into()))
                    }
                    Litral::BoolTrue(_) => {
                        let i1_type = context.bool_type();
                        Ok(Some(i1_type.const_int(1, false).into()))
                    }
                    Litral::BoolFalse(_) => {
                        let i1_type = context.bool_type();
                        Ok(Some(i1_type.const_int(0, false).into()))
                    }
                }
            }
            Expr::Call(e) => {
                let call_value = e.codegen(variable_context, context, module, builder)?;
                Ok(Some(call_value))
            }
            Expr::Binary(e) => {
                let Some(BasicValueEnum::IntValue(left)) =
                    e.left.codegen(variable_context, context, module, builder)?
                else {
                    unreachable!();
                };

                let Some(BasicValueEnum::IntValue(right)) =
                    e.right
                        .codegen(variable_context, context, module, builder)?
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

                if let Some((ptr_value, ty)) = variable_context.get(var_name) {
                    if ptr_value.is_null() {
                        panic!("Trying to load a null pointer for variable `{}`", var_name);
                    }
                    let llvm_ty = get_type(ty, context);
                    let load_value =
                        builder.build_load(llvm_ty, *ptr_value, &format!("loaded_{}", var_name))?;
                    match ty {
                        crate::ast::Type::Bool => {
                            return Ok(Some(load_value.into_int_value().into()));
                        }
                        crate::ast::Type::UnsignedNumber(_) => todo!(),
                        crate::ast::Type::SignedNumber(32) => {
                            return Ok(Some(load_value.into_int_value().into()))
                        }
                        crate::ast::Type::Float(32) => {
                            return Ok(Some(load_value.into_float_value().into()))
                        }
                        crate::ast::Type::Array(_, _) => todo!(),
                        crate::ast::Type::Pointer(_) => todo!(),
                        crate::ast::Type::Struct(_) => todo!(),
                        crate::ast::Type::Enum(_) => todo!(),
                        crate::ast::Type::Void => todo!(),
                        _ => unreachable!(),
                    }
                }

                panic!("Undefined variable `{:?}`", e);
            }
            Expr::IfElse(e) => {
                let cond_value = e
                    .condition
                    .codegen(variable_context, context, module, builder)?;
                let Some(BasicValueEnum::IntValue(cond)) = cond_value else {
                    panic!(
                        "Condition expression must return an integer value {:?}",
                        cond_value
                    );
                };

                let parent_function = builder.get_insert_block().unwrap().get_parent().unwrap();

                // Create blocks
                let then_block = context.append_basic_block(parent_function, "then");
                let else_block = context.append_basic_block(parent_function, "else");
                let merge_block = context.append_basic_block(parent_function, "ifcont");

                // Branch based on condition
                if e.else_branch.is_some() {
                    builder.build_conditional_branch(cond, then_block, else_block)?;
                } else {
                    builder.build_conditional_branch(cond, then_block, merge_block)?;
                }

                // Allocate a PHI node for the final value
                builder.position_at_end(merge_block);
                let i32_type = context.i32_type();
                let phi = builder.build_phi(i32_type, "iftmp")?;

                // Emit 'then' block
                builder.position_at_end(then_block);
                let Some(BasicValueEnum::IntValue(then_value)) =
                    e.then_branch
                        .codegen(variable_context, context, module, builder)?
                else {
                    panic!("Then branch must return a value");
                };
                builder.build_unconditional_branch(merge_block)?;

                // Add 'then' branch value to PHI
                let then_bb = builder.get_insert_block().unwrap();
                phi.add_incoming(&[(&then_value, then_bb)]);

                // Emit 'else' block (if exists)
                if let Some(else_expr) = &e.else_branch {
                    builder.position_at_end(else_block);
                    let Some(BasicValueEnum::IntValue(else_value)) =
                        else_expr.codegen(variable_context, context, module, builder)?
                    else {
                        panic!("Else branch must return a value");
                    };
                    builder.build_unconditional_branch(merge_block)?;

                    // Add 'else' branch value to PHI
                    let else_bb = builder.get_insert_block().unwrap();
                    phi.add_incoming(&[(&else_value, else_bb)]);
                }

                // Move to the merge block
                builder.position_at_end(merge_block);
                Ok(Some(phi.as_basic_value()))
            }
        }
    }
}
