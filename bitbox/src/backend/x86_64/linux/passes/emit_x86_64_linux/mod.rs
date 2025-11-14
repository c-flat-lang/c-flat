use inkwell::types::BasicMetadataTypeEnum;
use inkwell::{
    builder::Builder, context::Context as LlvmContext, module::Module as LlvmModule,
    types::BasicType, types::BasicTypeEnum, values::*,
};

use crate::backend::{Context as BackendContext, Lower};
use crate::ir;
use crate::ir::instruction::{IAssign, IReturn};

impl ir::Type {
    pub fn to_llvm_type<'ctx>(&self, ctx: &'ctx LlvmContext) -> BasicTypeEnum<'ctx> {
        match self {
            ir::Type::Signed(_) | ir::Type::Unsigned(_) => ctx.i32_type().into(),
            ir::Type::Float(_) => ctx.f32_type().into(),
            _ => panic!("Unsupported IR type for LLVM lowering: {:?}", self),
        }
    }
    pub fn to_int_type<'ctx>(
        &self,
        ctx: &'ctx LlvmContext,
    ) -> Option<inkwell::types::IntType<'ctx>> {
        match self {
            ir::Type::Signed(_) | ir::Type::Unsigned(_) => Some(ctx.i32_type()),
            _ => None,
        }
    }
    pub fn to_float_type<'ctx>(
        &self,
        ctx: &'ctx LlvmContext,
    ) -> Option<inkwell::types::FloatType<'ctx>> {
        match self {
            ir::Type::Float(_) => Some(ctx.f32_type()),
            _ => None,
        }
    }
    pub fn to_basic_type_enum<'ctx>(&self, ctx: &'ctx LlvmContext) -> BasicTypeEnum<'ctx> {
        match self {
            ir::Type::Signed(_) | ir::Type::Unsigned(_) => ctx.i32_type().into(),
            ir::Type::Float(_) => ctx.f32_type().into(),
            _ => panic!("unsupported type {:?}", self),
        }
    }
}

#[derive(Debug)]
pub struct LLVMContext<'ctx> {
    pub llvm_context: &'ctx LlvmContext,
    pub llvm_module: LlvmModule<'ctx>,
    pub builder: Builder<'ctx>,
}

impl<'ctx> LLVMContext<'ctx> {
    pub fn new(llvm_context: &'ctx LlvmContext, name: &str) -> Self {
        let llvm_module = llvm_context.create_module(name);
        let builder = llvm_context.create_builder();

        Self {
            llvm_context,
            llvm_module,
            builder,
        }
    }
}

#[derive(Debug)]
pub struct EmitX86_64LinuxPass;

impl crate::passes::Pass for EmitX86_64LinuxPass {
    fn run(
        &mut self,
        module: &mut ir::Module,
        ctx: &mut BackendContext,
    ) -> Result<(), crate::error::Error> {
        eprintln!("EmitLLVMIRPass");

        for function in module.functions.iter() {
            function.lower(ctx, &mut *self)?;
        }

        Ok(())
    }
}

impl Lower<EmitX86_64LinuxPass> for ir::Function {
    type Output = ();
    fn lower(
        &self,
        ctx: &mut crate::backend::Context,
        backend: &mut EmitX86_64LinuxPass,
    ) -> Result<Self::Output, crate::error::Error> {
        let x86_llvm_ctx = ctx.output.get_mut_x86_64();

        let param_types: Vec<BasicMetadataTypeEnum> = self
            .params
            .iter()
            .map(|p| match p.ty {
                ir::Type::Signed(_) | ir::Type::Unsigned(_) => {
                    x86_llvm_ctx.llvm_context.i32_type().into()
                }
                ir::Type::Float(_) => x86_llvm_ctx.llvm_context.f32_type().into(),
                _ => todo!("type not supported yet: {:?}", p.ty),
            })
            .collect();

        let ret_type = match self.return_type {
            ir::Type::Signed(_) | ir::Type::Unsigned(_) => x86_llvm_ctx
                .llvm_context
                .i32_type()
                .fn_type(&param_types, false),
            ir::Type::Float(_) => x86_llvm_ctx
                .llvm_context
                .f32_type()
                .fn_type(&param_types, false),
            ir::Type::Void => x86_llvm_ctx
                .llvm_context
                .void_type()
                .fn_type(&param_types, false),
            _ => todo!("unsupported return type: {:?}", self.return_type),
        };

        let function = x86_llvm_ctx
            .llvm_module
            .add_function(&self.name, ret_type, None);

        let mut lower_ctx = LoweringContext {
            builder: &x86_llvm_ctx.builder,
            llvm_ctx: x86_llvm_ctx.llvm_context,
            variables: std::collections::HashMap::new(),
        };
        for block in self.blocks.iter() {
            let entry = x86_llvm_ctx
                .llvm_context
                .append_basic_block(function, "entry");
            x86_llvm_ctx.builder.position_at_end(entry);
            block.lower_to_llvm(&mut lower_ctx)?;
        }

        Ok(())
    }
}

pub struct LoweringContext<'ctx> {
    pub builder: &'ctx Builder<'ctx>,
    pub llvm_ctx: &'ctx LlvmContext,
    pub variables: std::collections::HashMap<String, PointerValue<'ctx>>,
}

trait LowerToLlvm {
    fn lower_to_llvm(&self, ctx: &mut LoweringContext<'_>) -> Result<(), crate::error::Error>;
}

impl LowerToLlvm for ir::BasicBlock {
    fn lower_to_llvm(&self, ctx: &mut LoweringContext<'_>) -> Result<(), crate::error::Error> {
        for instruction in self.instructions.iter() {
            instruction.lower_to_llvm(ctx)?;
        }
        Ok(())
    }
}

impl LowerToLlvm for ir::Instruction {
    fn lower_to_llvm(&self, ctx: &mut LoweringContext<'_>) -> Result<(), crate::error::Error> {
        match self {
            ir::Instruction::NoOp(..) => todo!(),
            ir::Instruction::Add(..) => todo!("add"),
            ir::Instruction::Assign(IAssign { des, src }) => {
                let llvm_value = src.lower(ctx)?;

                let ptr = if let Some(existing_ptr) = ctx.variables.get(&des.name) {
                    *existing_ptr
                } else {
                    let alloca = match des.ty {
                        ir::Type::Signed(_) | ir::Type::Unsigned(_) => {
                            let int_ty = ctx.llvm_ctx.i32_type();
                            ctx.builder.build_alloca(int_ty, &des.name).map_err(|e| {
                                crate::error::Error::InvalidInstruction {
                                    index: 0,
                                    message: e.to_string(),
                                }
                            })?
                        }
                        ir::Type::Float(_) => {
                            let f_ty = ctx.llvm_ctx.f32_type();
                            ctx.builder.build_alloca(f_ty, &des.name).map_err(|e| {
                                crate::error::Error::InvalidInstruction {
                                    index: 0,
                                    message: e.to_string(),
                                }
                            })?
                        }
                        _ => todo!("unsupported type for variable {:?}", des.ty),
                    };
                    ctx.variables.insert(des.name.clone(), alloca);
                    alloca
                };

                ctx.builder.build_store(ptr, llvm_value).map_err(|e| {
                    crate::error::Error::InvalidInstruction {
                        index: 0,
                        message: e.to_string(),
                    }
                })?;
            }
            ir::Instruction::Alloc(..) => todo!("alloc"),
            ir::Instruction::Call(..) => todo!("call"),
            ir::Instruction::Cmp(..) => todo!("cmp"),
            ir::Instruction::ElemGet(..) => todo!("elemget"),
            ir::Instruction::ElemSet(..) => todo!("elemset"),
            ir::Instruction::Gt(..) => todo!("gt"),
            ir::Instruction::Lt(..) => todo!("lt"),
            ir::Instruction::Jump(_) => todo!("jump"),
            ir::Instruction::JumpIf(..) => todo!("jumpif"),
            ir::Instruction::Load(..) => todo!("load"),
            ir::Instruction::Mul(..) => todo!("mul"),
            ir::Instruction::Phi(..) => todo!("phi"),
            ir::Instruction::Return(IReturn { ty, src }) => {
                if src.is_none() {
                    debug_assert!(ty == &ir::Type::Void);
                    ctx.builder.build_return(None).map_err(|e| {
                        crate::error::Error::InvalidInstruction {
                            index: 0,
                            message: e.to_string(),
                        }
                    })?;
                    return Ok(());
                }

                let val = src.lower(ctx)?;

                ctx.builder.build_return(Some(&val)).map_err(|e| {
                    crate::error::Error::InvalidInstruction {
                        index: 0,
                        message: e.to_string(),
                    }
                })?;
            }
            ir::Instruction::Sub(..) => todo!("sub"),
            ir::Instruction::Div(..) => todo!("div"),
            ir::Instruction::IfElse(..) => {
                unreachable!("Lowering pass should be used before llvm pass")
            }
        }
        Ok(())
    }
}

impl<'ctx> ir::Operand {
    fn lower(
        &self,
        ctx: &mut LoweringContext<'ctx>,
    ) -> Result<BasicValueEnum<'ctx>, crate::error::Error> {
        match self {
            ir::Operand::ConstantInt { value, ty: _ } => Ok(ctx
                .llvm_ctx
                .i32_type()
                .const_int(value.parse::<u64>().expect("Failed to parse int"), false)
                .into()),

            ir::Operand::Variable(var) => {
                let Some(ptr) = ctx.variables.get(&var.name) else {
                    panic!("Variable {:?} not found", var);
                };

                let llvm_ty = var.ty.to_llvm_type(ctx.llvm_ctx);

                let output = ctx
                    .builder
                    .build_load(llvm_ty, *ptr, &var.name)
                    .map_err(|e| crate::error::Error::InvalidInstruction {
                        index: 0,
                        message: e.to_string(),
                    })?;
                Ok(output)
            }

            ir::Operand::None => panic!("None operand"),
        }
    }
}
