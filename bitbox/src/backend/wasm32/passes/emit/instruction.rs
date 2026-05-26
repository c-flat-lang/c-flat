use wasm_encoder::{BlockType, ValType};

use super::Wasm32LowerContext;
use crate::backend::Lower;

use crate::ir::instruction::{
    IAdd, IAlloc, IAnd, IAssign, ICall, ICast, ICmp, ICopy, IDiv, IElemGet, IElemSet, IGt, IGte,
    IIfElse, IJump, IJumpIf, ILoad, ILoop, ILt, IMul, INot, IOr, IRef, IRem, IReturn, ISub, IXOr,
};
use crate::ir::{BasicBlock, Instruction, Type};

fn branch_terminates(blocks: &[BasicBlock]) -> bool {
    blocks.iter().any(|b| {
        b.instructions
            .iter()
            .any(|i| matches!(i, Instruction::Return(_)))
    })
}

impl Lower<Wasm32LowerContext<'_>> for IAdd {
    type Output = ();
    fn lower(
        &self,
        ctx: &mut crate::backend::Context,
        target: &mut Wasm32LowerContext<'_>,
    ) -> Result<Self::Output, crate::error::Error> {
        self.lhs.lower(ctx, target)?;
        self.rhs.lower(ctx, target)?;
        match self.des.ty.clone().into() {
            ValType::I32 => target.assembler.i32_add(),
            ValType::I64 => target.assembler.i64_add(),
            ValType::F32 => target.assembler.f32_add(),
            ValType::F64 => target.assembler.f64_add(),
            ValType::V128 => todo!("@gt v128"),
            ValType::Ref(_) => todo!("@gt ref"),
        };
        let Some(idx) = ctx
            .local_function_variables
            .get(&target.function_name)
            .iter()
            .position(|v| v.name == self.des.name)
        else {
            panic!("Variable {:?} not found", self.des);
        };
        target.assembler.local_set(idx as u32);
        Ok(())
    }
}

impl Lower<Wasm32LowerContext<'_>> for ISub {
    type Output = ();
    fn lower(
        &self,
        ctx: &mut crate::backend::Context,
        target: &mut Wasm32LowerContext<'_>,
    ) -> Result<Self::Output, crate::error::Error> {
        self.lhs.lower(ctx, target)?;
        self.rhs.lower(ctx, target)?;
        match self.des.ty.clone().into() {
            ValType::I32 => target.assembler.i32_sub(),
            ValType::I64 => todo!("@gt i64"),
            ValType::F32 => todo!("@gt f32"),
            ValType::F64 => todo!("@gt f64"),
            ValType::V128 => todo!("@gt v128"),
            ValType::Ref(_) => todo!("@gt ref"),
        };
        let Some(idx) = ctx
            .local_function_variables
            .get(&target.function_name)
            .iter()
            .position(|v| v.name == self.des.name)
        else {
            panic!("Variable {:?} not found", self.des);
        };
        target.assembler.local_set(idx as u32);
        Ok(())
    }
}

impl Lower<Wasm32LowerContext<'_>> for IDiv {
    type Output = ();
    fn lower(
        &self,
        ctx: &mut crate::backend::Context,
        target: &mut Wasm32LowerContext<'_>,
    ) -> Result<Self::Output, crate::error::Error> {
        self.lhs.lower(ctx, target)?;
        self.rhs.lower(ctx, target)?;
        match self.des.ty.clone().into() {
            ValType::I32 => target.assembler.i32_div_s(),
            ValType::I64 => target.assembler.i64_div_s(),
            ValType::F32 => target.assembler.f32_div(),
            ValType::F64 => target.assembler.f64_div(),
            ValType::V128 => todo!("@div v128"),
            ValType::Ref(_) => todo!("@div ref"),
        };
        let Some(idx) = ctx
            .local_function_variables
            .get(&target.function_name)
            .iter()
            .position(|v| v.name == self.des.name)
        else {
            panic!("Variable {:?} not found", self.des);
        };
        target.assembler.local_set(idx as u32);
        Ok(())
    }
}

impl Lower<Wasm32LowerContext<'_>> for IMul {
    type Output = ();
    fn lower(
        &self,
        ctx: &mut crate::backend::Context,
        target: &mut Wasm32LowerContext<'_>,
    ) -> Result<Self::Output, crate::error::Error> {
        self.lhs.lower(ctx, target)?;
        self.rhs.lower(ctx, target)?;
        match self.des.ty.clone().into() {
            ValType::I32 => target.assembler.i32_mul(),
            ValType::I64 => todo!("@gt i64"),
            ValType::F32 => todo!("@gt f32"),
            ValType::F64 => todo!("@gt f64"),
            ValType::V128 => todo!("@gt v128"),
            ValType::Ref(_) => todo!("@gt ref"),
        };
        let Some(idx) = ctx
            .local_function_variables
            .get(&target.function_name)
            .iter()
            .position(|v| v.name == self.des.name)
        else {
            panic!("Variable {:?} not found", self.des);
        };
        target.assembler.local_set(idx as u32);
        Ok(())
    }
}

impl Lower<Wasm32LowerContext<'_>> for IAssign {
    type Output = ();
    fn lower(
        &self,
        ctx: &mut crate::backend::Context,
        target: &mut Wasm32LowerContext<'_>,
    ) -> Result<Self::Output, crate::error::Error> {
        self.src.lower(ctx, target)?;

        let variables = ctx.local_function_variables.get(&target.function_name);
        let Some(idx) = variables.iter().position(|v| v.name == self.des.name) else {
            panic!(
                "@assign des variable {:?} not found in {:?}",
                self.des, target.function_name
            );
        };
        match self.des.ty.clone().into() {
            ValType::I32 => {
                target.assembler.local_set(idx as u32);
            }
            ValType::I64 => {
                target.assembler.local_set(idx as u32);
            }
            ValType::F32 => {
                target.assembler.local_set(idx as u32);
            }
            ValType::F64 => {
                target.assembler.local_set(idx as u32);
            }
            ValType::V128 => todo!("@assign v128"),
            ValType::Ref(_) => todo!("@assign ref"),
        }
        Ok(())
    }
}

impl Lower<Wasm32LowerContext<'_>> for IAlloc {
    type Output = ();
    fn lower(
        &self,
        ctx: &mut crate::backend::Context,
        target: &mut Wasm32LowerContext<'_>,
    ) -> Result<Self::Output, crate::error::Error> {
        let Some(ptr_idx) = ctx
            .local_function_variables
            .get(&target.function_name)
            .iter()
            .position(|v| v.name == self.des.name)
        else {
            panic!("Variable {:?} not found", self.des);
        };

        target.assembler.global_get(0);
        target.assembler.local_tee(ptr_idx as u32);

        match &self.des.ty {
            Type::Struct(s) => {
                target.assembler.i32_const(s.size(&ctx.target));
                target.assembler.i32_add();
            }
            _ => {
                self.size.lower(ctx, target)?;
                target
                    .assembler
                    .i32_const(self.des.ty.element_size(&ctx.target));
                target.assembler.i32_mul();
                target.assembler.i32_add();
            }
        }

        target.assembler.global_set(0);
        Ok(())
    }
}

impl Lower<Wasm32LowerContext<'_>> for ICall {
    type Output = ();
    fn lower(
        &self,
        ctx: &mut crate::backend::Context,
        target: &mut Wasm32LowerContext<'_>,
    ) -> Result<Self::Output, crate::error::Error> {
        for operand in self.args.iter() {
            operand.lower(ctx, target)?;
        }
        let Some(function_id) = ctx
            .local_function_variables
            .get_function_id(self.callee.as_str())
        else {
            // We should have a front end error for this
            panic!("Function {:?} not found", self.callee);
        };
        target.assembler.call(function_id as u32);
        if let Some(variable) = &self.des {
            let variables = ctx.local_function_variables.get(&target.function_name);
            let Some(idx) = variables.iter().position(|v| v.name == variable.name) else {
                panic!("Variable {:?} not found", variable);
            };
            target.assembler.local_set(idx as u32);
        }
        Ok(())
    }
}

impl Lower<Wasm32LowerContext<'_>> for ICmp {
    type Output = ();
    fn lower(
        &self,
        ctx: &mut crate::backend::Context,
        target: &mut Wasm32LowerContext<'_>,
    ) -> Result<Self::Output, crate::error::Error> {
        self.lhs.lower(ctx, target)?;
        self.rhs.lower(ctx, target)?;
        match self.des.ty.clone().into() {
            ValType::I32 => target.assembler.i32_eq(),
            ValType::I64 => todo!("@gt i64"),
            ValType::F32 => todo!("@gt f32"),
            ValType::F64 => todo!("@gt f64"),
            ValType::V128 => todo!("@gt v128"),
            ValType::Ref(_) => todo!("@gt ref"),
        };
        let Some(idx) = ctx
            .local_function_variables
            .get(&target.function_name)
            .iter()
            .position(|v| v.name == self.des.name)
        else {
            panic!("Variable {:?} not found", self.des);
        };
        target.assembler.local_set(idx as u32);
        Ok(())
    }
}

impl Lower<Wasm32LowerContext<'_>> for ICopy {
    type Output = ();
    fn lower(
        &self,
        ctx: &mut crate::backend::Context,
        target: &mut Wasm32LowerContext<'_>,
    ) -> Result<Self::Output, crate::error::Error> {
        self.src.lower(ctx, target)?;
        let Some(idx) = ctx
            .local_function_variables
            .get(&target.function_name)
            .iter()
            .position(|v| v.name == self.des.name)
        else {
            panic!("Variable {:?} not found", self.des);
        };
        target.assembler.local_set(idx as u32);
        Ok(())
    }
}

impl Lower<Wasm32LowerContext<'_>> for IElemGet {
    type Output = ();

    fn lower(
        &self,
        ctx: &mut crate::backend::Context,
        target: &mut Wasm32LowerContext<'_>,
    ) -> Result<Self::Output, crate::error::Error> {
        match self.ptr.ty() {
            Some(Type::Struct(s)) => {
                let idx = self
                    .index
                    .as_const_usize()
                    .expect("struct indexing must be constant");

                let field_offset: i32 = s.fields[..idx]
                    .iter()
                    .map(|(_, ty)| ty.size(&ctx.target))
                    .sum();

                target.assembler.i32_const(field_offset);
            }

            Some(Type::Array(_, elem)) => {
                self.index.lower(ctx, target)?;
                target.assembler.i32_const(elem.size(&ctx.target));
                target.assembler.i32_mul();
            }

            Some(Type::Pointer(inner)) => match inner.as_ref() {
                Type::Struct(s) => {
                    let idx = self
                        .index
                        .as_const_usize()
                        .expect("struct indexing must be constant");

                    let field_offset: i32 = s.fields[..idx]
                        .iter()
                        .map(|(_, ty)| ty.size(&ctx.target))
                        .sum();

                    target.assembler.i32_const(field_offset);
                }

                ty => {
                    self.index.lower(ctx, target)?;
                    target.assembler.i32_const(ty.size(&ctx.target));
                    target.assembler.i32_mul();
                }
            },

            Some(Type::Unsigned(1..=8) | Type::Signed(1..=8)) => {
                self.index.lower(ctx, target)?;
                target.assembler.i32_const(1);
                target.assembler.i32_mul();
            }

            Some(Type::Unsigned(9..=16) | Type::Signed(9..=16)) => {
                self.index.lower(ctx, target)?;
                target.assembler.i32_const(2);
                target.assembler.i32_mul();
            }

            Some(Type::Unsigned(_) | Type::Signed(_)) => {
                self.index.lower(ctx, target)?;
                target.assembler.i32_const(4);
                target.assembler.i32_mul();
            }

            ty => panic!("bad elemget {ty:?}"),
        }

        self.ptr.lower(ctx, target)?;
        target.assembler.i32_add();

        let memarg_byte = wasm_encoder::MemArg {
            offset: 0,
            align: 0,
            memory_index: 0,
        };
        let memarg_word = wasm_encoder::MemArg {
            offset: 0,
            align: 1,
            memory_index: 0,
        };
        let memarg_dword = wasm_encoder::MemArg {
            offset: 0,
            align: 2,
            memory_index: 0,
        };

        match &self.des.ty {
            Type::Unsigned(1..=8) => target.assembler.i32_load8_u(memarg_byte),
            Type::Signed(1..=8) => target.assembler.i32_load8_s(memarg_byte),
            Type::Unsigned(9..=16) => target.assembler.i32_load16_u(memarg_word),
            Type::Signed(9..=16) => target.assembler.i32_load16_s(memarg_word),

            _ => match self.des.ty.clone().into() {
                ValType::I32 => target.assembler.i32_load(memarg_dword),
                ValType::I64 => todo!("@elemget i64"),
                ValType::F32 => todo!("@elemget f32"),
                ValType::F64 => todo!("@elemget f64"),
                ValType::V128 => todo!("@elemget v128"),
                ValType::Ref(_) => todo!("@elemget ref"),
            },
        };

        let variables = ctx.local_function_variables.get(&target.function_name);
        let Some(idx) = variables.iter().position(|v| v.name == self.des.name) else {
            panic!("Variable {:?} not found", self);
        };

        target.assembler.local_set(idx as u32);
        Ok(())
    }
}

impl Lower<Wasm32LowerContext<'_>> for IElemSet {
    type Output = ();
    fn lower(
        &self,
        ctx: &mut crate::backend::Context,
        target: &mut Wasm32LowerContext<'_>,
    ) -> Result<Self::Output, crate::error::Error> {
        let memarg_byte = wasm_encoder::MemArg {
            offset: 0,
            align: 0,
            memory_index: 0,
        };
        let memarg_word = wasm_encoder::MemArg {
            offset: 0,
            align: 1,
            memory_index: 0,
        };
        let memarg_dword = wasm_encoder::MemArg {
            offset: 0,
            align: 2,
            memory_index: 0,
        };

        match &self.addr.ty {
            Type::Struct(s) => {
                let idx = self
                    .index
                    .as_const_usize()
                    .expect("struct field index must be constant");
                let field_offset: i32 = s.fields[..idx]
                    .iter()
                    .map(|(_, ty)| ty.size(&ctx.target))
                    .sum();
                let field_ty = s.fields[idx].1.clone();

                self.addr.lower(ctx, target)?;
                target.assembler.i32_const(field_offset);
                target.assembler.i32_add();
                self.value.lower(ctx, target)?;

                match field_ty.size(&ctx.target) {
                    1 => target.assembler.i32_store8(memarg_byte),
                    2 => target.assembler.i32_store16(memarg_word),
                    _ => target.assembler.i32_store(memarg_dword),
                };
            }
            Type::Array(_, elem_ty) => {
                let elem_ty = *elem_ty.clone();

                self.addr.lower(ctx, target)?;
                self.index.lower(ctx, target)?;
                target.assembler.i32_const(elem_ty.size(&ctx.target));
                target.assembler.i32_mul();
                target.assembler.i32_add();
                self.value.lower(ctx, target)?;

                match elem_ty.size(&ctx.target) {
                    1 => target.assembler.i32_store8(memarg_byte),
                    2 => target.assembler.i32_store16(memarg_word),
                    _ => target.assembler.i32_store(memarg_dword),
                };
            }
            Type::Pointer(inner) => match inner.as_ref() {
                Type::Struct(s) => {
                    let idx = self
                        .index
                        .as_const_usize()
                        .expect("struct field index must be constant");
                    let field_offset: i32 = s.fields[..idx]
                        .iter()
                        .map(|(_, ty)| ty.size(&ctx.target))
                        .sum();
                    let field_ty = s.fields[idx].1.clone();

                    self.addr.lower(ctx, target)?;
                    target.assembler.i32_const(field_offset);
                    target.assembler.i32_add();
                    self.value.lower(ctx, target)?;

                    match field_ty.size(&ctx.target) {
                        1 => target.assembler.i32_store8(memarg_byte),
                        2 => target.assembler.i32_store16(memarg_word),
                        _ => target.assembler.i32_store(memarg_dword),
                    };
                }
                elem_ty => {
                    self.addr.lower(ctx, target)?;
                    self.index.lower(ctx, target)?;
                    target.assembler.i32_const(elem_ty.size(&ctx.target));
                    target.assembler.i32_mul();
                    target.assembler.i32_add();
                    self.value.lower(ctx, target)?;

                    match elem_ty.size(&ctx.target) {
                        1 => target.assembler.i32_store8(memarg_byte),
                        2 => target.assembler.i32_store16(memarg_word),
                        _ => target.assembler.i32_store(memarg_dword),
                    };
                }
            },
            Type::Unsigned(1..=8) | Type::Signed(1..=8) => {
                self.addr.lower(ctx, target)?;
                self.index.lower(ctx, target)?;
                target.assembler.i32_const(1);
                target.assembler.i32_mul();
                target.assembler.i32_add();
                self.value.lower(ctx, target)?;
                target.assembler.i32_store8(memarg_byte);
            }
            Type::Unsigned(9..=16) | Type::Signed(9..=16) => {
                self.addr.lower(ctx, target)?;
                self.index.lower(ctx, target)?;
                target.assembler.i32_const(2);
                target.assembler.i32_mul();
                target.assembler.i32_add();
                self.value.lower(ctx, target)?;
                target.assembler.i32_store16(memarg_word);
            }
            Type::Unsigned(_) | Type::Signed(_) => {
                self.addr.lower(ctx, target)?;
                self.index.lower(ctx, target)?;
                target.assembler.i32_const(4);
                target.assembler.i32_mul();
                target.assembler.i32_add();
                self.value.lower(ctx, target)?;
                target.assembler.i32_store(memarg_dword);
            }
            ty => panic!("bad elemset {ty:?}"),
        }
        Ok(())
    }
}

impl Lower<Wasm32LowerContext<'_>> for IAnd {
    type Output = ();
    fn lower(
        &self,
        ctx: &mut crate::backend::Context,
        target: &mut Wasm32LowerContext<'_>,
    ) -> Result<Self::Output, crate::error::Error> {
        self.lhs.lower(ctx, target)?;
        self.rhs.lower(ctx, target)?;
        match self.des.ty.clone().into() {
            ValType::I32 => target.assembler.i32_and(),
            ValType::I64 => todo!("@gt i64"),
            ValType::F32 => todo!("@gt f32"),
            ValType::F64 => todo!("@gt f64"),
            ValType::V128 => todo!("@gt v128"),
            ValType::Ref(_) => todo!("@gt ref"),
        };
        let Some(idx) = ctx
            .local_function_variables
            .get(&target.function_name)
            .iter()
            .position(|v| v.name == self.des.name)
        else {
            panic!("Variable {:?} not found", self.des);
        };
        target.assembler.local_set(idx as u32);
        Ok(())
    }
}

impl Lower<Wasm32LowerContext<'_>> for IOr {
    type Output = ();
    fn lower(
        &self,
        ctx: &mut crate::backend::Context,
        target: &mut Wasm32LowerContext<'_>,
    ) -> Result<Self::Output, crate::error::Error> {
        self.lhs.lower(ctx, target)?;
        self.rhs.lower(ctx, target)?;
        match self.des.ty.clone().into() {
            ValType::I32 => target.assembler.i32_or(),
            ValType::I64 => todo!("@gt i64"),
            ValType::F32 => todo!("@gt f32"),
            ValType::F64 => todo!("@gt f64"),
            ValType::V128 => todo!("@gt v128"),
            ValType::Ref(_) => todo!("@gt ref"),
        };
        let Some(idx) = ctx
            .local_function_variables
            .get(&target.function_name)
            .iter()
            .position(|v| v.name == self.des.name)
        else {
            panic!("Variable {:?} not found", self.des);
        };
        target.assembler.local_set(idx as u32);
        Ok(())
    }
}

impl Lower<Wasm32LowerContext<'_>> for IXOr {
    type Output = ();
    fn lower(
        &self,
        ctx: &mut crate::backend::Context,
        target: &mut Wasm32LowerContext<'_>,
    ) -> Result<Self::Output, crate::error::Error> {
        self.lhs.lower(ctx, target)?;
        self.rhs.lower(ctx, target)?;
        match self.des.ty.clone().into() {
            ValType::I32 => target.assembler.i32_xor(),
            ValType::I64 => todo!("@gt i64"),
            ValType::F32 => todo!("@gt f32"),
            ValType::F64 => todo!("@gt f64"),
            ValType::V128 => todo!("@gt v128"),
            ValType::Ref(_) => todo!("@gt ref"),
        };
        let Some(idx) = ctx
            .local_function_variables
            .get(&target.function_name)
            .iter()
            .position(|v| v.name == self.des.name)
        else {
            panic!("Variable {:?} not found", self.des);
        };
        target.assembler.local_set(idx as u32);
        Ok(())
    }
}

impl Lower<Wasm32LowerContext<'_>> for IGt {
    type Output = ();
    fn lower(
        &self,
        ctx: &mut crate::backend::Context,
        target: &mut Wasm32LowerContext<'_>,
    ) -> Result<Self::Output, crate::error::Error> {
        self.lhs.lower(ctx, target)?;
        self.rhs.lower(ctx, target)?;
        match self.des.ty.clone().into() {
            ValType::I32 => target.assembler.i32_gt_s(),
            ValType::I64 => target.assembler.i64_gt_s(),
            ValType::F32 => target.assembler.f32_gt(),
            ValType::F64 => target.assembler.f64_gt(),
            ValType::V128 => todo!("@gt v128"),
            ValType::Ref(_) => todo!("@gt ref"),
        };
        let Some(idx) = ctx
            .local_function_variables
            .get(&target.function_name)
            .iter()
            .position(|v| v.name == self.des.name)
        else {
            panic!("Variable {:?} not found", self.des);
        };
        target.assembler.local_set(idx as u32);
        Ok(())
    }
}

impl Lower<Wasm32LowerContext<'_>> for IGte {
    type Output = ();
    fn lower(
        &self,
        ctx: &mut crate::backend::Context,
        target: &mut Wasm32LowerContext<'_>,
    ) -> Result<Self::Output, crate::error::Error> {
        self.lhs.lower(ctx, target)?;
        self.rhs.lower(ctx, target)?;
        match self.des.ty.clone().into() {
            ValType::I32 => target.assembler.i32_ge_u(),
            ValType::I64 => todo!("@gt i64"),
            ValType::F32 => todo!("@gt f32"),
            ValType::F64 => todo!("@gt f64"),
            ValType::V128 => todo!("@gt v128"),
            ValType::Ref(_) => todo!("@gt ref"),
        };
        let Some(idx) = ctx
            .local_function_variables
            .get(&target.function_name)
            .iter()
            .position(|v| v.name == self.des.name)
        else {
            panic!("Variable {:?} not found", self.des);
        };
        target.assembler.local_set(idx as u32);
        Ok(())
    }
}

impl Lower<Wasm32LowerContext<'_>> for IRem {
    type Output = ();

    fn lower(
        &self,
        ctx: &mut crate::backend::Context,
        target: &mut Wasm32LowerContext<'_>,
    ) -> Result<Self::Output, crate::error::Error> {
        self.lhs.lower(ctx, target)?;
        self.rhs.lower(ctx, target)?;

        match &self.des.ty.clone().into() {
            ValType::I32 => target.assembler.i32_rem_s(),
            ValType::I64 => target.assembler.i64_rem_s(),
            ValType::F32 => todo!("@rem f32"),
            ValType::F64 => todo!("@rem f64"),
            ValType::V128 => todo!("@rem v128"),
            ValType::Ref(_) => todo!("@rem ref"),
        };
        let Some(idx) = ctx
            .local_function_variables
            .get(&target.function_name)
            .iter()
            .position(|v| v.name == self.des.name)
        else {
            panic!("Variable {:?} not found", self.des);
        };
        target.assembler.local_set(idx as u32);

        Ok(())
    }
}

impl Lower<Wasm32LowerContext<'_>> for ILt {
    type Output = ();
    fn lower(
        &self,
        ctx: &mut crate::backend::Context,
        target: &mut Wasm32LowerContext<'_>,
    ) -> Result<Self::Output, crate::error::Error> {
        self.lhs.lower(ctx, target)?;
        self.rhs.lower(ctx, target)?;
        match self.des.ty.clone().into() {
            ValType::I32 => target.assembler.i32_lt_s(),
            ValType::I64 => todo!("@gt i64"),
            ValType::F32 => todo!("@gt f32"),
            ValType::F64 => todo!("@gt f64"),
            ValType::V128 => todo!("@gt v128"),
            ValType::Ref(_) => todo!("@gt ref"),
        };
        let Some(idx) = ctx
            .local_function_variables
            .get(&target.function_name)
            .iter()
            .position(|v| v.name == self.des.name)
        else {
            panic!("Variable {:?} not found", self.des);
        };
        target.assembler.local_set(idx as u32);
        Ok(())
    }
}

impl Lower<Wasm32LowerContext<'_>> for IJump {
    type Output = ();
    fn lower(
        &self,
        _ctx: &mut crate::backend::Context,
        target: &mut Wasm32LowerContext<'_>,
    ) -> Result<Self::Output, crate::error::Error> {
        let Some(block_id) = target.blocks.get(&self.label).copied() else {
            panic!("Block {:?} not found {:#?}", self.label, target.blocks);
        };
        target.assembler.br(block_id);
        Ok(())
    }
}

impl Lower<Wasm32LowerContext<'_>> for IJumpIf {
    type Output = ();
    fn lower(
        &self,
        ctx: &mut crate::backend::Context,
        target: &mut Wasm32LowerContext<'_>,
    ) -> Result<Self::Output, crate::error::Error> {
        let Some(block_id) = target.blocks.get(&self.label).copied() else {
            panic!("Block {:?} not found {:#?}", self.label, target.blocks);
        };
        self.cond.lower(ctx, target)?;
        target.assembler.br_if(block_id);
        Ok(())
    }
}

impl Lower<Wasm32LowerContext<'_>> for ILoad {
    type Output = ();
    fn lower(
        &self,
        ctx: &mut crate::backend::Context,
        target: &mut Wasm32LowerContext<'_>,
    ) -> Result<Self::Output, crate::error::Error> {
        self.src.lower(ctx, target)?;
        let Some(idx) = ctx
            .local_function_variables
            .get(&target.function_name)
            .iter()
            .position(|v| v.name == self.des.name)
        else {
            panic!("Variable {:?} not found", self.des);
        };
        target.assembler.local_set(idx as u32);
        Ok(())
    }
}

impl Lower<Wasm32LowerContext<'_>> for IReturn {
    type Output = ();
    fn lower(
        &self,
        ctx: &mut crate::backend::Context,
        target: &mut Wasm32LowerContext<'_>,
    ) -> Result<Self::Output, crate::error::Error> {
        match self.ty.clone() {
            Type::Void => todo!("@return void"),
            _ => {
                self.src.lower(ctx, target)?;
                target.assembler.return_();
            }
        }
        Ok(())
    }
}

impl Lower<Wasm32LowerContext<'_>> for IIfElse {
    type Output = ();
    fn lower(
        &self,
        ctx: &mut crate::backend::Context,
        target: &mut Wasm32LowerContext<'_>,
    ) -> Result<Self::Output, crate::error::Error> {
        for block in self.cond.iter() {
            block.lower(ctx, target)?;
        }

        // let ty: BlockType = self
        //     .result
        //     .as_ref()
        //     .map(|r| r.ty.clone().into())
        //     .unwrap_or(BlockType::Empty);

        let Some(idx) = ctx
            .local_function_variables
            .get(&target.function_name)
            .iter()
            .position(|v| v.name == self.cond_result.name)
        else {
            panic!("Variable {:?} not found", self.cond_result);
        };

        target.assembler.local_get(idx as u32);
        target.assembler.if_(BlockType::Empty);

        for block in self.then_branch.iter() {
            block.lower(ctx, target)?;
        }

        if !self.else_branch.is_empty() {
            target.assembler.else_();
            for block in self.else_branch.iter() {
                block.lower(ctx, target)?;
            }
        }

        target.assembler.end();

        // If both branches unconditionally return, the code after `end` is unreachable.
        // Emit `unreachable` so the wasm validator knows the stack is polymorphic here,
        // satisfying any implicit return type the enclosing function declares.
        let both_terminate =
            branch_terminates(&self.then_branch) && branch_terminates(&self.else_branch);
        if both_terminate {
            target.assembler.unreachable();
        }

        Ok(())
    }
}

impl Lower<Wasm32LowerContext<'_>> for ILoop {
    type Output = ();
    fn lower(
        &self,
        ctx: &mut crate::backend::Context,
        target: &mut Wasm32LowerContext<'_>,
    ) -> Result<Self::Output, crate::error::Error> {
        target.assembler.block(BlockType::Empty); // exit block
        target.assembler.loop_(BlockType::Empty); // loop block

        for block in self.cond.iter() {
            block.lower(ctx, target)?;
        }

        let Some(idx) = ctx
            .local_function_variables
            .get(&target.function_name)
            .iter()
            .position(|v| v.name == self.cond_result.name)
        else {
            panic!("Variable {:?} not found", self.cond_result);
        };

        target.assembler.local_get(idx as u32);

        target.assembler.i32_eqz();
        target.assembler.br_if(1);

        for block in self.body.iter() {
            block.lower(ctx, target)?;
        }

        target.assembler.br(0); // loop back to cond

        target.assembler.end(); // end loop
        target.assembler.end(); // end block
        Ok(())
    }
}

impl Lower<Wasm32LowerContext<'_>> for IRef {
    type Output = ();
    fn lower(
        &self,
        ctx: &mut crate::backend::Context,
        target: &mut Wasm32LowerContext<'_>,
    ) -> Result<Self::Output, crate::error::Error> {
        let variables = ctx.local_function_variables.get(&target.function_name);
        let Some(src_idx) = variables.iter().position(|v| v.name == self.src.name) else {
            panic!("ref: src variable {:?} not found", self.src);
        };
        let Some(des_idx) = variables.iter().position(|v| v.name == self.des.name) else {
            panic!("ref: des variable {:?} not found", self.des);
        };

        match &self.src.ty {
            // Arrays, structs, and pointers are already i32 linear-memory addresses;
            // @ref just copies the address without spilling.
            Type::Array(_, _) | Type::Struct(_) | Type::Pointer(_) => {
                target.assembler.local_get(src_idx as u32);
                target.assembler.local_set(des_idx as u32);
            }
            _ => {
                let size = self.src.ty.size(&ctx.target);
                target.assembler.global_get(0);
                target.assembler.local_tee(des_idx as u32);
                target.assembler.local_get(src_idx as u32);
                match self.src.ty.clone().into() {
                    ValType::I32 => target.assembler.i32_store(wasm_encoder::MemArg {
                        offset: 0,
                        align: 2,
                        memory_index: 0,
                    }),
                    ValType::I64 => todo!("@ref spill i64"),
                    ValType::F32 => todo!("@ref spill f32"),
                    ValType::F64 => todo!("@ref spill f64"),
                    ValType::V128 => todo!("@ref spill v128"),
                    ValType::Ref(_) => todo!("@ref spill ref"),
                };
                target.assembler.global_get(0);
                target.assembler.i32_const(size);
                target.assembler.i32_add();
                target.assembler.global_set(0);
            }
        }
        Ok(())
    }
}

impl Lower<Wasm32LowerContext<'_>> for INot {
    type Output = ();
    fn lower(
        &self,
        ctx: &mut crate::backend::Context,
        target: &mut Wasm32LowerContext<'_>,
    ) -> Result<Self::Output, crate::error::Error> {
        let Some(src_idx) = ctx
            .local_function_variables
            .get(&target.function_name)
            .iter()
            .position(|v| v.name == self.src.name)
        else {
            panic!("Variable {:?} not found", self.src);
        };
        target.assembler.local_get(src_idx as u32);
        target.assembler.i32_eqz();
        let Some(des_idx) = ctx
            .local_function_variables
            .get(&target.function_name)
            .iter()
            .position(|v| v.name == self.des.name)
        else {
            panic!("Variable {:?} not found", self.des);
        };
        target.assembler.local_set(des_idx as u32);
        Ok(())
    }
}

impl Lower<Wasm32LowerContext<'_>> for ICast {
    type Output = ();
    fn lower(
        &self,
        ctx: &mut crate::backend::Context,
        target: &mut Wasm32LowerContext<'_>,
    ) -> Result<Self::Output, crate::error::Error> {
        self.src.lower(ctx, target)?;

        let src_bytes = self.src.ty.size(&ctx.target);
        let dst_bytes = self.des.ty.size(&ctx.target);
        let src_in_i64 = src_bytes > 4;
        let dst_in_i64 = dst_bytes > 4;

        match self.kind {
            // Truncate: shrink an integer to a smaller type.
            // Wasm has no explicit truncate instruction, values are already
            // represented in i32/i64. We just need to mask off the high bits
            // when crossing the i64→i32 boundary, or mask within i32/i64.
            crate::ir::CastKind::Truncate => {
                if src_in_i64 && !dst_in_i64 {
                    // i64 -> i32: wasm wrap instruction drops the high 32 bits
                    target.assembler.i32_wrap_i64();
                }
                // If we're staying within the same wasm word (i32->i8, i64->i32 already
                // handled), no instruction is needed, the value is already there.
                // Downstream loads/stores are responsible for correct width.
            }

            // ZeroExtend: widen an unsigned integer.
            crate::ir::CastKind::ZeroExtend => {
                if !src_in_i64 && dst_in_i64 {
                    // i32 → i64: zero-extend (unsigned widen)
                    target.assembler.i64_extend_i32_u();
                }
                // i32 -> i32 (e.g. u8 → u32): no-op, high bits are already 0
                // in a well-formed i32 value coming from a load.
            }

            // SignExtend: widen a signed integer, propagating the sign bit.
            crate::ir::CastKind::SignExtend => {
                match (src_bytes, dst_bytes) {
                    // Widen within i32 using sign-extend instructions
                    (1, 2) | (1, 4) => {
                        target.assembler.i32_extend8_s();
                    }
                    (2, 4) => {
                        target.assembler.i32_extend16_s();
                    }
                    // Widen from i32 → i64
                    (1, 5..=8) => {
                        target.assembler.i32_extend8_s();
                        target.assembler.i64_extend_i32_s();
                    }
                    (2, 5..=8) => {
                        target.assembler.i32_extend16_s();
                        target.assembler.i64_extend_i32_s();
                    }
                    (3, 5..=8) | (4, 5..=8) => {
                        // Already a full i32, just sign-extend to i64
                        target.assembler.i64_extend_i32_s();
                    }
                    // Widen within i64
                    (5, _) | (6, _) | (7, _) => {
                        target.assembler.i64_extend32_s();
                    }
                    _ => {} // same-size sign extend is a no-op
                }
            }

            // UnsignedToSigned: reinterpret bit pattern, e.g. u32 -> i32.
            // In Wasm, i32/i64 are untyped bit vectors, signedness is only
            // meaningful at operations, so this is always a no-op.
            crate::ir::CastKind::UnsignedToSigned => {
                // Only need to act if the wasm value type changes (i32 <-> i64)
                if !src_in_i64 && dst_in_i64 {
                    target.assembler.i64_extend_i32_u();
                } else if src_in_i64 && !dst_in_i64 {
                    target.assembler.i32_wrap_i64();
                }
                // Same wasm word type: no-op
            }

            // SignedToUnsigned: same reasoning as UnsignedToSigned, just a
            // reinterpretation. Your existing code handles the extend case;
            // the i32_trunc_sat_f64_u below looks like a bug (no float involved).
            crate::ir::CastKind::SignedToUnsigned => {
                if !src_in_i64 && dst_in_i64 {
                    target.assembler.i64_extend_i32_u();
                } else if src_in_i64 && !dst_in_i64 {
                    target.assembler.i32_wrap_i64();
                }
                // Same wasm word type: no-op
            }

            // FloatToInt: convert f32/f64 -> i32/i64 (truncating toward zero).
            // Using saturating variants to avoid Wasm traps on out-of-range values.
            crate::ir::CastKind::FloatToInt => {
                let src_is_f64 = src_bytes == 8; // f32=4, f64=8
                match (src_is_f64, dst_in_i64) {
                    (false, false) => {
                        target.assembler.i32_trunc_sat_f32_s();
                    }
                    (false, true) => {
                        target.assembler.i64_trunc_sat_f32_s();
                    }
                    (true, false) => {
                        target.assembler.i32_trunc_sat_f64_s();
                    }
                    (true, true) => {
                        target.assembler.i64_trunc_sat_f64_s();
                    }
                }
            }

            // IntToFloat: convert i32/i64 -> f32/f64.
            crate::ir::CastKind::IntToFloat => {
                let dst_is_f64 = dst_bytes == 8;
                match (src_in_i64, dst_is_f64) {
                    (false, false) => {
                        target.assembler.f32_convert_i32_s();
                    }
                    (false, true) => {
                        target.assembler.f64_convert_i32_s();
                    }
                    (true, false) => {
                        target.assembler.f32_convert_i64_s();
                    }
                    (true, true) => {
                        target.assembler.f64_convert_i64_s();
                    }
                }
            }

            // BitCast: reinterpret the raw bits as a different type.
            // Only meaningful across the int/float boundary in Wasm.
            crate::ir::CastKind::BitCast => {
                match (src_bytes, dst_bytes) {
                    (4, 4) if  /* f32→i32 */ true => {
                        // Distinguish by checking src type kind if you have it;
                        // here we cover both directions:
                        target.assembler.i32_reinterpret_f32(); // f32 bits → i32
                        // If src is i32 and dst is f32, use:
                        // target.assembler.f32_reinterpret_i32();
                    }
                    (8, 8) => {
                        target.assembler.i64_reinterpret_f64(); // f64 bits → i64
                        // target.assembler.f64_reinterpret_i64();
                    }
                    _ => {} // int<->int or float↔float same size: no-op
                }
            }

            // NoOp: source and destination types are the same wasm value type.
            crate::ir::CastKind::NoOp => {}
        };

        let Some(idx) = ctx
            .local_function_variables
            .get(&target.function_name)
            .iter()
            .position(|v| v.name == self.des.name)
        else {
            panic!("Variable {:?} not found", self.des);
        };

        target.assembler.local_set(idx as u32);

        Ok(())
    }
}
