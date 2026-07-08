use wasm_encoder::{BlockType, ValType};

use super::Wasm32LowerContext;
use crate::backend::Lower;

use crate::ir::instruction::{
    IAdd, IAlloc, IAnd, IAssign, IBitShiftRight, IBitWiseAnd, ICall, ICast, ICmp, ICopy, IDiv,
    IElemGet, IElemSet, IGt, IGte, IIfElse, IJump, IJumpIf, ILoad, ILoop, ILt, ILte, IMul, INot,
    IOr, IRef, IRem, IReturn, ISub, IXOr,
};
use crate::ir::{BasicBlock, Instruction, Type};

fn branch_terminates(blocks: &[BasicBlock]) -> bool {
    blocks.iter().any(|b| {
        b.instructions
            .iter()
            .any(|i| matches!(i, Instruction::Return(_)))
    })
}

/// Emit the store instruction matching a field/element `ty` (the value is
/// already on the stack). Integers narrower than 32 bits use sized stores;
/// otherwise the store width follows the value's wasm type so f32/f64/i64
/// fields are stored correctly (not truncated through `i32.store`).
fn emit_typed_store(ty: &Type, ctx: &crate::backend::Context, target: &mut Wasm32LowerContext<'_>) {
    let dword = wasm_encoder::MemArg {
        offset: 0,
        align: 2,
        memory_index: 0,
    };
    let qword = wasm_encoder::MemArg {
        offset: 0,
        align: 3,
        memory_index: 0,
    };
    match ty.clone().into() {
        ValType::I32 => match ty.size(&ctx.target) {
            1 => target.assembler.i32_store8(wasm_encoder::MemArg {
                offset: 0,
                align: 0,
                memory_index: 0,
            }),
            2 => target.assembler.i32_store16(wasm_encoder::MemArg {
                offset: 0,
                align: 1,
                memory_index: 0,
            }),
            _ => target.assembler.i32_store(dword),
        },
        ValType::I64 => target.assembler.i64_store(qword),
        ValType::F32 => target.assembler.f32_store(dword),
        ValType::F64 => target.assembler.f64_store(qword),
        ValType::V128 => unreachable!("@elemset v128: SIMD is not produced by c-flat"),
        ValType::Ref(_) => unreachable!("@elemset ref: reference types are not produced by c-flat"),
    };
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
            ValType::V128 => unreachable!("v128: SIMD is not produced by c-flat"),
            ValType::Ref(_) => unreachable!("ref: reference types are not produced by c-flat"),
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
            ValType::I64 => target.assembler.i64_sub(),
            ValType::F32 => target.assembler.f32_sub(),
            ValType::F64 => target.assembler.f64_sub(),
            ValType::V128 => unreachable!("@sub v128: SIMD is not produced by c-flat"),
            ValType::Ref(_) => unreachable!("@sub ref: reference types are not produced by c-flat"),
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
            ValType::V128 => unreachable!("@div v128: SIMD is not produced by c-flat"),
            ValType::Ref(_) => unreachable!("@div ref: reference types are not produced by c-flat"),
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
            ValType::I64 => target.assembler.i64_mul(),
            ValType::F32 => target.assembler.f32_mul(),
            ValType::F64 => target.assembler.f64_mul(),
            ValType::V128 => unreachable!("@mul v128: SIMD is not produced by c-flat"),
            ValType::Ref(_) => unreachable!("@mul ref: reference types are not produced by c-flat"),
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
            ValType::V128 => unreachable!("@assign v128: SIMD is not produced by c-flat"),
            ValType::Ref(_) => {
                unreachable!("@assign ref: reference types are not produced by c-flat")
            }
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
                target.assembler.i32_const(self.des.ty.size(&ctx.target));
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
        // If the callee is a defined function returning an aggregate by value, it
        // uses the sret convention: allocate a result buffer in *this* frame, point
        // global 1 at it, and let the callee copy its result there. The buffer
        // outlives the callee (it lives below the callee's frame base) and is
        // reclaimed when this function restores its own frame.
        if let Some(ret_ty) = target.agg_return_fns.get(&self.callee).cloned() {
            let size = ret_ty.size(&ctx.target);
            // buffer = current stack pointer; stash it in the sret register.
            target.assembler.global_get(0);
            target.assembler.global_set(1);
            // stack pointer += size
            target.assembler.global_get(1);
            target.assembler.i32_const(size);
            target.assembler.i32_add();
            target.assembler.global_set(0);
            // The result variable, if any, is the buffer address.
            if let Some(variable) = &self.des {
                let variables = ctx.local_function_variables.get(&target.function_name);
                let Some(idx) = variables.iter().position(|v| v.name == variable.name) else {
                    panic!("Variable {:?} not found", variable);
                };
                target.assembler.global_get(1);
                target.assembler.local_set(idx as u32);
            }
            // Args are already-evaluated operands (no nested calls, no allocs), so
            // global 1 still points at our buffer when the callee reads it at entry.
            for operand in self.args.iter() {
                operand.lower(ctx, target)?;
            }
            let Some(function_id) = ctx
                .local_function_variables
                .get_function_id(self.callee.as_str())
            else {
                panic!("Function {:?} not found", self.callee);
            };
            target.assembler.call(function_id as u32);
            return Ok(());
        }

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
            ValType::I64 => target.assembler.i64_eq(),
            ValType::F32 => target.assembler.f32_eq(),
            ValType::F64 => target.assembler.f64_eq(),
            ValType::V128 => unreachable!("@cmp v128: SIMD is not produced by c-flat"),
            ValType::Ref(_) => unreachable!("@cmp ref: reference types are not produced by c-flat"),
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
                    let ty = match ty {
                        Type::Array(_, e) => e.as_ref(),
                        other => other,
                    };
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
        let memarg_qword = wasm_encoder::MemArg {
            offset: 0,
            align: 3,
            memory_index: 0,
        };

        if !matches!(&self.des.ty, Type::Array(..) | Type::Struct(..)) {
            match &self.des.ty {
                Type::Unsigned(1..=8) => target.assembler.i32_load8_u(memarg_byte),
                Type::Signed(1..=8) => target.assembler.i32_load8_s(memarg_byte),
                Type::Unsigned(9..=16) => target.assembler.i32_load16_u(memarg_word),
                Type::Signed(9..=16) => target.assembler.i32_load16_s(memarg_word),

                _ => match self.des.ty.clone().into() {
                    ValType::I32 => target.assembler.i32_load(memarg_dword),
                    ValType::I64 => target.assembler.i64_load(memarg_qword),
                    ValType::F32 => target.assembler.f32_load(memarg_dword),
                    ValType::F64 => target.assembler.f64_load(memarg_qword),
                    ValType::V128 => unreachable!("@elemget v128: SIMD is not produced by c-flat"),
                    ValType::Ref(_) => {
                        unreachable!("@elemget ref: reference types are not produced by c-flat")
                    }
                },
            };
        }

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
        if matches!(self.value.ty(), Some(Type::Array(..) | Type::Struct(..))) {
            let size = self.value.ty().unwrap().size(&ctx.target);
            // dest = base + offset
            self.addr.lower(ctx, target)?;
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
                    target.assembler.i32_const(field_offset);
                    target.assembler.i32_add();
                }
                Type::Pointer(inner) if matches!(inner.as_ref(), Type::Struct(_)) => {
                    let Type::Struct(s) = inner.as_ref() else {
                        unreachable!()
                    };
                    let idx = self
                        .index
                        .as_const_usize()
                        .expect("struct field index must be constant");
                    let field_offset: i32 = s.fields[..idx]
                        .iter()
                        .map(|(_, ty)| ty.size(&ctx.target))
                        .sum();
                    target.assembler.i32_const(field_offset);
                    target.assembler.i32_add();
                }
                _ => {
                    self.index.lower(ctx, target)?;
                    target.assembler.i32_const(size);
                    target.assembler.i32_mul();
                    target.assembler.i32_add();
                }
            }
            // src = aggregate value's address
            self.value.lower(ctx, target)?;
            // len
            target.assembler.i32_const(size);
            target.assembler.memory_copy(0, 0);
            return Ok(());
        }

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

                emit_typed_store(&field_ty, ctx, target);
            }
            Type::Array(_, elem_ty) => {
                let elem_ty = *elem_ty.clone();

                self.addr.lower(ctx, target)?;
                self.index.lower(ctx, target)?;
                target.assembler.i32_const(elem_ty.size(&ctx.target));
                target.assembler.i32_mul();
                target.assembler.i32_add();
                self.value.lower(ctx, target)?;

                emit_typed_store(&elem_ty, ctx, target);
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
                    let elem_ty = match elem_ty {
                        Type::Array(_, e) => e.as_ref(),
                        other => other,
                    };
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
            ValType::I64 => target.assembler.i64_and(),
            ValType::F32 => {
                unreachable!("@and f32: bitwise-and on floats is rejected by the type checker")
            }
            ValType::F64 => {
                unreachable!("@and f64: bitwise-and on floats is rejected by the type checker")
            }
            ValType::V128 => unreachable!("@and v128: SIMD is not produced by c-flat"),
            ValType::Ref(_) => unreachable!("@and ref: reference types are not produced by c-flat"),
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
            ValType::I64 => target.assembler.i64_or(),
            ValType::F32 => unreachable!("@or f32"),
            ValType::F64 => unreachable!("@or f64"),
            ValType::V128 => unreachable!("@or v128"),
            ValType::Ref(_) => unimplemented!("@or ref"),
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
            ValType::I64 => target.assembler.i64_xor(),
            ValType::F32 => {
                unreachable!("@xor f32: bitwise-xor on floats is rejected by the type checker")
            }
            ValType::F64 => {
                unreachable!("@xor f64: bitwise-xor on floats is rejected by the type checker")
            }
            ValType::V128 => unreachable!("@xor v128: SIMD is not produced by c-flat"),
            ValType::Ref(_) => unreachable!("@xor ref: reference types are not produced by c-flat"),
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

impl Lower<Wasm32LowerContext<'_>> for IBitWiseAnd {
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
            ValType::I64 => target.assembler.i64_and(),
            ValType::F32 => {
                unreachable!("@bwand f32: bitwise-and on floats is rejected by the type checker")
            }
            ValType::F64 => {
                unreachable!("@bwand f64: bitwise-and on floats is rejected by the type checker")
            }
            ValType::V128 => unreachable!("@bwand v128: SIMD is not produced by c-flat"),
            ValType::Ref(_) => {
                unreachable!("@bwand ref: reference types are not produced by c-flat")
            }
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

impl Lower<Wasm32LowerContext<'_>> for IBitShiftRight {
    type Output = ();
    fn lower(
        &self,
        ctx: &mut crate::backend::Context,
        target: &mut Wasm32LowerContext<'_>,
    ) -> Result<Self::Output, crate::error::Error> {
        self.lhs.lower(ctx, target)?;
        self.rhs.lower(ctx, target)?;
        // Dispatch on ir::Type so the shift's signedness (arithmetic vs logical)
        // is correct, unlike the ValType-based binary ops.
        match &self.des.ty {
            Type::Unsigned(1..=32) => target.assembler.i32_shr_u(),
            Type::Signed(1..=32) => target.assembler.i32_shr_s(),
            Type::Unsigned(33..=64) => target.assembler.i64_shr_u(),
            Type::Signed(33..=64) => target.assembler.i64_shr_s(),
            ty => unreachable!("@bsr: unsupported operand type {ty:?}"),
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
            ValType::V128 => unreachable!("v128: SIMD is not produced by c-flat"),
            ValType::Ref(_) => unreachable!("ref: reference types are not produced by c-flat"),
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
            ValType::I64 => target.assembler.i64_ge_u(),
            ValType::F32 => target.assembler.f32_ge(),
            ValType::F64 => target.assembler.f64_ge(),
            ValType::V128 => unreachable!("@gte v128: SIMD is not produced by c-flat"),
            ValType::Ref(_) => unreachable!("@gte ref: reference types are not produced by c-flat"),
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
            // wasm has no float remainder instruction; `%` on floats is rejected
            // by the type checker.
            ValType::F32 => unreachable!("@rem f32: `%` on floats is not supported"),
            ValType::F64 => unreachable!("@rem f64: `%` on floats is not supported"),
            ValType::V128 => unreachable!("@rem v128: SIMD is not produced by c-flat"),
            ValType::Ref(_) => unreachable!("@rem ref: reference types are not produced by c-flat"),
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
            ValType::I64 => target.assembler.i64_lt_s(),
            ValType::F32 => target.assembler.f32_lt(),
            ValType::F64 => target.assembler.f64_lt(),
            ValType::V128 => unreachable!("@lt v128: SIMD is not produced by c-flat"),
            ValType::Ref(_) => unreachable!("@lt ref: reference types are not produced by c-flat"),
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

impl Lower<Wasm32LowerContext<'_>> for ILte {
    type Output = ();
    fn lower(
        &self,
        ctx: &mut crate::backend::Context,
        target: &mut Wasm32LowerContext<'_>,
    ) -> Result<Self::Output, crate::error::Error> {
        self.lhs.lower(ctx, target)?;
        self.rhs.lower(ctx, target)?;
        match self.des.ty.clone().into() {
            ValType::I32 => target.assembler.i32_le_s(),
            ValType::I64 => target.assembler.i64_le_s(),
            ValType::F32 => target.assembler.f32_le(),
            ValType::F64 => target.assembler.f64_le(),
            ValType::V128 => unreachable!("@lte v128: SIMD is not produced by c-flat"),
            ValType::Ref(_) => unreachable!("@lte ref: reference types are not produced by c-flat"),
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
        let memarg_qword = wasm_encoder::MemArg {
            offset: 0,
            align: 3,
            memory_index: 0,
        };

        match &self.des.ty {
            Type::Unsigned(1..=8) => target.assembler.i32_load8_u(memarg_byte),
            Type::Signed(1..=8) => target.assembler.i32_load8_s(memarg_byte),
            Type::Unsigned(9..=16) => target.assembler.i32_load16_u(memarg_word),
            Type::Signed(9..=16) => target.assembler.i32_load16_s(memarg_word),
            _ => match self.des.ty.clone().into() {
                ValType::I32 => target.assembler.i32_load(memarg_dword),
                ValType::I64 => target.assembler.i64_load(memarg_qword),
                ValType::F32 => target.assembler.f32_load(memarg_dword),
                ValType::F64 => target.assembler.f64_load(memarg_qword),
                ValType::V128 => unreachable!("@load v128: SIMD is not produced by c-flat"),
                ValType::Ref(_) => {
                    unreachable!("@load ref: reference types are not produced by c-flat")
                }
            },
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

impl Lower<Wasm32LowerContext<'_>> for IReturn {
    type Output = ();
    fn lower(
        &self,
        ctx: &mut crate::backend::Context,
        target: &mut Wasm32LowerContext<'_>,
    ) -> Result<Self::Output, crate::error::Error> {
        let frame_local = target.frame_local;
        if super::is_aggregate(&self.ty) {
            // sret: copy the aggregate to the caller-provided buffer (global 1,
            // latched into sret_local at entry) instead of returning a pointer
            // into this frame, which the epilogue below is about to reclaim.
            let size = self.ty.size(&ctx.target);
            target.assembler.local_get(target.sret_local); // dest
            self.src.lower(ctx, target)?; // src (aggregate address)
            target.assembler.i32_const(size); // len
            target.assembler.memory_copy(0, 0);
            // Epilogue: restore the stack pointer, then return no value.
            target.assembler.local_get(frame_local);
            target.assembler.global_set(0);
            target.assembler.return_();
        } else {
            if self.ty != Type::Void {
                self.src.lower(ctx, target)?;
            }
            // Epilogue: restore the stack pointer. The return value (if any) is
            // already on the operand stack and is unaffected by the global.set.
            target.assembler.local_get(frame_local);
            target.assembler.global_set(0);
            target.assembler.return_();
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
                let memarg_dword = wasm_encoder::MemArg {
                    offset: 0,
                    align: 2,
                    memory_index: 0,
                };
                let memarg_qword = wasm_encoder::MemArg {
                    offset: 0,
                    align: 3,
                    memory_index: 0,
                };
                match self.src.ty.clone().into() {
                    ValType::I32 => target.assembler.i32_store(memarg_dword),
                    ValType::I64 => target.assembler.i64_store(memarg_qword),
                    ValType::F32 => target.assembler.f32_store(memarg_dword),
                    ValType::F64 => target.assembler.f64_store(memarg_qword),
                    ValType::V128 => {
                        unreachable!("@ref spill v128: SIMD is not produced by c-flat")
                    }
                    ValType::Ref(_) => {
                        unreachable!("@ref spill ref: reference types are not produced by c-flat")
                    }
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
