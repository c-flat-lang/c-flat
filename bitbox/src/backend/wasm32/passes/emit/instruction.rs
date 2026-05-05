use wasm_encoder::{BlockType, ValType};

use super::Wasm32LowerContext;
use crate::backend::Lower;

use crate::ir::instruction::{
    IAdd, IAlloc, IAnd, IAssign, ICall, ICast, ICmp, ICopy, IDiv, IElemGet, IElemSet, IGt, IGte,
    IIfElse, IJump, IJumpIf, ILoad, ILoop, ILt, IMul, INot, IOr, IRef, IRem, IReturn, ISub, IXOr,
};
use crate::ir::{BasicBlock, Instruction, Operand, Type};

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
        // 1. Get heap pointer
        target.assembler.global_get(0);

        // 2. Store it as the variable (array base)
        let Some(ptr_idx) = ctx
            .local_function_variables
            .get(&target.function_name)
            .iter()
            .position(|v| v.name == self.des.name)
        else {
            panic!("Variable {:?} not found", self.des);
        };

        target.assembler.local_tee(ptr_idx as u32);

        // 3. Compute allocation size: size * element_size
        self.size.lower(ctx, target)?; // length
        target.assembler.i32_const(self.des.ty.size()); // element size
        target.assembler.i32_mul(); // total bytes

        // 4. heap_ptr + size
        target.assembler.i32_add();

        // 5. Update heap pointer
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
        self.index.lower(ctx, target)?;
        target.assembler.i32_const(self.des.ty.size());
        target.assembler.i32_mul();

        self.ptr.lower(ctx, target)?;
        target.assembler.i32_add();

        // Use narrow loads for sub-32-bit types so we don't read past the field.
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
                ValType::I64 => todo!("@gt i64"),
                ValType::F32 => todo!("@gt f32"),
                ValType::F64 => todo!("@gt f64"),
                ValType::V128 => todo!("@gt v128"),
                ValType::Ref(_) => todo!("@gt ref"),
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
        // For struct field access, look up the field type from the struct definition
        // (not the value type, integer literals default to s32 even for u8 fields).
        // For arrays, the element type already gives the correct stride.
        let ty = match &self.addr.ty {
            Type::Array(_, elem_ty) => *elem_ty.clone(),
            Type::Struct(s) => {
                // Struct field index is always a compile-time constant.
                if let Operand::ConstantInt(ref c) = self.index {
                    let idx: usize = c.value.parse().unwrap_or(0);
                    s.fields
                        .get(idx)
                        .map(|(_, t)| t.clone())
                        .unwrap_or(Type::Signed(32))
                } else {
                    Type::Signed(32)
                }
            }
            ty => ty.clone(),
        };
        self.index.lower(ctx, target)?;
        target.assembler.i32_const(ty.size());
        target.assembler.i32_mul();

        self.addr.lower(ctx, target)?;
        target.assembler.i32_add();

        self.value.lower(ctx, target)?;
        // Use narrow stores for sub-32-bit fields so we don't corrupt adjacent bytes.
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
        match ty.size() {
            1 => target.assembler.i32_store8(memarg_byte),
            2 => target.assembler.i32_store16(memarg_word),
            _ => target.assembler.i32_store(memarg_dword),
        };
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
            // Arrays and structs are already represented as i32 linear memory addresses
            // (the result of IAlloc), so @ref is just copying that address.
            Type::Array(_, _) | Type::Struct(_) => {
                target.assembler.local_get(src_idx as u32);
                target.assembler.local_set(des_idx as u32);
            }
            // Scalar locals don't live in linear memory. Spill to the bump-pointer heap
            // so we can hand out a real address.
            _ => {
                let size = self.src.ty.size();
                // des = heap_ptr  (save the address we're about to use)
                target.assembler.global_get(0);
                target.assembler.local_tee(des_idx as u32);
                // store the value at [heap_ptr]
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
                // bump heap pointer
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

        match self.kind {
            crate::ir::CastKind::Truncate => todo!("Truncate"),
            crate::ir::CastKind::ZeroExtend => todo!("ZeroExtend"),
            crate::ir::CastKind::SignExtend => todo!("SignExtend"),
            crate::ir::CastKind::UnsignedToSigned => todo!("UnsignedToSigned"),
            crate::ir::CastKind::SignedToUnsigned => {
                let src_bytes = self.src.ty.size();
                let target_bytes = self.des.ty.size();
                if target_bytes > src_bytes {
                    target.assembler.i64_extend_i32_u();
                    return Ok(());
                }
                target.assembler.i32_trunc_sat_f64_u();
            }
            crate::ir::CastKind::FloatToInt => todo!("FloatToInt"),
            crate::ir::CastKind::IntToFloat => todo!("IntToFloat"),
            crate::ir::CastKind::BitCast => todo!("BitCast"),
            crate::ir::CastKind::NoOp => todo!("NoOp"),
        }
        Ok(())
    }
}
