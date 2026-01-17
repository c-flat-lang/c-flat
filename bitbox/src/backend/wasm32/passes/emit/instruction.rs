use wasm_encoder::{BlockType, ValType};

use super::Wasm32LowerContext;
use crate::backend::Lower;

use crate::ir::Type;
use crate::ir::instruction::{
    IAdd, IAlloc, IAnd, IAssign, ICall, ICmp, ICopy, IElemGet, IElemSet, IGt, IGte, IIfElse, IJump,
    IJumpIf, ILoad, ILoop, ILt, IOr, IReturn, ISub, IXOr,
};

impl Lower<Wasm32LowerContext<'_>> for IAdd {
    type Output = ();
    fn lower(
        &self,
        ctx: &mut crate::backend::Context,
        target: &mut Wasm32LowerContext<'_>,
    ) -> Result<Self::Output, crate::error::Error> {
        self.lhs.lower(ctx, target);
        self.rhs.lower(ctx, target);
        match self.des.ty.clone().into() {
            ValType::I32 => target.assembler.i32_add(),
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

impl Lower<Wasm32LowerContext<'_>> for ISub {
    type Output = ();
    fn lower(
        &self,
        ctx: &mut crate::backend::Context,
        target: &mut Wasm32LowerContext<'_>,
    ) -> Result<Self::Output, crate::error::Error> {
        self.lhs.lower(ctx, target);
        self.rhs.lower(ctx, target);
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

impl Lower<Wasm32LowerContext<'_>> for IAssign {
    type Output = ();
    fn lower(
        &self,
        ctx: &mut crate::backend::Context,
        target: &mut Wasm32LowerContext<'_>,
    ) -> Result<Self::Output, crate::error::Error> {
        match self.des.ty.clone().into() {
            ValType::I32 => {
                let variables = ctx.local_function_variables.get(&target.function_name);
                let Some(idx) = variables.iter().position(|v| v.name == self.des.name) else {
                    panic!(
                        "Variable {:?} not found in {:?}",
                        self.des, target.function_name
                    );
                };
                self.src.lower(ctx, target);
                target.assembler.local_set(idx as u32);
            }
            ValType::I64 => todo!("@assign i64"),
            ValType::F32 => todo!("@assign f32"),
            ValType::F64 => todo!("@assign f64"),
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
        target.assembler.i32_const(self.des.ty.size()); // element size (e.g. 4)
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
            operand.lower(ctx, target);
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
        self.lhs.lower(ctx, target);
        self.rhs.lower(ctx, target);
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
        self.src.lower(ctx, target);
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
        self.index.lower(ctx, target);
        target.assembler.i32_const(self.des.ty.size());
        target.assembler.i32_mul();

        self.ptr.lower(ctx, target);
        target.assembler.i32_add();

        match self.des.ty.clone().into() {
            ValType::I32 => target.assembler.i32_load(wasm_encoder::MemArg {
                offset: 0,
                align: 2,
                memory_index: 0,
            }),
            ValType::I64 => todo!("@gt i64"),
            ValType::F32 => todo!("@gt f32"),
            ValType::F64 => todo!("@gt f64"),
            ValType::V128 => todo!("@gt v128"),
            ValType::Ref(_) => todo!("@gt ref"),
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
        let ty = match self.addr.ty.clone() {
            Type::Array(_, ty) => *ty,
            ty => ty,
        };
        self.index.lower(ctx, target);
        target.assembler.i32_const(ty.size());
        target.assembler.i32_mul();

        self.addr.lower(ctx, target);
        target.assembler.i32_add();

        self.value.lower(ctx, target);
        match ty.into() {
            ValType::I32 => target.assembler.i32_store(wasm_encoder::MemArg {
                offset: 0,
                align: 2,
                memory_index: 0,
            }),
            ValType::I64 => todo!("@gt i64"),
            ValType::F32 => todo!("@gt f32"),
            ValType::F64 => todo!("@gt f64"),
            ValType::V128 => todo!("@gt v128"),
            ValType::Ref(_) => todo!("@gt ref"),
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
        self.lhs.lower(ctx, target);
        self.rhs.lower(ctx, target);
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
        self.lhs.lower(ctx, target);
        self.rhs.lower(ctx, target);
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
        self.lhs.lower(ctx, target);
        self.rhs.lower(ctx, target);
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
        self.lhs.lower(ctx, target);
        self.rhs.lower(ctx, target);
        match self.des.ty.clone().into() {
            ValType::I32 => target.assembler.i32_gt_s(),
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

impl Lower<Wasm32LowerContext<'_>> for IGte {
    type Output = ();
    fn lower(
        &self,
        ctx: &mut crate::backend::Context,
        target: &mut Wasm32LowerContext<'_>,
    ) -> Result<Self::Output, crate::error::Error> {
        self.lhs.lower(ctx, target);
        self.rhs.lower(ctx, target);
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

impl Lower<Wasm32LowerContext<'_>> for ILt {
    type Output = ();
    fn lower(
        &self,
        ctx: &mut crate::backend::Context,
        target: &mut Wasm32LowerContext<'_>,
    ) -> Result<Self::Output, crate::error::Error> {
        self.lhs.lower(ctx, target);
        self.rhs.lower(ctx, target);
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
        ctx: &mut crate::backend::Context,
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
        self.cond.lower(ctx, target);
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
                self.src.lower(ctx, target);
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

        // let ty: BlockType = result
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
