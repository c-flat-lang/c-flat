use wasm_encoder::ValType;

use crate::backend::wasm32::passes::emit::Wasm32LowerContext;
use crate::backend::Lower;

use crate::ir::instruction::{
    IAdd, IAlloc, IAssign, ICall, ICmp, IElemGet, IElemSet, IGt, ILoad, ILt, IReturn,
};
use crate::ir::Type;

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
                    panic!("Variable {:?} not found", self.des);
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
        self.des.lower(ctx, target);
        self.size.lower(ctx, target)?;
        target.assembler.i32_mul();

        target.assembler.global_get(0); // HACK: probably need to make a HEAP index.

        let Some(ptr_idx) = ctx
            .local_function_variables
            .get(&target.function_name)
            .iter()
            .position(|v| v.name == self.des.name)
        else {
            panic!("Variable {:?} not found", self.des);
        };

        target.assembler.local_tee(ptr_idx as u32);
        target.assembler.i32_add();
        target.assembler.global_set(0); // HACK: probably need to make a HEAP index.
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
        let ty = self.addr.ty.clone();
        self.index.lower(ctx, target);
        target.assembler.i32_const(ty.size());
        target.assembler.i32_mul();
        self.value.lower(ctx, target);
        match ty.clone().into() {
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
