use bitbeat::Reg;

use super::BitbeatLowerContext;
use crate::backend::Lower;
use crate::backend::bitbeat::passes::emit::OperandResult;

use crate::ir::Type;
use crate::ir::instruction::{
    IAdd, IAlloc, IAnd, IAssign, ICall, ICmp, IElemGet, IElemSet, IGt, IJump, IJumpIf, ILoad, ILt,
    IMul, IOr, IReturn, ISub, IXOr,
};

impl Lower<BitbeatLowerContext<'_>> for IAdd {
    type Output = ();
    fn lower(
        &self,
        ctx: &mut crate::backend::Context,
        target: &mut BitbeatLowerContext<'_>,
    ) -> Result<Self::Output, crate::error::Error> {
        let lhs = self.lhs.lower(ctx, target)?.lower(ctx, target)?;
        let rhs = self.rhs.lower(ctx, target)?.lower(ctx, target)?;
        let des = self.des.lower(ctx, target)?;
        target.assembler.add(des, lhs, rhs);
        Ok(())
    }
}

impl Lower<BitbeatLowerContext<'_>> for ISub {
    type Output = ();
    fn lower(
        &self,
        ctx: &mut crate::backend::Context,
        target: &mut BitbeatLowerContext<'_>,
    ) -> Result<Self::Output, crate::error::Error> {
        let lhs = self.lhs.lower(ctx, target)?.lower(ctx, target)?;
        let rhs = self.rhs.lower(ctx, target)?.lower(ctx, target)?;
        let des = self.des.lower(ctx, target)?;
        target.assembler.sub(des, lhs, rhs);
        Ok(())
    }
}

impl Lower<BitbeatLowerContext<'_>> for IMul {
    type Output = ();
    fn lower(
        &self,
        ctx: &mut crate::backend::Context,
        target: &mut BitbeatLowerContext<'_>,
    ) -> Result<Self::Output, crate::error::Error> {
        let lhs = self.lhs.lower(ctx, target)?.lower(ctx, target)?;
        let rhs = self.rhs.lower(ctx, target)?.lower(ctx, target)?;
        let des = self.des.lower(ctx, target)?;
        target.assembler.mul(des, lhs, rhs);
        Ok(())
    }
}

impl Lower<BitbeatLowerContext<'_>> for IAssign {
    type Output = ();
    fn lower(
        &self,
        ctx: &mut crate::backend::Context,
        target: &mut BitbeatLowerContext<'_>,
    ) -> Result<Self::Output, crate::error::Error> {
        let src_reg = self.src.lower(ctx, target)?.lower(ctx, target)?;
        let des_reg = self.des.lower(ctx, target)?;
        target.assembler.mov(des_reg, src_reg);
        Ok(())
    }
}

impl Lower<BitbeatLowerContext<'_>> for IAlloc {
    type Output = ();
    fn lower(
        &self,
        ctx: &mut crate::backend::Context,
        target: &mut BitbeatLowerContext<'_>,
    ) -> Result<Self::Output, crate::error::Error> {
        Ok(())
    }
}

impl Lower<BitbeatLowerContext<'_>> for ICall {
    type Output = ();
    fn lower(
        &self,
        ctx: &mut crate::backend::Context,
        target: &mut BitbeatLowerContext<'_>,
    ) -> Result<Self::Output, crate::error::Error> {
        let mut args: Vec<Reg> = Vec::new();
        for arg in self.args.iter() {
            let arg_reg = arg.lower(ctx, target)?.lower(ctx, target)?;
            args.push(arg_reg);
        }

        let ret_pid_id_reg = target.alloc();
        // HACK: we need to track to module name of the function call
        target
            .assembler
            .spawn("main", self.callee.as_str(), args, ret_pid_id_reg);
        if let Some(des) = &self.des {
            let des_reg = des.lower(ctx, target)?;
            target.assembler.recv(des_reg);
        }

        Ok(())
    }
}

impl Lower<BitbeatLowerContext<'_>> for ICmp {
    type Output = ();
    fn lower(
        &self,
        ctx: &mut crate::backend::Context,
        target: &mut BitbeatLowerContext<'_>,
    ) -> Result<Self::Output, crate::error::Error> {
        let lhs = self.lhs.lower(ctx, target)?.lower(ctx, target)?;
        let rhs = self.rhs.lower(ctx, target)?.lower(ctx, target)?;
        let des = self.des.lower(ctx, target)?;
        target.assembler.cmp_eq(des, lhs, rhs);
        Ok(())
    }
}

impl Lower<BitbeatLowerContext<'_>> for IElemGet {
    type Output = ();
    fn lower(
        &self,
        ctx: &mut crate::backend::Context,
        target: &mut BitbeatLowerContext<'_>,
    ) -> Result<Self::Output, crate::error::Error> {
        Ok(())
    }
}

impl Lower<BitbeatLowerContext<'_>> for IElemSet {
    type Output = ();
    fn lower(
        &self,
        ctx: &mut crate::backend::Context,
        target: &mut BitbeatLowerContext<'_>,
    ) -> Result<Self::Output, crate::error::Error> {
        Ok(())
    }
}

impl Lower<BitbeatLowerContext<'_>> for IAnd {
    type Output = ();
    fn lower(
        &self,
        ctx: &mut crate::backend::Context,
        target: &mut BitbeatLowerContext<'_>,
    ) -> Result<Self::Output, crate::error::Error> {
        todo!("And");
    }
}

impl Lower<BitbeatLowerContext<'_>> for IOr {
    type Output = ();
    fn lower(
        &self,
        ctx: &mut crate::backend::Context,
        target: &mut BitbeatLowerContext<'_>,
    ) -> Result<Self::Output, crate::error::Error> {
        todo!("Or");
    }
}

impl Lower<BitbeatLowerContext<'_>> for IXOr {
    type Output = ();
    fn lower(
        &self,
        ctx: &mut crate::backend::Context,
        target: &mut BitbeatLowerContext<'_>,
    ) -> Result<Self::Output, crate::error::Error> {
        let lhs = self.lhs.lower(ctx, target)?.lower(ctx, target)?;
        let rhs = self.rhs.lower(ctx, target)?.lower(ctx, target)?;
        let des = self.des.lower(ctx, target)?;
        target.assembler.xor(des, lhs, rhs);
        Ok(())
    }
}

impl Lower<BitbeatLowerContext<'_>> for IGt {
    type Output = ();
    fn lower(
        &self,
        ctx: &mut crate::backend::Context,
        target: &mut BitbeatLowerContext<'_>,
    ) -> Result<Self::Output, crate::error::Error> {
        Ok(())
    }
}

impl Lower<BitbeatLowerContext<'_>> for ILt {
    type Output = ();
    fn lower(
        &self,
        ctx: &mut crate::backend::Context,
        target: &mut BitbeatLowerContext<'_>,
    ) -> Result<Self::Output, crate::error::Error> {
        let des = self.des.lower(ctx, target)?;
        let lhs = self.lhs.lower(ctx, target)?.lower(ctx, target)?;
        let rhs = self.rhs.lower(ctx, target)?.lower(ctx, target)?;
        target.assembler.lt(des, lhs, rhs);
        Ok(())
    }
}

impl Lower<BitbeatLowerContext<'_>> for ILoad {
    type Output = ();
    fn lower(
        &self,
        ctx: &mut crate::backend::Context,
        target: &mut BitbeatLowerContext<'_>,
    ) -> Result<Self::Output, crate::error::Error> {
        let OperandResult::Value(value) = self.src.lower(ctx, target)? else {
            panic!("Load Imm expected value found register");
        };
        let reg = self.des.lower(ctx, target)?;
        target.assembler.load_imm(reg, value);
        Ok(())
    }
}

impl Lower<BitbeatLowerContext<'_>> for IReturn {
    type Output = ();
    fn lower(
        &self,
        ctx: &mut crate::backend::Context,
        target: &mut BitbeatLowerContext<'_>,
    ) -> Result<Self::Output, crate::error::Error> {
        let operand_result = self.src.lower(ctx, target)?;
        let reg = operand_result.lower(ctx, target)?;
        if target.function_name != "main" {
            target.assembler.send(bitbeat::Reg(0), reg);
        } else {
            // NOTE: this is just for debugging
            target.assembler.print(reg);
        }
        target.assembler.halt();
        Ok(())
    }
}

impl Lower<BitbeatLowerContext<'_>> for IJump {
    type Output = ();
    fn lower(
        &self,
        ctx: &mut crate::backend::Context,
        target: &mut BitbeatLowerContext<'_>,
    ) -> Result<Self::Output, crate::error::Error> {
        target.assembler.jump(&self.label);
        Ok(())
    }
}

impl Lower<BitbeatLowerContext<'_>> for IJumpIf {
    type Output = ();
    fn lower(
        &self,
        ctx: &mut crate::backend::Context,
        target: &mut BitbeatLowerContext<'_>,
    ) -> Result<Self::Output, crate::error::Error> {
        let operand_result = self.cond.lower(ctx, target)?;
        let reg = operand_result.lower(ctx, target)?;
        target.assembler.jump_if(reg, &self.label);
        Ok(())
    }
}
