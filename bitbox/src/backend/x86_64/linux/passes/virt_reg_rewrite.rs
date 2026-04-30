use std::collections::HashMap;

use crate::backend::x86_64::linux::Function;
use crate::backend::x86_64::linux::passes::emit::assembler::{
    Instruction, Location, MemIndexed, PhysReg, Reg, Reg8, Reg16, Reg32, RegKind, XmmReg,
};
use crate::passes::PassOutput;

pub struct VirtRegRewritePass;

/// Physical scratch registers to use for vreg allocation.
/// R10/R11 first (pure scratch in SysV ABI), then caller-saved arg regs.
/// Rax and Rdx are intentionally excluded: they are implicitly clobbered by
/// cqo/idiv (Rax = dividend/quotient, Rdx = remainder) and Rax is the call
/// return register. Allocating vregs there would corrupt live values.
const SCRATCH_POOL: [PhysReg; 7] = [
    PhysReg::R10,
    PhysReg::R11,
    PhysReg::Rcx,
    PhysReg::Rsi,
    PhysReg::Rdi,
    PhysReg::R8,
    PhysReg::R9,
];

const XMM_SCRATCH_POOL: [XmmReg; 16] = [
    XmmReg::Xmm0,
    XmmReg::Xmm1,
    XmmReg::Xmm2,
    XmmReg::Xmm3,
    XmmReg::Xmm4,
    XmmReg::Xmm5,
    XmmReg::Xmm6,
    XmmReg::Xmm7,
    XmmReg::Xmm8,
    XmmReg::Xmm9,
    XmmReg::Xmm10,
    XmmReg::Xmm11,
    XmmReg::Xmm12,
    XmmReg::Xmm13,
    XmmReg::Xmm14,
    XmmReg::Xmm15,
];

impl crate::passes::Pass for VirtRegRewritePass {
    fn debug_pass(&self) -> crate::passes::DebugPass {
        crate::passes::DebugPass::VirtRegRewrite
    }

    fn debug(&self, _module: &crate::ir::Module, ctx: &crate::backend::Context) -> PassOutput {
        PassOutput::String(format!("{}", ctx.output.get_x86_64()))
    }

    fn run(
        &mut self,
        _module: &mut crate::ir::Module,
        ctx: &mut crate::backend::Context,
    ) -> Result<(), crate::error::Error> {
        let x86_64 = ctx.output.get_mut_x86_64();
        for f in x86_64.functions.iter_mut() {
            rewrite_function(f);
        }
        Ok(())
    }
}

/// Visit all `Location` operands of an instruction (read-only).
fn for_each_location<F: FnMut(&Location)>(instr: &Instruction, mut f: F) {
    match instr {
        Instruction::Add(a, b)
        | Instruction::And(a, b)
        | Instruction::Cmp(a, b)
        | Instruction::Lea(a, b)
        | Instruction::Mov(a, b)
        | Instruction::Movezx(a, b)
        | Instruction::Sub(a, b)
        | Instruction::Test(a, b)
        | Instruction::Imul(a, b)
        | Instruction::Movd(a, b)
        | Instruction::Movss(a, b)
        | Instruction::Movsd(a, b)
        | Instruction::Addss(a, b)
        | Instruction::Addsd(a, b)
        | Instruction::Subss(a, b)
        | Instruction::Subsd(a, b)
        | Instruction::Mulss(a, b)
        | Instruction::Mulsd(a, b)
        | Instruction::Divss(a, b)
        | Instruction::Divsd(a, b)
        | Instruction::Ucomiss(a, b)
        | Instruction::Ucomisd(a, b)
        | Instruction::Cvtsi2ss(a, b)
        | Instruction::Cvtsi2sd(a, b) => {
            f(a);
            f(b);
            for loc in [a, b] {
                if let Location::MemIndexed(mi) = loc {
                    let base_loc = Location::Reg(mi.base);
                    let idx_loc = Location::Reg(mi.index);
                    f(&base_loc);
                    f(&idx_loc);
                }
            }
        }
        Instruction::Pop(a)
        | Instruction::Push(a)
        | Instruction::Sete(a)
        | Instruction::Setg(a)
        | Instruction::Setge(a)
        | Instruction::Setl(a)
        | Instruction::Idiv(a) => {
            f(a);
        }
        _ => {}
    }
}

/// Replace all vreg `Location`s in an instruction using the given assignment.
fn map_locations(
    instr: Instruction,
    gp_assignment: &HashMap<usize, PhysReg>,
    xmm_assignment: &HashMap<usize, XmmReg>,
) -> Instruction {
    let rw = |loc: Location| rewrite_loc(loc, gp_assignment, xmm_assignment);
    match instr {
        Instruction::Add(a, b) => Instruction::Add(rw(a), rw(b)),
        Instruction::And(a, b) => Instruction::And(rw(a), rw(b)),
        Instruction::Cmp(a, b) => Instruction::Cmp(rw(a), rw(b)),
        Instruction::Lea(a, b) => Instruction::Lea(rw(a), rw(b)),
        Instruction::Mov(a, b) => Instruction::Mov(rw(a), rw(b)),
        Instruction::Movezx(a, b) => Instruction::Movezx(rw(a), rw(b)),
        Instruction::Sub(a, b) => Instruction::Sub(rw(a), rw(b)),
        Instruction::Test(a, b) => Instruction::Test(rw(a), rw(b)),
        Instruction::Imul(a, b) => Instruction::Imul(rw(a), rw(b)),
        Instruction::Pop(a) => Instruction::Pop(rw(a)),
        Instruction::Push(a) => Instruction::Push(rw(a)),
        Instruction::Sete(a) => Instruction::Sete(rw(a)),
        Instruction::Setg(a) => Instruction::Setg(rw(a)),
        Instruction::Setge(a) => Instruction::Setge(rw(a)),
        Instruction::Setl(a) => Instruction::Setl(rw(a)),
        Instruction::Movd(a, b) => Instruction::Movd(rw(a), rw(b)),
        Instruction::Movss(a, b) => Instruction::Movss(rw(a), rw(b)),
        Instruction::Movsd(a, b) => Instruction::Movsd(rw(a), rw(b)),
        Instruction::Addss(a, b) => Instruction::Addss(rw(a), rw(b)),
        Instruction::Addsd(a, b) => Instruction::Addsd(rw(a), rw(b)),
        Instruction::Subss(a, b) => Instruction::Subss(rw(a), rw(b)),
        Instruction::Subsd(a, b) => Instruction::Subsd(rw(a), rw(b)),
        Instruction::Mulss(a, b) => Instruction::Mulss(rw(a), rw(b)),
        Instruction::Mulsd(a, b) => Instruction::Mulsd(rw(a), rw(b)),
        Instruction::Divss(a, b) => Instruction::Divss(rw(a), rw(b)),
        Instruction::Divsd(a, b) => Instruction::Divsd(rw(a), rw(b)),
        Instruction::Ucomiss(a, b) => Instruction::Ucomiss(rw(a), rw(b)),
        Instruction::Ucomisd(a, b) => Instruction::Ucomisd(rw(a), rw(b)),
        Instruction::Cvtsi2ss(a, b) => Instruction::Cvtsi2ss(rw(a), rw(b)),
        Instruction::Cvtsi2sd(a, b) => Instruction::Cvtsi2sd(rw(a), rw(b)),
        Instruction::Idiv(a) => Instruction::Idiv(rw(a)),
        other => other,
    }
}

/// Rewrite a single `Location`, replacing any vreg with the assigned physical register.
fn rewrite_loc(
    loc: Location,
    gp_assignment: &HashMap<usize, PhysReg>,
    xmm_assignment: &HashMap<usize, XmmReg>,
) -> Location {
    if let Location::Reg(Reg::VReg(v)) = loc {
        match v.kind {
            RegKind::Xmm => {
                let &xmm = xmm_assignment
                    .get(&v.id)
                    .unwrap_or_else(|| panic!("unassigned xmm vreg {}", v.id));
                Location::Reg(Reg::Xmm(xmm))
            }
            _ => {
                let &phys = gp_assignment
                    .get(&v.id)
                    .unwrap_or_else(|| panic!("unassigned gp vreg {}", v.id));
                let base: Reg = phys.into();
                Location::Reg(match v.kind {
                    RegKind::Reg64 => base,
                    RegKind::Reg32 => base.cast_to::<Reg32>(),
                    RegKind::Reg16 => base.cast_to::<Reg16>(),
                    RegKind::Reg8 => base.cast_to::<Reg8>(),
                    RegKind::Xmm => unreachable!(),
                })
            }
        }
    } else if let Location::MemIndexed(mi) = loc {
        let rw_reg = |reg: Reg| -> Reg {
            if let Reg::VReg(v) = reg {
                let &phys = gp_assignment
                    .get(&v.id)
                    .unwrap_or_else(|| panic!("unassigned vreg {} in MemIndexed", v.id));
                phys.into()
            } else {
                reg
            }
        };
        Location::MemIndexed(MemIndexed::new(rw_reg(mi.base), rw_reg(mi.index), mi.scale))
    } else {
        loc
    }
}

/// Run virtual-register allocation and rewrite for one function.
fn rewrite_function(f: &mut Function) {
    // Step 1: Compute live intervals (first..=last occurrence) for each vreg id,
    // separated into GP and XMM pools.
    let mut gp_intervals: HashMap<usize, (usize, usize)> = HashMap::new();
    let mut xmm_intervals: HashMap<usize, (usize, usize)> = HashMap::new();
    let mut idx = 0usize;
    for section in [&f.prolog, &f.instructions, &f.epilog] {
        for instr in section {
            for_each_location(instr, |loc| {
                if let Location::Reg(Reg::VReg(v)) = loc {
                    let intervals = if v.kind == RegKind::Xmm {
                        &mut xmm_intervals
                    } else {
                        &mut gp_intervals
                    };
                    let e = intervals.entry(v.id).or_insert((idx, idx));
                    e.1 = idx;
                }
            });
            idx += 1;
        }
    }

    if gp_intervals.is_empty() && xmm_intervals.is_empty() {
        return;
    }

    // Step 2a: Linear-scan for GP vregs.
    let gp_assignment = linear_scan_gp(gp_intervals);
    // Step 2b: Linear-scan for XMM vregs.
    let xmm_assignment = linear_scan_xmm(xmm_intervals);

    // Step 3: Rewrite every instruction in-place.
    for section in [&mut f.prolog, &mut f.instructions, &mut f.epilog] {
        let owned = std::mem::take(section);
        *section = owned
            .into_iter()
            .map(|instr| map_locations(instr, &gp_assignment, &xmm_assignment))
            .collect();
    }
}

fn linear_scan_gp(intervals: HashMap<usize, (usize, usize)>) -> HashMap<usize, PhysReg> {
    if intervals.is_empty() {
        return HashMap::new();
    }

    let mut sorted: Vec<(usize, usize, usize)> = intervals
        .into_iter()
        .map(|(id, (start, end))| (start, end, id))
        .collect();
    sorted.sort_unstable();

    let mut assignment: HashMap<usize, PhysReg> = HashMap::new();
    let mut active: Vec<(usize, usize, usize)> = Vec::new();
    let mut free: Vec<usize> = (0..SCRATCH_POOL.len()).rev().collect();

    for (start, end, id) in &sorted {
        let mut reclaimed = Vec::new();
        active.retain(|&(ae, _, ai)| {
            if ae < *start {
                reclaimed.push(ai);
                false
            } else {
                true
            }
        });
        free.extend(reclaimed);

        let pool_idx = free.pop().unwrap_or_else(|| {
            panic!("GP register spill required — not implemented (too many live vregs)")
        });
        assignment.insert(*id, SCRATCH_POOL[pool_idx]);
        active.push((*end, *id, pool_idx));
    }

    assignment
}

fn linear_scan_xmm(intervals: HashMap<usize, (usize, usize)>) -> HashMap<usize, XmmReg> {
    if intervals.is_empty() {
        return HashMap::new();
    }

    let mut sorted: Vec<(usize, usize, usize)> = intervals
        .into_iter()
        .map(|(id, (start, end))| (start, end, id))
        .collect();
    sorted.sort_unstable();

    let mut assignment: HashMap<usize, XmmReg> = HashMap::new();
    let mut active: Vec<(usize, usize, usize)> = Vec::new();
    let mut free: Vec<usize> = (0..XMM_SCRATCH_POOL.len()).rev().collect();

    for (start, end, id) in &sorted {
        let mut reclaimed = Vec::new();
        active.retain(|&(ae, _, ai)| {
            if ae < *start {
                reclaimed.push(ai);
                false
            } else {
                true
            }
        });
        free.extend(reclaimed);

        let pool_idx = free.pop().unwrap_or_else(|| {
            panic!("XMM register spill required — not implemented (too many live xmm vregs)")
        });
        assignment.insert(*id, XMM_SCRATCH_POOL[pool_idx]);
        active.push((*end, *id, pool_idx));
    }

    assignment
}
