use std::collections::{HashMap, HashSet};

use crate::backend::x86_64::linux::Function;
use crate::backend::x86_64::linux::passes::emit::assembler::{
    Instruction, Location, MemIndexed, PhysReg, Reg, Reg8, Reg16, Reg32, RegConstraint, RegKind,
    XmmReg,
};
use crate::passes::PassOutput;

pub struct VirtRegRewritePass;

/// Physical scratch registers to use for vreg allocation.
/// R10/R11 first (pure scratch in SysV ABI), then caller-saved arg regs.
/// Rax and Rdx are intentionally excluded: they are implicitly clobbered by
/// cqo/idiv (Rax = dividend/quotient, Rdx = remainder) and Rax is the call
/// return register. Allocating vregs there would corrupt live values.
const SCRATCH_POOL: [PhysReg; 12] = [
    PhysReg::R10,
    PhysReg::R11,
    PhysReg::Rcx,
    PhysReg::Rsi,
    PhysReg::Rdi,
    PhysReg::R8,
    PhysReg::R9,
    // Callee-saved — must be preserved across calls (push/pop in prolog/epilog)
    PhysReg::Rbx,
    PhysReg::R12,
    PhysReg::R13,
    PhysReg::R14,
    PhysReg::R15,
];

const CALLEE_SAVED: &[PhysReg] = &[
    PhysReg::Rbx,
    PhysReg::R12,
    PhysReg::R13,
    PhysReg::R14,
    PhysReg::R15,
];

/// Index in `SCRATCH_POOL` of the first callee-saved register. Pool slots
/// before this are caller-saved (clobbered by `call`/`syscall`); slots from
/// here on are callee-saved and survive a call once the prolog/epilog save
/// them.
const CALLEE_SAVED_START: usize = 7;

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
    fn name(&self) -> &'static str {
        "Replacing virtual register"
    }
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
    // Helper that visits a Location AND recurses into MemIndexed sub-regs
    let mut visit = |loc: &Location| {
        f(loc);
        if let Location::MemIndexed(mi) = loc {
            f(&Location::Reg(mi.base));
            f(&Location::Reg(mi.index));
        }
    };

    match instr {
        Instruction::Add(a, b)
        | Instruction::And(a, b)
        | Instruction::Cmp(a, b)
        | Instruction::Lea(a, b)
        | Instruction::Mov(a, b)
        | Instruction::Movzx(a, b)
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
        | Instruction::Sar(a, b)
        | Instruction::Shr(a, b)
        | Instruction::Cvtss2sd(a, b)
        | Instruction::Cvtsd2ss(a, b)
        | Instruction::Cvttss2si(a, b)
        | Instruction::Cvttsd2si(a, b)
        | Instruction::Cvtsi2sd(a, b)
        | Instruction::Movsx(a, b)
        | Instruction::Xor(a, b) => {
            visit(a);
            visit(b);
        }
        Instruction::Pop(a)
        | Instruction::Push(a)
        | Instruction::Sete(a)
        | Instruction::Setg(a)
        | Instruction::Setge(a)
        | Instruction::Setl(a)
        | Instruction::Setle(a)
        | Instruction::Seta(a)
        | Instruction::Setae(a)
        | Instruction::Setb(a)
        | Instruction::Idiv(a)
        | Instruction::Div(a) => {
            visit(a);
        }
        Instruction::Call(..)
        | Instruction::Comment(..)
        | Instruction::DefineLabel(..)
        | Instruction::Jmp(..)
        | Instruction::Jnz(..)
        | Instruction::Jz(..)
        | Instruction::Ret
        | Instruction::Cqo
        | Instruction::Syscall
        | Instruction::Cdq => {}
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
        Instruction::Movzx(a, b) => Instruction::Movzx(rw(a), rw(b)),
        Instruction::Sub(a, b) => Instruction::Sub(rw(a), rw(b)),
        Instruction::Test(a, b) => Instruction::Test(rw(a), rw(b)),
        Instruction::Imul(a, b) => Instruction::Imul(rw(a), rw(b)),
        Instruction::Pop(a) => Instruction::Pop(rw(a)),
        Instruction::Push(a) => Instruction::Push(rw(a)),
        Instruction::Sete(a) => Instruction::Sete(rw(a)),
        Instruction::Setg(a) => Instruction::Setg(rw(a)),
        Instruction::Setge(a) => Instruction::Setge(rw(a)),
        Instruction::Setl(a) => Instruction::Setl(rw(a)),
        Instruction::Setle(a) => Instruction::Setle(rw(a)),
        Instruction::Seta(a) => Instruction::Seta(rw(a)),
        Instruction::Setae(a) => Instruction::Setae(rw(a)),
        Instruction::Setb(a) => Instruction::Setb(rw(a)),
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
        Instruction::Cvtss2sd(a, b) => Instruction::Cvtss2sd(rw(a), rw(b)),
        Instruction::Cvtsd2ss(a, b) => Instruction::Cvtsd2ss(rw(a), rw(b)),
        Instruction::Cvttss2si(a, b) => Instruction::Cvttss2si(rw(a), rw(b)),
        Instruction::Cvttsd2si(a, b) => Instruction::Cvttsd2si(rw(a), rw(b)),
        Instruction::Movsx(a, b) => Instruction::Movsx(rw(a), rw(b)),
        Instruction::Idiv(a) => Instruction::Idiv(rw(a)),
        Instruction::Div(a) => Instruction::Div(rw(a)),
        Instruction::Sar(a, b) => Instruction::Sar(rw(a), rw(b)),
        Instruction::Shr(a, b) => Instruction::Shr(rw(a), rw(b)),
        Instruction::Xor(a, b) => Instruction::Xor(rw(a), rw(b)),
        Instruction::Call(..)
        | Instruction::Comment(..)
        | Instruction::DefineLabel(..)
        | Instruction::Jmp(..)
        | Instruction::Jnz(..)
        | Instruction::Jz(..)
        | Instruction::Ret
        | Instruction::Cqo
        | Instruction::Syscall
        | Instruction::Cdq => instr,
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
    let mut constraints: HashMap<usize, PhysReg> = HashMap::new();

    // Instruction indices (in the flattened prolog+instructions+epilog space)
    // that clobber caller-saved registers: `call` and `syscall`. A vreg whose
    // live interval spans one of these must be given a callee-saved register.
    let mut clobber_indices: Vec<usize> = Vec::new();

    let mut idx = 0usize;
    for section in [&f.prolog, &f.instructions, &f.epilog] {
        for instr in section {
            if matches!(instr, Instruction::Call(..) | Instruction::Syscall) {
                clobber_indices.push(idx);
            }
            for_each_location(instr, |loc| {
                if let Location::Reg(Reg::VReg(v)) = loc {
                    if let RegConstraint::Fixed(phys) = v.constraint {
                        constraints.insert(v.id, phys);
                    }
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
    let gp_assignment = linear_scan_gp(gp_intervals, constraints, &clobber_indices);
    // Step 2b: Linear-scan for XMM vregs.
    let xmm_assignment = linear_scan_xmm(xmm_intervals);

    // Determine which callee-saved regs were actually allocated
    let used_callee_saved: Vec<PhysReg> = CALLEE_SAVED
        .iter()
        .filter(|&&r| gp_assignment.values().any(|&a| a == r))
        .copied()
        .collect();

    // Insert pushes just after the function's entry label (so they land inside
    // the function, not before its label) and pops before the final Ret. Push
    // in forward order and pop in reverse so the stack stays balanced.
    let insert_pos = f
        .prolog
        .iter()
        .position(|i| matches!(i, Instruction::DefineLabel(..)))
        .map(|p| p + 1)
        .unwrap_or(0);
    for (i, &reg) in used_callee_saved.iter().enumerate() {
        f.prolog
            .insert(insert_pos + i, Instruction::Push(Location::Reg(reg.into())));
    }
    for &reg in used_callee_saved.iter().rev() {
        // Insert before the final Ret
        let ret_pos = f
            .epilog
            .iter()
            .rposition(|i| matches!(i, Instruction::Ret))
            .unwrap_or(f.epilog.len());
        f.epilog
            .insert(ret_pos, Instruction::Pop(Location::Reg(reg.into())));
    }

    // Step 3: Rewrite every instruction in-place.
    for section in [&mut f.prolog, &mut f.instructions, &mut f.epilog] {
        let owned = std::mem::take(section);
        *section = owned
            .into_iter()
            .map(|instr| map_locations(instr, &gp_assignment, &xmm_assignment))
            .collect();
    }
}

fn linear_scan_gp(
    intervals: HashMap<usize, (usize, usize)>,
    constraints: HashMap<usize, PhysReg>,
    clobber_indices: &[usize],
) -> HashMap<usize, PhysReg> {
    if intervals.is_empty() {
        return HashMap::new();
    }

    let mut assignment: HashMap<usize, PhysReg> = HashMap::new();

    let mut fixed_intervals: Vec<(usize, usize, usize, PhysReg)> = Vec::new();
    let mut free_intervals: Vec<(usize, usize, usize)> = Vec::new();

    for (id, (start, end)) in &intervals {
        if let Some(&phys) = constraints.get(id) {
            assignment.insert(*id, phys);
            fixed_intervals.push((*start, *end, *id, phys));
        } else {
            free_intervals.push((*start, *end, *id));
        }
    }

    free_intervals.sort_unstable();

    let mut active: Vec<(usize, usize, usize)> = Vec::new();
    let mut free: Vec<usize> = (0..SCRATCH_POOL.len()).rev().collect();

    let mut next_slot: usize = 0; // bump allocator for stack slots
    let mut spills: HashMap<usize, usize> = HashMap::new(); // vreg → stack slot index

    for (start, end, id) in &free_intervals {
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

        let mut blocked: HashSet<usize> = fixed_intervals
            .iter()
            .filter(|(fs, fe, _, _)| fs <= start && start <= fe)
            .filter_map(|(_, _, _, phys)| SCRATCH_POOL.iter().position(|p| p == phys))
            .collect();

        // If this value is live across a `call`/`syscall`, a caller-saved
        // register would be clobbered by the callee. Restrict it to the
        // callee-saved slots, which the prolog/epilog preserve.
        let crosses_call = clobber_indices.iter().any(|&c| *start < c && c < *end);
        if crosses_call {
            for i in 0..CALLEE_SAVED_START {
                blocked.insert(i);
            }
        }

        let pool_idx = free
            .iter()
            .rposition(|idx| !blocked.contains(idx))
            .map(|pos| free.remove(pos));

        let pool_idx = match pool_idx {
            Some(idx) => idx,
            None => {
                // Find the active interval with the furthest end point
                // that isn't blocked by a fixed constraint at this position
                let victim_pos = active
                    .iter()
                    .enumerate()
                    .filter(|&(_, &(_, _, pidx))| !blocked.contains(&pidx))
                    .max_by_key(|&(_, &(ae, _, _))| ae);

                match victim_pos {
                    Some((pos, &(victim_end, victim_id, victim_pidx))) if victim_end > *end => {
                        // Spill the victim — it lives longer, so evicting it
                        // frees the register for more intervals overall
                        active.remove(pos);
                        let slot = next_slot;
                        next_slot += 1;
                        spills.insert(victim_id, slot);
                        victim_pidx // steal its register index
                    }
                    _ => {
                        // Spill current interval instead — it ends last
                        let slot = next_slot;
                        next_slot += 1;
                        spills.insert(*id, slot);
                        continue; // skip assigning a register, move to next interval
                    }
                }
            }
        };

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
