use std::collections::HashMap;

use crate::backend::x86_64::linux::Function;
use crate::backend::x86_64::linux::passes::emit::assembler::{
    Instruction, Location, PhysReg, Reg, Reg8, Reg16, Reg32, RegKind,
};
use crate::passes::PassOutput;

pub struct VirtRegRewritePass;

/// Physical scratch registers to use for vreg allocation.
/// R10/R11 first (pure scratch in SysV ABI), then caller-saved regs.
const SCRATCH_POOL: [PhysReg; 9] = [
    PhysReg::R10,
    PhysReg::R11,
    PhysReg::Rax,
    PhysReg::Rcx,
    PhysReg::Rdx,
    PhysReg::Rsi,
    PhysReg::Rdi,
    PhysReg::R8,
    PhysReg::R9,
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
        | Instruction::Test(a, b) => {
            f(a);
            f(b);
        }
        Instruction::Pop(a)
        | Instruction::Push(a)
        | Instruction::Sete(a)
        | Instruction::Setg(a)
        | Instruction::Setge(a)
        | Instruction::Setl(a) => {
            f(a);
        }
        _ => {}
    }
}

/// Replace all vreg `Location`s in an instruction using the given assignment.
fn map_locations(instr: Instruction, assignment: &HashMap<usize, PhysReg>) -> Instruction {
    let rw = |loc: Location| rewrite_loc(loc, assignment);
    match instr {
        Instruction::Add(a, b) => Instruction::Add(rw(a), rw(b)),
        Instruction::And(a, b) => Instruction::And(rw(a), rw(b)),
        Instruction::Cmp(a, b) => Instruction::Cmp(rw(a), rw(b)),
        Instruction::Lea(a, b) => Instruction::Lea(rw(a), rw(b)),
        Instruction::Mov(a, b) => Instruction::Mov(rw(a), rw(b)),
        Instruction::Movezx(a, b) => Instruction::Movezx(rw(a), rw(b)),
        Instruction::Sub(a, b) => Instruction::Sub(rw(a), rw(b)),
        Instruction::Test(a, b) => Instruction::Test(rw(a), rw(b)),
        Instruction::Pop(a) => Instruction::Pop(rw(a)),
        Instruction::Push(a) => Instruction::Push(rw(a)),
        Instruction::Sete(a) => Instruction::Sete(rw(a)),
        Instruction::Setg(a) => Instruction::Setg(rw(a)),
        Instruction::Setge(a) => Instruction::Setge(rw(a)),
        Instruction::Setl(a) => Instruction::Setl(rw(a)),
        other => other,
    }
}

/// Rewrite a single `Location`, replacing any vreg with the assigned physical register.
fn rewrite_loc(loc: Location, assignment: &HashMap<usize, PhysReg>) -> Location {
    if let Location::Reg(Reg::VReg(v)) = loc {
        let &phys = assignment
            .get(&v.id)
            .unwrap_or_else(|| panic!("unassigned vreg {}", v.id));
        let base: Reg = phys.into(); // Reg::Reg64(...)
        Location::Reg(match v.kind {
            RegKind::Reg64 => base,
            RegKind::Reg32 => base.cast_to::<Reg32>(),
            RegKind::Reg16 => base.cast_to::<Reg16>(),
            RegKind::Reg8 => base.cast_to::<Reg8>(),
        })
    } else {
        loc
    }
}

/// Run virtual-register allocation and rewrite for one function.
fn rewrite_function(f: &mut Function) {
    // Step 1: Compute live intervals (first..=last occurrence) for each vreg id.
    let mut intervals: HashMap<usize, (usize, usize)> = HashMap::new();
    let mut idx = 0usize;
    for section in [&f.prolog, &f.instructions, &f.epilog] {
        for instr in section {
            for_each_location(instr, |loc| {
                if let Location::Reg(Reg::VReg(v)) = loc {
                    let e = intervals.entry(v.id).or_insert((idx, idx));
                    e.1 = idx;
                }
            });
            idx += 1;
        }
    }

    if intervals.is_empty() {
        return;
    }

    // Step 2: Linear-scan register allocation (sorted by interval start).
    let mut sorted: Vec<(usize, usize, usize)> = intervals
        .into_iter()
        .map(|(id, (start, end))| (start, end, id))
        .collect();
    sorted.sort_unstable();

    let mut assignment: HashMap<usize, PhysReg> = HashMap::new();
    // active: (end_idx, vreg_id, pool_index)
    let mut active: Vec<(usize, usize, usize)> = Vec::new();
    // free pool indices (use as a stack so earlier entries stay available longer)
    let mut free: Vec<usize> = (0..SCRATCH_POOL.len()).rev().collect();

    for (start, end, id) in &sorted {
        // Expire intervals that ended before this interval starts.
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
            panic!("register spill required — not implemented (too many live vregs)")
        });
        assignment.insert(*id, SCRATCH_POOL[pool_idx]);
        active.push((*end, *id, pool_idx));
    }

    // Step 3: Rewrite every instruction in-place.
    for section in [&mut f.prolog, &mut f.instructions, &mut f.epilog] {
        let owned = std::mem::take(section);
        *section = owned
            .into_iter()
            .map(|instr| map_locations(instr, &assignment))
            .collect();
    }
}
