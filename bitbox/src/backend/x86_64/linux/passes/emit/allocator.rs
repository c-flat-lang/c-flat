use std::collections::HashMap;

use crate::{
    backend::x86_64::linux::passes::emit::assembler::{CastableReg, PhysReg, Reg, Stack, VReg},
    ir::{Type, Variable},
};

use super::assembler::Location;

#[derive(Debug, Clone)]
pub struct Allocation {
    pub location: Location,
    pub references: usize,
    pub ty: Type,
}

#[derive(Debug)]
pub struct Allocator {
    pub(crate) variable_to_allocations: HashMap<Variable, usize>,
    pub(crate) reg_to_alloc_id: HashMap<Reg, usize>,
    pub(crate) allocations: HashMap<usize, Allocation>,
    pub(crate) next_alloc_id: usize,
    pub(crate) free_registers: Vec<PhysReg>,
    pub(crate) used_registers: Vec<PhysReg>,
    pub(crate) stack_memory_offset: i32,
}

impl Default for Allocator {
    fn default() -> Self {
        Self {
            variable_to_allocations: HashMap::default(),
            reg_to_alloc_id: HashMap::default(),
            allocations: HashMap::default(),
            next_alloc_id: usize::default(),
            free_registers: vec![
                PhysReg::Rax,
                PhysReg::Rbx,
                PhysReg::Rcx,
                PhysReg::Rdx,
                PhysReg::Rsi,
                PhysReg::Rdi,
                PhysReg::R8,
                PhysReg::R9,
                PhysReg::R10,
                PhysReg::R11,
                PhysReg::R12,
                PhysReg::R13,
                PhysReg::R14,
                PhysReg::R15,
            ],
            used_registers: Vec::default(),
            stack_memory_offset: i32::default(),
        }
    }
}

impl Allocator {
    pub fn vreg<T>(&mut self) -> Reg
    where
        T: CastableReg,
    {
        let id = self.next_alloc_id;
        self.next_alloc_id += 1;
        VReg { id, kind: T::KIND }.into()
    }

    pub fn reg<T>(&mut self) -> Reg
    where
        T: From<PhysReg>,
    {
        let Some(reg) = self.free_registers.pop() else {
            panic!("Out of registers");
        };

        self.used_registers.push(reg);
        reg.into()
    }

    pub fn free_reg(&mut self, reg: impl Into<Reg>) {
        let phys = reg.into().as_phys();
        debug_assert!(
            self.used_registers.contains(&phys),
            "Double free or freeing unused register: {:?}, {:#?}",
            phys,
            self
        );

        self.used_registers.retain(|&r| r != phys);
        self.free_registers.push(phys);
    }

    pub fn store_variable(&mut self, var: &Variable, loc: impl Into<Location>) {
        let location = loc.into();
        let alloc_id = self.next_alloc_id;
        self.next_alloc_id += 1;

        if let Location::Reg(reg) = &location {
            self.reg_to_alloc_id.insert(*reg, alloc_id);
        }

        self.allocations.insert(
            alloc_id,
            Allocation {
                location,
                references: 1,
                ty: var.ty.clone(),
            },
        );
        self.variable_to_allocations.insert(var.clone(), alloc_id);
    }

    pub fn alias_variable(&mut self, dst: &Variable, src: &Variable) {
        let alloc_id = self
            .variable_to_allocations
            .get(src)
            .expect("Expected variable to be allocated");
        if let Some(alloc) = self.allocations.get_mut(alloc_id) {
            alloc.references += 1
        }
        self.variable_to_allocations.insert(dst.clone(), *alloc_id);
    }

    pub fn get_variable_location(&self, var: &Variable) -> Option<Location> {
        let alloc_id = self.variable_to_allocations.get(var)?;

        self.allocations
            .get(alloc_id)
            .map(|alloc| alloc.location.clone())
    }

    pub fn free_variable(&mut self, var: &Variable) {
        let Some(alloc_id) = self.variable_to_allocations.remove(var) else {
            return;
        };

        let mut freed_reg: Option<Reg> = None;
        let mut remove = false;

        if let Some(alloc) = self.allocations.get_mut(&alloc_id) {
            alloc.references -= 1;

            if alloc.references == 0 {
                remove = true;
                if let Location::Reg(reg) = alloc.location {
                    freed_reg = Some(reg);
                }
            }
        }

        if remove {
            self.allocations.remove(&alloc_id);
        }

        if let Some(reg) = &freed_reg {
            self.reg_to_alloc_id.remove(reg);
        }
    }

    pub fn allocated_variables(&self) -> Vec<Variable> {
        self.variable_to_allocations
            .keys()
            .cloned()
            .collect::<Vec<_>>()
    }

    pub fn alloc_stack(&mut self, ty: &Type, count: i32) -> Stack {
        let elem_size = Stack::access_size(ty);
        let alloc_size = match &ty {
            Type::Array(len, _) => elem_size * (*len as i32),
            _ => elem_size * count,
        };

        self.stack_memory_offset += alloc_size;

        Stack {
            offset: self.stack_memory_offset,
            access_size: elem_size,
        }
    }
}
