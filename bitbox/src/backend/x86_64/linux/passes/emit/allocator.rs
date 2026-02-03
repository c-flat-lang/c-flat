use std::collections::HashMap;

use crate::{
    backend::x86_64::linux::passes::emit::assembler::{PhysReg, Reg, Stack},
    ir::{Type, Variable},
};

use super::assembler::Location;

#[derive(Debug)]
pub struct Allocation {
    pub location: Location,
    pub references: usize,
}

#[derive(Debug)]
pub struct Allocator {
    // TODO: remove public
    pub variable_to_allocations: HashMap<Variable, usize>,
    pub allocations: HashMap<usize, Allocation>,
    pub next_alloc_id: usize,

    pub free_registers: Vec<PhysReg>,
    pub used_registers: Vec<PhysReg>,
    pub(crate) stack_memory_offset: i32,
}

impl Default for Allocator {
    fn default() -> Self {
        Self {
            variable_to_allocations: HashMap::default(),
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
            ],
            used_registers: Vec::default(),
            stack_memory_offset: i32::default(),
        }
    }
}

impl Allocator {
    pub fn alloc_reg<T>(&mut self) -> T
    where
        T: From<PhysReg>,
    {
        let Some(reg) = self.free_registers.pop() else {
            panic!(
                "Out of registers\nUsed registers: {:#?}\nFree registers: {:#?}",
                self.used_registers, self.free_registers
            );
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

    pub fn alloc_temp_reg<T>(&mut self) -> Location
    where
        T: From<PhysReg>,
        Reg: From<T>,
    {
        Location::Temp(Reg::from(self.alloc_reg::<T>()))
    }

    pub fn store_variable(&mut self, var: &Variable, loc: impl Into<Location>) {
        let location = loc.into();
        debug_assert!(!matches!(location, Location::Temp(_)));

        let alloc_id = self.next_alloc_id;
        self.next_alloc_id += 1;
        self.allocations.insert(
            alloc_id,
            Allocation {
                location,
                references: 1,
            },
        );
        self.variable_to_allocations.insert(var.clone(), alloc_id);
    }

    pub fn alias_variable(&mut self, dst: &Variable, src: &Variable) {
        let alloc_id = self
            .variable_to_allocations
            .get(src)
            .expect("Expected variable to be allocated");
        self.allocations
            .get_mut(alloc_id)
            .map(|alloc| alloc.references += 1);
        self.variable_to_allocations.insert(dst.clone(), *alloc_id);
    }

    pub fn get_variable_location(&self, var: &Variable) -> Option<Location> {
        let Some(alloc_id) = self.variable_to_allocations.get(var) else {
            return None;
        };

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
                if let Location::Reg(reg) | Location::Temp(reg) = alloc.location {
                    freed_reg = Some(reg);
                }
            }
        }

        if remove {
            self.allocations.remove(&alloc_id);
        }

        if let Some(reg) = freed_reg {
            self.free_reg(reg);
        }
    }

    pub fn allocated_variables(&self) -> Vec<Variable> {
        self.variable_to_allocations
            .keys()
            .cloned()
            .collect::<Vec<_>>()
    }

    pub fn alloc_stack(&mut self, ty: &Type, count: i32) -> Stack {
        let elem_size = Stack::access_size(&ty);
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
