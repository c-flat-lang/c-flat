mod emit;
mod instruction;

use bitbeat::InstructionBuilder;
pub use emit::EmitBitbeatPass;

#[derive(Debug)]
struct BitbeatLowerContext<'ctx> {
    function_name: String,
    assembler: bitbeat::InstructionBuilder<'ctx>,
    registery: [bool; bitbeat::REG_COUNT],
    current_block_id: crate::ir::BlockId,
    ip: usize,
    // HACK: key should be a Variable not String
    variables: std::collections::BTreeMap<String, bitbeat::Reg>,
}

impl<'ctx> BitbeatLowerContext<'ctx> {
    fn new(function_name: String, assembler: bitbeat::InstructionBuilder<'ctx>) -> Self {
        Self {
            function_name,
            assembler,
            registery: [false; bitbeat::REG_COUNT],
            current_block_id: crate::ir::BlockId(0),
            ip: 0,
            variables: std::collections::BTreeMap::new(),
        }
    }

    fn returns(&mut self) -> &mut Self {
        self.registery[0] = true;
        self
    }

    fn alloc(&mut self) -> bitbeat::Reg {
        let id = self
            .registery
            .iter()
            .position(|r| !r)
            .expect("Out of registers");
        self.registery[id] = true;
        bitbeat::Reg(id)
    }

    fn free(&mut self, reg: &bitbeat::Reg) {
        self.registery[reg.0] = false
    }
}

#[derive(Debug, Clone)]
enum OperandResult {
    Register(bitbeat::Reg),
    Value(i64),
}
