use crate::ir::{
    BasicBlock, BlockId, Constant, Function, Import, Instruction, Module, Operand, Symbol,
    SymbolTable, Type, Variable, Visibility,
};

#[derive(Debug, Default)]
pub struct ModuleBuilder {
    imports: Vec<Import>,
    constants: Vec<Constant>,
    functions: Vec<Function>,
}

impl ModuleBuilder {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn import(mut self, import: Import) -> Self {
        self.imports.push(import);
        self
    }

    pub fn push_import(&mut self, import: Import) {
        self.imports.push(import);
    }

    pub fn constant(mut self, constant: Constant) -> Self {
        self.constants.push(constant);
        self
    }

    pub fn push_constant(&mut self, constant: Constant) {
        self.constants.push(constant);
    }

    pub fn function(mut self, function: Function) -> Self {
        self.functions.push(function);
        self
    }

    pub fn push_function(&mut self, function: Function) {
        self.functions.push(function);
    }

    pub fn build(self) -> Module {
        Module {
            imports: self.imports,
            constants: self.constants,
            functions: self.functions,
        }
    }
}

pub struct AssemblerBuilder<'a> {
    blocks: &'a mut Vec<BasicBlock>,
    current_block: Option<BlockId>,
    counter: u32,
    block_map: &'a mut std::collections::HashMap<String, BlockId>,
    label_counter: u32,
    variables: &'a mut Vec<Variable>,
    symbol_table: &'a mut SymbolTable,
    backfill: &'a mut Vec<(String, BlockId)>,
}

impl<'a> AssemblerBuilder<'a> {
    pub fn new(
        blocks: &'a mut Vec<BasicBlock>,
        backfill: &'a mut Vec<(String, BlockId)>,
        block_map: &'a mut std::collections::HashMap<String, BlockId>,
        symbol_table: &'a mut SymbolTable,
        variables: &'a mut Vec<Variable>,
    ) -> Self {
        Self {
            blocks,
            backfill,
            current_block: None,
            counter: 0,
            block_map,
            label_counter: 0,
            variables,
            symbol_table,
        }
    }

    fn push_instruction(&mut self, instruction: Instruction) {
        let Some(BlockId(id)) = self.current_block else {
            panic!("block not set");
        };
        self.blocks[id].instructions.push(instruction);
    }

    /// Create a new block that you can jump to.
    pub fn create_block(&mut self, label: impl Into<String>) {
        let id = self.blocks.len();
        self.current_block = Some(BlockId(id));
        let label = label.into();
        self.blocks.push(BasicBlock {
            id: BlockId(self.blocks.len()),
            instructions: vec![],
            label: label.clone(),
            successors: vec![],
            predecessors: vec![],
        });
        self.block_map.insert(label, BlockId(id));
    }

    /// returns a new temporary variable
    pub fn var(&mut self, ty: Type) -> Variable {
        let counter = self.counter;
        self.counter += 1;
        let var = Variable::new(format!("tmp{}", counter), ty);
        self.variables.push(var.clone());
        var
    }

    /// @noop
    pub fn noop(&mut self) -> &mut Self {
        self.push_instruction(Instruction::NoOp);
        self
    }

    /// `@add <type> : <des>, <lhs>, <rhs>`
    pub fn add(
        &mut self,
        des: Variable,
        lhs: impl Into<Operand>,
        rhs: impl Into<Operand>,
    ) -> &mut Self {
        let lhs = lhs.into();
        let rhs = rhs.into();
        self.push_instruction(Instruction::Add(des, lhs, rhs));
        self
    }

    /// @assign <type> : <des>, <rhs>
    pub fn assign(&mut self, var: Variable, value: impl Into<Operand>) -> &mut Self {
        let value = value.into();
        self.push_instruction(Instruction::Assign(var, value).into());
        self
    }

    /// @call <type> : <des> <func>(<args>)
    /// @call s32 : exit_code write(fact_result, 0)
    pub fn call(&mut self, des: Variable, name: impl Into<String>, args: &[Operand]) -> &mut Self {
        self.push_instruction(Instruction::Call(des, name.into(), args.to_vec()));
        self
    }

    /// `@cmp <type> : <des>, <lhs>, <rhs>`
    /// `@cmp u1 : is_one, n, 1`
    pub fn eq(
        &mut self,
        des: Variable,
        lhs: impl Into<Operand>,
        rhs: impl Into<Operand>,
    ) -> &mut Self {
        let lhs = lhs.into();
        let rhs = rhs.into();
        self.push_instruction(Instruction::Cmp(des, lhs, rhs));
        self
    }

    /// `@gt <type> : <des>, <lhs>, <rhs>`
    /// `@gt u1 : is_one, n, 1`
    pub fn gt(
        &mut self,
        des: Variable,
        lhs: impl Into<Operand>,
        rhs: impl Into<Operand>,
    ) -> &mut Self {
        let lhs = lhs.into();
        let rhs = rhs.into();
        self.push_instruction(Instruction::Gt(des, lhs, rhs));
        self
    }

    /// @jump <label>
    /// @jump %recursive_case
    pub fn jump(&mut self, label: impl Into<String>) -> &mut Self {
        let Some(BlockId(id)) = self.current_block else {
            panic!("block not set");
        };
        let label = label.into();
        let current_block_id = self.blocks[id].id;
        self.push_instruction(Instruction::Jump(label.clone()));
        let Some(BlockId(target_id)) = self.block_map.get(&label) else {
            eprintln!("Label not found: {}", label);
            self.backfill.push((label, current_block_id));
            return self;
        };
        self.blocks[id].successors.push(*target_id);
        self.blocks[*target_id]
            .predecessors
            .push(current_block_id.0);
        self
    }

    /// @jumpif <reg>, <label>
    /// @jumpif is_one, %return_one
    pub fn jump_if(&mut self, reg: impl Into<Operand>, label: impl Into<String>) -> &mut Self {
        let reg = reg.into();
        let Some(BlockId(id)) = self.current_block else {
            panic!("block not set");
        };
        let label = label.into();
        let current_block_id = self.blocks[id].id;
        self.push_instruction(Instruction::JumpIf(reg, label.clone()));
        let Some(BlockId(target_id)) = self.block_map.get(&label) else {
            eprintln!("Label not found: {}", label);
            return self;
        };
        self.blocks[id].successors.push(*target_id);
        self.blocks[*target_id]
            .predecessors
            .push(current_block_id.0);
        self
    }

    /// @load <type> : <des>, <addr>
    pub fn load(&mut self, des: Variable, addr: impl Into<Operand>) -> &mut Self {
        let addr = addr.into();
        self.push_instruction(Instruction::Load(des, addr));
        self
    }

    /// @mul <type> : <des>, <lhs>, <rhs>
    pub fn mul(
        &mut self,
        des: Variable,
        lhs: impl Into<Operand>,
        rhs: impl Into<Operand>,
    ) -> &mut Self {
        let lhs = lhs.into();
        let rhs = rhs.into();
        self.push_instruction(Instruction::Mul(des, lhs, rhs));
        self
    }

    /// @phi <type> : <des>, [(<var>, <label>)]
    pub fn phi(&mut self, des: Variable, values: Vec<(Variable, String)>) -> &mut Self {
        self.push_instruction(Instruction::Phi(des, values));
        self
    }

    /// @ret <type> : <val>
    pub fn ret(&mut self, val: Variable) -> &mut Self {
        self.push_instruction(Instruction::Return(val.ty.clone(), Operand::from(val)));
        self
    }

    /// @ret void
    pub fn void_ret(&mut self) -> &mut Self {
        self.push_instruction(Instruction::Return(Type::Void, Operand::None));
        self
    }

    /// @sub <type> : <des>, <lhs>, <rhs>
    /// @sub s32 : n_minus_one, n, 1
    pub fn sub(
        &mut self,
        des: Variable,
        lhs: impl Into<Operand>,
        rhs: impl Into<Operand>,
    ) -> &mut Self {
        let lhs = lhs.into();
        let rhs = rhs.into();
        self.push_instruction(Instruction::Sub(des, lhs, rhs));
        self
    }

    /// @div <type> : <des>, <lhs>, <rhs>
    pub fn div(
        &mut self,
        des: Variable,
        lhs: impl Into<Operand>,
        rhs: impl Into<Operand>,
    ) -> &mut Self {
        let lhs = lhs.into();
        let rhs = rhs.into();
        self.push_instruction(Instruction::Div(des, lhs, rhs));
        self
    }
}

#[derive(Debug, Default)]
pub struct FunctionBuilder {
    name: String,
    visibility: Visibility,
    params: Vec<Variable>,
    return_type: Type,
    blocks: Vec<BasicBlock>,
    backfill: Vec<(String, BlockId)>,
    block_map: std::collections::HashMap<String, BlockId>,
    variables: Vec<Variable>,
    symbol_table: SymbolTable,
}

impl FunctionBuilder {
    pub fn new(name: impl Into<String>) -> Self {
        Self {
            name: name.into(),
            ..Default::default()
        }
    }

    pub fn with_visibility(mut self, visibility: Visibility) -> Self {
        self.visibility = visibility;
        self
    }

    pub fn with_param(mut self, param: Variable) -> Self {
        self.params.push(param);
        self
    }

    pub fn with_params(mut self, params: Vec<Variable>) -> Self {
        self.params.extend(params);
        self
    }

    pub fn with_return_type(mut self, ty: Type) -> Self {
        self.return_type = ty;
        self
    }

    pub fn assembler(&mut self) -> AssemblerBuilder<'_> {
        AssemblerBuilder::new(
            &mut self.blocks,
            &mut self.backfill,
            &mut self.block_map,
            &mut self.symbol_table,
            &mut self.variables,
        )
    }

    pub fn build(mut self) -> Function {
        for (label, block_id) in self.backfill {
            let Some(BlockId(target_id)) = self.block_map.get(&label) else {
                eprintln!("Label not found: {}", label);
                continue;
            };

            self.blocks[block_id.0].successors.push(*target_id);
            self.blocks[*target_id].predecessors.push(block_id.0);
        }

        Function {
            visibility: self.visibility,
            name: self.name,
            params: self.params,
            return_type: self.return_type,
            blocks: self.blocks,
            locals: self.variables,
            symbols: self.symbol_table,
        }
    }
}
