use crate::ir::{
    instruction::{
        IAdd, IAlloc, IAssign, ICall, ICmp, IDiv, IElemGet, IElemSet, IGt, ILt, IMul, INoOp, ISub,
    },
    BasicBlock, BlockId, Constant, Function, Import, Instruction, Module, Operand, Type, Variable,
    Visibility,
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

    pub fn import(&mut self, import: Import) -> &mut Self {
        self.imports.push(import);
        self
    }

    pub fn constant(&mut self, constant: Constant) -> &mut Self {
        self.constants.push(constant);
        self
    }

    pub fn function(&mut self, function: Function) -> &mut Self {
        self.functions.push(function);
        self
    }

    pub fn build(self) -> Module {
        Module {
            imports: self.imports,
            constants: self.constants,
            functions: self.functions,
        }
    }
}

#[derive(Debug)]
pub struct AssemblerBuilder<'a> {
    blocks: &'a mut Vec<BasicBlock>,
    current_block: Option<BlockId>,
    variables: &'a mut Vec<Variable>,
}

impl<'a> AssemblerBuilder<'a> {
    pub fn new(blocks: &'a mut Vec<BasicBlock>, variables: &'a mut Vec<Variable>) -> Self {
        Self {
            blocks,
            current_block: None,
            variables,
        }
    }

    fn push_instruction(&mut self, instruction: impl Into<Instruction>) {
        let Some(BlockId(id)) = self.current_block else {
            panic!("block not set");
        };
        self.blocks[id].instructions.push(instruction.into());
    }

    /// Create a new block that you can jump to.
    pub fn create_block(&mut self, label: impl Into<String>) {
        let id = self.blocks.len();
        self.current_block = Some(BlockId(id));
        self.blocks.push(BasicBlock {
            id: BlockId(id),
            instructions: vec![],
            label: label.into(),
        });
    }

    /// returns a new temporary variable
    pub fn var(&mut self, ty: Type) -> Variable {
        let var = Variable::new(format!("{}", uuid::Uuid::new_v4()), ty);
        self.variables.push(var.clone());
        var
    }

    /// @noop
    pub fn noop(&mut self) -> &mut Self {
        self.push_instruction(Instruction::NoOp(INoOp));
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
        self.push_instruction(IAdd::new(des, lhs, rhs));
        self
    }

    /// @assign <type> : <des>, <rhs>
    pub fn assign(&mut self, var: Variable, value: impl Into<Operand>) -> &mut Self {
        let value = value.into();
        self.push_instruction(IAssign::new(var, value));
        self
    }

    /// @alloc <type> : <des>
    pub fn alloc(&mut self, ty: Type, des: Variable, size: impl Into<Operand>) -> &mut Self {
        self.push_instruction(IAlloc::new(ty, des, size.into()));
        self
    }

    /// @call <type> : <des> <func>(<args>)
    /// @call s32 : exit_code write(fact_result, 0)
    pub fn call(
        &mut self,
        des: Option<Variable>,
        name: impl Into<String>,
        args: &[Operand],
    ) -> &mut Self {
        self.push_instruction(ICall::new(des, name.into(), args.to_vec()));
        self
    }

    /// @elemget <type> : <des>, <ptr>, <index>
    pub fn elemget(
        &mut self,
        des: Variable,
        ptr: impl Into<Operand>,
        index: impl Into<Operand>,
    ) -> &mut Self {
        self.push_instruction(IElemGet::new(des, ptr.into(), index.into()));
        self
    }

    /// @elemset <type> : <addr>, <index>, <value>
    pub fn elemset(
        &mut self,
        addr: Variable,
        index: impl Into<Operand>,
        value: impl Into<Operand>,
    ) -> &mut Self {
        self.push_instruction(IElemSet::new(addr, index.into(), value.into()));
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
        self.push_instruction(ICmp::new(des, lhs, rhs));
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
        self.push_instruction(IGt::new(des, lhs, rhs));
        self
    }

    /// `@lt <type> : <des>, <lhs>, <rhs>`
    /// `@lt u1 : is_one, n, 1`
    pub fn lt(
        &mut self,
        des: Variable,
        lhs: impl Into<Operand>,
        rhs: impl Into<Operand>,
    ) -> &mut Self {
        let lhs = lhs.into();
        let rhs = rhs.into();
        self.push_instruction(ILt::new(des, lhs, rhs));
        self
    }

    /// @jump <label>
    /// @jump %recursive_case
    pub fn jump(&mut self, label: impl Into<String>) -> &mut Self {
        self.push_instruction(Instruction::Jump(label.into()));
        self
    }

    /// @jumpif <reg>, <label>
    /// @jumpif is_one, %return_one
    pub fn jump_if(&mut self, reg: impl Into<Operand>, label: impl Into<String>) -> &mut Self {
        let reg = reg.into();
        self.push_instruction(Instruction::JumpIf(reg, label.into()));
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
        self.push_instruction(IMul::new(des, lhs, rhs));
        self
    }

    /// @phi <type> : <des>, [(<var>, <label>)]
    pub fn phi(&mut self, des: Variable, values: Vec<(Variable, String)>) -> &mut Self {
        self.push_instruction(Instruction::Phi(des, values));
        self
    }

    /// @ret <type> : <val>
    pub fn ret(&mut self, ty: Type, val: impl Into<Operand>) -> &mut Self {
        self.push_instruction(Instruction::Return(ty, val.into()));
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
        self.push_instruction(ISub::new(des, lhs, rhs));
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
        self.push_instruction(IDiv::new(des, lhs, rhs));
        self
    }

    pub fn if_else(
        &mut self,
        cond: Vec<BasicBlock>,
        cond_result: Variable,
        then_branch: Vec<BasicBlock>,
        else_branch: Vec<BasicBlock>,
        result: Option<Variable>,
    ) -> &mut Self {
        let instruction = Instruction::IfElse {
            cond,
            cond_result,
            then_branch,
            else_branch,
            result,
        };
        self.push_instruction(instruction);
        self
    }
}

#[derive(Debug, Default)]
pub struct FunctionBuilder {
    pub name: String,
    pub visibility: Visibility,
    pub params: Vec<Variable>,
    pub return_type: Type,
    pub blocks: Vec<BasicBlock>,
    pub variables: Vec<Variable>,
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
        AssemblerBuilder::new(&mut self.blocks, &mut self.variables)
    }

    pub fn build(self) -> Function {
        Function {
            visibility: self.visibility,
            name: self.name,
            params: self.params,
            return_type: self.return_type,
            blocks: self.blocks,
        }
    }
}
