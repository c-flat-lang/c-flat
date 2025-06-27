pub type Span = std::ops::Range<usize>;

pub type SymbolTable = std::collections::HashMap<String, Symbol>;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Symbol {
    Label { name: String, index: usize },
    Variable(Variable),
}

impl Symbol {
    fn label(label: &str, index: usize) -> Symbol {
        Symbol::Label {
            name: label.to_string(),
            index,
        }
    }
}

#[derive(Debug, Default, Clone, PartialEq, Eq, Hash)]
pub enum Type {
    Unsigned(u8),
    Signed(u8),
    Float(u8),
    Pointer(Box<Type>),
    Array(usize, Box<Type>),
    #[default]
    Void,
}

impl std::fmt::Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Type::Unsigned(bytes) => write!(f, "u{}", bytes),
            Type::Signed(bytes) => write!(f, "s{}", bytes),
            Type::Float(bytes) => write!(f, "f{}", bytes),
            Type::Pointer(ty) => write!(f, "*{}", ty),
            Type::Array(size, ty) => write!(f, "[{} x {}]", size, ty),
            Type::Void => write!(f, "void"),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Identifier {
    pub name: String,
    pub span: Option<Span>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Variable {
    pub name: String,
    pub ty: Type,
    pub version: usize,
}

impl Variable {
    pub fn new(name: impl Into<String>, ty: Type) -> Self {
        Self {
            name: name.into(),
            ty,
            version: 0,
        }
    }
    pub fn new_version(self) -> Self {
        Self {
            version: self.version + 1,
            ..self
        }
    }
}

impl std::fmt::Display for Variable {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "_{}_v{}_", self.name, self.version)
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct LabeledInstruction {
    pub label: Option<String>,
    pub instruction: Instruction,
}

impl std::fmt::Display for LabeledInstruction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if let Some(label) = &self.label {
            write!(f, "{}: ", label)?
        }
        write!(f, "  {}", self.instruction)
    }
}

impl LabeledInstruction {
    pub fn new(label: impl Into<String>, instruction: Instruction) -> Self {
        Self {
            label: Some(label.into()),
            instruction,
        }
    }

    pub fn without_label(instruction: Instruction) -> Self {
        Self {
            label: None,
            instruction,
        }
    }
}

impl From<Instruction> for LabeledInstruction {
    fn from(instruction: Instruction) -> Self {
        Self {
            label: None,
            instruction,
        }
    }
}

/// All instructions in text form start with @
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Instruction {
    /// @noop
    NoOp,
    /// `@add <type> : <des>, <lhs>, <rhs>`
    Add(Variable, Operand, Operand),
    /// @assign <type> : <des>, <rhs>
    Assign(Variable, Operand),
    /// @call <type> : <des> <func>(<args>)
    /// @call s32 : exit_code write(fact_result, 0)
    Call(Variable, String, Vec<Operand>),
    /// `@cmp <type> : <des>, <lhs>, <rhs>`
    /// `@cmp u1 : is_one, n, 1`
    Cmp(Variable, Operand, Operand),
    /// @jump <label>
    /// @jump %recursive_case
    Jump(String),
    /// @jumpif <reg>, <label>
    /// @jumpif is_one, %return_one
    JumpIf(Operand, String),
    /// @load <type> : <des>, <addr>
    Load(Variable, Operand),
    /// @mul <type> : <des>, <lhs>, <rhs>
    Mul(Variable, Operand, Operand),
    /// @phi <type> : <des>, [(<var>, <label>)]
    Phi(Variable, Vec<(Variable, String)>),
    /// @ret <type> : <val>
    Return(Type, Operand),
    /// @sub <type> : <des>, <lhs>, <rhs>
    /// @sub s32 : n_minus_one, n, 1
    Sub(Variable, Operand, Operand),
    /// @div <type> : <des>, <lhs>, <rhs>
    Div(Variable, Operand, Operand),
}

impl Instruction {
    pub fn with_label(self, label: impl Into<String>) -> LabeledInstruction {
        LabeledInstruction::new(label, self)
    }
}

impl std::fmt::Display for Instruction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Instruction::NoOp => write!(f, "@noop"),
            Instruction::Add(variable, operand, operand1) => write!(
                f,
                "@add {} : {}, {}, {}",
                variable.ty, variable, operand, operand1
            ),
            Instruction::Assign(variable, operand) => {
                write!(f, "@assign {} : {}, {}", variable.ty, variable, operand)
            }
            Instruction::Call(des, caller, args) => {
                write!(
                    f,
                    "@call {} : {}, {}({})",
                    des.ty,
                    des,
                    caller,
                    args.iter()
                        .map(|arg| format!("{}, ", arg))
                        .collect::<String>()
                )
            }
            Instruction::Cmp(variable, operand, operand1) => write!(
                f,
                "@cmp {} : {}, {}, {}",
                variable.ty, variable, operand, operand1
            ),
            Instruction::Jump(label) => write!(f, "@jump {}", label),
            Instruction::JumpIf(operand, label) => write!(f, "@jumpif {}, {}", operand, label),
            Instruction::Load(variable, operand) => {
                write!(f, "@load {} : {}, {}", variable.ty, variable, operand)
            }
            Instruction::Mul(variable, operand, operand1) => write!(
                f,
                "@mul {} : {}, {}, {}",
                variable.ty, variable, operand, operand1
            ),
            Instruction::Phi(variable, items) => {
                write!(
                    f,
                    "@phi {} : {}, [{}]",
                    variable.ty,
                    variable,
                    items
                        .iter()
                        .map(|(var, block)| format!("[{}, {}]", var, block))
                        .collect::<String>()
                )
            }
            Instruction::Return(ty, operand) => write!(f, "@ret {}, {}", ty, operand),
            Instruction::Sub(variable, operand, operand1) => write!(
                f,
                "@sub {} : {}, {}, {}",
                variable.ty, variable, operand, operand1
            ),
            Instruction::Div(variable, operand, operand1) => write!(
                f,
                "@div {} : {}, {}, {}",
                variable.ty, variable, operand, operand1
            ),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Operand {
    ConstantInt { value: String, ty: Type },
    Variable(Variable),
    None,
}

impl Operand {
    pub fn const_bool(value: bool) -> Self {
        if value {
            Operand::ConstantInt {
                value: "1".to_string(),
                ty: Type::Unsigned(32),
            }
        } else {
            Operand::ConstantInt {
                value: "0".to_string(),
                ty: Type::Unsigned(32),
            }
        }
    }

    pub fn const_unsigned(value: impl Into<String>, bits: u8) -> Self {
        Operand::ConstantInt {
            value: value.into(),
            ty: Type::Unsigned(bits),
        }
    }

    pub fn const_signed(value: impl Into<String>, bits: u8) -> Self {
        Operand::ConstantInt {
            value: value.into(),
            ty: Type::Signed(bits),
        }
    }

    pub fn const_float(value: impl Into<String>, bits: u8) -> Self {
        Operand::ConstantInt {
            value: value.into(),
            ty: Type::Float(bits),
        }
    }
}

impl From<Variable> for Operand {
    fn from(variable: Variable) -> Self {
        Operand::Variable(variable)
    }
}

impl From<&Variable> for Operand {
    fn from(variable: &Variable) -> Self {
        Operand::Variable(variable.clone())
    }
}

impl std::fmt::Display for Operand {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Operand::ConstantInt { value, .. } => write!(f, "{}", value),
            Operand::Variable(variable) => write!(f, "{}", variable),
            Operand::None => write!(f, ""),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct BlockId(usize);

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct BasicBlock {
    pub id: BlockId,
    pub instructions: Vec<LabeledInstruction>,
    pub successors: Vec<usize>,
    pub predecessors: Vec<usize>,
}

#[derive(Debug, Default, Clone, PartialEq, Eq)]
pub enum Visibility {
    Public,
    #[default]
    Private,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Function {
    pub visibility: Visibility,
    pub name: String,
    pub params: Vec<Variable>,
    pub return_type: Type,
    pub blocks: Vec<LabeledInstruction>,
    pub symbols: SymbolTable,
}

impl std::fmt::Display for Function {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if let Visibility::Public = self.visibility {
            write!(f, "public ")?;
        }
        let args = self
            .params
            .iter()
            .map(|p| format!("{}, ", p))
            .collect::<String>();
        write!(
            f,
            "function {}({}) {}{{\n",
            self.name, args, self.return_type
        )?;
        for block in &self.blocks {
            write!(f, "  {}\n", block)?;
        }
        write!(f, "{}", "}")
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FunctionSpec {
    pub module_name: String,
    pub name: String,
    pub params: Vec<Type>,
    pub return_type: Type,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Import {
    Function(FunctionSpec),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Directive {
    Len(String),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ConstantValue {
    String(String),
    Number(String),
    Directive(Directive),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Constant {
    pub name: String,
    pub ty: Type,
    pub value: ConstantValue,
}

#[derive(Debug, Default, Clone, PartialEq, Eq)]
pub struct Module {
    pub imports: Vec<Import>,
    pub constants: Vec<Constant>,
    pub functions: Vec<Function>,
}

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

pub struct InstructionBuilder<'a> {
    instructions: &'a mut Vec<LabeledInstruction>,
    counter: u32,
    label_counter: u32,
    symbol_table: &'a mut SymbolTable,
}

impl<'a> InstructionBuilder<'a> {
    pub fn new(
        instructions: &'a mut Vec<LabeledInstruction>,
        symbol_table: &'a mut SymbolTable,
    ) -> Self {
        Self {
            instructions,
            counter: 0,
            label_counter: 0,
            symbol_table,
        }
    }

    fn add_variable_symbol(&mut self, variable: &Variable) {
        self.symbol_table
            .insert(variable.name.clone(), Symbol::Variable(variable.clone()));
    }

    /// returns a new temporary variable
    pub fn var(&mut self, ty: Type) -> Variable {
        let counter = self.counter;
        self.counter += 1;
        Variable::new(format!("tmp{}", counter), ty)
    }

    /// returns a new label
    pub fn new_label(&mut self, custom: Option<impl Into<String>>) -> String {
        let id = self.label_counter;
        self.label_counter += 1;
        format!(
            "%label{}_{id}",
            custom.map(|s| format!("_{}", s.into())).unwrap_or_default()
        )
    }

    /// @noop
    pub fn noop(&mut self) -> &mut Self {
        self.instructions.push(Instruction::NoOp.into());
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
        self.instructions
            .push(Instruction::Add(des, lhs, rhs).into());
        self
    }

    /// @assign <type> : <des>, <rhs>
    pub fn assign(&mut self, var: Variable, value: impl Into<Operand>) -> &mut Self {
        let value = value.into();
        self.instructions
            .push(Instruction::Assign(var, value).into());
        self
    }

    /// @call <type> : <des> <func>(<args>)
    /// @call s32 : exit_code write(fact_result, 0)
    pub fn call(&mut self, des: Variable, name: impl Into<String>, args: &[Operand]) -> &mut Self {
        self.instructions
            .push(Instruction::Call(des, name.into(), args.to_vec()).into());
        self
    }

    /// `@cmp <type> : <des>, <lhs>, <rhs>`
    /// `@cmp u1 : is_one, n, 1`
    pub fn cmp(
        &mut self,
        des: Variable,
        lhs: impl Into<Operand>,
        rhs: impl Into<Operand>,
    ) -> &mut Self {
        let lhs = lhs.into();
        let rhs = rhs.into();
        self.instructions
            .push(Instruction::Cmp(des, lhs, rhs).into());
        self
    }

    /// @jump <label>
    /// @jump %recursive_case
    pub fn jump(&mut self, label: impl Into<String>) -> &mut Self {
        let label = label.into();
        self.instructions.push(Instruction::Jump(label).into());
        self
    }

    /// @jumpif <reg>, <label>
    /// @jumpif is_one, %return_one
    pub fn jump_if(&mut self, reg: impl Into<Operand>, label: impl Into<String>) -> &mut Self {
        let reg = reg.into();
        let label = label.into();
        self.instructions
            .push(Instruction::JumpIf(reg, label).into());
        self
    }

    /// @load <type> : <des>, <addr>
    pub fn load(&mut self, des: Variable, addr: impl Into<Operand>) -> &mut Self {
        let addr = addr.into();
        self.instructions.push(Instruction::Load(des, addr).into());
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
        self.instructions
            .push(Instruction::Mul(des, lhs, rhs).into());
        self
    }

    /// @phi <type> : <des>, [(<var>, <label>)]
    pub fn phi(&mut self, des: Variable, values: Vec<(Variable, String)>) -> &mut Self {
        self.instructions.push(Instruction::Phi(des, values).into());
        self
    }

    /// @ret <type> : <val>
    pub fn ret(&mut self, val: Variable) -> &mut Self {
        self.instructions
            .push(Instruction::Return(val.ty.clone(), Operand::from(val)).into());
        self
    }

    /// @ret void
    pub fn void_ret(&mut self) -> &mut Self {
        self.instructions
            .push(Instruction::Return(Type::Void, Operand::None).into());
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
        self.instructions
            .push(Instruction::Sub(des, lhs, rhs).into());
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
        self.instructions
            .push(Instruction::Div(des, lhs, rhs).into());
        self
    }

    /// %label: @noop
    pub fn label(&mut self, label: impl Into<String>) -> &mut Self {
        let label = label.into();
        let index = self.instructions.len();
        self.symbol_table
            .insert(label.clone(), Symbol::label(&label, index));
        self.instructions.push(LabeledInstruction {
            label: Some(label),
            instruction: Instruction::NoOp,
        });
        self
    }
}

#[derive(Debug, Default)]
pub struct FunctionBuilder {
    name: String,
    visibility: Visibility,
    params: Vec<Variable>,
    return_type: Type,
    instructions: Vec<LabeledInstruction>,
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

    pub fn instructions(&mut self) -> InstructionBuilder {
        InstructionBuilder::new(&mut self.instructions, &mut self.symbol_table)
    }

    pub fn build(self) -> Function {
        Function {
            visibility: self.visibility,
            name: self.name,
            params: self.params,
            return_type: self.return_type,
            blocks: self.instructions,
            symbols: self.symbol_table,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use pretty_assertions::assert_eq;

    #[test]
    fn testing_function_builder() {
        let x = Variable::new("x", Type::Unsigned(32));
        let y = Variable::new("y", Type::Unsigned(32));
        let mut function = FunctionBuilder::new("add")
            .with_visibility(Visibility::Public)
            .with_param(x.clone())
            .with_param(y.clone())
            .with_return_type(Type::Unsigned(32));
        let mut assembler = function.instructions();
        let ty = assembler.var(Type::Unsigned(32));
        assembler.add(ty, x.clone(), y.clone());
        let function = function.build();
        assert_eq!(
            function,
            Function {
                visibility: Visibility::Public,
                name: "add".to_string(),
                params: vec![x.clone(), y.clone()],
                return_type: Type::Unsigned(32),
                blocks: vec![Instruction::Add(
                    Variable::new("_tmp_0_", Type::Unsigned(32)),
                    Operand::Variable(x),
                    Operand::Variable(y)
                )
                .into()],
                symbols: SymbolTable::default(),
            }
        );
    }

    #[test]
    fn testing_instruction_builder() {
        let x = Variable::new("x", Type::Unsigned(32));
        let y = Variable::new("y", Type::Unsigned(32));
        let mut fb = FunctionBuilder::new("add")
            .with_visibility(Visibility::Public)
            .with_param(x.clone())
            .with_param(y.clone())
            .with_return_type(Type::Unsigned(32));
        let mut assember = fb.instructions();
        let des = assember.var(Type::Unsigned(32));
        assember.add(des.clone(), x.clone(), y.clone());

        let function = fb.build();
        assert_eq!(
            function,
            Function {
                visibility: Visibility::Public,
                name: "add".to_string(),
                params: vec![x.clone(), y.clone()],
                return_type: Type::Unsigned(32),
                blocks: vec![Instruction::Add(
                    Variable::new("_tmp_0_", Type::Unsigned(32)),
                    Operand::Variable(x),
                    Operand::Variable(y)
                )
                .into()],
                symbols: SymbolTable::default(),
            }
        );
    }
}
