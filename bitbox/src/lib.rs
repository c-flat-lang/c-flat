use std::collections::HashMap;
pub type Span = std::ops::Range<usize>;

#[derive(Debug, Default, Clone, Copy, PartialEq, Eq)]
pub enum Target {
    #[default]
    Bitbeat,
}

impl std::str::FromStr for Target {
    type Err = String;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "bitbeat" => Ok(Target::Bitbeat),
            _ => Err(format!("Unknown target '{}'", s)),
        }
    }
}

#[derive(Debug, Default, Clone, PartialEq, Eq)]
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

#[derive(Debug, Clone, PartialEq, Eq)]
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
        write!(f, "{}", self.name)
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
        write!(f, "{}", self.instruction)
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
    /// @phi <type> : <des>, [(<var>, <block>)]
    Phi(Variable, Vec<(Variable, usize)>),
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
                variable.ty, variable.name, operand, operand1
            ),
            Instruction::Assign(variable, operand) => write!(
                f,
                "@assign {} : {}, {}",
                variable.ty, variable.name, operand
            ),
            Instruction::Call(des, caller, args) => {
                write!(
                    f,
                    "@call {} : {}, {}({})",
                    des.ty,
                    des.name,
                    caller,
                    args.iter()
                        .map(|arg| format!("{}, ", arg))
                        .collect::<String>()
                )
            }
            Instruction::Cmp(variable, operand, operand1) => write!(
                f,
                "@cmp {} : {}, {}, {}",
                variable.ty, variable.name, operand, operand1
            ),
            Instruction::Jump(label) => write!(f, "@jump {}", label),
            Instruction::JumpIf(operand, label) => write!(f, "@jumpif {}, {}", operand, label),
            Instruction::Load(variable, operand) => {
                write!(f, "@load {} : {}, {}", variable.ty, variable.name, operand)
            }
            Instruction::Mul(variable, operand, operand1) => write!(
                f,
                "@mul {} : {}, {}, {}",
                variable.ty, variable.name, operand, operand1
            ),
            Instruction::Phi(variable, items) => {
                write!(
                    f,
                    "@phi {} : {}, [{}]",
                    variable.ty,
                    variable.name,
                    items
                        .iter()
                        .map(|(var, block)| format!("[{}, {}]", var.name, block))
                        .collect::<String>()
                )
            }
            Instruction::Return(ty, operand) => write!(f, "@ret {}, {}", ty, operand),
            Instruction::Sub(variable, operand, operand1) => write!(
                f,
                "@sub {} : {}, {}, {}",
                variable.ty, variable.name, operand, operand1
            ),
            Instruction::Div(variable, operand, operand1) => write!(
                f,
                "@div {} : {}, {}, {}",
                variable.ty, variable.name, operand, operand1
            ),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
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

impl std::fmt::Display for Operand {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Operand::ConstantInt { value, .. } => write!(f, "{}", value),
            Operand::Variable(variable) => write!(f, "{}", variable.name),
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
}

#[derive(Debug, Default, Clone, PartialEq, Eq)]
pub struct BlockBuilder {
    pub instructions: Vec<LabeledInstruction>,
    tmp_counter: usize,
    passed_var: Option<Variable>,
}

impl BlockBuilder {
    pub fn temp_var(&mut self, ty: Type) -> Variable {
        if let Some(var) = self.passed_var.take() {
            if var.ty == ty {
                let new_var = var.new_version();
                self.passed_var = Some(new_var.clone());
                return new_var;
            }
        }
        let name = format!("_tmp_{}_", self.tmp_counter);
        self.tmp_counter += 1;
        let var = Variable::new(&name, ty);
        self.passed_var = Some(var.clone());
        var
    }

    pub fn get_temp_var(&self) -> Option<Variable> {
        self.passed_var.clone()
    }

    pub fn add(&mut self, ty: Type, lhs: impl Into<Operand>, rhs: impl Into<Operand>) {
        let lhs = lhs.into();
        let rhs = rhs.into();
        let des = self.temp_var(ty);
        self.instructions
            .push(Instruction::Add(des, lhs, rhs).into());
    }

    pub fn r#return_void(&mut self) {
        self.instructions
            .push(Instruction::Return(Type::Void, Operand::None).into());
    }

    pub fn assign(&mut self, var: Variable, value: Operand) {
        self.instructions
            .push(Instruction::Assign(var, value).into());
    }

    pub fn call(&mut self, dest_var: Variable, name: String, args: Vec<Operand>) {
        self.instructions
            .push(Instruction::Call(dest_var, name, args).into());
    }

    pub fn build(self) -> Vec<LabeledInstruction> {
        self.instructions
    }

    pub fn cmp(
        &mut self,
        des: Variable,
        condition_var: impl Into<Operand>,
        constant_int: impl Into<Operand>,
    ) {
        let condition_var = condition_var.into();
        let constant_int = constant_int.into();
        self.instructions
            .push(Instruction::Cmp(des, condition_var, constant_int).into());
    }

    pub fn jump_if(&mut self, jump_condition: impl Into<Operand>, label: String) {
        self.instructions
            .push(Instruction::JumpIf(jump_condition.into(), label).into());
    }

    pub fn jump(&mut self, label: String) {
        self.instructions.push(Instruction::Jump(label).into());
    }

    pub fn label(&mut self, then_label: String) {
        self.instructions.push(LabeledInstruction {
            label: Some(then_label),
            instruction: Instruction::NoOp,
        });
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

#[derive(Debug, Default)]
pub struct FunctionBuilder {
    name: String,
    visibility: Visibility,
    params: Vec<Variable>,
    return_type: Type,
    block_builder: BlockBuilder,
    label_counter: usize,
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

    pub fn with_block<F>(mut self, f: F) -> Self
    where
        F: FnOnce(&mut BlockBuilder),
    {
        f(&mut self.block_builder);
        self
    }

    pub fn block<F>(&mut self, f: F)
    where
        F: FnOnce(&mut BlockBuilder),
    {
        f(&mut self.block_builder);
    }

    pub fn build(self) -> Function {
        Function {
            visibility: self.visibility,
            name: self.name,
            params: self.params,
            return_type: self.return_type,
            blocks: self.block_builder.build(),
        }
    }

    pub fn new_label(&mut self, custom: Option<impl Into<String>>) -> String {
        let id = self.label_counter;
        self.label_counter += 1;
        format!(
            "%label_{}{}_{id}",
            self.name,
            custom.map(|s| format!("_{}", s.into())).unwrap_or_default()
        )
    }

    pub fn var(&mut self, ty: Type) -> Variable {
        self.block_builder.temp_var(ty)
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
        let function = FunctionBuilder::new("add")
            .with_visibility(Visibility::Public)
            .with_param(x.clone())
            .with_param(y.clone())
            .with_return_type(Type::Unsigned(32))
            .with_block(|b| {
                b.add(Type::Unsigned(32), x.clone(), y.clone());
            })
            .build();
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
            }
        );
    }
}
