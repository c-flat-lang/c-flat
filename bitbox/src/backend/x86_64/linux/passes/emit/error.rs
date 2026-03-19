use crate::ir::Variable;

#[derive(Debug)]
pub enum Error {
    UndefinedVariable {
        variable: Box<Variable>,
        function_name: Box<String>,
        block_id: crate::ir::BlockId,
        instruction_index: usize,
        note: Box<String>,
    },
}

impl std::fmt::Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Error::UndefinedVariable {
                variable,
                function_name,
                block_id,
                instruction_index,
                note,
            } => {
                write!(
                    f,
                    "{}: undefined variable {} in {:?} at index {}\n{note}",
                    function_name, variable, block_id, instruction_index
                )
            }
        }
    }
}
