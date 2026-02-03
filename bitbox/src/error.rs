#[derive(Debug)]
pub enum Error {
    InvalidInstruction { index: usize, message: String },
    MissingMainFunction,
    X86_64AssemblyError(crate::backend::x86_64::linux::passes::emit::error::Error),
}

impl std::fmt::Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Error::InvalidInstruction { index, message } => {
                write!(f, "Invalid instruction at index {}: {}", index, message)
            }
            Error::MissingMainFunction => write!(f, "Missing main function"),
            Error::X86_64AssemblyError(e) => write!(f, "{}", e),
        }
    }
}
