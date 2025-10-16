#[derive(Debug)]
pub enum Error {
    InvalidInstruction { index: usize, message: String },
}

impl std::fmt::Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Error::InvalidInstruction { index, message } => {
                write!(f, "Invalid instruction at index {}: {}", index, message)
            }
        }
    }
}
