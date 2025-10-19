#[derive(Debug, Default)]
pub struct Cli {
    pub debug_mode: Option<DebugMode>,
}

#[derive(Debug)]
pub enum DebugMode {
    Token,
    Ast,
    SymbolTable,
    Ir,
    LoweredIr,
}

impl Into<Option<bitbox::passes::DebugPass>> for DebugMode {
    fn into(self) -> Option<bitbox::passes::DebugPass> {
        match self {
            DebugMode::LoweredIr => Some(bitbox::passes::DebugPass::LoweredIr),
            _ => None,
        }
    }
}
