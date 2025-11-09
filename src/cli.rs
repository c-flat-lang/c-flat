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

impl From<DebugMode> for Option<bitbox::passes::DebugPass> {
    fn from(value: DebugMode) -> Option<bitbox::passes::DebugPass> {
        match value {
            DebugMode::LoweredIr => Some(bitbox::passes::DebugPass::LoweredIr),
            _ => None,
        }
    }
}
