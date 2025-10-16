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
}
