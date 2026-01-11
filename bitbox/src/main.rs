mod cli;
use bitbox::{
    passes::DebugPass,
    text::{emitter::Emitter, lexer::lex, parser::Parser, semantic_analyzer::SymbolTableBuilder},
};

fn main() {
    let cli_options = cli::Cli::parse();

    let src = std::fs::read_to_string(&cli_options.file_path).expect("Could not read file");
    let tokens = lex(&src);

    if matches!(cli_options.debug_mode, Some(cli::DebugMode::Token)) {
        for token in tokens {
            eprintln!("{:?}", token);
        }
        std::process::exit(0);
    }

    let maybe_ast = Parser::new(tokens).parse();

    let ast = match maybe_ast {
        Ok(ast) => ast,
        Err(err) => {
            println!("{}", err.report(&cli_options.file_path, &src));
            std::process::exit(1);
        }
    };

    if matches!(cli_options.debug_mode, Some(cli::DebugMode::Ast)) {
        eprintln!("{:#?}", ast);
        std::process::exit(0);
    }

    let symbol_table = match SymbolTableBuilder::default().build(&ast) {
        Ok(table) => table,
        Err(errors) => {
            eprintln!("{}", errors.report(&cli_options.file_path, &src));
            std::process::exit(1);
        }
    };

    let emitter = Emitter::new(symbol_table, ast);
    let mut ir_module = emitter.emit();

    if matches!(cli_options.debug_mode, Some(cli::DebugMode::Ir)) {
        eprintln!("{}", ir_module);
        std::process::exit(0);
    }

    let compiler_debug_mode: Option<DebugPass> = cli_options.debug_mode.and_then(Into::into);
    if let Err(error) = bitbox::Compiler::new(
        &cli_options.file_path,
        cli_options.target,
        compiler_debug_mode,
    )
    .run(&mut ir_module)
    {
        eprintln!("{}", error);
        std::process::exit(1);
    }
}
