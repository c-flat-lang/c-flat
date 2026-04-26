use crate::passes::PassOutput;
use crate::text::emitter::Emitter;
use crate::text::lexer::lex;
use crate::text::parser::Parser;
use crate::text::semantic_analyzer::SymbolTableBuilder;
use crate::{Compiler, Target, passes::DebugPass};

#[derive(Debug, Clone)]
pub struct Cli {
    pub target: Target,
    pub file_path: String,
}

fn snapshot_compiler(target: Target, path: &str, src: &str) -> String {
    let cli_options = Cli {
        target,
        file_path: path.to_string(),
    };
    let tokens = lex(&src);
    let maybe_ast = Parser::new(tokens).parse();

    let ast = match maybe_ast {
        Ok(ast) => ast,
        Err(err) => {
            println!("{}", err.report(&cli_options.file_path, &src));
            std::process::exit(1);
        }
    };

    let symbol_table = match SymbolTableBuilder::default().build(&ast) {
        Ok(table) => table,
        Err(errors) => {
            eprintln!("{}", errors.report(&cli_options.file_path, &src));
            std::process::exit(1);
        }
    };

    let emitter = Emitter::new(symbol_table, ast);
    let mut ir_module = emitter.emit();

    let mut compiler = Compiler::new(
        &cli_options.file_path,
        cli_options.target,
        Some(DebugPass::Emit),
    );
    let output = match compiler.run(&mut ir_module) {
        Ok(output) => output,
        Err(err) => {
            eprintln!("{}", err);
            panic!("Compilation failed");
        }
    };
    let PassOutput::String(result) = output else {
        panic!("Expected PassOutput::String");
    };
    result
}

macro_rules! snapshot {
    ($target:expr, $name:tt, $path:tt) => {
        #[test]
        fn $name() {
            let contents = include_str!($path);
            let mut settings = insta::Settings::clone_current();
            settings.set_snapshot_path("testdata/output/");
            settings.bind(|| {
                insta::assert_snapshot!(snapshot_compiler($target, $path, contents));
            });
        }
    };
}

snapshot!(
    Target::Wasm32,
    test_array_wasm32,
    "../examples/array.bitbox"
);

snapshot!(
    Target::Wasm32,
    test_factorial_wasm32,
    "../examples/factorial.bitbox"
);

snapshot!(Target::Wasm32, test_fib_wasm32, "../examples/fib.bitbox");
snapshot!(Target::Bitbeat, test_fib_bitbeat, "../examples/fib.bitbox");
snapshot!(
    Target::X86_64Linux,
    test_fib_x86_64_linux,
    "../examples/fib.bitbox"
);
