use crate::passes::{DebugPass, PassOutput};
use crate::text::emitter::Emitter;
use crate::text::lexer::lex;
use crate::text::parser::Parser;
use crate::text::semantic_analyzer::SymbolTableBuilder;
use crate::{Compiler, Target};

#[derive(Debug, Clone)]
pub struct Cli {
    pub target: Target,
    pub file_path: String,
}

fn snapshot_compiler(target: Target, pass: DebugPass, path: &str, src: &str) -> String {
    let cli_options = Cli {
        target,
        file_path: path.to_string(),
    };
    let tokens = lex(src);
    let maybe_ast = Parser::new(tokens).parse();

    let ast = match maybe_ast {
        Ok(ast) => ast,
        Err(err) => {
            println!("{}", err.report(&cli_options.file_path, src));
            std::process::exit(1);
        }
    };

    let symbol_table = match SymbolTableBuilder::default().build(&ast) {
        Ok(table) => table,
        Err(errors) => {
            eprintln!("{}", errors.report(&cli_options.file_path, src));
            std::process::exit(1);
        }
    };

    let emitter = Emitter::new(symbol_table, ast);
    let mut ir_module = emitter.emit();

    let mut compiler = Compiler::new(&cli_options.file_path, cli_options.target, Some(pass));
    let output = match compiler.run(&mut ir_module) {
        Ok(output) => output,
        Err(err) => {
            eprintln!("{}", err);
            panic!("Compilation failed");
        }
    };
    let PassOutput::String(result) = output else {
        panic!("Expected PassOutput::String, got PassOutput::Nothing for pass {pass:?}");
    };
    result
}

macro_rules! snapshot {
    ($target:expr, $pass:expr, $name:tt, $path:tt) => {
        #[test]
        fn $name() {
            let contents = include_str!($path);
            let mut settings = insta::Settings::clone_current();
            settings.set_snapshot_path("testdata/output/");
            settings.bind(|| {
                insta::assert_snapshot!(snapshot_compiler($target, $pass, $path, contents));
            });
        }
    };
    (ignore: $reason:literal, $target:expr, $pass:expr, $name:tt, $path:tt) => {
        #[test]
        #[ignore = $reason]
        fn $name() {
            let contents = include_str!($path);
            let mut settings = insta::Settings::clone_current();
            settings.set_snapshot_path("testdata/output/");
            settings.bind(|| {
                insta::assert_snapshot!(snapshot_compiler($target, $pass, $path, contents));
            });
        }
    };
}

// ── wasm32 ────────────────────────────────────────────────────────────────────

snapshot!(
    Target::Wasm32,
    DebugPass::LocalFunctionVariables,
    test_array_local_vars_wasm32,
    "../examples/array.bitbox"
);
snapshot!(
    Target::Wasm32,
    DebugPass::ControlFlowGraph,
    test_array_cfg_wasm32,
    "../examples/array.bitbox"
);
snapshot!(
    Target::Wasm32,
    DebugPass::LivenessAnalysis,
    test_array_liveness_wasm32,
    "../examples/array.bitbox"
);
snapshot!(
    Target::Wasm32,
    DebugPass::StructuringIr,
    test_array_structuring_wasm32,
    "../examples/array.bitbox"
);
snapshot!(
    Target::Wasm32,
    DebugPass::Emit,
    test_array_emit_wasm32,
    "../examples/array.bitbox"
);

snapshot!(
    Target::Wasm32,
    DebugPass::LocalFunctionVariables,
    test_factorial_local_vars_wasm32,
    "../examples/factorial.bitbox"
);
snapshot!(
    Target::Wasm32,
    DebugPass::ControlFlowGraph,
    test_factorial_cfg_wasm32,
    "../examples/factorial.bitbox"
);
snapshot!(
    Target::Wasm32,
    DebugPass::LivenessAnalysis,
    test_factorial_liveness_wasm32,
    "../examples/factorial.bitbox"
);
snapshot!(
    Target::Wasm32,
    DebugPass::StructuringIr,
    test_factorial_structuring_wasm32,
    "../examples/factorial.bitbox"
);
snapshot!(
    Target::Wasm32,
    DebugPass::Emit,
    test_factorial_emit_wasm32,
    "../examples/factorial.bitbox"
);

snapshot!(
    Target::Wasm32,
    DebugPass::LocalFunctionVariables,
    test_fib_local_vars_wasm32,
    "../examples/fib.bitbox"
);
snapshot!(
    Target::Wasm32,
    DebugPass::ControlFlowGraph,
    test_fib_cfg_wasm32,
    "../examples/fib.bitbox"
);
snapshot!(
    Target::Wasm32,
    DebugPass::LivenessAnalysis,
    test_fib_liveness_wasm32,
    "../examples/fib.bitbox"
);
snapshot!(
    Target::Wasm32,
    DebugPass::StructuringIr,
    test_fib_structuring_wasm32,
    "../examples/fib.bitbox"
);
snapshot!(
    Target::Wasm32,
    DebugPass::Emit,
    test_fib_emit_wasm32,
    "../examples/fib.bitbox"
);

snapshot!(
    ignore: "bitbox parser does not support pointer types used in import_function.bitbox",
    Target::Wasm32,
    DebugPass::LocalFunctionVariables,
    test_import_function_local_vars_wasm32,
    "../examples/import_function.bitbox"
);
snapshot!(
    ignore: "bitbox parser does not support pointer types used in import_function.bitbox",
    Target::Wasm32,
    DebugPass::ControlFlowGraph,
    test_import_function_cfg_wasm32,
    "../examples/import_function.bitbox"
);
snapshot!(
    ignore: "bitbox parser does not support pointer types used in import_function.bitbox",
    Target::Wasm32,
    DebugPass::LivenessAnalysis,
    test_import_function_liveness_wasm32,
    "../examples/import_function.bitbox"
);
snapshot!(
    ignore: "bitbox parser does not support pointer types used in import_function.bitbox",
    Target::Wasm32,
    DebugPass::StructuringIr,
    test_import_function_structuring_wasm32,
    "../examples/import_function.bitbox"
);
snapshot!(
    ignore: "bitbox parser does not support pointer types used in import_function.bitbox",
    Target::Wasm32,
    DebugPass::Emit,
    test_import_function_emit_wasm32,
    "../examples/import_function.bitbox"
);

snapshot!(
    Target::Wasm32,
    DebugPass::LocalFunctionVariables,
    test_looping_local_vars_wasm32,
    "../examples/looping.bitbox"
);
snapshot!(
    Target::Wasm32,
    DebugPass::ControlFlowGraph,
    test_looping_cfg_wasm32,
    "../examples/looping.bitbox"
);
snapshot!(
    Target::Wasm32,
    DebugPass::LivenessAnalysis,
    test_looping_liveness_wasm32,
    "../examples/looping.bitbox"
);
snapshot!(
    Target::Wasm32,
    DebugPass::StructuringIr,
    test_looping_structuring_wasm32,
    "../examples/looping.bitbox"
);
snapshot!(
    ignore: "wasm32 backend does not yet implement @phi",
    Target::Wasm32,
    DebugPass::Emit,
    test_looping_emit_wasm32,
    "../examples/looping.bitbox"
);

snapshot!(
    ignore: "bitbox parser does not support @if syntax used in test.bitbox",
    Target::Wasm32,
    DebugPass::LocalFunctionVariables,
    test_test_local_vars_wasm32,
    "../examples/test.bitbox"
);
snapshot!(
    ignore: "bitbox parser does not support @if syntax used in test.bitbox",
    Target::Wasm32,
    DebugPass::ControlFlowGraph,
    test_test_cfg_wasm32,
    "../examples/test.bitbox"
);
snapshot!(
    ignore: "bitbox parser does not support @if syntax used in test.bitbox",
    Target::Wasm32,
    DebugPass::LivenessAnalysis,
    test_test_liveness_wasm32,
    "../examples/test.bitbox"
);
snapshot!(
    ignore: "bitbox parser does not support @if syntax used in test.bitbox",
    Target::Wasm32,
    DebugPass::StructuringIr,
    test_test_structuring_wasm32,
    "../examples/test.bitbox"
);
snapshot!(
    ignore: "bitbox parser does not support @if syntax used in test.bitbox",
    Target::Wasm32,
    DebugPass::Emit,
    test_test_emit_wasm32,
    "../examples/test.bitbox"
);

// ── x86_64-linux ──────────────────────────────────────────────────────────────

snapshot!(
    Target::X86_64Linux,
    DebugPass::LocalFunctionVariables,
    test_array_local_vars_x86_64,
    "../examples/array.bitbox"
);
snapshot!(
    Target::X86_64Linux,
    DebugPass::ControlFlowGraph,
    test_array_cfg_x86_64,
    "../examples/array.bitbox"
);
snapshot!(
    Target::X86_64Linux,
    DebugPass::LoweredIr,
    test_array_lowered_ir_x86_64,
    "../examples/array.bitbox"
);
snapshot!(
    Target::X86_64Linux,
    DebugPass::LivenessAnalysis,
    test_array_liveness_x86_64,
    "../examples/array.bitbox"
);
snapshot!(
    Target::X86_64Linux,
    DebugPass::Emit,
    test_array_emit_x86_64,
    "../examples/array.bitbox"
);
snapshot!(
    Target::X86_64Linux,
    DebugPass::VirtRegRewrite,
    test_array_virt_reg_x86_64,
    "../examples/array.bitbox"
);

snapshot!(
    Target::X86_64Linux,
    DebugPass::LocalFunctionVariables,
    test_factorial_local_vars_x86_64,
    "../examples/factorial.bitbox"
);
snapshot!(
    Target::X86_64Linux,
    DebugPass::ControlFlowGraph,
    test_factorial_cfg_x86_64,
    "../examples/factorial.bitbox"
);
snapshot!(
    Target::X86_64Linux,
    DebugPass::LoweredIr,
    test_factorial_lowered_ir_x86_64,
    "../examples/factorial.bitbox"
);
snapshot!(
    Target::X86_64Linux,
    DebugPass::LivenessAnalysis,
    test_factorial_liveness_x86_64,
    "../examples/factorial.bitbox"
);
snapshot!(
    ignore: "x86_64 backend does not yet implement mul",
    Target::X86_64Linux,
    DebugPass::Emit,
    test_factorial_emit_x86_64,
    "../examples/factorial.bitbox"
);
snapshot!(
    ignore: "x86_64 backend does not yet implement mul",
    Target::X86_64Linux,
    DebugPass::VirtRegRewrite,
    test_factorial_virt_reg_x86_64,
    "../examples/factorial.bitbox"
);

snapshot!(
    Target::X86_64Linux,
    DebugPass::LocalFunctionVariables,
    test_fib_local_vars_x86_64,
    "../examples/fib.bitbox"
);
snapshot!(
    Target::X86_64Linux,
    DebugPass::ControlFlowGraph,
    test_fib_cfg_x86_64,
    "../examples/fib.bitbox"
);
snapshot!(
    Target::X86_64Linux,
    DebugPass::LoweredIr,
    test_fib_lowered_ir_x86_64,
    "../examples/fib.bitbox"
);
snapshot!(
    Target::X86_64Linux,
    DebugPass::LivenessAnalysis,
    test_fib_liveness_x86_64,
    "../examples/fib.bitbox"
);
snapshot!(
    Target::X86_64Linux,
    DebugPass::Emit,
    test_fib_emit_x86_64,
    "../examples/fib.bitbox"
);
snapshot!(
    Target::X86_64Linux,
    DebugPass::VirtRegRewrite,
    test_fib_virt_reg_x86_64,
    "../examples/fib.bitbox"
);

snapshot!(
    ignore: "bitbox parser does not support pointer types used in import_function.bitbox",
    Target::X86_64Linux,
    DebugPass::LocalFunctionVariables,
    test_import_function_local_vars_x86_64,
    "../examples/import_function.bitbox"
);
snapshot!(
    ignore: "bitbox parser does not support pointer types used in import_function.bitbox",
    Target::X86_64Linux,
    DebugPass::ControlFlowGraph,
    test_import_function_cfg_x86_64,
    "../examples/import_function.bitbox"
);
snapshot!(
    ignore: "bitbox parser does not support pointer types used in import_function.bitbox",
    Target::X86_64Linux,
    DebugPass::LoweredIr,
    test_import_function_lowered_ir_x86_64,
    "../examples/import_function.bitbox"
);
snapshot!(
    ignore: "bitbox parser does not support pointer types used in import_function.bitbox",
    Target::X86_64Linux,
    DebugPass::LivenessAnalysis,
    test_import_function_liveness_x86_64,
    "../examples/import_function.bitbox"
);
snapshot!(
    ignore: "bitbox parser does not support pointer types used in import_function.bitbox",
    Target::X86_64Linux,
    DebugPass::Emit,
    test_import_function_emit_x86_64,
    "../examples/import_function.bitbox"
);
snapshot!(
    ignore: "bitbox parser does not support pointer types used in import_function.bitbox",
    Target::X86_64Linux,
    DebugPass::VirtRegRewrite,
    test_import_function_virt_reg_x86_64,
    "../examples/import_function.bitbox"
);

snapshot!(
    Target::X86_64Linux,
    DebugPass::LocalFunctionVariables,
    test_looping_local_vars_x86_64,
    "../examples/looping.bitbox"
);
snapshot!(
    Target::X86_64Linux,
    DebugPass::ControlFlowGraph,
    test_looping_cfg_x86_64,
    "../examples/looping.bitbox"
);
snapshot!(
    Target::X86_64Linux,
    DebugPass::LoweredIr,
    test_looping_lowered_ir_x86_64,
    "../examples/looping.bitbox"
);
snapshot!(
    Target::X86_64Linux,
    DebugPass::LivenessAnalysis,
    test_looping_liveness_x86_64,
    "../examples/looping.bitbox"
);

snapshot!(
    ignore: "x86_64 backend does not yet implement @phi",
    Target::X86_64Linux,
    DebugPass::Emit,
    test_looping_emit_x86_64,
    "../examples/looping.bitbox"
);
snapshot!(
    ignore: "x86_64 backend does not yet implement @phi",
    Target::X86_64Linux,
    DebugPass::VirtRegRewrite,
    test_looping_virt_reg_x86_64,
    "../examples/looping.bitbox"
);

snapshot!(
    ignore: "bitbox parser does not support @if syntax used in test.bitbox",
    Target::X86_64Linux,
    DebugPass::LocalFunctionVariables,
    test_test_local_vars_x86_64,
    "../examples/test.bitbox"
);
snapshot!(
    ignore: "bitbox parser does not support @if syntax used in test.bitbox",
    Target::X86_64Linux,
    DebugPass::ControlFlowGraph,
    test_test_cfg_x86_64,
    "../examples/test.bitbox"
);
snapshot!(
    ignore: "bitbox parser does not support @if syntax used in test.bitbox",
    Target::X86_64Linux,
    DebugPass::LoweredIr,
    test_test_lowered_ir_x86_64,
    "../examples/test.bitbox"
);
snapshot!(
    ignore: "bitbox parser does not support @if syntax used in test.bitbox",
    Target::X86_64Linux,
    DebugPass::LivenessAnalysis,
    test_test_liveness_x86_64,
    "../examples/test.bitbox"
);
snapshot!(
    ignore: "x86_64 backend does not yet implement mul",
    Target::X86_64Linux,
    DebugPass::Emit,
    test_test_emit_x86_64,
    "../examples/test.bitbox"
);
snapshot!(
    ignore: "x86_64 backend does not yet implement mul",
    Target::X86_64Linux,
    DebugPass::VirtRegRewrite,
    test_test_virt_reg_x86_64,
    "../examples/test.bitbox"
);

// ── bitbeat ───────────────────────────────────────────────────────────────────

snapshot!(
    ignore: "bitbeat backend does not support memory/arrays",
    Target::Bitbeat,
    DebugPass::Emit,
    test_array_emit_bitbeat,
    "../examples/array.bitbox"
);
snapshot!(
    Target::Bitbeat,
    DebugPass::Emit,
    test_factorial_emit_bitbeat,
    "../examples/factorial.bitbox"
);
snapshot!(
    Target::Bitbeat,
    DebugPass::Emit,
    test_fib_emit_bitbeat,
    "../examples/fib.bitbox"
);
snapshot!(
    ignore: "bitbox parser does not support pointer types used in import_function.bitbox",
    Target::Bitbeat,
    DebugPass::Emit,
    test_import_function_emit_bitbeat,
    "../examples/import_function.bitbox"
);
snapshot!(
    ignore: "wasm32 backend does not yet implement @phi",
    Target::Bitbeat,
    DebugPass::Emit,
    test_looping_emit_bitbeat,
    "../examples/looping.bitbox"
);
snapshot!(
    ignore: "bitbox parser does not support @if syntax used in test.bitbox",
    Target::Bitbeat,
    DebugPass::Emit,
    test_test_emit_bitbeat,
    "../examples/test.bitbox"
);
