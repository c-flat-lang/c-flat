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

fn snapshot_compiler(target: Target, path: &str, input: &str) -> String {
    match target {
        Target::Wasm32 => compile_wasm32(path, input),
        Target::X86_64Linux => todo!("X86_64Linux"),
        Target::Bitbeat => todo!("Bitbeat"),
    }
}

fn compile_wasm32(path: &str, src: &str) -> String {
    let cli_options = Cli {
        target: Target::Wasm32,
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

    let compiler = Compiler::new(&cli_options.file_path, cli_options.target, None);
    let results = compiler.run(&mut ir_module);
    eprintln!("{:#?}", results);
    "TEST".to_string()
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
    test_factorial_wasm32,
    "../examples/factorial.bitbox"
);

// #[test]
// fn test_simple_add() {
//     let mut fb = FunctionBuilder::new("simple_add").with_return_type(ty!(void));
//     let mut asm = fb.assembler();
//
//     asm.create_block("entry");
//
//     let a = tmp!(asm, s 32);
//     let b = tmp!(asm, s 32);
//     let sum = tmp!(asm, s 32);
//
//     asm.assign(a.clone(), op!(10 : s 32))
//         .assign(b.clone(), op!(20 : s 32))
//         .add(sum.clone(), a, b)
//         .ret(ty!(s 32), sum);
//
//     let mut mb = ModuleBuilder::new();
//     mb.function(fb.build());
//     let module = mb.build();
// }
//
// #[test]
// fn test_factorial() {
//     let n = var!("n" : s 32);
//
//     let mut fb = FunctionBuilder::new("factorial")
//         .with_return_type(ty!(s 32))
//         .with_param(n.clone());
//
//     let mut asm = fb.assembler();
//
//     let entry = asm.create_block("entry");
//     let recursive_case = asm.create_block("recursive_case");
//     let base_case = asm.create_block("base_case");
//
//     let is_one = tmp!(asm, u 1);
//     let n_minus_one = tmp!(asm, s 32);
//     let fact_n_minus_one = tmp!(asm, s 32);
//     let result = tmp!(asm, s 32);
//
//     asm
//         // if n == 1
//         .eq(is_one.clone(), n.clone(), op!(1 : s 32))
//         .jump_if(is_one, base_case.clone())
//         .jump(recursive_case.clone());
//
//     // Base case: return 1
//     asm.create_block("base_case"); // re-use label? better to use create_block again or set current
//     // Note: your AssemblerBuilder always appends to current_block.
//     // You may want to improve the API later.
//
//     // For now, let's do it properly with multiple create_block calls:
//
//     // Better version:
//     let mut asm = fb.assembler();
//
//     asm.create_block("entry");
//
//     let cond = tmp!(asm, u 1);
//     let n1 = tmp!(asm, s 32);
//     let rec = tmp!(asm, s 32);
//     let res = tmp!(asm, s 32);
//
//     asm.eq(cond.clone(), n.clone(), op!(1 : s 32))
//         .jump_if(cond, "base_case")
//         .jump("recursive");
//
//     asm.create_block("recursive");
//     asm.sub(n1.clone(), n.clone(), op!(1 : s 32))
//         .call(Some(rec.clone()), "factorial", &[n1.into()])
//         .mul(res.clone(), rec, n.clone())
//         .ret(ty!(s 32), res);
//
//     asm.create_block("base_case");
//     asm.ret(ty!(s 32), op!(1 : s 32));
//
//     // test your codegen / interpreter / etc with `func`
// }
//
// #[test]
// fn test_while_loop() {
//     let n = var!("n" : s 32);
//     let mut fb = FunctionBuilder::new("sum_to_n")
//         .with_return_type(ty!(s 32))
//         .with_param(n.clone());
//
//     let mut asm = fb.assembler();
//
//     asm.create_block("entry");
//     let loop_cond = asm.create_block("loop_cond");
//     let loop_body = asm.create_block("loop_body");
//     let exit = asm.create_block("exit");
//
//     let i = tmp!(asm, s 32);
//     let sum = tmp!(asm, s 32);
//     let cond = tmp!(asm, u 1);
//
//     asm.assign(i.clone(), op!(0 : s 32))
//         .assign(sum.clone(), op!(0 : s 32))
//         .jump(loop_cond.clone());
//
//     // Condition
//     asm.create_block("loop_cond");
//     asm.lt(cond.clone(), i.clone(), n.clone()) // i < n
//         .jump_if(cond, loop_body.clone())
//         .jump(exit.clone());
//
//     // Body
//     asm.create_block("loop_body");
//     asm.add(sum.clone(), sum.clone(), i.clone()) // sum += i
//         .add(i.clone(), i.clone(), op!(1 : s 32)) // i += 1
//         .jump(loop_cond.clone());
//
//     // Exit
//     asm.create_block("exit");
//     asm.ret(ty!(s 32), sum);
// }
