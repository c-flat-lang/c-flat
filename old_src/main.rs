// #![allow(unused)]
// mod ast;
// mod bbir_emitter;
// mod error;
// mod lexer;
// mod llvm_codgen;
// mod parser;
// mod semantic_analysis;
// mod wasm_runtime;
//
// use bitbox::Target;
// use std::str::FromStr;

fn main() {}
// fn main() {
//     let args = std::env::args().skip(1).collect::<Vec<_>>();
//
//     if args
//         .iter()
//         .find(|arg| arg == &"-h" || arg == &"--help")
//         .is_some()
//     {
//         eprintln!("Usage: {} <filename>", std::env::args().next().unwrap());
//         eprintln!("  Options:");
//         eprintln!("    -t            Print tokens");
//         eprintln!("    -a            Print AST");
//         eprintln!("    -ir           Print IR");
//         eprintln!("    --target      Target triple");
//         eprintln!("    -h, --help    Print this help message");
//         std::process::exit(0);
//     }
//
//     let Some(file_path) = args.last() else {
//         eprintln!("Usage: {} <filename>", std::env::args().next().unwrap());
//         std::process::exit(1);
//     };
//
//     let mut target = Target::default();
//     if let Some(arg) = args.iter().find(|arg| arg.starts_with("--target")) {
//         let value = arg.strip_prefix("--target=").unwrap();
//         target = match Target::from_str(value) {
//             Ok(target) => target,
//             Err(err) => {
//                 eprintln!("{} '{}'", err, value);
//                 std::process::exit(1);
//             }
//         };
//     }
//
//     let source = std::fs::read_to_string(&file_path).unwrap();
//
//     if args.iter().find(|arg| arg == &"-t").is_some() {
//         let tokens = lexer::tokenize(&source);
//         eprintln!("{tokens:#?}");
//         return;
//     }
//
//     let mut ast = match parser::Parser::new(&source).parse() {
//         Ok(ast) => ast,
//         Err(err) => {
//             let report = err.report(&file_path, &source);
//             eprintln!("{}", report);
//             std::process::exit(1);
//         }
//     };
//
//     ast.sort_by(|a, b| match (a, b) {
//         (ast::Item::Function(a), ast::Item::Function(b)) => {
//             if a.name.lexeme == "main" {
//                 std::cmp::Ordering::Greater
//             } else if b.name.lexeme == "main" {
//                 std::cmp::Ordering::Less
//             } else {
//                 a.name.lexeme.cmp(&b.name.lexeme)
//             }
//         }
//         _ => std::cmp::Ordering::Equal,
//     });
//
//     if args.iter().find(|arg| arg == &"-a").is_some() {
//         eprintln!("{ast:#?}");
//         return;
//     }
//
//     let program = bbir_emitter::emit(&mut ast);
//
//     if args.iter().find(|arg| arg == &"-ir").is_some() {
//         eprintln!("{program}");
//         return;
//     }
//
//     eprintln!("Compiling [{} {}]", target, file_path);
//     let mut compiler = bitbox::Compiler::new(target);
//     let bytes = compiler.build(&program);
//
//     let output_file_path = file_path.replace(".cb", &format!(".{}", target.file_extension()));
//     eprintln!("{} bytes to {}", bytes.len(), output_file_path);
//     match target {
//         Target::X86_64Linux => {
//             std::fs::write(output_file_path, bytes).unwrap();
//         }
//         Target::Wasm32 => {
//             std::fs::write(output_file_path, &bytes).unwrap();
//             if let Err(err) = wasm_runtime::run(&bytes) {
//                 eprintln!("{}", err);
//             }
//         }
//         Target::Bitbeat => {
//             std::fs::write(output_file_path, bytes).unwrap();
//             // let module: bitbeat::Model = bincode::deserialize(&bytes)?;
//             // let mut vm = bitbeat::Machine::default();
//             // vm.register_module(module);
//             // vm.run();
//         }
//     }
// }
