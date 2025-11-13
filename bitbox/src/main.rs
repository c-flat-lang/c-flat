use bitbox::text::{emitter::Emitter, lexer::lex, parser::Parser};

#[derive(Debug, Default)]
struct Cli {
    show_ir: bool,
    target: bitbox::Target,
}

fn main() {
    let mut cli_options = Cli::default();

    let args = std::env::args().skip(1).collect::<Vec<_>>();
    let Some(file_path) = args.last() else {
        eprintln!("Usage: {} <filename>", std::env::args().next().unwrap());
        std::process::exit(1);
    };

    if args.iter().any(|arg| arg == "-ir") {
        cli_options.show_ir = true;
    }
    if args.iter().any(|arg| arg == "--target=bitbeat") {
        cli_options.target = bitbox::Target::Bitbeat;
    }

    let src = std::fs::read_to_string(file_path).expect("Could not read file");
    let tokens = lex(&src);
    let maybe_ast = Parser::new(tokens).parse();

    let ast = match maybe_ast {
        Ok(ast) => ast,
        Err(err) => {
            println!("{}", err.report(file_path, &src));
            std::process::exit(1);
        }
    };

    let emitter = Emitter::new(ast);
    let mut ir_module = emitter.emit();

    if cli_options.show_ir {
        println!("{}", ir_module);
        std::process::exit(0);
    }

    if let Err(error) =
        bitbox::Compiler::new(file_path, cli_options.target, None).run(&mut ir_module)
    {
        eprintln!("{}", error);
        std::process::exit(1);
    }
}
