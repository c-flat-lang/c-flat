use bitbox::text::{emitter::Emitter, lexer::lex, parser::Parser};

fn main() {
    let src = include_str!("../examples/basic.bitbox");
    let tokens = lex(src);
    let maybe_ast = Parser::new(tokens).parse();
    let ast = match maybe_ast {
        Ok(ast) => ast,
        Err(err) => {
            println!("{}", err.report("basic.bitbox", src));
            std::process::exit(1);
        }
    };

    let emitter = Emitter::new(ast);
    let mut ir_module = emitter.emit();

    if let Err(error) =
        bitbox::Compiler::new("testing.bitbox", bitbox::Target::Wasm32, None).run(&mut ir_module)
    {
        eprintln!("{}", error);
        std::process::exit(1);
    }
}
