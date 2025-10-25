use bitbox::text::lexer::lex;
use bitbox::text::parser::Parser;
fn main() {
    let src = include_str!("../examples/basic.bitbox");
    let tokens = lex(src);
    let ast = Parser::new(tokens).parse();
    match ast {
        Ok(ast) => println!("{:#?}", ast),
        Err(err) => println!("{}", err.report("basic.bitbox", src)),
    }
}
