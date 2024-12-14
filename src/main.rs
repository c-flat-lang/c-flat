mod error;
mod lexer;
mod parser;

fn main() {
    let Some(filename) = std::env::args().nth(1) else {
        eprintln!("Usage: {} <filename>", std::env::args().next().unwrap());
        std::process::exit(1);
    };

    let source = std::fs::read_to_string(&filename).unwrap();
    let tokens = lexer::tokenize(&source);
    println!("{:#?}", tokens);
}
