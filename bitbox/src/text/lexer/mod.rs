mod scanner;
pub mod token;

#[cfg(test)]
mod test;

pub fn lex(filename: &str, src: &str) -> Vec<token::Token> {
    scanner::Lexer::new(filename, src).collect()
}
