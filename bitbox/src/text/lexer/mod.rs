mod scanner;
pub mod token;

#[cfg(test)]
mod test;

pub fn lex(src: &str) -> Vec<token::Token> {
    scanner::Lexer::new(src).collect()
}
