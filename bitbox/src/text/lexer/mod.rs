mod lexer;
pub mod token;

#[cfg(test)]
mod test;

pub fn lex(src: &str) -> Vec<token::Token> {
    lexer::Lexer::new(src).collect()
}
