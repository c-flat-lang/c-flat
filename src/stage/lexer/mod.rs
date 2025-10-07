pub mod token;
mod tokenizer;
use token::Token;

use super::Stage;

#[derive(Debug, Default)]
pub struct Lexer {}

impl Stage<&str, Vec<Token>> for Lexer {
    fn run(&mut self, input: &str) -> Vec<Token> {
        let tokenizer = tokenizer::Tokenizer::new(input);
        tokenizer.collect()
    }
}
