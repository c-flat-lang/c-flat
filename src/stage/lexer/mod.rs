pub mod token;
mod tokenizer;
use token::Token;

#[cfg(test)]
mod tests;

use super::Stage;

#[derive(Debug)]
pub struct Lexer;

impl Stage<(&str, &str), Vec<Token>> for Lexer {
    fn run(&mut self, (filename, input): (&str, &str)) -> Vec<Token> {
        let tokenizer = tokenizer::Tokenizer::new(filename, input);
        eprint!("{: >30}", "Tokenizing");
        eprintln!(" {filename}");
        tokenizer.collect()
    }
}
