pub mod token;
pub mod tokenizer;

pub fn tokenize(source: &str) -> Vec<token::Token> {
    tokenizer::Tokenizer::new(source).collect()
}
