pub mod ast;
mod parser;
use super::Stage;
use crate::{error::CompilerError, stage::lexer::token::Token};

#[derive(Debug, Default)]
pub struct Parser {}

impl Stage<Vec<Token>, Result<Vec<ast::Item>, CompilerError>> for Parser {
    fn run(&mut self, input: Vec<Token>) -> Result<Vec<ast::Item>, CompilerError> {
        parser::Parser::new(input.into_iter().peekable()).parse()
    }
}
