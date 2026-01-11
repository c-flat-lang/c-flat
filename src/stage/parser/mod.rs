pub mod ast;
mod syntax_analyzer;
use super::Stage;
use crate::{error::Result, stage::lexer::token::Token};

#[derive(Debug, Default)]
pub struct Parser {}

impl Stage<Vec<Token>, Result<Vec<ast::Item>>> for Parser {
    fn run(&mut self, input: Vec<Token>) -> Result<Vec<ast::Item>> {
        syntax_analyzer::Parser::new(input.into_iter().peekable()).parse()
    }
}
