pub mod ast;
mod syntax_analyzer;
use super::Stage;
use crate::{error::Result, stage::lexer::token::Token};

#[derive(Debug, Default)]
pub struct Parser {}

impl Stage<(&str, Vec<Token>), Result<Vec<ast::Item>>> for Parser {
    fn run(&mut self, (filename, input): (&str, Vec<Token>)) -> Result<Vec<ast::Item>> {
        eprintln!("{: >30}", "Parser");
        syntax_analyzer::Parser::new(filename, input.into_iter().peekable()).parse()
    }
}
