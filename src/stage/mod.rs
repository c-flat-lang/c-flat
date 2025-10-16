pub mod ir_builder;
pub mod lexer;
pub mod parser;
pub mod semantic_analyzer;

pub trait Stage<In, Out> {
    fn run(&mut self, input: In) -> Out;
}
