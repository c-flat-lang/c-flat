mod error;
mod stage;
use stage::Stage;

use bitbox::Target;

use crate::error::CompilerError;

fn front_end_compiler(src: &str) -> Result<bitbox::ir::Module, CompilerError> {
    let tokens = stage::lexer::Lexer::default().run(src);
    let mut ast = stage::parser::Parser::default().run(tokens)?;
    let symbol_table = stage::semantic_analyzer::SemanticAnalyzer::default().run(&mut ast)?;
    stage::ir_builder::IRBuilder::default().run((symbol_table, ast))
}

fn main() {
    let src_name = "main.cb";
    let src = r#"
pub fn main() s32 {
  const x: s32 = 123;
  const y: s32 = 321;
  return if x > y {
    x
  } else {
    y
  };
}
"#;
    let target = Target::Wasm32;
    let mut module = match front_end_compiler(src) {
        Ok(module) => module,
        Err(err) => {
            println!("{:?}", err);
            return;
        }
    };
    let mut ctx = bitbox::backend::Context::default();
    bitbox::Compiler::new(src_name, target).run(&mut module, &mut ctx);
}
