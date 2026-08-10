use crate::stage::{Stage, StageContext, StageOutput};
use std::{fmt::Write, path::Path};
pub mod token;
pub mod tokenizer;
use crate::DebugMode;
use crate::error::{ErrorMessage, Report};
use token::Token;

#[cfg(test)]
mod tests;

#[inline]
pub fn lex(file_path: &str, source: &str) -> Vec<Token> {
    tokenizer::Tokenizer::new(file_path, source).collect()
}

pub struct LexerStage {
    tokens: Vec<Token>,
}

impl Stage for LexerStage {
    fn name(&self) -> &'static str {
        "Lexing"
    }
    fn debug_mode(&self) -> &'static [DebugMode] {
        &[crate::DebugMode::Lexer]
    }

    fn debug(&self, _ctx: &mut StageContext) -> StageOutput {
        let mut output = String::new();
        for token in self.tokens.iter() {
            write!(&mut output, "{:?}", token)
                .expect("Failed to write to output string in Lexer debug stage");
        }

        StageOutput::Output(output)
    }

    #[cfg(not(feature = "wasm"))]
    fn run(&mut self, ctx: &mut StageContext) -> report::Result<()> {
        let path = Path::new(&ctx.entry);

        let raw = std::fs::read_to_string(path).map_err(|err| -> Box<dyn Report> {
            Box::new(ErrorMessage(format!(
                "could not read `{}`: {}",
                path.display(),
                err
            )))
        })?;

        let source = if ctx.unix_newlines {
            raw.replace("\r\n", "\n")
        } else {
            raw
        };

        self.tokens = lex(path.to_str().unwrap_or_default(), &source);
        Ok(())
    }

    #[cfg(feature = "wasm")]
    fn run(&mut self, ctx: &mut StageContext) -> report::Result<()> {
        self.tokens = lex(ctx.entry.as_str(), &ctx.source);
        Ok(())
    }
}
