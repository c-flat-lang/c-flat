use crate::error::{ErrorMessage, Report, Result, ScopedReport};
use crate::stage::lexer::token::Token;
use crate::stage::{Stage, StageContext, StageOutput};
use std::fmt::Write;
use std::path::Path;
pub mod ast;
mod syntax_analyzer;
use crate::DebugMode;

#[inline]
pub fn parse(filename: &str, tokens: Vec<Token>) -> Result<Vec<ast::Item>> {
    syntax_analyzer::Parser::new(filename, tokens.into_iter().peekable()).parse()
}

pub struct ParserStage;

impl Stage for ParserStage {
    fn name(&self) -> &'static str {
        "Parser"
    }
    fn debug_mode(&self) -> &'static [DebugMode] {
        &[DebugMode::Parser]
    }

    fn debug(&self, ctx: &mut StageContext) -> StageOutput {
        let mut output = String::new();
        for token in ctx.items.iter() {
            write!(&mut output, "{:?}", token)
                .expect("Failed to write to output string in Parser debug stage");
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

        let tokens = crate::stage::lexer::lex(path.to_str().unwrap_or_default(), &source);

        let filename = path.to_str().unwrap_or_default();
        let items = parse(filename, tokens).map_err(|err| -> Box<dyn Report> {
            Box::new(ScopedReport::new(
                path.display().to_string(),
                source.clone(),
                err,
            ))
        })?;

        ctx.items = items;
        Ok(())
    }

    #[cfg(feature = "wasm")]
    fn run(&mut self, ctx: &mut StageContext) -> report::Result<()> {
        let tokenizer =
            crate::stage::lexer::tokenizer::Tokenizer::new(ctx.entry.as_str(), &ctx.source);
        let tokens: Vec<Token> = tokenizer.collect();

        let items = syntax_analyzer::Parser::new(ctx.entry.as_str(), tokens.into_iter().peekable())
            .parse()
            .map_err(|err| -> Box<dyn Report> {
                Box::new(ScopedReport::new(ctx.entry.as_str(), &ctx.source, err))
            })?;

        ctx.items = items;
        Ok(())
    }
}
