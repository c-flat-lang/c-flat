use crate::ir::Type;
use crate::text::lexer::token::{Keyword, Span, Token, TokenKind};
use report::ReportBuilder;
pub use report::{Report, Result};

#[derive(Debug)]
pub struct ErrorMissMatchedType {
    pub span: Span,
    pub found: Type,
    pub expected: Type,
}

impl Report for ErrorMissMatchedType {
    fn report(&self, filename: &str, src: &str) -> String {
        ReportBuilder::new(filename, src, &self.span)
            .with_message("mismatched type")
            .with_note(format!(
                "expected `{}`, found `{}`",
                self.expected, self.found
            ))
            .build()
    }
}

#[derive(Debug)]
pub struct ErrorUnsupportedBinaryOp {
    pub span: Span,
    pub lhs: Type,
    pub rhs: Type,
    pub op: Token,
}

impl Report for ErrorUnsupportedBinaryOp {
    fn report(&self, filename: &str, src: &str) -> String {
        ReportBuilder::new(filename, src, &self.span)
            .with_message("unsupported binary operator")
            .build()
    }
}

#[derive(Debug)]
pub struct ErrorExpectedKeyWord {
    pub span: Span,
    pub actual: Token,
    pub expected: Vec<Keyword>,
}

impl Report for ErrorExpectedKeyWord {
    fn report(&self, filename: &str, src: &str) -> String {
        ReportBuilder::new(filename, src, &self.span)
            .with_message("expected keyword")
            .with_note(format!(
                "expected `{}`, found `{}`",
                self.expected
                    .iter()
                    .map(|k| k.to_string())
                    .collect::<Vec<_>>()
                    .join(", "),
                self.actual.lexeme
            ))
            .build()
    }
}

#[derive(Debug)]
pub struct ErrorExpectedToken {
    pub actual: Token,
    pub expected: TokenKind,
}

impl Report for ErrorExpectedToken {
    fn report(&self, filename: &str, src: &str) -> String {
        let message = match &self.expected {
            TokenKind::Keyword(keyword) => {
                format!(
                    "expected keyword like `{}`, but found `{}`",
                    keyword, self.actual.lexeme
                )
            }
            TokenKind::Instruction(instruction) => {
                format!(
                    "expected instruction like `{}`, but found `{}`",
                    instruction, self.actual.lexeme
                )
            }
            _ => format!(
                "expected token `{:?}`, found `{}`",
                self.expected, self.actual.lexeme
            ),
        };
        ReportBuilder::new(filename, src, &self.actual.span)
            .with_message(message)
            .with_lines_above(3)
            .build()
    }
}

#[derive(Debug)]
pub struct ErrorUnexpectedEndOfInput;

impl Report for ErrorUnexpectedEndOfInput {
    fn report(&self, filename: &str, src: &str) -> String {
        ReportBuilder::new(filename, src, &Span::default())
            .with_message("unexpected end of input")
            .build()
    }
}

#[derive(Debug)]
pub struct ErrorExpectedType {
    pub found: Token,
}

impl Report for ErrorExpectedType {
    fn report(&self, filename: &str, src: &str) -> String {
        ReportBuilder::new(filename, src, &self.found.span)
            .with_message(format!(
                "expected a type definition but found '{}'",
                self.found.lexeme
            ))
            .with_lines_above(3)
            .with_note(
                "types must be defined as `u8`, `s8`, `u16`, `s16`, `u32`, `s32`, `f32`, `*`",
            )
            .build()
    }
}

#[derive(Debug)]
pub struct ErrorMissingPairedClosingChar {
    pub span: Span,
    pub expected: TokenKind,
}

impl Report for ErrorMissingPairedClosingChar {
    fn report(&self, filename: &str, src: &str) -> String {
        ReportBuilder::new(filename, src, &self.span)
            .with_message("missing closing pair")
            .with_note(format!("expected {:?}", self.expected))
            .with_lines_above(3)
            .build()
    }
}

#[derive(Debug)]
pub struct ErrorUnexpectedTopLevelItem {
    pub found: Token,
    pub expected: Vec<Keyword>,
}

impl Report for ErrorUnexpectedTopLevelItem {
    fn report(&self, filename: &str, src: &str) -> String {
        ReportBuilder::new(filename, src, &self.found.span)
            .with_message(format!(
                "unexpected top level item `{}`",
                &self.found.lexeme
            ))
            .with_note(format!(
                "expected one of `{}`",
                self.expected
                    .iter()
                    .map(|k| k.to_string())
                    .collect::<Vec<_>>()
                    .join(", ")
            ))
            .with_lines_above(3)
            .build()
    }
}

#[derive(Debug)]
pub struct Errors {
    pub errors: Vec<Box<dyn Report>>,
}

impl Report for Errors {
    fn report(&self, filename: &str, src: &str) -> String {
        let mut final_report = String::new();
        for error in self.errors.iter() {
            final_report.push_str(&error.report(filename, src));
            final_report.push('\n');
        }
        final_report
    }
}

#[derive(Debug)]
pub struct ErrorUndefinedSymbol {
    pub found: Token,
}

impl Report for ErrorUndefinedSymbol {
    fn report(&self, filename: &str, src: &str) -> String {
        ReportBuilder::new(filename, src, &self.found.span)
            .with_message(format!("undefined symbol `{}`", &self.found.lexeme))
            .build()
    }
}

#[derive(Debug)]
pub struct ErrorMissingSymbol {
    pub symbol: Token,
}

impl Report for ErrorMissingSymbol {
    fn report(&self, filename: &str, src: &str) -> String {
        ReportBuilder::new(filename, src, &self.symbol.span)
            .with_message(format!("symbol `{}` not found", &self.symbol.lexeme))
            .build()
    }
}
