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
    fn filename(&self) -> &str {
        &self.span.filename
    }

    fn report(&self, src: &str) -> String {
        ReportBuilder::new(&self.span, src)
            .message("mismatched type")
            .note(format!(
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
    fn filename(&self) -> &str {
        &self.span.filename
    }

    fn report(&self, src: &str) -> String {
        ReportBuilder::new(&self.span, src)
            .message("unsupported binary operator")
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
    fn filename(&self) -> &str {
        &self.span.filename
    }

    fn report(&self, src: &str) -> String {
        ReportBuilder::new(&self.span, src)
            .message("expected keyword")
            .note(format!(
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
    #[cfg(feature = "debug")]
    pub compiler_line: String,
}

impl Report for ErrorExpectedToken {
    fn filename(&self) -> &str {
        &self.actual.span.filename
    }

    fn report(&self, src: &str) -> String {
        let message = {
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
            #[cfg(feature = "debug")]
            {
                format!("{}\n{}", message, self.compiler_line)
            }
            #[cfg(not(feature = "debug"))]
            {
                message
            }
        };
        ReportBuilder::new(&self.actual.span, src)
            .message(message)
            .lines_above(3)
            .build()
    }
}

#[derive(Debug, Default)]
pub struct ErrorUnexpectedEndOfInput {
    pub last_known_token: Option<Token>,
    pub filename: Option<String>,
}

impl Report for ErrorUnexpectedEndOfInput {
    fn filename(&self) -> &str {
        self.last_known_token
            .as_ref()
            .map(|token| &token.span.filename)
            .or(self.filename.as_ref())
            .expect("No filename")
            .as_str()
    }

    fn report(&self, src: &str) -> String {
        let span = self
            .last_known_token
            .as_ref()
            .map(|t| t.span.clone())
            .or(self.filename.as_ref().map(|filename| Span::new(filename)))
            .unwrap_or(Span::new("unknown"));

        ReportBuilder::new(&span, src)
            .message("unexpected end of input")
            .build()
    }
}

#[derive(Debug)]
pub struct ErrorExpectedType {
    pub found: Token,
}

impl Report for ErrorExpectedType {
    fn filename(&self) -> &str {
        &self.found.span.filename
    }

    fn report(&self, src: &str) -> String {
        ReportBuilder::new(&self.found.span, src)
            .message(format!(
                "expected a type definition but found '{}'",
                self.found.lexeme
            ))
            .lines_above(3)
            .note("types must be defined as `u8`, `s8`, `u16`, `s16`, `u32`, `s32`, `f32`, `*`")
            .build()
    }
}

#[derive(Debug)]
pub struct ErrorMissingPairedClosingChar {
    pub span: Span,
    pub expected: TokenKind,
}

impl Report for ErrorMissingPairedClosingChar {
    fn filename(&self) -> &str {
        &self.span.filename
    }

    fn report(&self, src: &str) -> String {
        ReportBuilder::new(&self.span, src)
            .message("missing closing pair")
            .note(format!("expected {:?}", self.expected))
            .lines_above(3)
            .build()
    }
}

#[derive(Debug)]
pub struct ErrorUnexpectedTopLevelItem {
    pub found: Token,
    pub expected: Vec<Keyword>,
}

impl Report for ErrorUnexpectedTopLevelItem {
    fn filename(&self) -> &str {
        &self.found.span.filename
    }

    fn report(&self, src: &str) -> String {
        ReportBuilder::new(&self.found.span, src)
            .message(format!(
                "unexpected top level item `{}`",
                &self.found.lexeme
            ))
            .note(format!(
                "expected one of `{}`",
                self.expected
                    .iter()
                    .map(|k| k.to_string())
                    .collect::<Vec<_>>()
                    .join(", ")
            ))
            .lines_above(3)
            .build()
    }
}

#[derive(Debug)]
pub struct Errors {
    pub errors: Vec<Box<dyn Report>>,
}

impl Report for Errors {
    fn filename(&self) -> &str {
        "ERRORS has no single filename"
    }

    fn report(&self, src: &str) -> String {
        let mut final_report = String::new();
        for error in self.errors.iter() {
            final_report.push_str(&error.report(src));
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
    fn filename(&self) -> &str {
        &self.found.span.filename
    }

    fn report(&self, src: &str) -> String {
        ReportBuilder::new(&self.found.span, src)
            .message(format!("undefined symbol `{}`", &self.found.lexeme))
            .build()
    }
}

#[derive(Debug)]
pub struct ErrorMissingSymbol {
    pub symbol: Token,
}

impl Report for ErrorMissingSymbol {
    fn filename(&self) -> &str {
        &self.symbol.span.filename
    }

    fn report(&self, src: &str) -> String {
        ReportBuilder::new(&self.symbol.span, src)
            .message(format!("symbol `{}` not found", &self.symbol.lexeme))
            .build()
    }
}
