use crate::stage::lexer::token::{Keyword, Span, Token, TokenKind};
use crate::stage::parser::ast::{Type, TypeKind};
use report::ReportBuilder;
pub use report::{Report, Result};
use std::fmt::Write;

#[derive(Debug)]
pub struct ErrorMissMatchedType {
    alt_span: Option<Span>,
    found: Type,
    expected: TypeKind,
    #[cfg(feature = "debug")]
    compiler_line: String,
}

impl ErrorMissMatchedType {
    pub fn new(
        found: Type,
        expected: TypeKind,
        #[cfg(feature = "debug")] compiler_line: String,
    ) -> Self {
        Self {
            alt_span: None,
            found,
            expected,
            #[cfg(feature = "debug")]
            compiler_line,
        }
    }

    pub fn alt_span(&mut self, alt_span: Span) {
        self.alt_span = Some(alt_span);
    }
}

impl Report for ErrorMissMatchedType {
    fn report(&self, filename: &str, src: &str) -> String {
        let span = self.alt_span.as_ref().unwrap_or(&self.found.span);
        let mut report = ReportBuilder::new(filename, src, &span);
        report.message("mismatched type");

        let mut note = String::new();
        write!(
            &mut note,
            "expected `{}`, found `{}`",
            self.expected, self.found
        )
        .expect("Failed to write to note in ErrorExpectedType");

        #[cfg(feature = "debug")]
        {
            write!(&mut note, "\n{}", self.compiler_line)
                .expect("Failed to write debug info to note in ErrorExpectedType");
        }

        report.note(note);
        report.build()
    }
}

#[derive(Debug)]
pub struct ErrorUnsupportedBinaryOp {
    lhs: Type,
    rhs: Type,
    op: Token,
    #[cfg(feature = "debug")]
    compiler_line: String,
}

impl ErrorUnsupportedBinaryOp {
    pub fn new(
        op: Token,
        lhs: Type,
        rhs: Type,
        #[cfg(feature = "debug")] compiler_line: impl Into<String>,
    ) -> Self {
        Self {
            lhs,
            rhs,
            op,
            #[cfg(feature = "debug")]
            compiler_line: compiler_line.into(),
        }
    }
}

impl Report for ErrorUnsupportedBinaryOp {
    fn report(&self, filename: &str, src: &str) -> String {
        let span = self.lhs.span.start..self.rhs.span.end;
        let mut report = ReportBuilder::new(filename, src, &span);
        report.message(format!(
            "unsupported binary operator with {} {} {}",
            self.lhs.kind, self.op.lexeme, self.rhs.kind,
        ));
        #[cfg(feature = "debug")]
        {
            report.note(&self.compiler_line)
        }
        report.build()
    }
}

#[derive(Debug)]
pub struct ErrorExpectedKeyWord {
    pub span: Span,
    pub actual: Token,
    pub expected: Vec<Keyword>,
    #[cfg(feature = "debug")]
    compiler_line: String,
}

impl ErrorExpectedKeyWord {
    pub fn new(
        span: Span,
        actual: Token,
        expected: &[Keyword],

        #[cfg(feature = "debug")] compiler_line: impl Into<String>,
    ) -> Self {
        Self {
            span,
            actual,
            expected: expected.to_vec(),
            #[cfg(feature = "debug")]
            compiler_line: compiler_line.into(),
        }
    }
}

impl Report for ErrorExpectedKeyWord {
    fn report(&self, filename: &str, src: &str) -> String {
        let mut note = String::new();
        write!(
            &mut note,
            "expected `{}`, found `{}`",
            self.expected
                .iter()
                .map(|k| k.to_string())
                .collect::<Vec<_>>()
                .join(", "),
            self.actual.lexeme,
        )
        .expect("Failed to write note in ErrorExpectedKeyWord");

        #[cfg(feature = "debug")]
        {
            write!(&mut note, "\n{}", self.compiler_line)
                .expect("Failed to write note in ErrorExpectedKeyWord");
        }

        ReportBuilder::new(filename, src, &self.span)
            .message("expected keyword")
            .note(note)
            .build()
    }
}

#[derive(Debug)]
pub struct ErrorExpectedToken {
    pub actual: Token,
    pub expected: Vec<TokenKind>,
}

impl Report for ErrorExpectedToken {
    fn report(&self, filename: &str, src: &str) -> String {
        ReportBuilder::new(filename, src, &self.actual.span)
            .with_message({
                let expected = self
                    .expected
                    .iter()
                    .map(|k| format!("`{k:?}`"))
                    .collect::<Vec<_>>();
                let expected_str = if expected.len() == 1 {
                    format!("exected {}", expected[0])
                } else {
                    format!("expected one of: {}", expected.join(", "))
                };
                format!("{}, found `{}`", expected_str, self.actual.lexeme)
            })
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
            .with_note("types must be defined as `u8`, `s8`, `u16`, `s16`, `u32`, `s32`, `f32`")
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
pub enum ErrorUndefinedSymbol {
    Token(Token),
    Type(Type),
}

impl Report for ErrorUndefinedSymbol {
    fn report(&self, filename: &str, src: &str) -> String {
        let span = match self {
            ErrorUndefinedSymbol::Token(token) => &token.span,
            ErrorUndefinedSymbol::Type(ty) => &ty.span,
        };
        let name = match self {
            ErrorUndefinedSymbol::Token(token) => &token.lexeme,
            ErrorUndefinedSymbol::Type(ty) => &ty.to_string(),
        };
        ReportBuilder::new(filename, src, span)
            .with_message(format!("undefined symbol `{}`", name))
            .build()
    }
}
