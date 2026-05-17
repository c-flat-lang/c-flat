use crate::stage::lexer::token::{Keyword, Span, Token, TokenKind};
use crate::stage::parser::ast::{Expr, Type, TypeKind};
use report::ReportBuilder;
pub use report::{Report, Result};
use std::fmt::Write;

#[derive(Debug)]
pub struct ErrorUnexpectedExpression {
    expr: Expr,
    expected: Vec<TokenKind>,
    #[cfg(feature = "debug")]
    compiler_line: String,
}

impl ErrorUnexpectedExpression {
    pub fn new(
        expr: Expr,
        expected: &[TokenKind],
        #[cfg(feature = "debug")] compiler_line: String,
    ) -> Self {
        Self {
            expr,
            expected: expected.to_vec(),
            #[cfg(feature = "debug")]
            compiler_line,
        }
    }
}

impl Report for ErrorUnexpectedExpression {
    fn report(&self, filename: &str, src: &str) -> String {
        let span = self.expr.span();
        let mut report = ReportBuilder::new(filename, src, &span);
        report.message({
            let expected = self
                .expected
                .iter()
                .map(|k| format!("`{k:?}`"))
                .collect::<Vec<_>>();
            if expected.len() == 1 {
                format!("exected {}", expected[0])
            } else {
                format!("expected one of: {}", expected.join(", "))
            }
        });
        report.lines_above(3);
        #[cfg(feature = "debug")]
        report.note(&self.compiler_line);
        report.build()
    }
}

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
        let mut report = ReportBuilder::new(filename, src, span);
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
            report.note(&self.compiler_line);
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
    actual: Token,
    expected: Vec<TokenKind>,
    #[cfg(feature = "debug")]
    compiler_line: String,
}

impl ErrorExpectedToken {
    pub fn new(
        actual: Token,
        expected: &[TokenKind],
        #[cfg(feature = "debug")] compiler_line: impl Into<String>,
    ) -> Self {
        Self {
            actual,
            expected: expected.to_vec(),
            #[cfg(feature = "debug")]
            compiler_line: compiler_line.into(),
        }
    }
}

impl Report for ErrorExpectedToken {
    fn report(&self, filename: &str, src: &str) -> String {
        let mut report = ReportBuilder::new(filename, src, &self.actual.span);
        report.message({
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
        });
        report.lines_above(3);
        #[cfg(feature = "debug")]
        report.note(&self.compiler_line);
        report.build()
    }
}

#[derive(Debug)]
#[cfg(not(feature = "debug"))]
pub struct ErrorUnexpectedEndOfInput;

#[derive(Debug)]
#[cfg(feature = "debug")]
pub struct ErrorUnexpectedEndOfInput {
    compiler_line: String,
}

#[allow(clippy::new_without_default)]
impl ErrorUnexpectedEndOfInput {
    pub fn new(#[cfg(feature = "debug")] compiler_line: impl Into<String>) -> Self {
        #[cfg(not(feature = "debug"))]
        {
            Self
        }
        #[cfg(feature = "debug")]
        {
            Self {
                compiler_line: compiler_line.into(),
            }
        }
    }
}

impl Report for ErrorUnexpectedEndOfInput {
    fn report(&self, filename: &str, src: &str) -> String {
        let span = src.len().saturating_sub(1)..src.len();
        let mut report = ReportBuilder::new(filename, src, &span);
        report.message("unexpected end of input");
        #[cfg(feature = "debug")]
        {
            report.note(&self.compiler_line);
        }
        report.build()
    }
}

#[derive(Debug)]
pub struct ErrorExpectedType {
    found: Token,
    #[cfg(feature = "debug")]
    compiler_line: String,
}

impl ErrorExpectedType {
    pub fn new(found: Token, #[cfg(feature = "debug")] compiler_line: impl Into<String>) -> Self {
        Self {
            found,
            #[cfg(feature = "debug")]
            compiler_line: compiler_line.into(),
        }
    }
}

impl Report for ErrorExpectedType {
    fn report(&self, filename: &str, src: &str) -> String {
        let mut report = ReportBuilder::new(filename, src, &self.found.span);
        report.message(format!(
            "expected a type definition but found '{}'",
            self.found.lexeme
        ));
        report.lines_above(3);

        #[allow(unused_mut)]
        let mut note =
            "types must be defined as `u8`, `s8`, `u16`, `s16`, `u32`, `s32`, `f32`".to_string();

        #[cfg(feature = "debug")]
        {
            write!(&mut note, "\n{}", self.compiler_line)
                .expect("Failed to write debug info to ErrorExpectedType");
        }

        report.note(note);
        report.build()
    }
}

#[derive(Debug)]
pub struct ErrorMissingPairedClosingChar {
    span: Span,
    expected: TokenKind,
    #[cfg(feature = "debug")]
    compiler_line: String,
}

impl ErrorMissingPairedClosingChar {
    pub fn new(
        span: Span,
        expected: TokenKind,
        #[cfg(feature = "debug")] compiler_line: impl Into<String>,
    ) -> Self {
        Self {
            span,
            expected,
            #[cfg(feature = "debug")]
            compiler_line: compiler_line.into(),
        }
    }
}

impl Report for ErrorMissingPairedClosingChar {
    fn report(&self, filename: &str, src: &str) -> String {
        let mut report = ReportBuilder::new(filename, src, &self.span);
        report.message("missing closing pair");
        let mut note = String::new();

        write!(&mut note, "expected {:?}", self.expected)
            .expect("Failed to write debug info to ErrorMissingPairedClosingChar");

        #[cfg(feature = "debug")]
        {
            write!(&mut note, "\n{}", self.compiler_line)
                .expect("Failed to write debug info to ErrorMissingPairedClosingChar");
        }
        report.note(note);
        report.lines_above(3);
        report.build()
    }
}

#[derive(Debug)]
pub struct ErrorUnexpectedTopLevelItem {
    found: Token,
    expected: Vec<Keyword>,
    #[cfg(feature = "debug")]
    compiler_line: String,
}

impl ErrorUnexpectedTopLevelItem {
    pub fn new(
        found: Token,
        expected: &[Keyword],
        #[cfg(feature = "debug")] compiler_line: impl Into<String>,
    ) -> Self {
        Self {
            found,
            expected: expected.to_vec(),
            #[cfg(feature = "debug")]
            compiler_line: compiler_line.into(),
        }
    }
}

impl Report for ErrorUnexpectedTopLevelItem {
    fn report(&self, filename: &str, src: &str) -> String {
        let mut report = ReportBuilder::new(filename, src, &self.found.span);
        report.message(format!(
            "unexpected top level item `{}`",
            &self.found.lexeme
        ));

        let mut note = String::new();
        write!(
            &mut note,
            "expected one of `{}`",
            self.expected
                .iter()
                .map(|k| k.to_string())
                .collect::<Vec<_>>()
                .join(", ")
        )
        .expect("Failed writing to ErrorUnexpectedTopLevelItem note");
        #[cfg(feature = "debug")]
        {
            write!(&mut note, "\n{}", self.compiler_line)
                .expect("Failed to write debug info to ErrorUnexpectedTopLevelItem");
        }

        report.note(note);

        report.lines_above(3);
        report.build()
    }
}

#[derive(Debug)]
pub enum ErrorUndefinedSymbol {
    Token(Token),
    Type(Type),

    #[cfg(feature = "debug")]
    TokenDebug(Token, String),

    #[cfg(feature = "debug")]
    TypeDebug(Type, String),
}

impl Report for ErrorUndefinedSymbol {
    fn report(&self, filename: &str, src: &str) -> String {
        #[allow(unused_variables)]
        let (span, name, compiler_line) = match self {
            ErrorUndefinedSymbol::Token(token) => (&token.span, &token.lexeme, None::<&String>),
            ErrorUndefinedSymbol::Type(ty) => (&ty.span, &ty.to_string(), None),

            #[cfg(feature = "debug")]
            ErrorUndefinedSymbol::TokenDebug(token, compiler_line) => {
                (&token.span, &token.lexeme, Some(compiler_line))
            }
            #[cfg(feature = "debug")]
            ErrorUndefinedSymbol::TypeDebug(ty, compiler_line) => {
                (&ty.span, &ty.to_string(), Some(compiler_line))
            }
        };

        let mut report = ReportBuilder::new(filename, src, span);
        report.message(format!("undefined symbol `{}`", name));

        #[cfg(feature = "debug")]
        if let Some(line) = compiler_line {
            report.note(format!("undefined symbol `{}`\n{}", name, line));
        }

        report.build()
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
