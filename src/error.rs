#![allow(unused)]

use crate::stage::lexer::token::{Keyword, Span, Token, TokenKind};
use crate::stage::parser::ast::Type;
use std::fmt::Write;

pub trait Report: std::fmt::Debug {
    fn report(&self, filename: &str, src: &str) -> String;
}

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
        ReportBuilder::new(filename, src, &self.actual.span)
            .with_message(format!(
                "expected token `{:?}`, found `{}`",
                self.expected, self.actual.lexeme
            ))
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
            .with_note(&format!("expected {:?}", self.expected))
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
pub enum CompilerError {
    UnsupportedBinaryOp(ErrorUnsupportedBinaryOp),
    MissMatchedType(ErrorMissMatchedType),
    ExpectedKeyWord(ErrorExpectedKeyWord),
    ExpectedToken(ErrorExpectedToken),
    UnexpectedEndOfInput(ErrorUnexpectedEndOfInput),
    ExpectedType(ErrorExpectedType),
    MissingPairedClosingChar(ErrorMissingPairedClosingChar),
    UnexpectedTopLevelItem(ErrorUnexpectedTopLevelItem),
    UndefinedSymbol(ErrorUndefinedSymbol),
    Errors(Errors),
}

impl CompilerError {
    pub fn report(&self, filename: &str, src: &str) -> String {
        match self {
            Self::UnsupportedBinaryOp(error) => error.report(filename, src),
            Self::MissMatchedType(error) => error.report(filename, src),
            Self::ExpectedKeyWord(error) => error.report(filename, src),
            Self::ExpectedToken(error) => error.report(filename, src),
            Self::UnexpectedEndOfInput(error) => error.report(filename, src),
            Self::ExpectedType(error) => error.report(filename, src),
            Self::MissingPairedClosingChar(error) => error.report(filename, src),
            Self::UnexpectedTopLevelItem(error) => error.report(filename, src),
            Self::UndefinedSymbol(error) => error.report(filename, src),
            Self::Errors(error) => error.report(filename, src),
        }
    }
}

pub struct ReportBuilder<'a> {
    message: String,
    filename: &'a str,
    row: usize,
    col: usize,
    src: &'a str,
    problem_line: &'a str,
    lines_above: usize,
    note: Option<String>,
    underline: String,
}

impl<'a> ReportBuilder<'a> {
    pub fn new(filename: &'a str, src: &'a str, span: &'a Span) -> Self {
        let (row, col) = Self::get_row_col_from_span(src, span);
        let problem_line = Self::get_problem_src_line(src, row);
        let underline = Self::get_underline(col, span);

        Self {
            message: String::new(),
            filename,
            row,
            col,
            src,
            problem_line,
            lines_above: 0,
            note: None,
            underline,
        }
    }

    pub fn with_message(mut self, message: impl Into<String>) -> Self {
        self.message = message.into();
        self
    }

    pub fn with_note(mut self, note: impl Into<String>) -> Self {
        self.note = Some(note.into());
        self
    }

    /// Lines to use above starting [Span]
    pub fn with_lines_above(mut self, lines_above: usize) -> Self {
        self.lines_above = lines_above;
        self
    }

    pub fn build(&self) -> String {
        let mut report = String::new();
        writeln!(&mut report, "{}:{}:{}", self.row, self.col, self.filename)
            .expect("failed to write report");
        if !self.message.is_empty() {
            writeln!(&mut report, " --> {}", self.message).expect("failed to write message");
        }
        let starting_row_above = self.row.saturating_sub(self.lines_above);
        for (idx, line) in self
            .src
            .lines()
            .enumerate()
            .skip(starting_row_above)
            .take(self.lines_above.min(starting_row_above))
        {
            writeln!(&mut report, "{:<3}| {}", idx, line).expect("failed to write line");
        }

        writeln!(&mut report, "{:<3}| {}", self.row, self.problem_line)
            .expect("failed to write problem line");
        writeln!(&mut report, "   | {}", self.underline).expect("failed to write underline");
        if let Some(note) = &self.note {
            writeln!(&mut report, "   | = note: {}", note).expect("failed to write help");
        }
        report
    }

    fn get_row_col_from_span(src: &str, span: &Span) -> (usize, usize) {
        let row = src[..span.start].chars().filter(|&c| c == '\n').count();
        let col = span.start - src[..span.start].rfind('\n').map(|n| n + 1).unwrap_or(0);
        (row, col)
    }

    fn get_problem_src_line(src: &'a str, row: usize) -> &'a str {
        src.lines()
            .nth(row)
            .unwrap_or("Failed to extract source line")
    }

    fn get_underline(col: usize, span: &Span) -> String {
        let spacing = " ".repeat(col);
        let underline = "^".repeat(span.len());
        format!("{}{}", spacing, underline)
    }
}
