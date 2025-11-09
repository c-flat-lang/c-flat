#![allow(unused)]
use crate::stage::lexer::token::{Span, Token, TokenKind};
use std::fmt::Write;

#[derive(Debug)]
pub enum CompilerError {
    InvalidToken(Token),
    ExpectedToken { actual: Token, expected: TokenKind },
    UnexpectedEndOfInput,
    ExpectedAType(Token),
    MissingClosingParen(Span),
    UnexpectedTopLevelItem(Token),
    SymbolTableErrors(Vec<String>),
    TypeErrors(Vec<String>),
}

impl CompilerError {
    pub fn report(&self, filename: &str, src: &str) -> String {
        match self {
            CompilerError::InvalidToken(token) => ReportBuilder::new(filename, src, &token.span)
                .with_message("invalid token")
                .with_lines_above(3)
                .build(),
            CompilerError::ExpectedToken { actual, expected } => {
                ReportBuilder::new(filename, src, &actual.span)
                    .with_message(format!(
                        "expected token `{:?}`, found `{}`",
                        expected, actual.lexeme
                    ))
                    .with_lines_above(3)
                    .build()
            }
            CompilerError::UnexpectedEndOfInput => {
                ReportBuilder::new(filename, src, &Span::default())
                    .with_message("unexpected end of input")
                    .build()
            }
            CompilerError::ExpectedAType(token) => ReportBuilder::new(filename, src, &token.span)
                .with_message(format!(
                    "expected a type definition but found '{}'",
                    token.lexeme
                ))
                .with_lines_above(3)
                .with_note(
                    "types must be defined as `u8`, `s8`, `u16`, `s16`, `u32`, `s32`, `f32`, `*`",
                )
                .build(),
            CompilerError::MissingClosingParen(range) => ReportBuilder::new(filename, src, range)
                .with_message("missing closing parenthesis")
                .with_lines_above(3)
                .build(),
            CompilerError::UnexpectedTopLevelItem(token) => {
                ReportBuilder::new(filename, src, &token.span)
                    .with_message(format!("unexpected top level item `{}`", token.lexeme))
                    .with_lines_above(3)
                    .build()
            }
            CompilerError::SymbolTableErrors(errors) => {
                // TODO: lets not
                let mut report = String::new();
                for error in errors {
                    report.push_str(error);
                    report.push('\n');
                }
                report
            }
            CompilerError::TypeErrors(errors) => {
                // TODO: lets not
                let mut report = String::new();
                for error in errors {
                    report.push_str(error);
                    report.push('\n');
                }
                report
            }
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
