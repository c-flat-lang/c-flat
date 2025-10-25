use crate::text::lexer::token::{Span, Token, TokenKind};
use std::fmt::Write;

#[derive(Debug)]
pub enum BitBoxError {
    UnexpectedToken {
        expected: TokenKind,
        actual: Token,
    },
    InvalidContantValue(Token),
    InvalidInstruction(Token),
    InvalidToken(Token),
    UnexpectedEndOfStream,
    ExpectedTopLevelItem(Token),
    ExpectedType(Token),
    UndefinedVariable(Token),
    InvalidType {
        expected: String,
        actual_type: String,
        actual: Token,
    },
    ExpectedBlockName(Token),
    ToManyItemsOnOneLine(Token),
}

impl BitBoxError {
    pub fn report(&self, filename: &str, src: &str) -> String {
        match self {
            Self::InvalidToken(token) => ReportBuilder::new(filename, src, &token.span)
                .with_message("invalid token found")
                .build(),
            Self::InvalidContantValue(token) => ReportBuilder::new(filename, src, &token.span)
                .with_message(format!("invalid constant value '{}'", token.lexeme))
                .with_note("expected a directive, number or string")
                .build(),
            Self::InvalidInstruction(token) => ReportBuilder::new(filename, src, &token.span)
                .with_message("invalid instruction")
                .with_note("expected one of: add, call, ret, sub ...")
                .build(),
            Self::UnexpectedToken { expected, actual } => {
                ReportBuilder::new(filename, src, &actual.span)
                    .with_message(format!("unexpected token '{}'", actual.lexeme))
                    .with_lines_above(3)
                    .with_note(format!(
                        "expected: {:?}, found: {}",
                        expected, actual.lexeme
                    ))
                    .build()
            }
            Self::UnexpectedEndOfStream => {
                ReportBuilder::new(filename, src, &(src.len().saturating_sub(1)..src.len()))
                    .with_message("unexpected end of stream")
                    .build()
            }
            Self::ExpectedTopLevelItem(token) => ReportBuilder::new(filename, src, &token.span)
                .with_message("expected a top level item")
                .with_note("function, import or constant")
                .with_lines_above(3)
                .build(),
            Self::ExpectedType(token) => ReportBuilder::new(filename, src, &token.span)
                .with_message(format!("expected a type but found {:?}", token.kind))
                .with_note("expected a type: s32, u32, f32, ...")
                .build(),
            Self::UndefinedVariable(token) => ReportBuilder::new(filename, src, &token.span)
                .with_message("undefined variable")
                .with_note("variable's must be forwarded defined")
                .build(),
            Self::InvalidType {
                expected,
                actual_type,
                actual,
            } => ReportBuilder::new(filename, src, &actual.span)
                .with_message(format!("invalid type for {}", actual.lexeme))
                .with_note(format!("expected: {}, found: {}", expected, actual_type))
                .build(),

            Self::ExpectedBlockName(token) => ReportBuilder::new(filename, src, &token.span)
                .with_message("expected a block name")
                .with_note("example %block_name:")
                .build(),
            Self::ToManyItemsOnOneLine(token) => ReportBuilder::new(filename, src, &token.span)
                .with_message("too many items on one line")
                .with_note("expected a single line")
                .build(),
        }
    }
}

impl From<Token> for BitBoxError {
    fn from(token: Token) -> Self {
        Self::InvalidToken(token)
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
            .skip(starting_row_above.min(starting_row_above))
            .take(self.lines_above)
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
