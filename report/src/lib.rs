use std::fmt::Write;

pub trait Report: std::fmt::Debug {
    fn report(&self, filename: &str, src: &str) -> String;
}

pub type Span = std::ops::Range<usize>;
pub type CompilerError = Box<dyn Report>;
pub type Result<T> = std::result::Result<T, CompilerError>;

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
