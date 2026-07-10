use crate::stage::lexer::token::{Keyword, Span, Token, TokenKind};
use crate::stage::parser::ast::{Expr, Type, TypeKind};
use report::ReportBuilder;
pub use report::{Report, Result};
use std::fmt::Write;

#[derive(Debug)]
pub struct ErrorMemberAccess {
    span: Span,
    #[cfg(feature = "debug")]
    compiler_line: String,
}

impl ErrorMemberAccess {
    pub fn new(span: Span, #[cfg(feature = "debug")] compiler_line: String) -> Self {
        Self {
            span,
            #[cfg(feature = "debug")]
            compiler_line,
        }
    }
}

impl Report for ErrorMemberAccess {
    fn filename(&self) -> &str {
        &self.span.filename
    }

    fn report(&self, src: &str) -> String {
        let span = &self.span;
        let mut report = ReportBuilder::new(span, src);
        report.message("unknown member");
        report.lines_above(3);
        #[cfg(feature = "debug")]
        report.note(&self.compiler_line);
        report.build()
    }
}

#[derive(Debug)]
pub struct ErrorUnexpectedToken {
    found: Token,
    expected: Vec<TokenKind>,
    #[cfg(feature = "debug")]
    compiler_line: String,
}

impl ErrorUnexpectedToken {
    pub fn new(
        found: Token,
        expected: &[TokenKind],
        #[cfg(feature = "debug")] compiler_line: String,
    ) -> Self {
        Self {
            found,
            expected: expected.to_vec(),
            #[cfg(feature = "debug")]
            compiler_line,
        }
    }
}

impl Report for ErrorUnexpectedToken {
    fn filename(&self) -> &str {
        &self.found.span.filename
    }

    fn report(&self, src: &str) -> String {
        let span = &self.found.span;
        let mut report = ReportBuilder::new(span, src);
        report.message({
            let expected = self
                .expected
                .iter()
                .map(|k| format!("`{k:?}`"))
                .collect::<Vec<_>>();
            if expected.len() == 1 {
                format!("expected {}", expected[0])
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
pub struct ErrorUnexpectedExpression {
    expr: Expr,
    expected: Vec<TokenKind>,
    span: Span,
    #[cfg(feature = "debug")]
    compiler_line: String,
}

impl ErrorUnexpectedExpression {
    pub fn new(
        expr: Expr,
        expected: &[TokenKind],
        #[cfg(feature = "debug")] compiler_line: String,
    ) -> Self {
        let span = expr.span();
        Self {
            expr,
            expected: expected.to_vec(),
            span,
            #[cfg(feature = "debug")]
            compiler_line,
        }
    }
}

impl Report for ErrorUnexpectedExpression {
    fn filename(&self) -> &str {
        &self.span.filename
    }

    fn report(&self, src: &str) -> String {
        let span = self.expr.span();
        let mut report = ReportBuilder::new(&span, src);
        report.message({
            let expected = self
                .expected
                .iter()
                .map(|k| format!("`{k:?}`"))
                .collect::<Vec<_>>();
            if expected.len() == 1 {
                format!("expected {}", expected[0])
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

    pub fn alt_span(mut self, alt_span: Span) -> Self {
        self.alt_span = Some(alt_span);
        self
    }
}

impl Report for ErrorMissMatchedType {
    fn filename(&self) -> &str {
        self.alt_span
            .as_ref()
            .map(|span| span.filename.as_str())
            .unwrap_or(self.found.span.filename.as_str())
    }

    fn report(&self, src: &str) -> String {
        let span = self.alt_span.as_ref().unwrap_or(&self.found.span);
        let mut report = ReportBuilder::new(span, src);
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
    fn filename(&self) -> &str {
        &self.op.span.filename
    }

    fn report(&self, src: &str) -> String {
        let span = Span {
            start: self.lhs.span.start,
            end: self.rhs.span.end,
            filename: self.rhs.span.filename.clone(),
        };
        let mut report = ReportBuilder::new(&span, src);
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
    fn filename(&self) -> &str {
        &self.span.filename
    }

    fn report(&self, src: &str) -> String {
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

        ReportBuilder::new(&self.span, src)
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
    fn filename(&self) -> &str {
        &self.actual.span.filename
    }

    fn report(&self, src: &str) -> String {
        let mut report = ReportBuilder::new(&self.actual.span, src);
        report.message({
            let expected = self
                .expected
                .iter()
                .map(|k| format!("`{k:?}`"))
                .collect::<Vec<_>>();
            let expected_str = if expected.len() == 1 {
                format!("expected {}", expected[0])
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
pub struct ErrorUnexpectedEndOfInput {
    last_token: Option<Token>,
    filename: String,
    #[cfg(feature = "debug")]
    compiler_line: String,
}

#[allow(clippy::new_without_default)]
impl ErrorUnexpectedEndOfInput {
    pub fn new(
        last_token: Option<Token>,
        filename: impl Into<String>,
        #[cfg(feature = "debug")] compiler_line: impl Into<String>,
    ) -> Self {
        Self {
            last_token,
            filename: filename.into(),
            #[cfg(feature = "debug")]
            compiler_line: compiler_line.into(),
        }
    }
}

impl Report for ErrorUnexpectedEndOfInput {
    fn filename(&self) -> &str {
        self.last_token
            .as_ref()
            .map(|token| token.span.filename.as_str())
            .unwrap_or(self.filename.as_str())
    }

    fn report(&self, src: &str) -> String {
        let span = self
            .last_token
            .as_ref()
            .map(|t| t.span.clone())
            .unwrap_or(Span::new(&self.filename));

        let mut report = ReportBuilder::new(&span, src);
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
    fn filename(&self) -> &str {
        &self.found.span.filename
    }

    fn report(&self, src: &str) -> String {
        let mut report = ReportBuilder::new(&self.found.span, src);
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
    fn filename(&self) -> &str {
        &self.span.filename
    }

    fn report(&self, src: &str) -> String {
        let mut report = ReportBuilder::new(&self.span, src);
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
    fn filename(&self) -> &str {
        &self.found.span.filename
    }

    fn report(&self, src: &str) -> String {
        let mut report = ReportBuilder::new(&self.found.span, src);
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
    fn filename(&self) -> &str {
        match self {
            ErrorUndefinedSymbol::Token(token) => &token.span.filename,
            ErrorUndefinedSymbol::Type(ty) => &ty.span.filename,
            #[cfg(feature = "debug")]
            ErrorUndefinedSymbol::TokenDebug(token, _) => &token.span.filename,
            #[cfg(feature = "debug")]
            ErrorUndefinedSymbol::TypeDebug(ty, _) => &ty.span.filename,
        }
    }

    fn report(&self, src: &str) -> String {
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

        let mut report = ReportBuilder::new(span, src);
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
    fn filename(&self) -> &str {
        "Errors dont have a single filename"
    }

    fn report(&self, _src: &str) -> String {
        let mut final_report = String::new();
        for error in self.errors.iter() {
            let filename = error.filename();
            if filename == "ErrorMessage" {
                final_report.push_str(&error.report(""));
                final_report.push('\n');
                continue;
            }
            let maybe_src = std::fs::read_to_string(filename).map_err(|err| -> Box<dyn Report> {
                Box::new(ErrorMessage(format!(
                    "could not read `{}`: {}",
                    filename, err,
                )))
            });

            let src = match maybe_src {
                Ok(src) => src,
                Err(err) => {
                    let errors = err.report("");
                    eprintln!("{}", errors);
                    std::process::exit(1);
                }
            };

            final_report.push_str(&error.report(src.as_str()));
            final_report.push('\n');
        }
        final_report
    }
}

/// Wraps an error with the filename and source of the module it originated in.
///
/// Spans are byte offsets into a *specific* file's source, so once we compile
/// across multiple files we can no longer rely on the single `(filename, src)`
/// pair passed into [`Report::report`]. A `ScopedReport` carries its own copies
/// and ignores the ones it is handed, so each error always renders against the
/// file it actually came from.
#[derive(Debug)]
pub struct ScopedReport {
    pub filename: String,
    pub source: String,
    pub inner: Box<dyn Report>,
}

impl ScopedReport {
    pub fn new(
        filename: impl Into<String>,
        source: impl Into<String>,
        inner: Box<dyn Report>,
    ) -> Self {
        Self {
            filename: filename.into(),
            source: source.into(),
            inner,
        }
    }
}

impl Report for ScopedReport {
    fn filename(&self) -> &str {
        &self.filename
    }

    fn report(&self, _src: &str) -> String {
        self.inner.report(&self.source)
    }
}

/// A free-form error message with no source span (e.g. a file could not be read).
#[derive(Debug)]
pub struct ErrorMessage(pub String);

impl Report for ErrorMessage {
    fn filename(&self) -> &str {
        "ErrorMessage"
    }

    fn report(&self, _src: &str) -> String {
        format!("error: {}\n", self.0)
    }
}

/// A `use` path that does not resolve to a file on disk.
#[derive(Debug)]
pub struct ErrorUnresolvedImport {
    pub span: Span,
    pub path: String,
    pub note: String,
    #[cfg(feature = "debug")]
    pub compiler_line: String,
}

impl ErrorUnresolvedImport {
    pub fn new(
        span: Span,
        path: impl Into<String>,
        note: impl Into<String>,
        #[cfg(feature = "debug")] compiler_line: impl Into<String>,
    ) -> Self {
        Self {
            span,
            path: path.into(),
            note: note.into(),
            #[cfg(feature = "debug")]
            compiler_line: compiler_line.into(),
        }
    }
}

impl Report for ErrorUnresolvedImport {
    fn filename(&self) -> &str {
        &self.span.filename
    }

    fn report(&self, src: &str) -> String {
        let mut report = ReportBuilder::new(&self.span, src);
        report.message(format!("unresolved import `{}`", self.path));
        #[allow(unused_mut)]
        let mut note = self.note.clone();
        #[cfg(feature = "debug")]
        {
            write!(&mut note, "\n{}", self.compiler_line)
                .expect("Failed to write debug info to ErrorUnresolvedImport");
        }
        report.note(note);
        report.lines_above(3);
        report.build()
    }
}

/// Importing (or accessing) a symbol that exists but is not `pub`.
#[derive(Debug)]
pub struct ErrorPrivateImport {
    pub span: Span,
    pub name: String,
    pub module: String,
    #[cfg(feature = "debug")]
    pub compiler_line: String,
}

impl ErrorPrivateImport {
    pub fn new(
        span: Span,
        name: impl Into<String>,
        module: impl Into<String>,
        #[cfg(feature = "debug")] compiler_line: impl Into<String>,
    ) -> Self {
        Self {
            span,
            name: name.into(),
            module: module.into(),
            #[cfg(feature = "debug")]
            compiler_line: compiler_line.into(),
        }
    }
}

impl Report for ErrorPrivateImport {
    fn filename(&self) -> &str {
        &self.span.filename
    }

    fn report(&self, src: &str) -> String {
        let mut report = ReportBuilder::new(&self.span, src);
        report.message(format!("`{}` is private", self.name));
        #[allow(unused_mut)]
        let mut note = format!(
            "`{}` is not declared `pub` in module `{}`",
            self.name, self.module
        );
        #[cfg(feature = "debug")]
        {
            write!(&mut note, "\n{}", self.compiler_line)
                .expect("Failed to write debug info to ErrorPrivateImport");
        }
        report.note(note);
        report.lines_above(3);
        report.build()
    }
}

/// A cycle in the module dependency graph (A uses B, B uses A).
#[derive(Debug)]
pub struct ErrorImportCycle {
    pub span: Span,
    pub chain: Vec<String>,
    #[cfg(feature = "debug")]
    pub compiler_line: String,
}

impl ErrorImportCycle {
    pub fn new(
        span: Span,
        chain: Vec<String>,
        #[cfg(feature = "debug")] compiler_line: impl Into<String>,
    ) -> Self {
        Self {
            span,
            chain,
            #[cfg(feature = "debug")]
            compiler_line: compiler_line.into(),
        }
    }
}

impl Report for ErrorImportCycle {
    fn filename(&self) -> &str {
        &self.span.filename
    }

    fn report(&self, src: &str) -> String {
        let mut report = ReportBuilder::new(&self.span, src);
        report.message("import cycle detected");
        #[allow(unused_mut)]
        let mut note = format!("cycle: {}", self.chain.join(" -> "));
        #[cfg(feature = "debug")]
        {
            write!(&mut note, "\n{}", self.compiler_line)
                .expect("Failed to write debug info to ErrorImportCycle");
        }
        report.note(note);
        report.lines_above(3);
        report.build()
    }
}
