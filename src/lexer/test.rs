use crate::lexer;
use std::fmt::Write;

pub fn snapshot_lexing(input: &str) -> String {
    let tokens = lexer::tokenize(input);
    let mut tokens = std::collections::VecDeque::from(tokens);
    let mut output = String::new();
    let mut total = 0;
    for line in input.lines() {
        output += line;
        output += "\n";
        while let Some(tok) = tokens.pop_front() {
            if total + line.len() <= tok.span.start {
                tokens.push_front(tok);
                break;
            }

            output += &" ".repeat(tok.span.start.saturating_sub(total));
            output += &"^".repeat(tok.span.len());
            write!(&mut output, " {tok:?}").expect("failed to write()");
            output += "\n"
        }

        total += line.len() + 1;
    }

    output
}

macro_rules! snapshot {
    ($name:tt, $path:tt) => {
        #[test]
        fn $name() {
            let contents = include_str!($path);
            let mut settings = insta::Settings::clone_current();
            settings.set_snapshot_path("testdata/output/");
            settings.bind(|| {
                insta::assert_snapshot!(snapshot_lexing(contents));
            });
        }
    };
}

snapshot!(hello_world, "../../examples/hello_world.cb");
snapshot!(char, "../../examples/char.cb");
