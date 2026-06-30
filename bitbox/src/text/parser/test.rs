use crate::text::lexer::lex;
use crate::text::parser;

pub fn snapshot_parsing(path: &str, input: &str) -> String {
    let tokens = lex(path, input);
    let program = parser::Parser::new(path, tokens).parse();
    format!("{:#?}", program)
}

macro_rules! snapshot {
    ($name:tt, $path:tt) => {
        #[test]
        fn $name() {
            let contents = include_str!($path);
            let contents = contents.replace("\r\n", "\n");
            let mut settings = insta::Settings::clone_current();
            settings.set_snapshot_path("testdata/output/");
            settings.bind(|| {
                insta::assert_snapshot!(snapshot_parsing($path, &contents));
            });
        }
    };
}

snapshot!(binary, "../../../examples/array.bitbox");
snapshot!(import_function, "../../../examples/import_function.bitbox");
snapshot!(looping, "../../../examples/looping.bitbox");
