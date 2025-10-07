use crate::parser;

pub fn snapshot_parsing(input: &str) -> String {
    format!("{:#?}", parser::Parser::new(input).parse().unwrap())
}

macro_rules! snapshot {
    ($name:tt, $path:tt) => {
        #[test]
        fn $name() {
            let contents = include_str!($path);
            let mut settings = insta::Settings::clone_current();
            settings.set_snapshot_path("testdata/output/");
            settings.bind(|| {
                insta::assert_snapshot!(snapshot_parsing(contents));
            });
        }
    };
}

snapshot!(hello_world, "../../examples/hello_world.cb");
snapshot!(char, "../../examples/char.cb");
snapshot!(if_else, "../../examples/if-else.cb");
snapshot!(structure, "../../examples/struct.cb");
