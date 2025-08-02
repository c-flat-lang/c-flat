use super::ir::{FunctionBuilder, Module, ModuleBuilder, Type, Variable};
use std::fmt::Write;

pub fn snapshot_module(module: Module) -> String {
    let mut output = String::new();
    for func in module.functions.iter() {
        write!(&mut output, "{}", func).expect("failed to write()");
    }
    // let tokens = lex(input);
    // let mut tokens = std::collections::VecDeque::from(tokens);
    // let mut total = 0;
    // for line in input.lines() {
    //     output += line;
    //     output += "\n";
    //     while let Some(tok) = tokens.pop_front() {
    //         if total + line.len() <= tok.span.start {
    //             tokens.push_front(tok);
    //             break;
    //         }
    //
    //         output += &" ".repeat(tok.span.start.saturating_sub(total));
    //         output += &"^".repeat(tok.span.len());
    //         write!(&mut output, " {tok:?}").expect("failed to write()");
    //         output += "\n"
    //     }
    //
    //     total += line.len() + 1;
    // }

    output
}

macro_rules! snapshot {
    ($name:tt, $module:expr) => {
        #[test]
        fn $name() {
            let contents = snapshot_module($module);
            let mut settings = insta::Settings::clone_current();
            settings.set_snapshot_path("testdata/output/");
            settings.bind(|| {
                insta::assert_snapshot!(contents);
            });
        }
    };
}
snapshot!(test_basic_function_with_return, {
    let mut module = ModuleBuilder::default();
    let x = Variable::new("x", Type::Unsigned(32));
    let y = Variable::new("y", Type::Unsigned(32));
    let mut function = FunctionBuilder::new("add")
        .with_params(vec![x.clone(), y.clone()])
        .with_return_type(Type::Unsigned(32));
    let mut assembler = function.instructions();
    let des = assembler.var(Type::Unsigned(32));
    assembler.add(des.clone(), x, y);
    assembler.ret(des);
    module.push_function(function.build());
    module.build()
});

//
// snapshot!(binary, "../../snapshots/basic.bitbox");
// snapshot!(import_function, "../../snapshots/import_function.bitbox");
