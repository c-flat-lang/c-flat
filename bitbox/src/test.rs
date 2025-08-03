use super::ir::{FunctionBuilder, Module, ModuleBuilder, Type, Variable};
use std::fmt::Write;

pub fn snapshot_module(module: Module) -> String {
    let mut output = String::new();
    for func in module.functions.iter() {
        write!(&mut output, "{}", func).expect("failed to write()");
    }

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

snapshot!(test_basic_function_with_if_else, {
    let mut module = ModuleBuilder::default();
    let lhs = Variable::new("lhs", Type::Unsigned(32));
    let rhs = Variable::new("rhs", Type::Unsigned(32));
    let mut function = FunctionBuilder::new("max");
    let mut assembler = function.instructions();
    let branch_then_label = assembler.new_label(Some("branch_then_label"));
    let branch_else_label = assembler.new_label(Some("branch_else_label"));

    let condition = assembler.var(Type::Unsigned(32));
    assembler.cmp(condition.clone(), lhs.clone(), rhs.clone());
    assembler.jump_if(condition.clone(), branch_then_label.clone());
    assembler.jump(branch_else_label.clone());
    assembler.label(branch_then_label);
    assembler.ret(lhs.clone());
    assembler.label(branch_else_label);
    assembler.ret(rhs.clone());

    module.push_function(function.build());
    module.build()
});
