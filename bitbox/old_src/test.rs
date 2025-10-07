use crate::ir::{Type, Variable, Visibility};

#[test]
fn test_ir_builder() {
    use crate::ir_builder::FunctionBuilder;
    let x = Variable::new("x", Type::Unsigned(32));
    let y = Variable::new("y", Type::Unsigned(32));
    let mut fb = FunctionBuilder::new("max")
        .with_params(vec![x.clone(), y.clone()])
        .with_visibility(Visibility::Public)
        .with_return_type(Type::Unsigned(32));

    let mut assembler = fb.assembler();
    let des = assembler.var(Type::Unsigned(32));
    let condition = assembler.var(Type::Unsigned(8));
    assembler.create_block("entry");
    assembler.gt(condition.clone(), x.clone(), y.clone());
    assembler.jump_if(condition, "TRUE");
    assembler.create_block("TRUE");
    assembler.assign(des.clone(), x);
    assembler.jump("END");
    assembler.create_block("FALSE");
    assembler.assign(des.clone(), y);
    assembler.jump("END");
    assembler.create_block("END");
    assembler.ret(des);
    let function = fb.build();
    println!("{}", function);
    assert!(false);
}
// use super::{
//     ir::{Module, Type, Variable},
//     ir_builder::{FunctionBuilder, ModuleBuilder},
// };

// use std::fmt::Write;
//
// pub fn snapshot_module(module: Module) -> String {
//     let mut output = String::new();
//     for func in module.functions.iter() {
//         write!(&mut output, "{}", func).expect("failed to write()");
//     }
//
//     output
// }
//
// macro_rules! snapshot {
//     ($name:tt, $module:expr) => {
//         #[test]
//         fn $name() {
//             let contents = snapshot_module($module);
//             let mut settings = insta::Settings::clone_current();
//             settings.set_snapshot_path("testdata/output/");
//             settings.bind(|| {
//                 insta::assert_snapshot!(contents);
//             });
//         }
//     };
// }
//
// snapshot!(test_basic_function_with_return, {
//     let mut module = ModuleBuilder::default();
//     let x = Variable::new("x", Type::Unsigned(32));
//     let y = Variable::new("y", Type::Unsigned(32));
//     let mut function = FunctionBuilder::new("add")
//         .with_params(vec![x.clone(), y.clone()])
//         .with_return_type(Type::Unsigned(32));
//     let mut assembler = function.instructions();
//     let des = assembler.var(Type::Unsigned(32));
//     assembler.add(des.clone(), x, y);
//     assembler.ret(des);
//     module.push_function(function.build());
//     module.build()
// });
//
// snapshot!(test_basic_function_with_if_else, {
//     let mut module = ModuleBuilder::default();
//     let lhs = Variable::new("lhs", Type::Unsigned(32));
//     let rhs = Variable::new("rhs", Type::Unsigned(32));
//     let mut function = FunctionBuilder::new("max");
//     let mut assembler = function.instructions();
//     let branch_then_label = assembler.new_label(Some("branch_then_label"));
//     let branch_else_label = assembler.new_label(Some("branch_else_label"));
//
//     let condition = assembler.var(Type::Unsigned(32));
//     assembler.eq(condition.clone(), lhs.clone(), rhs.clone());
//     assembler.jump_if(condition.clone(), branch_then_label.clone());
//     assembler.jump(branch_else_label.clone());
//     assembler.label(branch_then_label);
//     assembler.ret(lhs.clone());
//     assembler.label(branch_else_label);
//     assembler.ret(rhs.clone());
//
//     module.push_function(function.build());
//     module.build()
// });

// #[cfg(test)]
// mod tests {
//     use super::*;
//     use crate::ir::{Function, Instruction, Operand, SymbolTable, Visibility};
//     use pretty_assertions::assert_eq;
//
//     #[test]
//     fn testing_function_builder() {
//         let x = Variable::new("x", Type::Unsigned(32));
//         let y = Variable::new("y", Type::Unsigned(32));
//         let mut function = FunctionBuilder::new("add")
//             .with_visibility(Visibility::Public)
//             .with_param(x.clone())
//             .with_param(y.clone())
//             .with_return_type(Type::Unsigned(32));
//         let mut assembler = function.instructions();
//         let ty = assembler.var(Type::Unsigned(32));
//         assembler.add(ty, x.clone(), y.clone());
//         let function = function.build();
//         assert_eq!(
//             function,
//             Function {
//                 visibility: Visibility::Public,
//                 name: "add".to_string(),
//                 params: vec![x.clone(), y.clone()],
//                 return_type: Type::Unsigned(32),
//                 blocks: vec![Instruction::Add(
//                     Variable::new("tmp0", Type::Unsigned(32)),
//                     Operand::Variable(x),
//                     Operand::Variable(y)
//                 )
//                 .into()],
//                 locals: vec![Variable::new("tmp0", Type::Unsigned(32))],
//                 symbols: SymbolTable::default(),
//             }
//         );
//     }
//
//     #[test]
//     fn testing_instruction_builder() {
//         let x = Variable::new("x", Type::Unsigned(32));
//         let y = Variable::new("y", Type::Unsigned(32));
//         let mut fb = FunctionBuilder::new("add")
//             .with_visibility(Visibility::Public)
//             .with_param(x.clone())
//             .with_param(y.clone())
//             .with_return_type(Type::Unsigned(32));
//         let mut assember = fb.instructions();
//         let des = assember.var(Type::Unsigned(32));
//         assember.add(des.clone(), x.clone(), y.clone());
//
//         let function = fb.build();
//         assert_eq!(
//             function,
//             Function {
//                 visibility: Visibility::Public,
//                 name: "add".to_string(),
//                 params: vec![x.clone(), y.clone()],
//                 return_type: Type::Unsigned(32),
//                 locals: vec![Variable::new("tmp0", Type::Unsigned(32))],
//                 blocks: vec![Instruction::Add(
//                     Variable::new("tmp0", Type::Unsigned(32)),
//                     Operand::Variable(x),
//                     Operand::Variable(y)
//                 )
//                 .into()],
//                 symbols: SymbolTable::default(),
//             }
//         );
//     }
//
//     #[test]
//     fn testing_if_else() {
//         let x = Variable::new("x", Type::Unsigned(32));
//         let y = Variable::new("y", Type::Unsigned(32));
//         let mut fb = FunctionBuilder::new("main").with_visibility(Visibility::Public);
//         let mut assember = fb.instructions();
//         assember.assign(
//             x.clone(),
//             Operand::ConstantInt {
//                 value: "123".into(),
//                 ty: Type::Unsigned(32),
//             },
//         );
//         assember.assign(
//             y.clone(),
//             Operand::ConstantInt {
//                 value: "321".into(),
//                 ty: Type::Unsigned(32),
//             },
//         );
//         let condition = assember.var(Type::Unsigned(32));
//         let condition_label = assember.new_label(Some("condition"));
//         let true_label = assember.new_label(Some("true_branch"));
//         let false_label = assember.new_label(Some("false_branch"));
//
//         assember.label(condition_label);
//         assember.eq(condition.clone(), x.clone(), y.clone());
//         assember.jump_if(condition.clone(), true_label.clone());
//         assember.label(false_label);
//         assember.ret(y.clone());
//         assember.label(true_label);
//         assember.ret(x.clone());
//     }
// }
