use crate::error::Result;
use crate::stage::{Stage, StageContext, StageOutput};
use crate::stage::{
    ir_builder::IRBuilderStage,
    module_loader::{FlattenModulesStage, LoadedModuleStage},
    monomorphize::MonomorphizerStage,
    semantic_analyzer::{SymbolTableBuilderStage, TypeCheckerStage},
};
use bitbox::ir::Module;

pub fn common_tail() -> Vec<Box<dyn Stage>> {
    vec![
        Box::new(MonomorphizerStage),
        Box::new(SymbolTableBuilderStage),
        Box::new(TypeCheckerStage),
        Box::new(IRBuilderStage),
    ]
}

#[cfg(not(feature = "wasm"))]
pub fn native_pipeline() -> Vec<Box<dyn Stage>> {
    let mut stages = vec![
        Box::new(LoadedModuleStage) as _,
        Box::new(FlattenModulesStage) as _,
    ];
    stages.extend(common_tail());
    stages
}

#[cfg(feature = "wasm")]
pub fn wasm_pipeline() -> Vec<Box<dyn Stage>> {
    use crate::stage::parser::ParserStage;
    let mut stages = vec![Box::new(ParserStage) as _];
    stages.extend(common_tail());
    stages
}

fn emit_dump_and_exit(msg: impl Into<String>) {
    #[cfg(not(feature = "wasm"))]
    eprintln!("{}", msg.into());

    #[cfg(feature = "wasm")]
    use wasm_bindgen::prelude::*;
    #[cfg(feature = "wasm")]
    web_sys::console::log_1(&JsValue::from_str(&msg.into()));
}

pub fn drive(mut ctx: StageContext) -> Result<Module> {
    #[cfg(not(feature = "wasm"))]
    let pipeline = native_pipeline();
    #[cfg(feature = "wasm")]
    let pipeline = wasm_pipeline();

    for mut pass in pipeline {
        if let StageOutput::Output(s) = pass.execute(&mut ctx)? {
            emit_dump_and_exit(s);
        }
    }

    Ok(ctx.module)
}
