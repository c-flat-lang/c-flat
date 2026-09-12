use crate::DebugMode;
use crate::stage::parser::ast;
use crate::stage::{Stage, StageContext, StageOutput};
use crate::type_interner::TypeInterner;
use std::fmt::Write;

#[derive(Debug)]
pub struct DefineTypeStage;

impl DefineTypeStage {
    fn define_struct(
        &self,
        interner: &mut TypeInterner,
        struct_def: &ast::Struct,
    ) -> report::Result<()> {
        match interner.declare_struct(&struct_def.name.lexeme, struct_def.name.span.clone()) {
            Ok(_id) => Ok(()),
            Err(_existing_id) => {
                panic!("duplicate struct declaration");
                // let existing_span = interner
                //     .decl_span(existing_id)
                //     .expect("declared id must have a span")
                //     .clone();
                // ctx.scope_error(ErrorDuplicateType {
                //     name: struct_def.name.lexeme.clone(),
                //     new_span: struct_def.name.span.clone(),
                //     existing_span,
                // })
            }
        }
    }

    fn define_struct_template(&self, interner: &mut TypeInterner, struct_def: &ast::Struct) {
        // register_struct_template doesn't fail on its own the way
        // declare_struct does (no TypeId reserved in `defs`), so no
        // Result to handle here — duplicate template names would need
        // their own check if that's a real concern.
        interner.register_struct_template(struct_def);
    }

    fn define_enum(&self, interner: &mut TypeInterner, enum_def: &ast::Enum) -> report::Result<()> {
        match interner.declare_enum(&enum_def.name.lexeme, enum_def.name.span.clone()) {
            Ok(_id) => Ok(()),
            Err(_existing_id) => {
                panic!("duplicate enum declaration");
                // let existing_span = ctx
                //     .interner
                //     .decl_span(existing_id)
                //     .expect("declared id must have a span")
                //     .clone();
                // ctx.scope_error(ErrorDuplicateType {
                //     name: enum_def.name.lexeme.clone(),
                //     new_span: enum_def.name.span.clone(),
                //     existing_span,
                // })
            }
        }
    }
}

impl Stage for DefineTypeStage {
    fn name(&self) -> &'static str {
        "Defining Type"
    }

    fn debug_mode(&self) -> &'static [DebugMode] {
        &[DebugMode::TypeCollection]
    }

    fn debug(&self, _ctx: &mut StageContext) -> StageOutput {
        let mut output = String::new();

        writeln!(&mut output, "").expect("DefiningTypeStage failed to write debug output");

        StageOutput::Output(output)
    }

    fn run(&mut self, ctx: &mut StageContext) -> report::Result<()> {
        for item in ctx.items.iter() {
            match item {
                ast::Item::Function(_) => {}
                ast::Item::Type(type_def) => match type_def {
                    ast::TypeDef::Struct(struct_def) if struct_def.type_params.is_none() => {
                        self.define_struct(&mut ctx.interner, struct_def)?;
                    }
                    ast::TypeDef::Struct(struct_def) => {
                        self.define_struct_template(&mut ctx.interner, struct_def);
                    }
                    ast::TypeDef::Enum(enum_def) if enum_def.type_params.is_some() => {
                        // doc: "generic enums are not supported yet"
                        // ctx.scope_error(/* ErrorUnsupportedGenericEnum { span: enum_def.name.span.clone() } */)?;
                        panic!("ErrorUnsupportedGenericEnum")
                    }
                    ast::TypeDef::Enum(enum_def) => {
                        self.define_enum(&mut ctx.interner, enum_def)?;
                    }
                },
                ast::Item::Use(_) => {}
                ast::Item::ExternFunction(_) => {}
            }
        }
        Ok(())
    }
}

#[test]
fn define_type_stage_test() {
    use crate::error::Result;
    use crate::stage::module_loader::{FlattenModulesStage, LoadedModuleStage};
    use bitbox::ir::Module;

    pub fn drive(ctx: &mut StageContext) -> Result<Module> {
        let pipeline: Vec<Box<dyn Stage>> = vec![
            Box::new(LoadedModuleStage) as _,
            Box::new(FlattenModulesStage) as _,
            Box::new(DefineTypeStage) as _,
            // Box::new(MonomorphizerStage) as _,
            // Box::new(SymbolTableBuilderStage) as _,
            // Box::new(TypeCheckerStage) as _,
            // Box::new(IRBuilderStage) as _,
        ];

        for mut pass in pipeline {
            if let StageOutput::Output(s) = pass.execute(ctx)? {
                eprintln!("{:#?}", ctx);
                eprintln!("{}", s);
            }
        }

        Ok(ctx.module.clone())
    }

    let mut ctx = StageContext {
        debug_mode: None, // Some(DebugMode::TypeCollection),
        entry: std::path::PathBuf::from("./examples/test.cb"),
        ..Default::default()
    };

    drive(&mut ctx).expect("Drive failed in type_coolection_stage test");

    eprintln!("{:#?}", ctx.interner);

    assert!(false);
}
