use crate::DebugMode;
use crate::stage::parser::ast;
use crate::stage::{Stage, StageContext, StageOutput};
use crate::type_interner::TypeInterner;

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
        let _ = interner.register_struct_template(struct_def);
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
        StageOutput::Output(String::new())
    }

    fn run(&mut self, ctx: &mut StageContext) -> report::Result<()> {
        ctx.interner = TypeInterner::new(ctx.target);

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

#[cfg(all(test, not(feature = "wasm")))]
mod tests {
    use super::*;
    use crate::stage::module_loader::{FlattenModulesStage, LoadedModuleStage};
    use crate::stage::monomorphize::MonomorphizerStage;

    fn run_pipeline(entry: &str, mut pipeline: Vec<Box<dyn Stage>>) -> StageContext {
        let mut ctx = StageContext {
            entry: std::path::PathBuf::from(entry),
            ..Default::default()
        };

        for pass in pipeline.iter_mut() {
            let name = pass.name();
            pass.execute(&mut ctx)
                .unwrap_or_else(|err| panic!("stage `{name}` failed: {err:?}"));
        }

        ctx
    }

    fn define_types_for(entry: &str) -> StageContext {
        run_pipeline(
            entry,
            vec![
                Box::new(LoadedModuleStage),
                Box::new(FlattenModulesStage),
                Box::new(DefineTypeStage),
            ],
        )
    }

    fn define_types_after_monomorphization(entry: &str) -> StageContext {
        run_pipeline(
            entry,
            vec![
                Box::new(LoadedModuleStage),
                Box::new(FlattenModulesStage),
                Box::new(MonomorphizerStage),
                Box::new(DefineTypeStage),
            ],
        )
    }

    #[test]
    fn declares_nominal_types_and_leaves_them_unfilled() {
        let ctx = define_types_for("./testing/test/structs.cb");

        assert_eq!(
            ctx.interner.unfilled().count(),
            1,
            "expected `List` to be declared but not yet filled"
        );
    }

    #[test]
    fn file_without_type_definitions_declares_nothing() {
        let ctx = define_types_for("./examples/test.cb");

        assert_eq!(ctx.interner.unfilled().count(), 0);
    }

    #[test]
    fn sees_generics_as_templates_when_run_before_monomorphization() {
        let ctx = define_types_for("./testing/test/generics.cb");

        assert_eq!(
            ctx.interner.unfilled().count(),
            0,
            "a generic struct should register as a template, not a declaration"
        );
    }

    #[test]
    fn monomorphization_turns_templates_into_concrete_declarations() {
        let ctx = define_types_after_monomorphization("./testing/test/generics.cb");

        assert_eq!(
            ctx.interner.unfilled().count(),
            1,
            "expected the specialized `Pair__s32` to be declared and awaiting a fill"
        );
    }
}
