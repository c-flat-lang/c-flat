pub mod cli;
pub mod error;
pub mod stage;
pub use bitbox;
pub use bitbox::{Target, backend::CompilerResult, passes::DebugPass};
pub use cli::{Cli, DebugMode};
pub use stage::StageContext;
pub use stage::pipelines::drive;

#[cfg(feature = "wasm")]
use wasm_bindgen::prelude::*;

#[cfg(feature = "wasm")]
#[wasm_bindgen]
pub fn compile_source(
    source: &str,
    filename: &str,
) -> std::result::Result<js_sys::Uint8Array, JsValue> {
    let ctx = StageContext {
        source: source.to_string(),
        ..Default::default()
    };
    let target = ctx.target;

    let compiler_debug_mode: Option<DebugPass> = ctx.debug_mode.and_then(Into::into);

    let mut module = match drive(ctx) {
        Ok(module) => module,
        Err(err) => {
            let string = format!("{:?}", err);
            web_sys::console::log_1(&string.into());
            return Err(JsValue::from_str(&err.report(source)));
        }
    };

    let mut compiler = bitbox::Compiler::new(filename, target, compiler_debug_mode);

    match compiler.run(&mut module) {
        Ok(_) => {
            let CompilerResult::Wasm32(bytes) = compiler.results.unwrap_or_default() else {
                unimplemented!()
            };
            Ok(js_sys::Uint8Array::from(&bytes[..]))
        }
        Err(error) => {
            web_sys::console::log_1(&"ERROR:".into());
            Err(JsValue::from_str(&error.to_string()))
        }
    }
}
