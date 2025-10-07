mod emitter;
pub mod ir;
pub mod ir_builder;
#[cfg(test)]
mod test;

pub use emitter::{Compiler, Target};
