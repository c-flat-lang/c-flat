#![allow(unused)]
use std::collections::HashMap;

use crate::backend::Lower;
use crate::ir::{self, Module, Type, Visibility};
use crate::passes::Pass;

#[derive(Debug)]
pub struct EmitX86_64LinuxPass;

impl Pass for EmitX86_64LinuxPass {
    fn run(
        &mut self,
        _module: &mut Module,
        _ctx: &mut crate::backend::Context,
    ) -> Result<(), crate::error::Error> {
        eprintln!("EmitX86_64LinuxPass");
        Ok(())
    }
}
