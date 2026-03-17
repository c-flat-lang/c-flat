pub mod passes;
use crate::backend::Backend;
use crate::backend::x86_64::linux::passes::emit::assembler::Instruction;
use crate::passes::Pass;
use crate::passes::control_flow_graph::ControlFlowGraphPass;
use crate::passes::liveness::LivenessAnalysisPass;
use crate::passes::local_function_variables::LocalFunctionVariablesPass;
use crate::passes::lowering::LoweringPass;
use crate::passes::phi_node_elimination::PhiNodeEliminationPass;
use passes::emit::EmitX86_64LinuxPass;
use passes::virt_reg_rewrite::VirtRegRewritePass;

#[derive(Debug, Default)]
pub struct Function {
    pub name: String,
    pub prolog: Vec<Instruction>,
    pub instructions: Vec<Instruction>,
    pub epilog: Vec<Instruction>,
}

impl std::fmt::Display for Function {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for i in &self.prolog {
            writeln!(f, "{i}")?;
        }
        for i in &self.instructions {
            writeln!(f, "{i}")?;
        }
        for i in &self.epilog {
            writeln!(f, "{i}")?;
        }
        Ok(())
    }
}

#[derive(Debug, Default)]
pub struct Module {
    pub directives: Vec<String>,
    pub functions: Vec<Function>,
}

impl Module {
    pub fn push_directive(&mut self, directive: impl Into<String>) {
        self.directives.push(directive.into());
    }

    pub fn push_function(&mut self, function: Function) {
        self.functions.push(function);
    }
}

impl std::fmt::Display for Module {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for directive in &self.directives {
            write!(f, "{directive}\n")?;
        }
        for func in &self.functions {
            write!(f, "{func}")?;
        }
        Ok(())
    }
}

pub struct X86_64LinuxBackend;

impl Backend for X86_64LinuxBackend {
    fn passes(&self) -> Vec<Box<dyn Pass>> {
        vec![
            Box::new(LoweringPass),
            Box::new(LocalFunctionVariablesPass),
            Box::new(ControlFlowGraphPass),
            Box::new(LivenessAnalysisPass),
            Box::new(PhiNodeEliminationPass),
            Box::new(EmitX86_64LinuxPass),
            Box::new(VirtRegRewritePass),
        ]
    }
}
