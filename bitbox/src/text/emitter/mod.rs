use crate::ir;
use crate::text::lexer::token::TokenKind;
use crate::text::parser::ast;

impl From<&str> for ir::Type {
    fn from(s: &str) -> Self {
        if s.starts_with("u") {
            ir::Type::Unsigned(s[1..].parse().unwrap())
        } else if s.starts_with("s") {
            ir::Type::Signed(s[1..].parse().unwrap())
        } else if s.starts_with("f") {
            ir::Type::Float(s[1..].parse().unwrap())
        } else {
            unreachable!()
        }
    }
}

#[derive(Debug, Default)]
pub struct Emitter {
    ir_module: ir::builder::ModuleBuilder,
    ast_module: ast::Module,
}

impl Emitter {
    pub fn emit(mut self) -> ir::Module {
        self.emit_functions();
        self.ir_module.build()
    }

    fn emit_functions(&mut self) {
        for func in self.ast_module.functions.iter() {
            let params = func
                .params
                .iter()
                .map(|(name, tok_ty)| {
                    debug_assert_eq!(tok_ty.kind, TokenKind::Identifier);
                    let ty = ir::Type::from(tok_ty.lexeme.as_str());
                    ir::Variable::new(name.lexeme.clone(), ty.clone())
                })
                .collect();
            let return_ty = ir::Type::from(func.return_type.lexeme.as_str());
            let mut function = ir::builder::FunctionBuilder::new(&func.name.lexeme)
                .with_visibility(func.visibility)
                .with_params(params)
                .with_return_type(return_ty);

            let mut assembler = function.assembler();
            self.emit_block(&mut assembler, &func.block);

            self.ir_module.function(function.build());
        }
    }

    fn emit_block(
        &self,
        assembler: &mut ir::builder::AssemblerBuilder,
        blocks: &[ast::BasicBlock],
    ) {
        for block in blocks.iter() {
            assembler.create_block(&block.label.lexeme);
            for instruction in block.instructions.iter() {
                self.emit_instruction(assembler, instruction);
            }
        }
    }

    fn emit_instruction(
        &self,
        _assembler: &mut ir::builder::AssemblerBuilder,
        instruction: &ast::Instruction,
    ) {
        match instruction.instruction_kind {
            // ast::Instruction:: => {
            //     todo!();
            //     // let [ty, src] = instruction.arguments.as_slice() else {
            //     //     // NOTE: If we hit this panic there is a problem in the parser
            //     //     panic!("expected 2 arguments for assign");
            //     // };
            //     //
            //     // let ty = ir::Type::from(ty.lexeme.as_str());
            //     // let src = ir::Variable::from(src.lexeme.as_str());
            //     // assembler.assign(ty, src);
            // }
            _ => unimplemented!(),
        }
    }
}
