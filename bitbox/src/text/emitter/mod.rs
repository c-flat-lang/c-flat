use std::collections::HashMap;

use crate::ir::{self, Operand, Variable};
use crate::text::lexer::token::{Token, TokenKind};
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
    functions: HashMap<String, ast::Function>,
}

impl Emitter {
    pub fn new(ast_module: ast::Module) -> Self {
        let mut functions = HashMap::new();
        for f in ast_module.functions.iter() {
            functions.insert(f.name.lexeme.clone(), f.clone());
        }
        Self {
            ir_module: ir::builder::ModuleBuilder::default(),
            ast_module,
            functions,
        }
    }
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
            super::lexer::token::Instruction::Add => {
                let [ty, des, lhs, rhs] = instruction.arguments.as_slice() else {
                    // NOTE: If this happens then the parser is broken.
                    panic!("Invalid instruction arguments for @add");
                };
                let ty = ir::Type::from(ty.lexeme.as_str());
                let lhs = Variable::new(lhs.lexeme.clone(), ty.clone());
                let rhs = Variable::new(rhs.lexeme.clone(), ty.clone());
                let des = Variable::new(des.lexeme.clone(), ty.clone());
                _assembler.add(des, lhs, rhs);
            }
            super::lexer::token::Instruction::Alloc => {
                let [ty, des, size] = instruction.arguments.as_slice() else {
                    // NOTE: If this happens then the parser is broken.
                    panic!("Invalid instruction arguments for @alloc");
                };
                let ty = ir::Type::from(ty.lexeme.as_str());
                let des = Variable::new(des.lexeme.clone(), ty.clone());
                // HACK: We need a better way to get the types of tokens
                _assembler.alloc(ty, des, token_to_operand(size, &ir::Type::Unsigned(32)));
            }
            super::lexer::token::Instruction::Call => {
                let [ty, des, name] = &instruction.arguments[0..3] else {
                    // NOTE: If this happens then the parser is broken.
                    panic!("Invalid instruction arguments for @call");
                };
                let token_args = &instruction.arguments[3..];
                let args = token_args
                    .iter()
                    .enumerate()
                    .map(|(idx, token)| {
                        let Some(function) = self.functions.get(&name.lexeme) else {
                            // NOTE: If this happens then the parser is broken.
                            panic!("No function named {} for @call", name.lexeme);
                        };
                        let Some(ty) = function.get_param_type(idx) else {
                            panic!("Invalid index {} for @call", name.lexeme);
                        };
                        let ty = ir::Type::from(ty.lexeme.as_str());
                        token_to_operand(token, &ty)
                    })
                    .collect::<Vec<_>>();
                println!(
                    "@call {} : {} {}({:?})",
                    ty.lexeme, des.lexeme, name.lexeme, args
                );
                let ty = ir::Type::from(ty.lexeme.as_str());
                let des = Variable::new(des.lexeme.clone(), ty.clone());
                _assembler.call(Some(des), name.lexeme.clone(), &args);
            }
            super::lexer::token::Instruction::Cmp => todo!("Cmp"),
            super::lexer::token::Instruction::ElemGet => {
                let [ty, des, addr, index] = &instruction.arguments.as_slice() else {
                    // NOTE: If this happens then the parser is broken.
                    panic!("Invalid instruction arguments for @elemget");
                };
                let ty = ir::Type::from(ty.lexeme.as_str());
                let des = Variable::new(des.lexeme.clone(), ty.clone());
                // HACK: We need a better way to get the types of tokens
                let addr = token_to_operand(addr, &ir::Type::Unsigned(32));
                // HACK: We need a better way to get the types of tokens
                let index = token_to_operand(index, &ir::Type::Unsigned(32));
                _assembler.elemget(des, addr, index);
            }
            super::lexer::token::Instruction::ElemSet => {
                let [ty, addr, index, value] = &instruction.arguments.as_slice() else {
                    // NOTE: If this happens then the parser is broken.
                    panic!("Invalid instruction arguments for @elemset");
                };
                let ty = ir::Type::from(ty.lexeme.as_str());
                let addr = Variable::new(addr.lexeme.clone(), ty.clone());
                let index = token_to_operand(index, &ir::Type::Unsigned(32));
                let value = token_to_operand(value, &ty);
                _assembler.elemset(addr, index, value);
            }
            super::lexer::token::Instruction::Jump => todo!("Jump"),
            super::lexer::token::Instruction::JumpIf => todo!("JumpIf"),
            super::lexer::token::Instruction::Load => todo!("Load"),
            super::lexer::token::Instruction::Mul => todo!("Mul"),
            super::lexer::token::Instruction::Phi => todo!("Phi"),
            super::lexer::token::Instruction::Ret => {
                let args = instruction.arguments.as_slice();

                if args.len() == 1 {
                    _assembler.void_ret();
                } else {
                    let [ty, des] = args else {
                        // NOTE: If this happens then the parser is broken.
                        panic!("Invalid instruction arguments for @ret");
                    };

                    let ty = ir::Type::from(ty.lexeme.as_str());
                    let des = token_to_operand(des, &ty);
                    _assembler.ret(ty, des);
                }
            }
            super::lexer::token::Instruction::Sub => todo!("Sub"),
        }
    }
}

fn token_to_operand(token: &Token, ty: &ir::Type) -> Operand {
    match token.kind {
        TokenKind::Identifier => Operand::from(Variable::new(token.lexeme.clone(), ty.clone())),
        TokenKind::Number => Operand::from(Operand::ConstantInt {
            value: token.lexeme.clone(),
            ty: ty.clone(),
        }),
        _ => unreachable!(),
    }
}
