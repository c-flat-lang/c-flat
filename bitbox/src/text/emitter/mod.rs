// TODO: create a global variable hashmap.
// TODO: add local variable table in context.
use std::collections::HashMap;

use crate::text::semantic_analyzer::SymbolTable;

use crate::ir::{self, Operand, Type, Variable, Visibility};
use crate::text::lexer::token::{Token, TokenKind};
use crate::text::parser::ast;

struct EmitterContext<'ctx> {
    // function_name: String,
    assembler: ir::builder::AssemblerBuilder<'ctx>,
    variables: HashMap<String, usize>,
}

impl EmitterContext<'_> {
    fn alloc_variable(&mut self, ty: &Type, token: &Token) -> Variable {
        let name = token.lexeme.clone();
        let version = *self.variables.entry(name.clone()).or_insert(0);
        self.variables.insert(name.clone(), version + 1);
        Variable {
            name,
            ty: ty.clone(),
            version,
        }
    }

    // fn get_variable(&self, lexeme: &str, ty: &Type) -> Variable {
    //     let Some(version) = self.variables.get(lexeme).copied() else {
    //         panic!("No variable named {}", lexeme);
    //     };
    //     Variable {
    //         name: lexeme.to_string(),
    //         ty: ty.clone(),
    //         version: version - 1,
    //     }
    // }
}

#[derive(Debug, Default)]
pub struct Emitter {
    ir_module: ir::builder::ModuleBuilder,
    ast_module: ast::Module,
    functions: HashMap<String, ast::Function>,
    symbol_table: SymbolTable,
}

impl Emitter {
    pub fn new(symbol_table: SymbolTable, ast_module: ast::Module) -> Self {
        let mut functions = HashMap::new();
        for f in ast_module.functions.iter() {
            functions.insert(f.name.lexeme.clone(), f.clone());
        }
        Self {
            ir_module: ir::builder::ModuleBuilder::default(),
            ast_module,
            functions,
            symbol_table,
        }
    }

    pub fn emit(mut self) -> ir::Module {
        self.emit_imports();
        self.emit_functions();
        self.ir_module.build()
    }

    fn emit_imports(&mut self) {
        for import in self.ast_module.imports.iter() {
            match import {
                ast::Import::Function(func) => {
                    let function = ast::Function {
                        name: func.name.clone(),
                        visibility: Visibility::Public,
                        params: func
                            .params
                            .iter()
                            .map(|ty| {
                                (
                                    Token {
                                        kind: TokenKind::Identifier,
                                        lexeme: format!("var_{}", ty.lexeme),
                                        span: ty.span.clone(),
                                    },
                                    ty.clone(),
                                )
                            })
                            .collect(),
                        return_type: func.return_type.clone(),
                        block: vec![],
                    };
                    self.functions.insert(func.name.lexeme.clone(), function);
                }
            }
        }
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

            let mut target = EmitterContext {
                // function_name: func.name.lexeme.clone(),
                assembler: function.assembler(),
                variables: HashMap::new(),
            };

            self.symbol_table.enter_scope(&func.name.lexeme);

            self.emit_block(&func.block, &mut target);

            self.symbol_table.exit_scope();

            self.ir_module.function(function.build());
        }
    }

    fn emit_block(&self, blocks: &[ast::BasicBlock], target: &mut EmitterContext) {
        for block in blocks.iter() {
            target.assembler.create_block(&block.label.lexeme);
            for instruction in block.instructions.iter() {
                self.emit_instruction(instruction, target);
            }
        }
    }

    fn emit_instruction(&self, instruction: &ast::Instruction, target: &mut EmitterContext) {
        match instruction.instruction_kind {
            super::lexer::token::Instruction::Add => {
                let [ty, des, lhs, rhs] = instruction.arguments.as_slice() else {
                    // NOTE: If this happens then the parser is broken.
                    panic!("Invalid instruction arguments for @add");
                };
                let ty = ir::Type::from(ty.lexeme.as_str());
                let lhs = self.token_to_operand(lhs, &ty);
                let rhs = self.token_to_operand(rhs, &ty);
                let des = target.alloc_variable(&ty, des);
                target.assembler.add(des, lhs, rhs);
            }
            super::lexer::token::Instruction::Alloc => {
                let [ty, des, size] = instruction.arguments.as_slice() else {
                    // NOTE: If this happens then the parser is broken.
                    panic!("Invalid instruction arguments for @alloc");
                };
                let ty = ir::Type::from(ty.lexeme.as_str());
                let des = target.alloc_variable(&ty, des);
                // HACK: We need a better way to get the types of tokens
                let src = self.token_to_operand(size, &ir::Type::Unsigned(32));
                target.assembler.alloc(ty, des, src);
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
                        self.token_to_operand(token, &ty)
                    })
                    .collect::<Vec<_>>();
                let ty = ir::Type::from(ty.lexeme.as_str());
                let des = target.alloc_variable(&ty, des);
                target.assembler.call(Some(des), name.lexeme.clone(), &args);
            }
            super::lexer::token::Instruction::Cmp => {
                let [ty, des, lhs, rhs] = &instruction.arguments.as_slice() else {
                    // NOTE: If this happens then the parser is broken.
                    panic!("Invalid instruction arguments for @cmp");
                };

                let ty = ir::Type::from(ty.lexeme.as_str());
                let des = target.alloc_variable(&ty, des);
                let lhs = self.token_to_operand(lhs, &ty);
                let rhs = self.token_to_operand(rhs, &ty);
                target.assembler.eq(des, lhs, rhs);
            }
            super::lexer::token::Instruction::ElemGet => {
                let [ty, des, addr, index] = &instruction.arguments.as_slice() else {
                    // NOTE: If this happens then the parser is broken.
                    panic!("Invalid instruction arguments for @elemget");
                };
                let ty = ir::Type::from(ty.lexeme.as_str());
                let des = target.alloc_variable(&ty, des);
                let addr = self.token_to_operand(addr, &ty);
                let index = self.token_to_operand(index, &ty);
                target.assembler.elemget(des, addr, index);
            }
            super::lexer::token::Instruction::ElemSet => {
                let [ty, addr, index, value] = &instruction.arguments.as_slice() else {
                    // NOTE: If this happens then the parser is broken.
                    panic!("Invalid instruction arguments for @elemset");
                };
                let ty = ir::Type::from(ty.lexeme.as_str());
                let addr = Variable::new(addr.lexeme.clone(), ty.clone());
                let index = self.token_to_operand(index, &ir::Type::Unsigned(32));
                let value = self.token_to_operand(value, &ty);
                target.assembler.elemset(addr, index, value);
            }
            super::lexer::token::Instruction::Jump => {
                let [label] = &instruction.arguments.as_slice() else {
                    // NOTE: If this happens then the parser is broken.
                    panic!("Invalid instruction arguments for @jump");
                };
                target.assembler.jump(label.lexeme.clone());
            }
            super::lexer::token::Instruction::JumpIf => {
                let [cond, label] = &instruction.arguments.as_slice() else {
                    // NOTE: If this happens then the parser is broken.
                    panic!("Invalid instruction arguments for @jumpif");
                };
                let cond = Variable::new(cond.lexeme.clone(), ir::Type::Unsigned(1));
                target.assembler.jump_if(cond, label.lexeme.clone());
            }
            super::lexer::token::Instruction::Load => {
                let [ty, des, value] = &instruction.arguments.as_slice() else {
                    // NOTE: If this happens then the parser is broken.
                    panic!("Invalid instruction arguments for @load");
                };

                let ty = ir::Type::from(ty.lexeme.as_str());
                let des = target.alloc_variable(&ty, des);
                let value = self.token_to_operand(value, &ty);
                target.assembler.load(des, value);
            }
            super::lexer::token::Instruction::Mul => {
                let [ty, des, lhs, rhs] = instruction.arguments.as_slice() else {
                    // NOTE: If this happens then the parser is broken.
                    panic!("Invalid instruction arguments for @mul");
                };
                let ty = ir::Type::from(ty.lexeme.as_str());
                let lhs = self.token_to_operand(lhs, &ty);
                let rhs = self.token_to_operand(rhs, &ty);
                let des = target.alloc_variable(&ty, des);
                target.assembler.mul(des, lhs, rhs);
            }
            super::lexer::token::Instruction::Phi => {
                let [ty, des] = &instruction.arguments[0..2] else {
                    // NOTE: If this happens then the parser is broken.
                    panic!("Invalid instruction arguments for @mul");
                };
                let ty = ir::Type::from(ty.lexeme.as_str());
                let des = target.alloc_variable(&ty, des);
                if !instruction.arguments.len().is_multiple_of(2) {
                    // NOTE: If this happens then the parser is broken.
                    panic!("Invalid instruction arguments for @phi");
                }

                let mut values = Vec::new();
                for token in instruction.arguments[2..].chunks(2) {
                    let [label, value] = token else {
                        // NOTE: If this happens then the parser is broken.
                        panic!("Invalid instruction arguments for @phi");
                    };
                    let label = label.lexeme.clone();
                    let Operand::Variable(value) = self.token_to_operand(value, &ty) else {
                        // NOTE: If this happens then the parser is broken.
                        panic!("Invalid instruction arguments for @phi");
                    };
                    values.push((label, value));
                }
                target.assembler.phi(des, values);
            }
            super::lexer::token::Instruction::Ret => {
                let args = instruction.arguments.as_slice();

                if args.len() == 1 {
                    target.assembler.void_ret();
                } else {
                    let [ty, des] = args else {
                        // NOTE: If this happens then the parser is broken.
                        panic!("Invalid instruction arguments for @ret");
                    };

                    let ty = ir::Type::from(ty.lexeme.as_str());
                    let des = self.token_to_operand(des, &ty);
                    target.assembler.ret(ty, des);
                }
            }
            super::lexer::token::Instruction::Sub => {
                let [ty, des, lhs, rhs] = instruction.arguments.as_slice() else {
                    // NOTE: If this happens then the parser is broken.
                    panic!("Invalid instruction arguments for @sub");
                };
                let ty = ir::Type::from(ty.lexeme.as_str());
                let lhs = self.token_to_operand(lhs, &ty);
                let rhs = self.token_to_operand(rhs, &ty);
                let des = target.alloc_variable(&ty, des);
                target.assembler.sub(des, lhs, rhs);
            }
        }
    }

    fn token_to_operand(&self, token: &Token, ty: &ir::Type) -> Operand {
        match token.kind {
            TokenKind::Identifier => {
                let Some(symbol) = self.symbol_table.get(&token.lexeme) else {
                    // NOTE: If this happens then the parser is broken.
                    panic!("Unknown variable: {}", token.lexeme);
                };

                if &symbol.ty != ty {
                    // NOTE: If this happens then the parser is broken.
                    panic!("Type mismatch for variable: {}", token.lexeme);
                }

                let var = Variable::new(symbol.name.clone(), symbol.ty.clone());

                Operand::from(var)
            }
            TokenKind::Number => Operand::ConstantInt {
                value: token.lexeme.clone(),
                ty: ty.clone(),
            },
            _ => unreachable!(),
        }
    }
}
