use crate::ir;
use crate::text::error::{ErrorMissMatchedType, ErrorMissingSymbol, Errors, Report, Result};

use crate::text::parser::ast;
use std::collections::HashMap;

#[derive(Debug)]
pub struct SymbolTableBuilder {
    pub table: SymbolTable,
    errors: Vec<Box<dyn Report>>,
}

impl Default for SymbolTableBuilder {
    fn default() -> Self {
        let mut table = SymbolTable::default();
        table.enter_scope("global");
        table.push(Symbol {
            name: "write_int".to_string(),
            ty: ir::Type::Void,
            visibility: ir::Visibility::Public,
            kind: SymbolKind::Function,
            params: Some(vec![ir::Type::Unsigned(32)]),
        });
        table.push(Symbol {
            name: "writeln".to_string(),
            ty: ir::Type::Void,
            visibility: ir::Visibility::Public,
            kind: SymbolKind::Function,
            params: Some(vec![]),
        });
        table.push(Symbol {
            name: "write_char".to_string(),
            ty: ir::Type::Void,
            visibility: ir::Visibility::Public,
            kind: SymbolKind::Function,
            params: Some(vec![ir::Type::Unsigned(32)]),
        });
        table.exit_scope();

        Self {
            table,
            errors: Vec::new(),
        }
    }
}

impl SymbolTableBuilder {
    pub fn build(mut self, module: &ast::Module) -> Result<SymbolTable> {
        for import in &module.imports {
            let Err(error) = self.walk_import(import) else {
                continue;
            };
            self.errors.push(error);
        }
        for constant in &module.constants {
            let Err(error) = self.walk_constant(constant) else {
                continue;
            };
            self.errors.push(error);
        }
        for function in &module.functions {
            let Err(error) = self.walk_function(function) else {
                continue;
            };
            self.errors.push(error);
        }
        if !self.errors.is_empty() {
            return Err(Box::new(Errors {
                errors: self.errors,
            }));
        }
        Ok(self.table)
    }

    fn walk_function(&mut self, function: &ast::Function) -> Result<()> {
        let ast::Function {
            visibility,
            name,
            params,
            return_type,
            block,
            ..
        } = function;

        self.table.push(Symbol {
            name: name.lexeme.clone(),
            kind: SymbolKind::Function,
            ty: return_type.into(),
            visibility: *visibility,
            params: Some(params.iter().map(|param| (&param.1).into()).collect()),
        });
        self.table.enter_scope(&name.lexeme);

        for param in params {
            self.table.push(Symbol {
                name: param.0.lexeme.clone(),
                kind: SymbolKind::Parameter,
                ty: (&param.1).into(),
                visibility: ir::Visibility::Private,
                params: None,
            });
        }

        for bb in block.iter() {
            self.table.push(Symbol {
                name: bb.label.lexeme.clone(),
                kind: SymbolKind::Label,
                ty: ir::Type::Void,
                visibility: ir::Visibility::Private,
                params: None,
            });
            self.walk_basic_block(bb)?;
        }
        self.table.exit_scope();
        Ok(())
    }

    fn walk_import(&mut self, _: &ast::Import) -> Result<()> {
        todo!("import")
    }

    fn walk_constant(&mut self, _: &ast::Constant) -> Result<()> {
        todo!("walk_constant")
    }

    fn walk_basic_block(&mut self, block: &ast::BasicBlock) -> Result<()> {
        for intruction in block.instructions.iter() {
            self.walk_instruction(intruction)?;
        }
        Ok(())
    }

    fn walk_instruction(&mut self, instruction: &ast::Instruction) -> Result<()> {
        match instruction.instruction_kind {
            super::lexer::token::Instruction::Add => {
                let [ty, des, _, _] = &instruction.arguments.as_slice() else {
                    // NOTE: If this happens then the parser is broken.
                    panic!("Invalid instruction arguments for @add");
                };
                let ty = ir::Type::from(ty);
                self.table.push(Symbol {
                    name: des.lexeme.to_string(),
                    kind: SymbolKind::Variable,
                    ty: ty.clone(),
                    visibility: ir::Visibility::Private,
                    params: None,
                });
            }
            super::lexer::token::Instruction::Alloc => todo!(),
            super::lexer::token::Instruction::Call => todo!(),
            super::lexer::token::Instruction::Cmp => {
                let [ty, des, _, _] = &instruction.arguments.as_slice() else {
                    // NOTE: If this happens then the parser is broken.
                    panic!("Invalid instruction arguments for @cmp");
                };
                let ty = ir::Type::from(ty);
                self.table.push(Symbol {
                    name: des.lexeme.to_string(),
                    kind: SymbolKind::Variable,
                    ty: ty.clone(),
                    visibility: ir::Visibility::Private,
                    params: None,
                });
            }
            super::lexer::token::Instruction::ElemGet => todo!(),
            super::lexer::token::Instruction::ElemSet => todo!(),
            super::lexer::token::Instruction::Jump => {
                // Maybe a symbole type should be Label?
            }
            super::lexer::token::Instruction::JumpIf => {
                // Maybe a symbole type should be Label?
            }
            super::lexer::token::Instruction::Load => {
                let [ty, des, _] = &instruction.arguments.as_slice() else {
                    // NOTE: If this happens then the parser is broken.
                    panic!("Invalid instruction arguments for @load");
                };
                let ty = ir::Type::from(ty);
                self.table.push(Symbol {
                    name: des.lexeme.to_string(),
                    kind: SymbolKind::Variable,
                    ty: ty.clone(),
                    visibility: ir::Visibility::Private,
                    params: None,
                });
            }
            super::lexer::token::Instruction::Mul => todo!(),
            super::lexer::token::Instruction::Phi => {
                let [ty, des] = &instruction.arguments[0..2] else {
                    // NOTE: If this happens then the parser is broken.
                    panic!("Invalid instruction arguments for @phi");
                };
                let ty = ir::Type::from(ty);
                self.table.push(Symbol {
                    name: des.lexeme.to_string(),
                    kind: SymbolKind::Variable,
                    ty: ty.clone(),
                    visibility: ir::Visibility::Private,
                    params: None,
                });
            }
            super::lexer::token::Instruction::Ret => {
                let [ty, src] = &instruction.arguments.as_slice() else {
                    return Ok(());
                };
                let Some(symbol) = self.table.get(&src.lexeme) else {
                    return Err(Box::new(ErrorMissingSymbol {
                        symbol: src.clone(),
                    }));
                };
                if symbol.ty == ir::Type::from(ty) {
                    return Ok(());
                }
                return Err(Box::new(ErrorMissMatchedType {
                    span: ty.span.clone(),
                    found: ir::Type::from(ty),
                    expected: symbol.ty.clone(),
                }));
            }
            super::lexer::token::Instruction::Sub => todo!(),
        }
        Ok(())
    }
}

#[derive(Debug, Default, Clone, PartialEq, Eq)]
pub enum SymbolKind {
    #[default]
    Variable,
    Function,
    Parameter,
    Constant,
    Import,
    Label,
}

#[derive(Debug, Default, Clone, PartialEq, Eq)]
pub struct Symbol {
    pub name: String,
    pub kind: SymbolKind,
    pub ty: ir::Type,
    pub visibility: ir::Visibility,
    pub params: Option<Vec<ir::Type>>,
}

#[derive(Debug, Default, Clone, PartialEq, Eq)]
pub struct Scope {
    symbols: HashMap<String, Symbol>,
}

impl Scope {
    pub fn insert(&mut self, symbol: Symbol) {
        let name = symbol.name.clone();
        self.symbols.insert(name, symbol);
    }

    pub fn get(&self, name: &str) -> Option<&Symbol> {
        self.symbols.get(name)
    }

    pub fn get_mut(&mut self, name: &str) -> Option<&mut Symbol> {
        self.symbols.get_mut(name)
    }

    pub fn lookup(&self, name: &str) -> Option<&Symbol> {
        self.symbols.get(name)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ScopePath(Vec<String>);

impl ScopePath {
    fn new(parts: &[String]) -> Self {
        Self(parts.to_vec())
    }

    // fn as_key(&self) -> String {
    //     self.0.join(SymbolTable::SEPARATOR)
    // }

    fn global() -> Self {
        Self::new(&[SymbolTable::GLOBAL_SCOPE.to_string()])
    }
}

#[derive(Debug, Default, Clone, PartialEq, Eq)]
pub struct SymbolTable {
    pub scopes: HashMap<ScopePath, Scope>,
    pub scope_stack: Vec<String>,
}

impl SymbolTable {
    pub const SEPARATOR: &'static str = "::";
    pub const GLOBAL_SCOPE: &'static str = "global";

    pub fn get_scope_name(&self) -> String {
        self.scope_stack.last().cloned().unwrap_or_default()
    }

    fn get_full_scope_name(&self) -> ScopePath {
        ScopePath::new(&self.scope_stack)
    }

    pub fn enter_scope(&mut self, name: &str) {
        self.scope_stack.push(name.to_string());
    }

    pub fn exit_scope(&mut self) {
        self.scope_stack.pop();
    }

    pub fn scope_level(&self) -> usize {
        self.scope_stack.len()
    }

    pub fn get(&self, name: &str) -> Option<&Symbol> {
        for i in (0..=self.scope_stack.len()).rev() {
            let scope_path = ScopePath::new(&self.scope_stack[0..i]);

            if let Some(scope) = self.scopes.get(&scope_path)
                && let Some(symbol) = scope.lookup(name)
            {
                return Some(symbol);
            }
        }

        self.scopes
            .get(&ScopePath::global())
            .and_then(|scope| scope.lookup(name))
    }

    pub fn get_mut(&mut self, name: &str, mut f: impl FnMut(&mut Symbol)) {
        for i in (0..self.scope_stack.len()).rev() {
            let scope_path = ScopePath::new(&self.scope_stack[0..=i]);
            if let Some(scope) = self.scopes.get_mut(&scope_path)
                && let Some(symbol) = scope.get_mut(name)
            {
                f(symbol);
                return;
            }
        }

        if let Some(symbol) = self
            .scopes
            .get_mut(&ScopePath::global())
            .and_then(|scope| scope.get_mut(name))
        {
            f(symbol);
        }
    }

    fn push(&mut self, symbol: Symbol) {
        let scope_name = self.get_full_scope_name();
        self.scopes.entry(scope_name).or_default().insert(symbol);
    }

    pub fn extend(&mut self, other: SymbolTable) {
        self.scopes.extend(other.scopes);
    }
}
