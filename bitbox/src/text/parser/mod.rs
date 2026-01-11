#[cfg(test)]
mod test;
use crate::text::error::{
    ErrorExpectedToken, ErrorUnexpectedEndOfInput, ErrorUnexpectedTopLevelItem, Result,
};
use crate::text::lexer::token::{self, Token, TokenKind};
pub mod ast;
use crate::ir::Visibility;

enum TopLevel {
    Function(ast::Function),
    Import(ast::Import),
    Constant(ast::Constant),
}

pub struct Parser {
    stream: std::iter::Peekable<std::vec::IntoIter<Token>>,
}

// Helpers
impl Parser {
    fn parse_visibility(&mut self) -> Visibility {
        self.stream
            .next_if(|token| token.is_keyword(token::Keyword::Public))
            .map(|_| Visibility::Public)
            .unwrap_or_default()
    }

    fn consume(&mut self, expected: TokenKind) -> Result<Token> {
        match self.stream.next() {
            Some(actual) if actual.kind == expected => Ok(actual),
            Some(actual) => Err(Box::new(ErrorExpectedToken { expected, actual })),
            None => Err(Box::new(ErrorUnexpectedEndOfInput)),
        }
    }

    fn is_peek_a(&mut self, kind: TokenKind) -> bool {
        matches!(self.stream.peek(), Some(token) if token.kind == kind)
    }

    fn end_of_stream(&mut self) -> bool {
        self.stream.peek().is_none()
    }

    fn next(&mut self) -> Result<Token> {
        let Some(token) = self.stream.next() else {
            return Err(Box::new(ErrorUnexpectedEndOfInput));
        };
        Ok(token)
    }
}

impl Parser {
    pub fn new(tokens: Vec<Token>) -> Self {
        Self {
            stream: tokens.into_iter().peekable(),
        }
    }

    pub fn parse(&mut self) -> Result<ast::Module> {
        let mut imports = vec![];
        let mut functions = vec![];
        let mut constants = vec![];

        while !self.end_of_stream() {
            while self.is_peek_a(TokenKind::Delimiter) {
                self.consume(TokenKind::Delimiter)?;
            }
            match self.parse_top_level()? {
                TopLevel::Import(import) => imports.push(import),
                TopLevel::Function(func) => functions.push(func),
                TopLevel::Constant(constant) => constants.push(constant),
            }
        }

        Ok(ast::Module {
            functions,
            imports,
            constants,
        })
    }

    fn parse_top_level(&mut self) -> Result<TopLevel> {
        let visibility = self.parse_visibility();
        if self.is_peek_a(TokenKind::Keyword(token::Keyword::Function)) {
            Ok(TopLevel::Function(self.parse_function(visibility)?))
        } else if self.is_peek_a(TokenKind::Keyword(token::Keyword::Import)) {
            Ok(TopLevel::Import(self.parse_import()?))
        } else if self.is_peek_a(TokenKind::Keyword(token::Keyword::Const)) {
            Ok(TopLevel::Constant(self.parse_constant()?))
        } else {
            let tok = self.next()?;
            Err(Box::new(ErrorUnexpectedTopLevelItem {
                found: tok,
                expected: vec![
                    token::Keyword::Function,
                    token::Keyword::Import,
                    token::Keyword::Const,
                ],
            }))
        }
    }

    fn parse_function(&mut self, visibility: Visibility) -> Result<ast::Function> {
        self.consume(TokenKind::Keyword(token::Keyword::Function))?;
        let func_name = self.consume(TokenKind::Identifier)?;
        let params = self.parse_function_params()?;
        let return_type = self.consume(TokenKind::Identifier)?;

        let block = self.parse_function_block()?;
        let function = ast::Function {
            name: func_name,
            visibility,
            params,
            return_type,
            block,
        };

        Ok(function)
    }

    fn parse_function_params(&mut self) -> Result<Vec<(Token, Token)>> {
        self.consume(TokenKind::LeftParen)?;
        let mut params = vec![];

        while !self.end_of_stream() && !self.is_peek_a(TokenKind::RightParen) {
            let name = self.consume(TokenKind::Identifier)?;
            self.consume(TokenKind::Colon)?;
            let ty = self.consume(TokenKind::Identifier)?;
            params.push((name, ty));
            if !self.is_peek_a(TokenKind::Comma) {
                break;
            }
            self.consume(TokenKind::Comma)?;
        }
        self.consume(TokenKind::RightParen)?;
        Ok(params)
    }

    fn _parse_label(&mut self) -> Result<Token> {
        let name = self.next()?;
        if name.kind != TokenKind::LabelDefinition {
            return Err(Box::new(ErrorExpectedToken {
                actual: name,
                expected: TokenKind::LabelDefinition,
            }));
        }
        let delim = self.next()?;
        if delim.kind != TokenKind::Delimiter {
            return Err(Box::new(ErrorExpectedToken {
                actual: delim,
                expected: TokenKind::Delimiter,
            }));
        }
        Ok(name)
    }

    fn parse_function_block(&mut self) -> Result<Vec<ast::BasicBlock>> {
        let mut blocks = vec![];
        self.consume(TokenKind::LeftBrace)?;
        while !self.end_of_stream() && !self.is_peek_a(TokenKind::RightBrace) {
            let mut instructions = vec![];
            let label = self.consume(TokenKind::LabelDefinition)?;
            self.consume(TokenKind::Delimiter)?;
            while !self.end_of_stream() && !self.is_peek_a(TokenKind::RightBrace) {
                let instruction = self.parse_instruction()?;
                instructions.push(instruction);
                if self.is_peek_a(TokenKind::LabelDefinition) {
                    break;
                }
            }
            blocks.push(ast::BasicBlock {
                label,
                instructions,
            });
        }
        self.consume(TokenKind::RightBrace)?;
        Ok(blocks)
    }

    fn parse_instruction(&mut self) -> Result<ast::Instruction> {
        let tok = self.next()?;
        let TokenKind::Instruction(inst) = tok.kind else {
            return Err(Box::new(ErrorExpectedToken {
                actual: tok,
                expected: TokenKind::Instruction(token::Instruction::Add),
            }));
        };

        let arguments = match inst {
            token::Instruction::Add => self.parse_add()?,
            token::Instruction::Alloc => self.parse_alloc()?,
            token::Instruction::Call => self.parse_call()?,
            token::Instruction::Cmp => self.parse_cmp()?,
            token::Instruction::ElemGet => self.parse_elem_get()?,
            token::Instruction::ElemSet => self.parse_elem_set()?,
            token::Instruction::Jump => self.parse_jump()?,
            token::Instruction::JumpIf => self.parse_jump_if()?,
            token::Instruction::Load => self.parse_load()?,
            token::Instruction::Mul => self.parse_mul()?,
            token::Instruction::Phi => self.parse_phi()?,
            token::Instruction::Ret => self.parse_return()?,
            token::Instruction::Sub => self.parse_sub()?,
        };

        let instruction = ast::Instruction {
            instruction_kind: inst,
            arguments,
        };
        Ok(instruction)
    }

    fn parse_arguments(&mut self) -> Result<Vec<Token>> {
        self.consume(TokenKind::LeftParen)?;
        let mut args = vec![];
        while !self.end_of_stream() && !self.is_peek_a(TokenKind::RightParen) {
            let arg = self.next()?;
            args.push(arg);
            if !self.is_peek_a(TokenKind::Comma) {
                break;
            }
            self.consume(TokenKind::Comma)?;
        }
        self.consume(TokenKind::RightParen)?;
        Ok(args)
    }

    fn parse_import_function_params(&mut self) -> Result<Vec<Token>> {
        let mut params = vec![];
        self.consume(TokenKind::LeftParen)?;
        while !self.end_of_stream() {
            let ty = self.consume(TokenKind::Identifier)?;
            params.push(ty);
            if !self.is_peek_a(TokenKind::Comma) {
                break;
            }
            self.consume(TokenKind::Comma)?;
        }
        self.consume(TokenKind::RightParen)?;
        Ok(params)
    }

    fn parse_import(&mut self) -> Result<ast::Import> {
        self.consume(TokenKind::Keyword(token::Keyword::Import))?;
        self.consume(TokenKind::Keyword(token::Keyword::Function))?;
        let module_name = self.consume(TokenKind::Identifier)?;
        self.consume(TokenKind::PathSeparator)?;
        let name = self.consume(TokenKind::Identifier)?;
        let params = self.parse_import_function_params()?;
        let return_type = self.consume(TokenKind::Identifier)?;
        self.consume(TokenKind::Delimiter)?;
        Ok(ast::Import::Function(ast::ImportFunction {
            module_name,
            name,
            params,
            return_type,
        }))
    }

    fn parse_constant(&mut self) -> Result<ast::Constant> {
        self.consume(TokenKind::Keyword(token::Keyword::Const))?;
        let name = self.consume(TokenKind::Identifier)?;
        self.consume(TokenKind::Colon)?;
        let ty = self.consume(TokenKind::Identifier)?;
        self.consume(TokenKind::Equals)?;
        let value = self.next()?;
        self.consume(TokenKind::Delimiter)?;
        Ok(ast::Constant { name, ty, value })
    }

    fn parse_return(&mut self) -> Result<Vec<Token>> {
        let ty = self.consume(TokenKind::Identifier)?;
        self.consume(TokenKind::Colon)?;
        let value = self.next()?;
        self.consume(TokenKind::Delimiter)?;
        Ok(vec![ty, value])
    }

    fn parse_add(&mut self) -> Result<Vec<Token>> {
        let ty = self.consume(TokenKind::Identifier)?;
        self.consume(TokenKind::Colon)?;
        let des = self.consume(TokenKind::Identifier)?;
        self.consume(TokenKind::Comma)?;
        let lhs = self.next()?;
        self.consume(TokenKind::Comma)?;
        let rhs = self.next()?;
        self.consume(TokenKind::Delimiter)?;
        Ok(vec![ty, des, lhs, rhs])
    }

    fn parse_phi(&mut self) -> Result<Vec<Token>> {
        let ty = self.consume(TokenKind::Identifier)?;
        self.consume(TokenKind::Colon)?;
        let des = self.consume(TokenKind::Identifier)?;
        self.consume(TokenKind::Comma)?;
        self.consume(TokenKind::LeftBracket)?;

        let mut items = vec![ty, des];
        while !self.is_peek_a(TokenKind::RightBracket) {
            self.consume(TokenKind::LeftParen)?;

            let label = self.consume(TokenKind::Label)?;
            self.consume(TokenKind::Comma)?;
            let var = self.consume(TokenKind::Identifier)?;

            items.push(label);
            items.push(var);

            self.consume(TokenKind::RightParen)?;
            if self.is_peek_a(TokenKind::Comma) {
                self.consume(TokenKind::Comma)?;
            }
        }

        self.consume(TokenKind::RightBracket)?;
        self.consume(TokenKind::Delimiter)?;
        Ok(items)
    }

    fn parse_alloc(&mut self) -> Result<Vec<Token>> {
        let ty = self.consume(TokenKind::Identifier)?;
        self.consume(TokenKind::Colon)?;
        let des = self.consume(TokenKind::Identifier)?;
        self.consume(TokenKind::Comma)?;
        let size = self.next()?;
        self.consume(TokenKind::Delimiter)?;
        Ok(vec![ty, des, size])
    }

    fn parse_elem_get(&mut self) -> Result<Vec<Token>> {
        let ty = self.consume(TokenKind::Identifier)?;
        self.consume(TokenKind::Colon)?;
        let des = self.consume(TokenKind::Identifier)?;
        self.consume(TokenKind::Comma)?;
        let ptr = self.next()?;
        self.consume(TokenKind::Comma)?;
        let idx = self.next()?;
        self.consume(TokenKind::Delimiter)?;
        Ok(vec![ty, des, ptr, idx])
    }

    fn parse_elem_set(&mut self) -> Result<Vec<Token>> {
        let ty = self.consume(TokenKind::Identifier)?;
        self.consume(TokenKind::Colon)?;
        let des = self.consume(TokenKind::Identifier)?;
        self.consume(TokenKind::Comma)?;
        let idx = self.next()?;
        self.consume(TokenKind::Comma)?;
        let value = self.next()?;
        self.consume(TokenKind::Delimiter)?;
        Ok(vec![ty, des, idx, value])
    }

    fn parse_mul(&mut self) -> Result<Vec<Token>> {
        let ty = self.consume(TokenKind::Identifier)?;
        self.consume(TokenKind::Colon)?;
        let des = self.consume(TokenKind::Identifier)?;
        self.consume(TokenKind::Comma)?;
        let lhs = self.next()?;
        self.consume(TokenKind::Comma)?;
        let rhs = self.next()?;
        self.consume(TokenKind::Delimiter)?;
        Ok(vec![ty, des, lhs, rhs])
    }

    fn parse_sub(&mut self) -> Result<Vec<Token>> {
        let ty = self.consume(TokenKind::Identifier)?;
        self.consume(TokenKind::Colon)?;
        let des = self.consume(TokenKind::Identifier)?;
        self.consume(TokenKind::Comma)?;
        let lhs = self.next()?;
        self.consume(TokenKind::Comma)?;
        let rhs = self.next()?;
        self.consume(TokenKind::Delimiter)?;
        Ok(vec![ty, des, lhs, rhs])
    }

    fn parse_call(&mut self) -> Result<Vec<Token>> {
        let ty = self.consume(TokenKind::Identifier)?;
        self.consume(TokenKind::Colon)?;
        let des = self.consume(TokenKind::Identifier)?;
        let name = self.consume(TokenKind::Identifier)?;
        let arguments = self.parse_arguments()?;
        self.consume(TokenKind::Delimiter)?;
        let mut result = vec![ty, des, name];
        result.extend_from_slice(&arguments);
        Ok(result)
    }

    fn parse_cmp(&mut self) -> Result<Vec<Token>> {
        let ty = self.consume(TokenKind::Identifier)?;
        self.consume(TokenKind::Colon)?;
        let des = self.consume(TokenKind::Identifier)?;
        self.consume(TokenKind::Comma)?;
        let lhs = self.next()?;
        self.consume(TokenKind::Comma)?;
        let rhs = self.next()?;
        self.consume(TokenKind::Delimiter)?;
        Ok(vec![ty, des, lhs, rhs])
    }

    fn parse_jump_if(&mut self) -> Result<Vec<Token>> {
        let condition = self.next()?;
        self.consume(TokenKind::Comma)?;
        let target = self.consume(TokenKind::Label)?;
        self.consume(TokenKind::Delimiter)?;
        Ok(vec![condition, target])
    }

    fn parse_jump(&mut self) -> Result<Vec<Token>> {
        let target = self.consume(TokenKind::Label)?;
        self.consume(TokenKind::Delimiter)?;
        Ok(vec![target])
    }

    fn parse_load(&mut self) -> Result<Vec<Token>> {
        let ty = self.consume(TokenKind::Identifier)?;
        self.consume(TokenKind::Colon)?;
        let des = self.consume(TokenKind::Identifier)?;
        self.consume(TokenKind::Comma)?;
        let value = self.next()?;
        self.consume(TokenKind::Delimiter)?;
        Ok(vec![ty, des, value])
    }
}
