use crate::value::{Function, FunctionType};
use crate::{lex::Token, lex::TokenKind, Chunk, Lexer, OpCode, Value};
use miette::LabeledSpan;
use std::collections::HashMap;

type ParseFn<'de, 'fun> = fn(&mut Parser<'de, 'fun>, Token<'de>, bool) -> miette::Result<()>;

#[derive(Debug, PartialEq, PartialOrd, Eq)]
struct ParseRule<'de, 'fun> {
    infix: Option<ParseFn<'de, 'fun>>,
    prefix: Option<ParseFn<'de, 'fun>>,
    prec: Precedence,
}

pub struct Parser<'de, 'fun> {
    lexer: Lexer<'de>,
    fun: &'fun mut Function,
    fn_type: FunctionType,
    rules: HashMap<TokenKind, ParseRule<'de, 'fun>>,
    scope: Scope<'de>,
}

#[derive(Debug)]
struct Local<'de> {
    name: &'de str,
    depth: u8, // Supports at max 256 nested scopes
    defined: bool,
}

#[derive(Debug)]
struct Scope<'de> {
    locals: Vec<Local<'de>>,
    scope_depth: u8,
}

impl<'de> Scope<'de> {
    fn new() -> Self {
        let mut locals = Vec::new();
        locals.push(Local {
            name: "reserved",
            depth: 0,
            defined: true,
        });
        Self {
            locals,
            scope_depth: 0,
        }
    }

    fn add_local(&mut self, name: &'de str) {
        if self.locals.len() as u8 == u8::MAX {
            panic!("Exceeded 256 local variables!");
        }
        self.locals.push(Local::new(name, self.scope_depth))
    }

    fn begin_scope(&mut self) {
        self.scope_depth += 1;
    }

    fn end_scope(&mut self) {
        self.scope_depth -= 1;
    }

    fn init(&mut self) {
        self.locals.last_mut().unwrap().defined = true;
    }
}

impl<'de> Local<'de> {
    fn new(name: &'de str, depth: u8) -> Self {
        Self {
            name,
            depth,
            defined: false,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, PartialOrd, Eq)]
enum Precedence {
    None,
    Assignment, // =
    Or,         // or
    And,        // and
    Equality,   // == !=
    Comparison, // < > <= >=
    Term,       // + -
    Factor,     // * /
    Unary,      // ! -
    Call,       // . ()
    Primary,
}

impl Precedence {
    fn next(&self) -> Self {
        use Precedence::*;
        match *self {
            None => Assignment,
            Assignment => Or,
            Or => And,
            And => Equality,
            Equality => Comparison,
            Comparison => Term,
            Term => Factor,
            Factor => Unary,
            Unary => Call,
            Call => Primary,
            Primary => None,
        }
    }
}

impl<'de, 'fun> Parser<'de, 'fun> {
    pub fn new(source: &'de str, fun: &'fun mut Function, fn_type: FunctionType) -> Self {
        use TokenKind::*;
        Parser {
            lexer: Lexer::new(source),
            fun,
            fn_type,
            scope: Scope::new(),
            rules: HashMap::from([
                (
                    LeftParen,
                    ParseRule {
                        prefix: Some(Parser::parse_group),
                        infix: Some(Parser::parse_call),
                        prec: Precedence::Call,
                    },
                ),
                (
                    RightParen,
                    ParseRule {
                        prefix: None,
                        infix: None,
                        prec: Precedence::None,
                    },
                ),
                (
                    LeftBracket,
                    ParseRule {
                        prefix: None,
                        infix: None,
                        prec: Precedence::None,
                    },
                ),
                (
                    RightBracket,
                    ParseRule {
                        prefix: None,
                        infix: None,
                        prec: Precedence::None,
                    },
                ),
                (
                    Comma,
                    ParseRule {
                        prefix: None,
                        infix: None,
                        prec: Precedence::None,
                    },
                ),
                (
                    Dot,
                    ParseRule {
                        prefix: None,
                        infix: None,
                        prec: Precedence::None,
                    },
                ),
                (
                    Minus,
                    ParseRule {
                        prefix: Some(Parser::parse_unary),
                        infix: Some(Parser::parse_binary),
                        prec: Precedence::Term,
                    },
                ),
                (
                    Plus,
                    ParseRule {
                        prefix: None,
                        infix: Some(Parser::parse_binary),
                        prec: Precedence::Term,
                    },
                ),
                (
                    Semicolon,
                    ParseRule {
                        prefix: None,
                        infix: None,
                        prec: Precedence::None,
                    },
                ),
                (
                    Star,
                    ParseRule {
                        prefix: None,
                        infix: Some(Parser::parse_binary),
                        prec: Precedence::Factor,
                    },
                ),
                (
                    Slash,
                    ParseRule {
                        prefix: None,
                        infix: Some(Parser::parse_binary),
                        prec: Precedence::Factor,
                    },
                ),
                (
                    Bang,
                    ParseRule {
                        prefix: Some(Parser::parse_unary),
                        infix: None,
                        prec: Precedence::None,
                    },
                ),
                (
                    Eq,
                    ParseRule {
                        prefix: None,
                        infix: None,
                        prec: Precedence::None,
                    },
                ),
                (
                    Less,
                    ParseRule {
                        prefix: None,
                        infix: Some(Parser::parse_binary),
                        prec: Precedence::Comparison,
                    },
                ),
                (
                    Greater,
                    ParseRule {
                        prefix: None,
                        infix: Some(Parser::parse_binary),
                        prec: Precedence::Comparison,
                    },
                ),
                (
                    BangEq,
                    ParseRule {
                        prefix: None,
                        infix: Some(Parser::parse_binary),
                        prec: Precedence::Equality,
                    },
                ),
                (
                    EqEq,
                    ParseRule {
                        prefix: None,
                        infix: Some(Parser::parse_binary),
                        prec: Precedence::Equality,
                    },
                ),
                (
                    LessEq,
                    ParseRule {
                        prefix: None,
                        infix: Some(Parser::parse_binary),
                        prec: Precedence::Comparison,
                    },
                ),
                (
                    GreaterEq,
                    ParseRule {
                        prefix: None,
                        infix: Some(Parser::parse_binary),
                        prec: Precedence::Comparison,
                    },
                ),
                (
                    TString,
                    ParseRule {
                        prefix: Some(Parser::parse_literal),
                        infix: None,
                        prec: Precedence::None,
                    },
                ),
                (
                    Number(0.), // The actual value inside number doesn't matter
                    ParseRule {
                        prefix: Some(Parser::parse_literal),
                        infix: None,
                        prec: Precedence::None,
                    },
                ),
                (
                    Ident,
                    ParseRule {
                        prefix: Some(Parser::parse_ident),
                        infix: None,
                        prec: Precedence::None,
                    },
                ),
                (
                    And,
                    ParseRule {
                        prefix: None,
                        infix: Some(Parser::parse_and),
                        prec: Precedence::And,
                    },
                ),
                (
                    Class,
                    ParseRule {
                        prefix: None,
                        infix: None,
                        prec: Precedence::None,
                    },
                ),
                (
                    Else,
                    ParseRule {
                        prefix: None,
                        infix: None,
                        prec: Precedence::None,
                    },
                ),
                (
                    False,
                    ParseRule {
                        prefix: Some(Parser::parse_literal),
                        infix: None,
                        prec: Precedence::None,
                    },
                ),
                (
                    For,
                    ParseRule {
                        prefix: None,
                        infix: None,
                        prec: Precedence::None,
                    },
                ),
                (
                    Fun,
                    ParseRule {
                        prefix: None,
                        infix: None,
                        prec: Precedence::None,
                    },
                ),
                (
                    If,
                    ParseRule {
                        prefix: None,
                        infix: None,
                        prec: Precedence::None,
                    },
                ),
                (
                    Nil,
                    ParseRule {
                        prefix: Some(Parser::parse_literal),
                        infix: None,
                        prec: Precedence::None,
                    },
                ),
                (
                    Or,
                    ParseRule {
                        prefix: None,
                        infix: Some(Parser::parse_or),
                        prec: Precedence::Or,
                    },
                ),
                (
                    Return,
                    ParseRule {
                        prefix: None,
                        infix: None,
                        prec: Precedence::None,
                    },
                ),
                (
                    Super,
                    ParseRule {
                        prefix: None,
                        infix: None,
                        prec: Precedence::None,
                    },
                ),
                (
                    This,
                    ParseRule {
                        prefix: None,
                        infix: None,
                        prec: Precedence::None,
                    },
                ),
                (
                    True,
                    ParseRule {
                        prefix: Some(Parser::parse_literal),
                        infix: None,
                        prec: Precedence::None,
                    },
                ),
                (
                    Var,
                    ParseRule {
                        prefix: None,
                        infix: None,
                        prec: Precedence::None,
                    },
                ),
                (
                    While,
                    ParseRule {
                        prefix: None,
                        infix: None,
                        prec: Precedence::None,
                    },
                ),
            ]),
        }
    }

    fn end_scope(&mut self) {
        let local_vars = self
            .scope
            .locals
            .iter()
            .rev()
            .take_while(|x| x.depth == self.scope.scope_depth);

        let n = local_vars.fold(0, |acc: usize, x| {
            self.fun.code.push_opcode(OpCode::OpPop);
            acc + 1
        });

        self.scope.locals.truncate(self.scope.locals.len() - n);
        self.scope.end_scope();
    }

    fn get_rule(&self, kind: TokenKind) -> &ParseRule<'de, 'fun> {
        self.rules
            .get(&kind)
            .expect("HashMap contains all possible TokenKind variants")
    }

    pub fn compile(&mut self) -> miette::Result<()> {
        while self.lexer.peek().is_some_and(|x| x.is_ok()) {
            self.parse_decl()?;
        }
        self.fun.code.push_opcode(OpCode::OpReturn);
        Ok(())
    }

    fn parse_decl(&mut self) -> miette::Result<()> {
        let token = self
            .lexer
            .peek()
            .expect("By the callee")
            .as_ref()
            .expect("By the callee");

        match token.kind {
            TokenKind::Var => self.parse_var_decl(),
            TokenKind::Fun => self.parse_fun_decl(),
            _ => self.parse_statement(),
        }
    }

    fn parse_fun_decl(&mut self) -> miette::Result<()> {
        let _ = self
            .lexer
            .expect(TokenKind::Var, "Caller should have checked");
        let token = self
            .lexer
            .expect(TokenKind::Ident, "Expected identifier after 'var' keyword")?;

        // Declare variable with the following name
        if let Some(index) = self.parse_variable(token, true)? {
            let offset = self.parse_fun()?;
            self.lexer.sync(offset - self.lexer.offset);
            self.define_var(Some(index));
            Ok(())
        } else {
            // TODO: miette spans
            Err(miette::miette!("No function name provided"))
        }
    }

    fn parse_fun(&mut self) -> miette::Result<usize> {
        let offset;
        let mut fun = Function::new();
        // TODO: please dont copy the whole source code for every function invocation
        let source = self.lexer.source();
        let mut parser = Parser::new(&source, &mut fun, FunctionType::Function);
        parser.lexer.sync(self.lexer.offset);
        parser.scope.begin_scope();
        parser
            .lexer
            .expect(TokenKind::LeftParen, "Expected '(' after function name")?;
        parser
            .lexer
            .expect(TokenKind::RightParen, "Expected ')' after parameters")?;
        parser.lexer.expect(
            TokenKind::LeftBracket,
            "Expected '{{' to start function block",
        )?;

        parser.parse_block()?;

        offset = parser.lexer.offset;
        self.fun.code.push_const(Value::Fun(fun));
        Ok(offset)
    }

    fn parse_var_decl(&mut self) -> miette::Result<()> {
        let _ = self
            .lexer
            .expect(TokenKind::Var, "Caller should have checked");
        let token = self
            .lexer
            .expect(TokenKind::Ident, "Expected identifier after 'var' keyword")?;

        // Declare variable with the following name
        let index = self.parse_variable(token, true)?;

        // TODO: miette error reporting
        let token = self.lexer.peek().expect("By the callee").as_ref().unwrap();
        match token.kind {
            TokenKind::Eq => {
                let _ = self.lexer.next();
                self.parse_expr()?;
            }
            _ => self.fun.code.push_opcode(OpCode::OpNil),
        }
        let _ = self
            .lexer
            .expect(TokenKind::Semicolon, "Expected ';' after expression")?;

        self.define_var(index);
        Ok(())
    }

    fn define_var(&mut self, index: Option<u32>) {
        if let Some(index) = index {
            self.fun.code.push_defvar(OpCode::OpDefGlobal(index))
        }
    }

    fn resolve_local_var(&mut self, token: Token) -> miette::Result<Option<u32>> {
        let index = self
            .scope
            .locals
            .iter()
            .rev()
            .position(|x| x.name == token.source);

        match index {
            Some(index) => {
                let index = self.scope.locals.len() - 1 - index;
                if !self.scope.locals.get(index).unwrap().defined {
                    Err(miette::miette!(
                        "Can't read local variable in its own initializer"
                    ))
                } else {
                    Ok(Some(index as u32))
                }
            }
            None => Ok(None),
        }
    }

    fn parse_ident(&mut self, token: Token<'de>, can_assign: bool) -> miette::Result<()> {
        let (set_op, get_op) = if let Some(index) = self.resolve_local_var(token)? {
            (OpCode::OpDefLocal(index), OpCode::OpGetLocal(index))
        } else {
            let index = self.global_variable(token, can_assign);
            (OpCode::OpDefGlobal(index), OpCode::OpGetGlobal(index))
        };

        let token = self.lexer.peek().unwrap().as_ref().unwrap();

        match token.kind {
            TokenKind::Eq => {
                if can_assign {
                    self.lexer.next();
                    self.parse_expr()?;
                    self.fun.code.push_defvar(set_op)
                };
            }
            _ => {
                self.fun.code.push_getvar(get_op);
            }
        };
        Ok(())
    }

    fn declare_var(&mut self, token: Token<'de>) -> miette::Result<()> {
        if self.scope.scope_depth == 0 {
            return Ok(());
        }
        if self.scope.locals.iter().fold(false, |_, e| {
            e.name == token.source && e.depth == self.scope.scope_depth
        }) {
            return Err(miette::miette!(
                "A variable with the name \"{}\" already exists",
                token.source
            ));
        }

        self.scope.add_local(token.source);
        Ok(())
    }

    fn parse_variable(&mut self, token: Token<'de>, b: bool) -> miette::Result<Option<u32>> {
        self.declare_var(token)?;
        if self.scope.scope_depth > 0 {
            self.scope.init();
            return Ok(None);
        }
        Ok(Some(self.global_variable(token, b)))
    }

    fn global_variable(&mut self, token: Token<'de>, _: bool) -> u32 {
        self.fun.code.push_value(Value::String(token.to_string()))
    }

    fn parse_statement(&mut self) -> miette::Result<()> {
        let token = self.lexer.peek().unwrap().as_ref().unwrap();
        match token.kind {
            TokenKind::Print => {
                self.lexer.next();
                self.print_statement()?;
            }
            TokenKind::LeftBracket => {
                self.lexer.next();
                self.scope.begin_scope();
                self.parse_block()?;
                self.end_scope();
            }
            TokenKind::If => {
                self.lexer.next();
                self.parse_if_statement()?;
            }
            TokenKind::While => {
                self.lexer.next();
                self.parse_while_statement()?;
            }
            TokenKind::For => {
                self.lexer.next();
                self.parse_for_statement()?;
            }
            _ => self.parse_expr_statement()?,
        };
        Ok(())
    }

    fn parse_for_statement(&mut self) -> miette::Result<()> {
        // Variables declared in a for loop initializer clause
        // belong to the for loop's inner scope
        self.scope.begin_scope();

        self.lexer.expect(
            TokenKind::LeftParen,
            "Expected opening paren '(' after for keyword",
        )?;

        let token = self
            .lexer
            .peek()
            .expect("Expected expression after '('")
            .as_ref()
            .unwrap();

        match token.kind {
            TokenKind::Semicolon => {
                let _ = self.lexer.next().unwrap();
            }
            // Parse initializer expression
            TokenKind::Var => self.parse_var_decl()?,
            _ => self.parse_expr_statement()?,
        };

        // Main loop body is right after initializer expression
        let mut loop_start = self.fun.code.last_op();

        let mut break_jump: Option<usize> = None;

        let token = self.lexer.peek().unwrap().as_ref().unwrap();
        match token.kind {
            TokenKind::Semicolon => {
                let _ = self.lexer.next();
            }
            // Condition clause exists
            _ => {
                self.parse_expr()?;
                let _ = self
                    .lexer
                    .expect(TokenKind::Semicolon, "Expected ';' after expression")?;
                // We only exit the loop from the condition clause
                break_jump = Some(self.fun.code.push_jump(OpCode::OpJumpIfFalse(0)));
                self.fun.code.push_opcode(OpCode::OpPop);
            }
        };

        let token = self.lexer.peek().unwrap().as_ref().unwrap();
        match token.kind {
            TokenKind::RightParen => {
                let _ = self.lexer.next();
            }
            // Increment clause exists
            _ => {
                // In the first iteration we don't evaluate the increment clause and just skip it
                let body_jump = self.fun.code.push_jump(OpCode::OpJump(0));
                let increment_start = self.fun.code.next_op();
                self.parse_expr()?;
                self.fun.code.push_opcode(OpCode::OpPop);

                self.lexer.expect(
                    TokenKind::RightParen,
                    "Expected closing paren ')' after for expression",
                )?;

                // Jump to condition clause
                self.fun.code.push_loop(loop_start);
                loop_start = increment_start;
                self.patch_jump(body_jump);
            }
        };

        self.parse_statement()?;
        self.fun.code.push_loop(loop_start);

        if let Some(jump) = break_jump {
            self.patch_jump(jump);
            self.fun.code.push_opcode(OpCode::OpPop);
        };

        self.scope.end_scope();
        Ok(())
    }

    fn parse_while_statement(&mut self) -> miette::Result<()> {
        let loop_start = self.fun.code.next_op();
        self.lexer
            .expect(TokenKind::LeftParen, "Expected '(' after \"while\" keyword")?;
        self.parse_expr()?;
        self.lexer.expect(
            TokenKind::RightParen,
            "Expected closing '(' after condition",
        )?;

        let break_jump = self.fun.code.push_jump(OpCode::OpJumpIfFalse(0));
        self.fun.code.push_opcode(OpCode::OpPop);

        self.parse_statement()?;

        self.fun.code.push_loop(loop_start);
        self.patch_jump(break_jump);
        self.fun.code.push_opcode(OpCode::OpPop);
        Ok(())
    }

    fn parse_if_statement(&mut self) -> miette::Result<()> {
        self.lexer
            .expect(TokenKind::LeftParen, "Expected '(' after \"if\" keyword")?;
        self.parse_expr()?;
        self.lexer
            .expect(TokenKind::RightParen, "Expected '(' after condition")?;

        let else_offset = self.fun.code.push_jump(OpCode::OpJumpIfFalse(0));
        self.fun.code.push_opcode(OpCode::OpPop);
        self.parse_statement()?;

        let if_offset = self.fun.code.push_jump(OpCode::OpJump(0));

        self.patch_jump(else_offset);
        self.fun.code.push_opcode(OpCode::OpPop);

        if let Some(Ok(token)) = self.lexer.peek() {
            if let TokenKind::Else = token.kind {
                self.lexer.next();
                self.parse_statement()?;
            };
        }
        self.patch_jump(if_offset);
        Ok(())
    }

    fn patch_jump(&mut self, from: usize) {
        self.fun.code.patch_jump(from, self.fun.code.last_op());
    }

    fn parse_block(&mut self) -> miette::Result<()> {
        let token = self.lexer.peek();
        loop {
            let token = self.lexer.peek();
            match token {
                Some(Ok(token)) => {
                    if token.kind == TokenKind::RightBracket {
                        let _ = self.lexer.next();
                        return Ok(());
                    }
                    //let _ = self.lexer.next();
                    self.parse_decl()?;
                }
                Some(Err(_)) => return Err(miette::miette!("couldn't parse thing inside block")),
                None => return Err(miette::miette!("Expected '}}' after block")),
            }
        }
    }

    fn parse_expr_statement(&mut self) -> miette::Result<()> {
        self.parse_expr()?;
        let _ = self
            .lexer
            .expect(TokenKind::Semicolon, "Expected ';' after expression")?;
        self.fun.code.push_opcode(OpCode::OpPop);
        Ok(())
    }

    fn parse_expr(&mut self) -> miette::Result<()> {
        self.parse_prec(Precedence::Assignment)
    }

    fn parse_prec(&mut self, prec: Precedence) -> miette::Result<()> {
        let token = self.lexer.next().unwrap()?;
        let rules = self.get_rule(token.kind);
        let prefix_rule = rules.prefix;

        let prefix_rule = match prefix_rule {
            Some(p) => p,
            None => return Err(miette::miette!{
                labels = vec![LabeledSpan::at(token.offset..token.offset + token.source.len(), "this token")],
                "No prefix rule for token '{token}'"
            }.with_source_code(self.lexer.source())),
        };

        let can_assign = prec <= Precedence::Assignment;
        prefix_rule(self, token, can_assign)?;

        loop {
            let token = match self.lexer.peek() {
                Some(Ok(token)) => *token,
                // I couldn't return a &Report and so for now we get this whenever
                // there is an error in .peek()
                Some(Err(_)) => return Err(miette::miette!("Very bad error message")),
                None => return Ok(()),
            };

            let rule = self.get_rule(token.kind);
            let next_prec = rule.prec;

            if prec >= next_prec {
                break;
            }

            let infix_rule = rule.infix;

            let infix_rule = match infix_rule {
                Some(p) => p,
                None => return Err(miette::miette!{
                    labels = vec![LabeledSpan::at(token.offset..token.offset + token.source.len(), "this token")],
                    "No infix rule for token '{token}'"
                }.with_source_code(self.lexer.source())),
            };
            let _ = self.lexer.next();

            infix_rule(self, token, can_assign)?;
        }

        if !can_assign && self.lexer.peek().unwrap().as_ref().unwrap().kind == TokenKind::Eq {
            return Err(miette::miette!("Invalid assignment target."));
        }

        Ok(())
    }

    fn parse_group(&mut self, _: Token, _: bool) -> miette::Result<()> {
        self.parse_expr()?;

        self.lexer
            .expect(TokenKind::RightParen, "expected closing parentheses")?;

        Ok(())
    }

    fn parse_call(&mut self, a: Token, _: bool) -> miette::Result<()> {
        let n_args = self.parse_args()?;
        self.fun.code.push_opcode(OpCode::OpCall(n_args));
        Ok(())
    }

    fn parse_args(&mut self) -> miette::Result<u8> {
        let mut n_args: u8 = 0;
        // TODO match on this
        let token = self.lexer.peek().unwrap().as_ref().unwrap();

        if token.kind != TokenKind::RightParen {
            loop {
                self.parse_expr()?;
                n_args += 1;
                match self.lexer.peek() {
                    Some(Ok(t)) => match t.kind {
                        TokenKind::Comma => {
                            let _ = self.lexer.next();
                        }
                        _ => break,
                    },
                    Some(Err(e)) => {
                        return Err(miette::miette!(
                            "I don't know how to implement errors: {:?}",
                            e
                        ))
                    }
                    None => break,
                }
            }
        }
        self.lexer.expect(
            TokenKind::RightParen,
            "Expected ')' after function arguments",
        )?;

        Ok(n_args)
    }

    fn parse_and(&mut self, _: Token, _: bool) -> miette::Result<()> {
        let jump = self.fun.code.push_jump(OpCode::OpJumpIfFalse(0));

        self.fun.code.push_opcode(OpCode::OpPop);
        self.parse_prec(Precedence::And)?;
        self.patch_jump(jump);
        Ok(())
    }

    fn parse_or(&mut self, _: Token, _: bool) -> miette::Result<()> {
        // If the first condition is false, evaluate the second
        let second_cond = self.fun.code.push_jump(OpCode::OpJumpIfFalse(0));
        // If the first condition is true, jump to the end
        let first_cond = self.fun.code.push_jump(OpCode::OpJump(0));

        self.patch_jump(second_cond);
        self.fun.code.push_opcode(OpCode::OpPop);
        self.parse_prec(Precedence::Or)?;

        self.patch_jump(first_cond);
        Ok(())
    }

    fn parse_unary(&mut self, token: Token, _: bool) -> miette::Result<()> {
        self.parse_prec(Precedence::Unary)?;

        match token.kind {
            TokenKind::Minus => self.fun.code.push_opcode(OpCode::OpNegate),
            TokenKind::Bang => self.fun.code.push_opcode(OpCode::OpNot),
            _ => unreachable!("by the callee"),
        }
        Ok(())
    }

    fn parse_binary(&mut self, token: Token, _: bool) -> miette::Result<()> {
        let prec = self.get_rule(token.kind).prec;

        self.parse_prec(prec)?;

        match token.kind {
            TokenKind::Plus => self.fun.code.push_opcode(OpCode::OpAdd),
            TokenKind::Minus => self.fun.code.push_opcode(OpCode::OpSubtract),
            TokenKind::Star => self.fun.code.push_opcode(OpCode::OpMultiply),
            TokenKind::Slash => self.fun.code.push_opcode(OpCode::OpDivide),
            TokenKind::BangEq => self.fun.code.push_opcodes(OpCode::OpEq, OpCode::OpNot),
            TokenKind::EqEq => self.fun.code.push_opcode(OpCode::OpEq),
            TokenKind::Greater => self.fun.code.push_opcode(OpCode::OpGreater),
            TokenKind::GreaterEq => self.fun.code.push_opcodes(OpCode::OpLess, OpCode::OpNot),
            TokenKind::Less => self.fun.code.push_opcode(OpCode::OpLess),
            TokenKind::LessEq => self.fun.code.push_opcodes(OpCode::OpGreater, OpCode::OpNot),
            _ => unreachable!("By the callee"),
        }
        Ok(())
    }

    fn parse_literal(&mut self, token: Token, _: bool) -> miette::Result<()> {
        match token.kind {
            TokenKind::False => self.fun.code.push_opcode(OpCode::OpFalse),
            TokenKind::True => self.fun.code.push_opcode(OpCode::OpTrue),
            TokenKind::Nil => self.fun.code.push_opcode(OpCode::OpNil),
            TokenKind::Number(n) => self.fun.code.push_const(Value::Number(n)),
            TokenKind::TString => self.fun.code.push_const(Value::String(token.to_string())),
            _ => unreachable!("By the callee"),
        }
        Ok(())
    }

    fn print_statement(&mut self) -> miette::Result<()> {
        self.parse_expr()?;
        let _ = self
            .lexer
            .expect(TokenKind::Semicolon, "Expected ';' after value.")?;
        self.fun.code.push_opcode(OpCode::OpPrint);
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parse_number() {
        let source = "1;";
        let mut expected = Chunk::new();
        expected.push_const(Value::Number(1.));
        expected.push_opcode(OpCode::OpPop);
        expected.push_opcode(OpCode::OpReturn);

        let mut fun = Function::new();
        let mut parser = Parser::new(&source, &mut fun, FunctionType::Script);
        let _ = parser.compile().unwrap();
        assert_eq!(expected, fun.code);
    }

    #[test]
    fn parse_number_inside_parens() {
        let source = "(101);";
        let mut expected = Chunk::new();
        expected.push_const(Value::Number(101.));
        expected.push_opcode(OpCode::OpPop);
        expected.push_opcode(OpCode::OpReturn);

        let mut fun = Function::new();
        let mut parser = Parser::new(&source, &mut fun, FunctionType::Script);
        let _ = parser.compile().unwrap();
        assert_eq!(expected, fun.code);
    }

    #[test]
    fn parse_addition() {
        let source = "1+2;";
        let mut expected = Chunk::new();
        expected.push_const(Value::Number(1.));
        expected.push_const(Value::Number(2.));
        expected.push_opcode(OpCode::OpAdd);
        expected.push_opcode(OpCode::OpPop);
        expected.push_opcode(OpCode::OpReturn);

        let mut fun = Function::new();
        let mut parser = Parser::new(&source, &mut fun, FunctionType::Script);
        let _ = parser.compile().unwrap();
        assert_eq!(expected, fun.code);
    }

    #[test]
    fn parse_sub() {
        let source = "1-2;";
        let mut expected = Chunk::new();
        expected.push_const(Value::Number(1.));
        expected.push_const(Value::Number(2.));
        expected.push_opcode(OpCode::OpSubtract);
        expected.push_opcode(OpCode::OpPop);
        expected.push_opcode(OpCode::OpReturn);

        let mut fun = Function::new();
        let mut parser = Parser::new(&source, &mut fun, FunctionType::Script);
        let _ = parser.compile().unwrap();
        assert_eq!(expected, fun.code);
    }

    #[test]
    fn parse_mul() {
        let source = "1*2;";
        let mut expected = Chunk::new();
        expected.push_const(Value::Number(1.));
        expected.push_const(Value::Number(2.));
        expected.push_opcode(OpCode::OpMultiply);
        expected.push_opcode(OpCode::OpPop);
        expected.push_opcode(OpCode::OpReturn);

        let mut fun = Function::new();
        let mut parser = Parser::new(&source, &mut fun, FunctionType::Script);
        let _ = parser.compile().unwrap();
        assert_eq!(expected, fun.code);
    }

    #[test]
    fn parse_div() {
        let source = "1/2;";
        let mut expected = Chunk::new();
        expected.push_const(Value::Number(1.));
        expected.push_const(Value::Number(2.));
        expected.push_opcode(OpCode::OpDivide);
        expected.push_opcode(OpCode::OpPop);
        expected.push_opcode(OpCode::OpReturn);

        let mut fun = Function::new();
        let mut parser = Parser::new(&source, &mut fun, FunctionType::Script);
        let _ = parser.compile().unwrap();
        assert_eq!(expected, fun.code);
    }

    #[test]
    fn parse_expr() {
        let source = "(10 + 10) * (3 - 1 / (4));";
        let mut expected = Chunk::new();
        expected.push_const(Value::Number(10.));
        expected.push_const(Value::Number(10.));
        expected.push_opcode(OpCode::OpAdd);
        expected.push_const(Value::Number(3.));
        expected.push_const(Value::Number(1.));
        expected.push_const(Value::Number(4.));
        expected.push_opcode(OpCode::OpDivide);
        expected.push_opcode(OpCode::OpSubtract);
        expected.push_opcode(OpCode::OpMultiply);
        expected.push_opcode(OpCode::OpPop);
        expected.push_opcode(OpCode::OpReturn);

        let mut fun = Function::new();
        let mut parser = Parser::new(&source, &mut fun, FunctionType::Script);
        let _ = parser.compile().unwrap();
        assert_eq!(expected, fun.code);
    }

    #[test]
    fn parse_many_parens() {
        let source = "(((1+2)-3)*4)/5;";
        let mut expected = Chunk::new();
        expected.push_const(Value::Number(1.));
        expected.push_const(Value::Number(2.));
        expected.push_opcode(OpCode::OpAdd);
        expected.push_const(Value::Number(3.));
        expected.push_opcode(OpCode::OpSubtract);
        expected.push_const(Value::Number(4.));
        expected.push_opcode(OpCode::OpMultiply);
        expected.push_const(Value::Number(5.));
        expected.push_opcode(OpCode::OpDivide);
        expected.push_opcode(OpCode::OpPop);
        expected.push_opcode(OpCode::OpReturn);

        let mut fun = Function::new();
        let mut parser = Parser::new(&source, &mut fun, FunctionType::Script);
        let _ = parser.compile().unwrap();
        assert_eq!(expected, fun.code);
    }

    #[test]
    fn parse_book_example() {
        let source = "!(5 - 4 > 3 * 2 == !nil);";
        let mut expected = Chunk::new();
        // 5 - 4
        expected.push_const(Value::Number(5.));
        expected.push_const(Value::Number(4.));
        expected.push_opcode(OpCode::OpSubtract);
        // 3 * 2
        expected.push_const(Value::Number(3.));
        expected.push_const(Value::Number(2.));
        expected.push_opcode(OpCode::OpMultiply);
        // (5 - 4) > (3 * 2)
        expected.push_opcode(OpCode::OpGreater);
        // !nil
        expected.push_opcode(OpCode::OpNil);
        expected.push_opcode(OpCode::OpNot);
        // (5 - 4) > (3 * 2) == !nil
        // !((5 - 4) > (3 * 2) == !nil)
        expected.push_opcode(OpCode::OpEq);
        expected.push_opcode(OpCode::OpNot);
        expected.push_opcode(OpCode::OpPop);
        expected.push_opcode(OpCode::OpReturn);

        let mut fun = Function::new();
        let mut parser = Parser::new(&source, &mut fun, FunctionType::Script);
        let _ = parser.compile().unwrap();
        assert_eq!(expected, fun.code);
    }

    #[test]
    fn parse_string() {
        let source = "\"Hello, world!\";";
        let mut expected = Chunk::new();
        expected.push_const(Value::String(source[1..14].to_string()));
        expected.push_opcode(OpCode::OpPop);
        expected.push_opcode(OpCode::OpReturn);

        let mut fun = Function::new();
        let mut parser = Parser::new(&source, &mut fun, FunctionType::Script);
        let _ = parser.compile().unwrap();
        assert_eq!(expected, fun.code);
    }

    #[test]
    fn parse_defvar() {
        let source = "var eita = 1;";
        let mut expected = Chunk::new();
        expected.push_value(Value::String("eita".to_string()));
        expected.push_const(Value::Number(1.));
        expected.push_defvar(OpCode::OpDefGlobal(1));
        expected.push_opcode(OpCode::OpReturn);

        let mut fun = Function::new();
        let mut parser = Parser::new(&source, &mut fun, FunctionType::Script);
        let _ = parser.compile().unwrap();
        assert_eq!(expected, fun.code);
    }

    #[test]
    fn parse_getvar() {
        let source = "var eita = 1; print eita;";
        let mut expected = Chunk::new();
        expected.push_value(Value::String("eita".to_string()));
        expected.push_const(Value::Number(1.));
        expected.push_defvar(OpCode::OpDefGlobal(1));
        // We have to add "eita" again since everytime a variable
        // is encountered we add it to the constants table
        expected.push_value(Value::String("eita".to_string()));
        expected.push_getvar(OpCode::OpGetGlobal(3));
        expected.push_opcode(OpCode::OpPrint);
        expected.push_opcode(OpCode::OpReturn);

        let mut fun = Function::new();
        let mut parser = Parser::new(&source, &mut fun, FunctionType::Script);
        let _ = parser.compile().unwrap();
        assert_eq!(expected, fun.code);
    }

    #[test]
    fn global_inside_scope() {
        let source = "var eita = 1; {print eita;}";
        let mut expected = Chunk::new();
        expected.push_value(Value::String("eita".to_string()));
        expected.push_const(Value::Number(1.));
        expected.push_defvar(OpCode::OpDefGlobal(1));
        // We have to add "eita" again since everytime a variable
        // is encountered we add it to the constants table
        expected.push_value(Value::String("eita".to_string()));
        expected.push_getvar(OpCode::OpGetGlobal(3));
        expected.push_opcode(OpCode::OpPrint);
        expected.push_opcode(OpCode::OpReturn);

        let mut fun = Function::new();
        let mut parser = Parser::new(&source, &mut fun, FunctionType::Script);
        let _ = parser.compile().unwrap();
        assert_eq!(expected, fun.code);
    }

    #[test]
    fn scoped_var() {
        let source = "{var eita = 1; print eita;}";
        let mut expected = Chunk::new();
        expected.push_const(Value::Number(1.));
        expected.push_getvar(OpCode::OpGetLocal(1));
        expected.push_opcode(OpCode::OpPrint);
        expected.push_opcode(OpCode::OpPop);
        expected.push_opcode(OpCode::OpReturn);

        let mut fun = Function::new();
        let mut parser = Parser::new(&source, &mut fun, FunctionType::Script);
        let _ = parser.compile().unwrap();
        assert_eq!(expected, fun.code);
    }

    #[test]
    fn nested_scope_var() {
        let source = "{var eita = 1; {var eita = \"value\"; print eita;} print eita;}";
        let mut expected = Chunk::new();
        expected.push_const(Value::Number(1.));
        expected.push_const(Value::String("value".to_string()));
        expected.push_getvar(OpCode::OpGetLocal(2));
        expected.push_opcode(OpCode::OpPrint);
        expected.push_opcode(OpCode::OpPop);
        expected.push_getvar(OpCode::OpGetLocal(1));
        expected.push_opcode(OpCode::OpPrint);
        expected.push_opcode(OpCode::OpPop);
        expected.push_opcode(OpCode::OpReturn);

        let mut fun = Function::new();
        let mut parser = Parser::new(&source, &mut fun, FunctionType::Script);
        let _ = parser.compile().unwrap();
        assert_eq!(expected, fun.code);
    }
}
