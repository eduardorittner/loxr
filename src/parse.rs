use crate::{lex::Token, lex::TokenKind, Chunk, Lexer, OpCode, Value};
use miette::{LabeledSpan, WrapErr};
use std::collections::HashMap;

type ParseFn<'de> = fn(&mut Parser<'de>, Token<'de>, bool) -> miette::Result<()>;

#[derive(Debug, PartialEq, PartialOrd, Eq)]
struct ParseRule<'de> {
    infix: Option<ParseFn<'de>>,
    prefix: Option<ParseFn<'de>>,
    prec: Precedence,
}

pub struct Parser<'de> {
    lexer: Lexer<'de>,
    whole: &'de str,
    chunk: &'de mut Chunk,
    rules: HashMap<TokenKind, ParseRule<'de>>,
    scope: Scope<'de>,
}

#[derive(Debug)]
struct Local<'de> {
    name: &'de str,
    depth: u8, // Supports at max 256 nested scopes
}

#[derive(Debug)]
struct Scope<'de> {
    locals: Vec<Local<'de>>,
    scope_depth: u8,
}

impl<'de> Scope<'de> {
    fn new() -> Self {
        Self {
            locals: Vec::new(),
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
}

impl<'de> Local<'de> {
    fn new(name: &'de str, depth: u8) -> Self {
        Self { name, depth }
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

impl<'de> Parser<'de> {
    pub fn new(input: &'de str, chunk: &'de mut Chunk) -> Self {
        use TokenKind::*;
        Parser {
            lexer: Lexer::new(input),
            whole: input,
            chunk,
            scope: Scope::new(),
            rules: HashMap::from([
                (
                    LeftParen,
                    ParseRule {
                        prefix: Some(Parser::parse_group),
                        infix: None,
                        prec: Precedence::None,
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
                        infix: None,
                        prec: Precedence::None,
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
                        infix: None,
                        prec: Precedence::None,
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
            .take_while(|x| x.depth == self.scope.scope_depth);

        let n = local_vars.fold(0, |acc: usize, x| {
            self.chunk.push_opcode(OpCode::OpPop);
            acc + 1
        });

        self.scope.locals.truncate(self.scope.locals.len() - n);
        self.scope.end_scope();
    }

    fn get_rule(&self, kind: TokenKind) -> &ParseRule<'de> {
        self.rules
            .get(&kind)
            .expect("HashMap contains all possible TokenKind variants")
    }

    pub fn compile(&mut self) -> miette::Result<&Chunk> {
        while self.lexer.peek().is_some_and(|x| x.is_ok()) {
            self.parse_decl()?;
        }
        self.chunk.push_opcode(OpCode::OpReturn);
        Ok(self.chunk)
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
            _ => self.parse_statement(),
        }
    }

    fn parse_var_decl(&mut self) -> miette::Result<()> {
        let _ = self
            .lexer
            .expect(TokenKind::Var, "Caller should have checked");
        let token = self
            .lexer
            .expect(TokenKind::Ident, "Expected identifier after 'var' keyword")?;

        let index = self.parse_variable(token, true)?;

        // TODO: miette error reporting
        let token = self.lexer.peek().expect("By the callee").as_ref().unwrap();
        match token.kind {
            TokenKind::Eq => {
                let _ = self.lexer.next();
                self.parse_expr()?;
            }
            _ => self.chunk.push_opcode(OpCode::OpNil),
        }
        let _ = self
            .lexer
            .expect(TokenKind::Semicolon, "Expected ';' after expression")?;

        self.define_var(index);
        Ok(())
    }

    fn define_var(&mut self, index: Option<u8>) {
        match index {
            Some(index) => self.chunk.push_defvar(index, OpCode::OpDefGlobal),
            None => (),
        }
    }

    fn resolve_local_var(&mut self, token: Token) -> Option<u8> {
        self.scope
            .locals
            .iter()
            .position(|x| x.name == token.source)
            .map(|x| x as u8)
    }

    fn parse_ident(&mut self, token: Token<'de>, can_assign: bool) -> miette::Result<()> {
        let (index, set_op, get_op) = if let Some(index) = self.resolve_local_var(token) {
            (Some(index), OpCode::OpDefLocal, OpCode::OpGetLocal)
        } else {
            (
                self.parse_variable(token, can_assign)?,
                OpCode::OpDefGlobal,
                OpCode::OpGetGlobal,
            )
        };

        match token.kind {
            TokenKind::Eq => {
                if can_assign {
                    self.parse_expr()?;
                    self.chunk.push_defvar(index.unwrap(), get_op)
                };
            }
            _ => {
                self.chunk.push_getvar(index.unwrap(), get_op);
            }
        };
        Ok(())
    }

    fn declare_var(&mut self, token: Token<'de>) -> miette::Result<()> {
        if self.scope.scope_depth == 0 {
            return Ok(());
        }
        if self
            .scope
            .locals
            .iter()
            .fold(false, |_, e| e.name == token.source)
        {
            return Err(miette::miette!(
                "A variable with the name \"{}\" already exists",
                token.source
            ));
        }

        self.scope.add_local(&token.source);
        Ok(())
    }

    // TODO: Return u8 instead of Option<u8>
    fn parse_variable(&mut self, token: Token<'de>, _: bool) -> miette::Result<Option<u8>> {
        self.chunk.push_const(Value::String(token.to_string()));
        self.declare_var(token)?;
        if self.scope.scope_depth > 0 {
            return Ok(None);
        }
        Ok(Some(self.chunk.last_const()))
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
            _ => self.parse_expr_statement()?,
        };
        Ok(())
    }

    fn parse_block(&mut self) -> miette::Result<()> {
        loop {
            let token = self.lexer.peek();
            match token {
                Some(Ok(token)) => {
                    if token.kind == TokenKind::RightBracket {
                        let _ = self.lexer.next();
                        return Ok(());
                    }
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
        self.chunk.push_opcode(OpCode::OpPop);
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
            }.with_source_code(self.whole.to_string())),
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
                }.with_source_code(self.whole.to_string())),
            };
            let _ = self.lexer.next();

            infix_rule(self, token, can_assign)?;
        }

        if can_assign && self.lexer.peek().unwrap().as_ref().unwrap().kind == TokenKind::Eq {
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

    fn parse_unary(&mut self, token: Token, _: bool) -> miette::Result<()> {
        self.parse_prec(Precedence::Unary)?;

        match token.kind {
            TokenKind::Minus => self.chunk.push_opcode(OpCode::OpNegate),
            TokenKind::Bang => self.chunk.push_opcode(OpCode::OpNot),
            _ => unreachable!("by the callee"),
        }
        Ok(())
    }

    fn parse_binary(&mut self, token: Token, _: bool) -> miette::Result<()> {
        let prec = self.get_rule(token.kind).prec;

        self.parse_prec(prec)?;

        match token.kind {
            TokenKind::Plus => self.chunk.push_opcode(OpCode::OpAdd),
            TokenKind::Minus => self.chunk.push_opcode(OpCode::OpSubtract),
            TokenKind::Star => self.chunk.push_opcode(OpCode::OpMultiply),
            TokenKind::Slash => self.chunk.push_opcode(OpCode::OpDivide),
            TokenKind::BangEq => self.chunk.push_opcodes(OpCode::OpEq, OpCode::OpNot),
            TokenKind::EqEq => self.chunk.push_opcode(OpCode::OpEq),
            TokenKind::Greater => self.chunk.push_opcode(OpCode::OpGreater),
            TokenKind::GreaterEq => self.chunk.push_opcodes(OpCode::OpLess, OpCode::OpNot),
            TokenKind::Less => self.chunk.push_opcode(OpCode::OpLess),
            TokenKind::LessEq => self.chunk.push_opcodes(OpCode::OpGreater, OpCode::OpNot),
            _ => unreachable!("By the callee"),
        }
        Ok(())
    }

    fn parse_literal(&mut self, token: Token, _: bool) -> miette::Result<()> {
        match token.kind {
            TokenKind::False => self.chunk.push_opcode(OpCode::OpFalse),
            TokenKind::True => self.chunk.push_opcode(OpCode::OpTrue),
            TokenKind::Nil => self.chunk.push_opcode(OpCode::OpNil),
            TokenKind::Number(n) => self.chunk.push_opconst(Value::Number(n)),
            TokenKind::TString => self.chunk.push_opconst(Value::String(token.to_string())),
            _ => unreachable!("By the callee"),
        }
        Ok(())
    }

    fn print_statement(&mut self) -> miette::Result<()> {
        self.parse_expr()?;
        let _ = self
            .lexer
            .expect(TokenKind::Semicolon, "Expected ';' after value.")?;
        self.chunk.push_opcode(OpCode::OpPrint);
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
        expected.push_opconst(Value::Number(1.));
        expected.push_opcode(OpCode::OpPop);
        expected.push_opcode(OpCode::OpReturn);

        let mut chunk = Chunk::new();
        let mut parser = Parser::new(&source, &mut chunk);
        let _ = parser.compile().unwrap();
        assert_eq!(expected, chunk);
    }

    #[test]
    fn parse_number_inside_parens() {
        let source = "(101);";
        let mut expected = Chunk::new();
        expected.push_opconst(Value::Number(101.));
        expected.push_opcode(OpCode::OpPop);
        expected.push_opcode(OpCode::OpReturn);

        let mut chunk = Chunk::new();
        let mut parser = Parser::new(&source, &mut chunk);
        let _ = parser.compile().unwrap();
        assert_eq!(expected, chunk);
    }

    #[test]
    fn parse_addition() {
        let source = "1+2;";
        let mut expected = Chunk::new();
        expected.push_opconst(Value::Number(1.));
        expected.push_opconst(Value::Number(2.));
        expected.push_opcode(OpCode::OpAdd);
        expected.push_opcode(OpCode::OpPop);
        expected.push_opcode(OpCode::OpReturn);

        let mut chunk = Chunk::new();
        let mut parser = Parser::new(&source, &mut chunk);
        let _ = parser.compile().unwrap();
        assert_eq!(expected, chunk);
    }

    #[test]
    fn parse_sub() {
        let source = "1-2;";
        let mut expected = Chunk::new();
        expected.push_opconst(Value::Number(1.));
        expected.push_opconst(Value::Number(2.));
        expected.push_opcode(OpCode::OpSubtract);
        expected.push_opcode(OpCode::OpPop);
        expected.push_opcode(OpCode::OpReturn);

        let mut chunk = Chunk::new();
        let mut parser = Parser::new(&source, &mut chunk);
        let _ = parser.compile().unwrap();
        assert_eq!(expected, chunk);
    }

    #[test]
    fn parse_mul() {
        let source = "1*2;";
        let mut expected = Chunk::new();
        expected.push_opconst(Value::Number(1.));
        expected.push_opconst(Value::Number(2.));
        expected.push_opcode(OpCode::OpMultiply);
        expected.push_opcode(OpCode::OpPop);
        expected.push_opcode(OpCode::OpReturn);

        let mut chunk = Chunk::new();
        let mut parser = Parser::new(&source, &mut chunk);
        let _ = parser.compile().unwrap();
        assert_eq!(expected, chunk);
    }

    #[test]
    fn parse_div() {
        let source = "1/2;";
        let mut expected = Chunk::new();
        expected.push_opconst(Value::Number(1.));
        expected.push_opconst(Value::Number(2.));
        expected.push_opcode(OpCode::OpDivide);
        expected.push_opcode(OpCode::OpPop);
        expected.push_opcode(OpCode::OpReturn);

        let mut chunk = Chunk::new();
        let mut parser = Parser::new(&source, &mut chunk);
        let _ = parser.compile().unwrap();
        assert_eq!(expected, chunk);
    }

    #[test]
    fn parse_expr() {
        let source = "(10 + 10) * (3 - 1 / (4));";
        let mut expected = Chunk::new();
        expected.push_opconst(Value::Number(10.));
        expected.push_opconst(Value::Number(10.));
        expected.push_opcode(OpCode::OpAdd);
        expected.push_opconst(Value::Number(3.));
        expected.push_opconst(Value::Number(1.));
        expected.push_opconst(Value::Number(4.));
        expected.push_opcode(OpCode::OpDivide);
        expected.push_opcode(OpCode::OpSubtract);
        expected.push_opcode(OpCode::OpMultiply);
        expected.push_opcode(OpCode::OpPop);
        expected.push_opcode(OpCode::OpReturn);

        let mut chunk = Chunk::new();
        let mut parser = Parser::new(&source, &mut chunk);
        let _ = parser.compile().unwrap();
        assert_eq!(expected, chunk);
    }

    #[test]
    fn parse_many_parens() {
        let source = "(((1+2)-3)*4)/5;";
        let mut expected = Chunk::new();
        expected.push_opconst(Value::Number(1.));
        expected.push_opconst(Value::Number(2.));
        expected.push_opcode(OpCode::OpAdd);
        expected.push_opconst(Value::Number(3.));
        expected.push_opcode(OpCode::OpSubtract);
        expected.push_opconst(Value::Number(4.));
        expected.push_opcode(OpCode::OpMultiply);
        expected.push_opconst(Value::Number(5.));
        expected.push_opcode(OpCode::OpDivide);
        expected.push_opcode(OpCode::OpPop);
        expected.push_opcode(OpCode::OpReturn);

        let mut chunk = Chunk::new();
        let mut parser = Parser::new(&source, &mut chunk);
        let _ = parser.compile().unwrap();
        assert_eq!(expected, chunk);
    }

    #[test]
    fn parse_book_example() {
        let source = "!(5 - 4 > 3 * 2 == !nil);";
        let mut expected = Chunk::new();
        // 5 - 4
        expected.push_opconst(Value::Number(5.));
        expected.push_opconst(Value::Number(4.));
        expected.push_opcode(OpCode::OpSubtract);
        // 3 * 2
        expected.push_opconst(Value::Number(3.));
        expected.push_opconst(Value::Number(2.));
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

        let mut chunk = Chunk::new();
        let mut parser = Parser::new(&source, &mut chunk);
        let _ = parser.compile().unwrap();
        assert_eq!(expected, chunk);
    }

    #[test]
    fn parse_string() {
        let source = "\"Hello, world!\";";
        let mut expected = Chunk::new();
        expected.push_opconst(Value::String(source[1..14].to_string()));
        expected.push_opcode(OpCode::OpPop);
        expected.push_opcode(OpCode::OpReturn);

        let mut chunk = Chunk::new();
        let mut parser = Parser::new(&source, &mut chunk);
        let _ = parser.compile().unwrap();
        assert_eq!(expected, chunk);
    }

    #[test]
    fn parse_defvar() {
        let source = "var eita = 1;";
        let mut expected = Chunk::new();
        expected.push_const(Value::String("eita".to_string()));
        expected.push_opconst(Value::Number(1.));
        expected.push_defvar(0, OpCode::OpDefGlobal);
        expected.push_opcode(OpCode::OpReturn);

        let mut chunk = Chunk::new();
        let mut parser = Parser::new(&source, &mut chunk);
        let _ = parser.compile().unwrap();
        assert_eq!(expected, chunk);
    }

    #[test]
    fn parse_getvar() {
        let source = "var eita = 1; print eita;";
        let mut expected = Chunk::new();
        expected.push_const(Value::String("eita".to_string()));
        expected.push_opconst(Value::Number(1.));
        expected.push_defvar(0, OpCode::OpDefGlobal);
        // We have to add "eita" again since everytime a variable
        // is encountered we add it to the constants table
        expected.push_const(Value::String("eita".to_string()));
        expected.push_getvar(2, OpCode::OpGetGlobal);
        expected.push_opcode(OpCode::OpPrint);
        expected.push_opcode(OpCode::OpReturn);

        let mut chunk = Chunk::new();
        let mut parser = Parser::new(&source, &mut chunk);
        let _ = parser.compile().unwrap();
        assert_eq!(expected, chunk);
    }
}
