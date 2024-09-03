use crate::{
    lex::{Token, TokenKind},
    Chunk, Lexer, OpCode, Value,
};
use miette::WrapErr;
use std::collections::HashMap;
use std::fmt;

pub type ParseFn<'de> = fn(&mut Parser<'de>, TokenKind) -> miette::Result<()>;

#[derive(Debug, PartialEq, PartialOrd, Eq)]
pub struct ParseRule<'de> {
    infix: Option<ParseFn<'de>>,
    prefix: Option<ParseFn<'de>>,
    prec: Precedence,
}

impl From<&ParseRule<'_>> for ParseRule<'_> {
    fn from(value: &ParseRule) -> Self {
        value.into()
    }
}

pub struct Parser<'de> {
    lexer: Lexer<'de>,
    whole: &'de str,
    chunk: &'de mut Chunk,
    rules: HashMap<TokenKind, ParseRule<'de>>,
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
            rules: HashMap::from([
                (
                    LeftParen,
                    ParseRule {
                        prefix: None,
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
                        prec: Precedence::None,
                    },
                ),
                (
                    Plus,
                    ParseRule {
                        prefix: None,
                        infix: Some(Parser::parse_binary),
                        prec: Precedence::None,
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
                        prec: Precedence::None,
                    },
                ),
                (
                    Slash,
                    ParseRule {
                        prefix: None,
                        infix: Some(Parser::parse_binary),
                        prec: Precedence::None,
                    },
                ),
                (
                    Bang,
                    ParseRule {
                        prefix: None,
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
                        infix: None,
                        prec: Precedence::None,
                    },
                ),
                (
                    Greater,
                    ParseRule {
                        prefix: None,
                        infix: None,
                        prec: Precedence::None,
                    },
                ),
                (
                    BangEq,
                    ParseRule {
                        prefix: None,
                        infix: None,
                        prec: Precedence::None,
                    },
                ),
                (
                    EqEq,
                    ParseRule {
                        prefix: None,
                        infix: None,
                        prec: Precedence::None,
                    },
                ),
                (
                    LessEq,
                    ParseRule {
                        prefix: None,
                        infix: None,
                        prec: Precedence::None,
                    },
                ),
                (
                    GreaterEq,
                    ParseRule {
                        prefix: None,
                        infix: None,
                        prec: Precedence::None,
                    },
                ),
                (
                    TString,
                    ParseRule {
                        prefix: None,
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
                        prefix: None,
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

    pub fn get_rule(&self, kind: TokenKind) -> &ParseRule<'de> {
        self.rules
            .get(&kind)
            .expect("HashMap contains all possible TokenKind variants")
    }

    pub fn compile(&mut self) -> miette::Result<&Chunk> {
        self.parse_expr()?;
        self.chunk.push_opcode(OpCode::OpReturn);
        return Ok(self.chunk);
    }

    pub fn parse_expr(&mut self) -> miette::Result<()> {
        self.parse_prec(Precedence::Assignment)
    }

    pub fn parse_prec(&mut self, prec: Precedence) -> miette::Result<()> {
        let token = self.lexer.next().unwrap()?;

        let rules = self.get_rule(token.kind);
        let mut current_prec = rules.prec;
        let prefix_rule = rules.prefix;

        let prefix_rule = match prefix_rule {
            Some(p) => p,
            //TODO: better miette error reporting
            None => return Err(miette::miette!("Unknown prefix rule for token: {token}")),
        };

        prefix_rule(self, token.kind)?;

        loop {
            if prec <= current_prec {
                break;
            }

            let token = match self.lexer.next() {
                Some(Ok(token)) => token,
                Some(Err(e)) => return Err(e).wrap_err("Expected expression after prefix"),
                None => return Ok(()),
            };

            let rule = self.get_rule(token.kind);

            let infix_rule = rule.infix.expect("Expected valid infix operator");
            current_prec = rule.prec;

            infix_rule(self, token.kind);
        }

        Ok(())
    }

    pub fn parse_group(&mut self) -> miette::Result<()> {
        self.parse_expr()?;

        self.lexer
            .expect(TokenKind::RightParen, "expected closing parentheses")?;

        Ok(())
    }

    pub fn parse_unary(&mut self, kind: TokenKind) -> miette::Result<()> {
        self.parse_prec(Precedence::Unary);

        match kind {
            TokenKind::Minus => self.chunk.push_opcode(OpCode::OpNegate),
            _ => unreachable!("by the callee"),
        }
        Ok(())
    }

    pub fn parse_binary(&mut self, kind: TokenKind) -> miette::Result<()> {
        let prec = self.get_rule(kind).prec;

        self.parse_prec(prec)?;

        match kind {
            TokenKind::Plus => self.chunk.push_opcode(OpCode::OpAdd),
            TokenKind::Minus => self.chunk.push_opcode(OpCode::OpSubtract),
            TokenKind::Star => self.chunk.push_opcode(OpCode::OpMultiply),
            TokenKind::Slash => self.chunk.push_opcode(OpCode::OpDivide),
            _ => unreachable!("By the callee"),
        }
        Ok(())
    }

    pub fn parse_literal(&mut self, kind: TokenKind) -> miette::Result<()> {
        match kind {
            TokenKind::False => self.chunk.push_opcode(OpCode::OpFalse),
            TokenKind::True => self.chunk.push_opcode(OpCode::OpTrue),
            TokenKind::Nil => self.chunk.push_opcode(OpCode::OpNil),
            TokenKind::Number(n) => self.chunk.push_const(Value::Number(n)),
            _ => unreachable!("By the callee"),
        }
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    pub fn parse_number() {
        let source = "1";
        let mut expected = Chunk::new();
        expected.push_const(Value::Number(1.));
        expected.push_opcode(OpCode::OpReturn);

        let mut chunk = Chunk::new();
        let mut parser = Parser::new(&source, &mut chunk);
        let _ = parser.compile().unwrap();
        assert_eq!(expected, chunk);
    }

    //#[test]
    // TODO: add this test when added Value string variant
    pub fn parse_string() {
        let source = "\"Hello, world!\"";
        let mut expected = Chunk::new();
        //expected.push_const(Value(source));
        expected.push_opcode(OpCode::OpReturn);

        let mut chunk = Chunk::new();
        let mut parser = Parser::new(&source, &mut chunk);
        let _ = parser.compile().unwrap();
        assert_eq!(expected, chunk);
    }
}
