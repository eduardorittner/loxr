mod decl;
mod expr;
mod stmt;

use std::iter::Peekable;

use lex::token::{Token, TokenKind};
use lex::{LexError, Lexer, T};

pub struct Parser<'source, I>
where
    I: Iterator<Item = Result<Token, LexError>>,
{
    input: &'source str,
    tokens: Peekable<I>,
}

impl<'source> Parser<'source, Lexer<'source>> {
    pub fn new(source: &'source str) -> Self {
        Self {
            input: source,
            tokens: Lexer::new(source).peekable(),
        }
    }
}

impl<'source, I> Parser<'source, I>
where
    I: Iterator<Item = Result<Token, LexError>>,
{
    /// Get a token's source text
    pub(crate) fn source(&self, token: Token) -> &'source str {
        token.text(self.input)
    }

    /// Get the next token, unwrapping on a lexer error
    pub(crate) fn next(&mut self) -> Option<Token> {
        match self.tokens.next() {
            Some(Ok(t)) => Some(t),
            // TODO return err
            Some(Err(e)) => panic!("{:?}", e),
            None => None,
        }
    }

    /// Peek at the next token, unwrapping on a lexer error
    pub(crate) fn peek(&mut self) -> Option<TokenKind> {
        match self.tokens.peek() {
            Some(Ok(t)) => Some(t.kind),
            Some(Err(e)) => panic!("{:?}", e),
            None => None,
        }
    }

    /// Check if the next token is the same as 'kind'
    pub(crate) fn peek_is(&mut self, kind: TokenKind) -> bool {
        self.peek().unwrap() == kind
    }

    /// Consume a token if it matches 'kind', panic otherwise
    pub(crate) fn consume(&mut self, kind: TokenKind) -> Token {
        let token = self.next().expect("Expected token");
        assert_eq!(token.kind, kind);
        token
    }

    /// Consumes the next token as an identifier and return its corresponding
    /// string
    pub(crate) fn consume_ident(&mut self) -> String {
        let token = self.consume(T![ident]);
        self.source(token).to_string()
    }

    pub fn parse(&mut self) -> Vec<ast::Decl> {
        let mut decls = Vec::new();
        loop {
            match self.peek() {
                Some(_) => {
                    decls.push(self.declaration());
                }
                None => break,
            }
        }
        decls
    }
}
