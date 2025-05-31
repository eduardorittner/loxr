use ast;
use lex::{LexError, T, token::Token};

use crate::Parser;

impl<'source, I> Parser<'source, I>
where
    I: Iterator<Item = Result<Token, LexError>>,
{
    pub fn declaration(&mut self) -> ast::Decl {
        match self.peek() {
            Some(T![var]) => self.var_declaration(),
            Some(T![fun]) => self.fun_declaration(),
            Some(T![class]) => self.class_declaration(),
            _ => ast::Decl::Stmt {
                stmt: self.statement(),
            },
        }
    }

    fn var_declaration(&mut self) -> ast::Decl {
        self.consume(T![var]);

        let name = self.consume_ident();

        let expr = if self.peek_is(T![=]) {
            self.expression()
        } else {
            // Zero-initialize variable
            ast::Expr::Literal(ast::Lit::Nil)
        };
        self.consume(T![;]);

        ast::Decl::Var { name, value: expr }
    }

    fn fun_declaration(&mut self) -> ast::Decl {
        self.consume(T![fun]);

        let name = self.consume_ident();

        self.consume(T!['(']);
        let mut args = Vec::new();
        while !self.peek_is(T![')']) {
            args.push(self.consume_ident());

            if self.peek_is(T![,]) {
                self.consume(T![,]);
            }
        }
        self.consume(T![')']);

        let mut block = Vec::new();

        self.consume(T!['{']);
        while !self.peek_is(T!['}']) {
            block.push(self.declaration())
        }
        self.consume(T!['}']);

        ast::Decl::Fun(ast::FunDecl {
            name,
            args,
            body: todo!(),
        })
    }

    fn class_declaration(&mut self) -> ast::Decl {
        todo!()
    }
}
