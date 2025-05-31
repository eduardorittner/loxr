use ast;
use lex::{LexError, T, token::Token};

use crate::Parser;

impl<'source, I> Parser<'source, I>
where
    I: Iterator<Item = Result<Token, LexError>>,
{
    pub fn statement(&mut self) -> ast::Stmt {
        match self.peek() {
            Some(T!['{']) => self.block(),
            Some(T![print]) => self.print(),
            _ => ast::Stmt::Expr {
                expr: self.expression(),
            },
        }
    }

    pub(crate) fn block(&mut self) -> ast::Stmt {
        self.consume(T!['{']);
        let mut decls = Vec::new();
        while !self.peek_is(T!['}']) {
            decls.push(self.declaration());
        }
        self.consume(T!['}']);

        ast::Stmt::Block(ast::Block(decls))
    }

    pub(crate) fn print(&mut self) -> ast::Stmt {
        self.consume(T![print]);
        println!("hre");
        let expr = self.expression();
        self.consume(T![;]);
        ast::Stmt::Print { expr }
    }
}

#[cfg(test)]
mod tests {
    use crate::Parser;
    use ast;

    #[test]
    fn test_print_stmt() {
        let source = "print ident;";
        let mut parser = Parser::new(source);

        let stmt = parser.statement();
        assert_eq!(
            stmt,
            ast::Stmt::Print {
                expr: ast::Expr::Ident("ident".to_string())
            }
        )
    }
}
