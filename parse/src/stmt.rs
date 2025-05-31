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
            _ => {
                let expr = ast::Stmt::Expr {
                    expr: self.expression(),
                };
                self.consume(T![;]);
                expr
            }
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
    use ast::*;

    fn test_stmt(source: &str, expected: ast::Stmt) {
        let mut parser = Parser::new(source);

        let stmt = parser.statement();
        assert_eq!(stmt, expected)
    }

    #[test]
    fn test_print_stmt() {
        let source = "print ident;";

        test_stmt(
            source,
            ast::Stmt::Print {
                expr: ast::Expr::Ident("ident".to_string()),
            },
        );
    }

    #[test]
    fn test_expr_stmt() {
        test_stmt(
            "1 + 2;",
            ast::Stmt::Expr {
                expr: Expr::BinaryOp {
                    op: BinaryOp::Add,
                    rhs: Box::new(Expr::Literal(Lit::Number(2.))),
                    lhs: Box::new(Expr::Literal(Lit::Number(1.))),
                },
            },
        );

        test_stmt(
            "27;",
            ast::Stmt::Expr {
                expr: Expr::Literal(Lit::Number(27.)),
            },
        );

        test_stmt(
            "\"I am a string\";",
            ast::Stmt::Expr {
                expr: Expr::Literal(Lit::Str("I am a string".to_string())),
            },
        );

        test_stmt(
            "false;",
            ast::Stmt::Expr {
                expr: Expr::Literal(Lit::Bool(false)),
            },
        );

        test_stmt(
            "true;",
            ast::Stmt::Expr {
                expr: Expr::Literal(Lit::Bool(true)),
            },
        );
    }

    #[test]
    #[should_panic]
    fn test_expr_stmt_without_semicolon() {
        let source = "1 + 2";

        let mut parser = Parser::new(source);
        parser.statement();
    }
}
