use lex::{
    LexError, T,
    token::{Token, TokenKind},
};

use crate::Parser;
use ast;

impl<'source, I> Parser<'source, I>
where
    I: Iterator<Item = Result<Token, LexError>>,
{
    pub fn expression(&mut self) -> ast::Expr {
        self.parse_expression(0)
    }

    pub(crate) fn parse_expression(&mut self, binding_power: u8) -> ast::Expr {
        let mut lhs = match self.peek().unwrap() {
            lit @ T![num] | lit @ T![string] | lit @ T![true] | lit @ T![false] | lit @ T![nil] => {
                let lit_text = {
                    let lit_token = self.next().unwrap();
                    self.source(lit_token)
                };
                let lit = match lit {
                    T![num] => ast::Lit::Number(
                        lit_text
                            .parse()
                            .expect(&format!("Invalid float literal: {}", lit_text)),
                    ),
                    T![string] => ast::Lit::Str(lit_text.to_string()),
                    T![true] => ast::Lit::Bool(true),
                    T![false] => ast::Lit::Bool(false),
                    T![nil] => ast::Lit::Nil,
                    _ => unreachable!(),
                };
                ast::Expr::Literal(lit)
            }
            T![ident] => {
                let name = {
                    let ident_token = self.next().unwrap();
                    self.source(ident_token).to_string()
                };

                if !self.peek_is(T!['(']) {
                    // Plain identifier
                    ast::Expr::Ident(name)
                } else {
                    // Function call

                    self.consume(T!['(']);
                    let mut args = Vec::new();
                    while !self.peek_is(T![')']) {
                        let arg = self.parse_expression(0);
                        args.push(arg);

                        if self.peek_is(T![,]) {
                            self.consume(T![,]);
                        }
                    }
                    self.consume(T![')']);
                    ast::Expr::FunCall {
                        fn_name: name,
                        args,
                    }
                }
            }
            T!['('] => {
                self.consume(T!['(']);
                let expr = self.parse_expression(0);
                self.consume(T![')']);
                expr
            }
            prefix_op @ T![-] | prefix_op @ T![!] => {
                self.consume(prefix_op);
                let ((), right_bp) = prefix_op.prefix_bp();
                let expr = self.parse_expression(right_bp);
                ast::Expr::PrefixOp {
                    op: prefix_op.into(),
                    rhs: Box::new(expr),
                }
            }
            kind => {
                panic!("Expected start of expression, got: {}", kind)
            }
        };

        loop {
            if let Some(op) = self.peek() {
                match op {
                    op @ T![+] | op @ T![-] | op @ T![*] | op @ T![/] => op,
                    T![')'] | T!['}'] | T![,] | T![;] => break,
                    kind => panic!("Expected binary operator, got: {:?}", kind),
                };

                if let Some((left_bp, right_bp)) = op.infix_bp() {
                    if left_bp < binding_power {
                        // Previous operator has higher precedence than the current operator
                        //  so we return now
                        break;
                    }
                    self.consume(op);

                    let rhs = self.parse_expression(right_bp);
                    lhs = ast::Expr::BinaryOp {
                        op: op.into(),
                        lhs: Box::new(lhs),
                        rhs: Box::new(rhs),
                    };
                    continue;
                }
            }
            break;
        }

        lhs
    }
}

trait Operator {
    /// Prefix operators bind their operand to the right.
    fn prefix_bp(&self) -> ((), u8);
    /// Infix operators bind their operand to the right and left.
    fn infix_bp(&self) -> Option<(u8, u8)>;
    /// Postfix operators bind their operand to the left.
    fn postfix_bp(&self) -> Option<(u8, ())>;
}

impl Operator for TokenKind {
    fn prefix_bp(&self) -> ((), u8) {
        match self {
            T![-] | T![!] => ((), 51),
            _ => unreachable!(),
        }
    }

    fn infix_bp(&self) -> Option<(u8, u8)> {
        match self {
            // Factor
            T![*] | T![/] => Some((11, 12)),
            // Term
            T![+] | T![-] => Some((9, 10)),
            // Comparison
            T![>] | T![<] | T![<=] | T![>=] => Some((7, 8)),
            // Equality
            T![==] | T![!=] => Some((5, 6)),
            // And
            T![and] => Some((3, 4)),
            // Or
            T![or] => Some((1, 2)),
            _ => None,
        }
    }

    fn postfix_bp(&self) -> Option<(u8, ())> {
        todo!()
    }
}

#[cfg(test)]
mod tests {
    use crate::Parser;
    use ast;

    #[test]
    fn test_literal_float() {
        let source = "123.1";
        let mut parser = Parser::new(source);
        assert_eq!(
            parser.expression(),
            ast::Expr::Literal(ast::Lit::Number(123.1))
        );
    }

    #[test]
    fn test_literal_float_zero() {
        let source = "0.1";
        let mut parser = Parser::new(source);
        assert_eq!(
            parser.expression(),
            ast::Expr::Literal(ast::Lit::Number(0.1))
        );
    }

    #[test]
    fn test_literal_string() {
        let source = "\"my stringy\"";
        let mut parser = Parser::new(source);
        assert_eq!(
            parser.expression(),
            ast::Expr::Literal(ast::Lit::Str("my stringy".to_string()))
        );
    }

    #[test]
    fn test_fn_call() {
        let source = "myfunc(foo, bar, baz)";
        let mut parser = Parser::new(source);
        let expr = parser.expression();
        println!("{:?}", expr);
        assert_eq!(
            expr,
            ast::Expr::FunCall {
                fn_name: "myfunc".to_string(),
                args: vec![
                    ast::Expr::Ident("foo".to_string()),
                    ast::Expr::Ident("bar".to_string()),
                    ast::Expr::Ident("baz".to_string())
                ]
            }
        );
    }

    #[test]
    fn test_fn_call_literals() {
        let source = "myfunc(-1, foo(3), \"argy\")";
        let mut parser = Parser::new(source);
        let expr = parser.expression();
        println!("{:?}", expr);
        assert_eq!(
            expr,
            ast::Expr::FunCall {
                fn_name: "myfunc".to_string(),
                args: vec![
                    ast::Expr::PrefixOp {
                        op: ast::PrefixOp::Neg,
                        rhs: Box::new(ast::Expr::Literal(ast::Lit::Number(1.)))
                    },
                    ast::Expr::FunCall {
                        fn_name: "foo".to_string(),
                        args: vec![ast::Expr::Literal(ast::Lit::Number(3.))]
                    },
                    ast::Expr::Literal(ast::Lit::Str("argy".to_string())),
                ]
            }
        );
    }

    #[test]
    fn test_binary_expr() {
        fn parse(source: &str) -> ast::Expr {
            let mut parser = Parser::new(source);
            parser.expression()
        }

        let expr = parse("4+2*3");
        assert_eq!(expr.to_string(), "(4 + (2 * 3))");

        let expr = parse("4*2+3");
        assert_eq!(expr.to_string(), "((4 * 2) + 3)");

        assert_eq!(parse("(((4*2))+(3))"), parse("4*2+3"));

        assert_eq!(parse("4+(2*3)"), parse("4+2*3"));
    }
}
