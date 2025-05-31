use lex::T;
use lex::token::TokenKind;
use parse::Parser;
use vm_ast::VM;

pub fn main() {
    let mut parser = Parser::new("{1 + 2 * 3}");
    let decls = parser.parse();
    let mut vm = VM::new();
    for decl in decls {
        match decl {
            ast::Decl::Class => todo!(),
            ast::Decl::Fun(fun_decl) => todo!(),
            ast::Decl::Var { name, value } => todo!(),
            ast::Decl::Stmt { stmt } => match stmt {
                ast::Stmt::While { cond, body } => todo!(),
                ast::Stmt::Return { expr } => todo!(),
                ast::Stmt::Print { expr } => todo!(),
                ast::Stmt::If {
                    cond,
                    iftrue,
                    iffalse,
                } => todo!(),
                ast::Stmt::For {
                    initial,
                    cond,
                    increment,
                    body,
                } => todo!(),
                ast::Stmt::Expr { expr } => {
                    println!("{:?}", vm.eval_expr(expr))
                }
                ast::Stmt::Block(block) => {
                    vm.execute_stmt(ast::Stmt::Block(block));
                }
            },
        }
    }
}
