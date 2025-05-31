use ast::*;
use parse::Parser;
use std::{collections::HashMap, rc::Rc};

pub struct VM {
    // TODO index functions by an ID
    functions: HashMap<String, Rc<(Vec<String>, Block)>>,
    scopes: Vec<HashMap<String, Lit>>,
}

impl VM {
    pub fn new() -> Self {
        VM {
            functions: HashMap::new(),
            scopes: vec![HashMap::new()], // Start with global scope
        }
    }

    pub fn execute(&mut self, decls: Vec<ast::Decl>) -> Result<(), String> {
        for decl in decls {
            self.execute_decl(decl)?;
        }

        Ok(())
    }

    fn execute_decl(&mut self, decl: Decl) -> Result<(), String> {
        match decl {
            Decl::Fun(FunDecl { name, args, body }) => {
                self.functions.insert(name, Rc::new((args, body)));
                Ok(())
            }
            Decl::Class => todo!(),
            Decl::Stmt { stmt } => self.execute_stmt(stmt),
            _ => Ok(()),
        }
    }

    pub fn execute_stmt(&mut self, stmt: Stmt) -> Result<(), String> {
        match stmt {
            Stmt::Expr { expr } => {
                self.eval_expr(expr)?;
                Ok(())
            }
            Stmt::Print { expr } => {
                let value = self.eval_expr(expr)?;
                println!("{}", value.to_string());
                Ok(())
            }
            Stmt::Block(Block(block)) => {
                self.scopes.push(HashMap::new());
                for decl in block {
                    self.execute_decl(decl)?;
                }
                self.scopes.pop();
                Ok(())
            }
            Stmt::Return { expr } => Err("Return statements not implemented".into()),
            _ => Err("Unsupported statement type".into()),
        }
    }

    pub fn eval_expr(&mut self, expr: Expr) -> Result<Lit, String> {
        match expr {
            Expr::Literal(lit) => Ok(lit),
            Expr::Ident(name) => self
                .scopes
                .iter()
                .rev()
                .find_map(|scope| scope.get(&name).cloned())
                .ok_or_else(|| format!("Undefined variable: {}", name)),
            Expr::PrefixOp { op, rhs } => {
                let rhs_val = self.eval_expr(*rhs)?;
                match op {
                    ast::PrefixOp::Neg => match rhs_val {
                        Lit::Number(n) => Ok(Lit::Number(-n)),
                        lit => Err(format!("Cannot negate non-number: {}", lit)),
                    },
                    ast::PrefixOp::Not => match rhs_val {
                        Lit::Bool(b) => Ok(Lit::Bool(!b)),
                        Lit::Number(_) | Lit::Str(_) | Lit::Fun(_) => Ok(Lit::Bool(false)), // "Truthy" values
                        Lit::Nil => Ok(Lit::Bool(true)),
                    },
                }
            }
            Expr::BinaryOp { op, lhs, rhs } => {
                // Helper macro for arithmetic operations
                macro_rules! arithmetic_op {
                    ($lhs:expr, $rhs:expr, $op:tt) => {{
                        if let (Lit::Number(a), Lit::Number(b)) = ($lhs, $rhs) {
                            Ok(Lit::Number(a $op b))
                        } else {
                            Err(format!("Invalid operands for arithmetic operation '$op': {} and {}", $lhs, $rhs))
                        }
                    }};
                }
                let lhs_val = self.eval_expr(*lhs)?;
                let rhs_val = self.eval_expr(*rhs)?;
                match op {
                    BinaryOp::Add => match (&lhs_val, &rhs_val) {
                        (Lit::Number(a), Lit::Number(b)) => Ok(Lit::Number(a + b)),
                        (Lit::Str(a), Lit::Str(b)) => Ok(Lit::Str(format!("{}{}", a, b))),
                        _ => Err("Invalid addition operands".into()),
                    },
                    BinaryOp::Sub => arithmetic_op!(&lhs_val, &rhs_val, -),
                    BinaryOp::Mul => arithmetic_op!(&lhs_val, &rhs_val, *),
                    BinaryOp::Div => arithmetic_op!(&lhs_val, &rhs_val, /),
                }
            }
            Expr::FunCall { fn_name, args } => self.call_function(&fn_name, args),
        }
    }

    fn call_function(&mut self, name: &str, args: Vec<Expr>) -> Result<Lit, String> {
        let evaluated_args: Vec<Lit> = args
            .into_iter()
            .map(|arg| self.eval_expr(arg))
            .collect::<Result<_, _>>()?;

        let func = self
            .functions
            .get(name)
            .ok_or_else(|| format!("Undefined function: {}", name))?
            .clone();

        if evaluated_args.len() != func.0.len() {
            return Err(format!(
                "Expected {} arguments, got {}",
                func.0.len(),
                evaluated_args.len()
            ));
        }

        self.scopes.push(HashMap::new());

        for (param, value) in func.0.iter().zip(evaluated_args) {
            self.current_scope().insert(param.clone(), value);
        }

        let mut result = Lit::Nil;
        for decl in &func.1.0 {
            if let ast::Decl::Stmt {
                stmt: Stmt::Return { expr },
            } = decl
            {
                // TODO
                // Simple return implementation (would need proper stack handling)
                break;
            }
            self.execute_decl(decl.clone())?;
        }

        self.scopes.pop();
        Ok(result)
    }

    fn current_scope(&mut self) -> &mut HashMap<String, Lit> {
        self.scopes.last_mut().expect("Always at least one scope")
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn test_expr(expr: Expr, expected: Lit) {
        let mut vm = VM::new();
        let result = vm.eval_expr(expr).unwrap();
        assert_eq!(result, expected);
    }

    #[test]
    fn test_lit_expr() {
        test_expr(Expr::Literal(Lit::Number(1.)), Lit::Number(1.));
        test_expr(
            Expr::Literal(Lit::Str("abc".to_string())),
            Lit::Str("abc".to_string()),
        );
        test_expr(Expr::Literal(Lit::Bool(false)), Lit::Bool(false));
        test_expr(Expr::Literal(Lit::Nil), Lit::Nil);
    }

    #[test]
    fn test_prefix_expr() {
        test_expr(
            Expr::PrefixOp {
                op: PrefixOp::Neg,
                rhs: Box::new(Expr::Literal(Lit::Number(1.))),
            },
            Lit::Number(-1.),
        );
        test_expr(
            Expr::PrefixOp {
                op: PrefixOp::Not,
                rhs: Box::new(Expr::Literal(Lit::Bool(false))),
            },
            Lit::Bool(true),
        );
        test_expr(
            Expr::PrefixOp {
                op: PrefixOp::Not,
                rhs: Box::new(Expr::Literal(Lit::Str("a".to_string()))),
            },
            Lit::Bool(false),
        );
    }
}
