use lex::{T, token::TokenKind};

#[derive(Debug, PartialEq, Clone)]
pub enum Expr {
    Literal(Lit),
    Ident(String),
    PrefixOp {
        op: PrefixOp,
        rhs: Box<Expr>,
    },
    BinaryOp {
        op: BinaryOp,
        rhs: Box<Expr>,
        lhs: Box<Expr>,
    },
    FunCall {
        fn_name: String,
        args: Vec<Expr>,
    },
}

#[derive(Debug, PartialEq, Clone)]
pub enum Stmt {
    While {
        cond: Expr,
        body: Box<Stmt>,
    },
    Return {
        expr: Expr,
    },
    Print {
        expr: Expr,
    },
    If {
        cond: Expr,
        iftrue: Box<Stmt>,
        iffalse: Option<Box<Stmt>>,
    },
    For {
        initial: Box<Stmt>,
        cond: Box<Stmt>,
        increment: Box<Stmt>,
        body: Box<Stmt>,
    },
    Expr {
        expr: Expr,
    },
    Block(Block),
}

#[derive(Debug, PartialEq, Clone)]
pub struct Block(pub Vec<Decl>);

#[derive(Debug, PartialEq, Clone)]
pub enum Decl {
    // TODO move class and var into their own structs
    Class,
    Fun(FunDecl),
    Var { name: String, value: Expr },
    Stmt { stmt: Stmt },
}

#[derive(Debug, PartialEq, Clone)]
pub struct FunDecl {
    pub name: String,
    pub args: Vec<String>,
    pub body: Block,
}

#[derive(Debug, PartialEq, Clone)]
pub enum Lit {
    Number(f64),
    Str(String),
    Bool(bool),
    Fun(FunDecl),
    Nil,
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum PrefixOp {
    Neg, // '-' arithmetic negation
    Not, // '!' logical negation
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum BinaryOp {
    Add,
    Sub,
    Mul,
    Div,
}

impl From<TokenKind> for PrefixOp {
    fn from(value: TokenKind) -> Self {
        match value {
            T![-] => PrefixOp::Neg,
            T![!] => PrefixOp::Not,
            token => {
                panic!("Not a prefix operation: {:?}", token)
            }
        }
    }
}

impl From<TokenKind> for BinaryOp {
    fn from(value: TokenKind) -> Self {
        match value {
            T![+] => BinaryOp::Add,
            T![-] => BinaryOp::Sub,
            T![*] => BinaryOp::Mul,
            T![/] => BinaryOp::Div,
            token => {
                panic!("Not a binary operation: {:?}", token)
            }
        }
    }
}

impl std::fmt::Display for Lit {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Lit::Number(n) => write!(f, "{}", n),
            Lit::Str(s) => write!(f, "{}", s),
            Lit::Bool(b) => write!(f, "{}", b),
            Lit::Fun(FunDecl { name, args, body }) => {
                write!(f, "fun {}(", name)?;
                for arg in args {
                    write!(f, " {}, ", arg)?;
                }
                write!(f, ") {}", body)
            }
            Lit::Nil => write!(f, "NIL"),
        }
    }
}

impl std::fmt::Display for PrefixOp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            PrefixOp::Neg => write!(f, "-"),
            PrefixOp::Not => write!(f, "!"),
        }
    }
}
impl std::fmt::Display for BinaryOp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                BinaryOp::Add => "+",
                BinaryOp::Sub => "-",
                BinaryOp::Mul => "*",
                BinaryOp::Div => "/",
            }
        )
    }
}

impl std::fmt::Display for Expr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Expr::Literal(lit) => write!(f, "{}", lit),
            Expr::Ident(id) => write!(f, "{}", id),
            Expr::PrefixOp { op, rhs } => write!(f, "({} {})", op, rhs),
            Expr::BinaryOp { op, rhs, lhs } => write!(f, "({} {} {})", lhs, op, rhs),
            Expr::FunCall { fn_name, args } => {
                write!(f, "{}(", fn_name)?;
                for arg in args {
                    write!(f, "{},", arg)?;
                }
                write!(f, ")")
            }
        }
    }
}

impl std::fmt::Display for Decl {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Decl::Class => todo!(),
            Decl::Fun(fun_decl) => todo!(),
            Decl::Var { name, value } => todo!(),
            Decl::Stmt { stmt } => todo!(),
        }
    }
}

impl std::fmt::Display for Block {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{{ ")?;
        for decl in self.0.iter() {
            write!(f, "{} ", decl)?;
        }
        write!(f, "}}")
    }
}
