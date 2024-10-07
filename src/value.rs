use crate::Chunk;
use std::fmt;

#[derive(Clone, PartialEq, PartialOrd, Debug)]
pub enum Value {
    Bool(bool),
    Number(f64),
    String(String), // Would like to be &str but couldn't handle all the lifetimes
    Fun(Function),
    Nil,
}

impl Value {
    pub fn is_truthy(&self) -> bool {
        match self {
            Value::Bool(b) => *b,
            Value::Nil => false,
            Value::String(_) => true,
            _ => true,
        }
    }

    pub fn is_falsey(&self) -> bool {
        !self.is_truthy()
    }
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Value::Bool(b) => write!(f, "{b}"),
            Value::Number(n) => write!(f, "{n}"),
            Value::String(s) => write!(f, "{s}"),
            Value::Fun(fun) => match &fun.name {
                Some(name) => write!(f, "{}", name),
                None => write!(f, "script"),
            },
            Value::Nil => write!(f, "Nil"),
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct Function {
    pub name: Option<String>,
    pub code: Chunk,
    pub arity: u8,
}

impl Default for Function {
    fn default() -> Self {
        Self::new()
    }
}

impl Function {
    pub fn new() -> Self {
        Self {
            name: None,
            code: Chunk::new(),
            arity: 0,
        }
    }

    pub fn new_script() -> Self {
        Self {
            name: Some(String::new()),
            code: Chunk::new(),
            arity: 0,
        }
    }
}

impl std::cmp::PartialOrd for Function {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.name.cmp(&other.name))
    }
}
