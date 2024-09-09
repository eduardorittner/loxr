use std::fmt;

#[derive(Clone, PartialEq, PartialOrd, Debug)]
pub enum Value {
    Bool(bool),
    Number(f64),
    String(String), // Would like to be &str but couldn't handle all the lifetimes
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
            Value::Nil => write!(f, "Nil"),
        }
    }
}
