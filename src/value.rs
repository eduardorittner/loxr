use std::fmt;

#[derive(Clone, Copy, PartialEq, Debug)]
pub enum Value {
    Bool(bool),
    Number(f64),
    Nil,
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Value::Bool(b) => write!(f, "Bool({b})"),
            Value::Number(n) => write!(f, "Number({n})"),
            Value::Nil => write!(f, "Nil"),
        }
    }
}

pub struct Values {
    values: Vec<Value>,
}

impl Values {
    fn new() -> Self {
        Values {
            values: Vec::with_capacity(16),
        }
    }

    fn push(&mut self, val: Value) {
        self.values.push(val);
    }
}
