use crate::chunk::Chunk;
use crate::{OpCode, Parser, Value};
use miette::{IntoDiagnostic, WrapErr};
use std::collections::HashMap;
use std::fmt::Write;

pub struct Vm {
    pub code: Chunk,
    pc: u8,
    debug: bool,
    stack: Vec<Value>,
    globals: HashMap<String, Value>,
}

impl Default for Vm {
    fn default() -> Self {
        Vm::new()
    }
}

impl Vm {
    pub fn new() -> Self {
        Self {
            code: Chunk::new(),
            pc: 0,
            debug: false,
            stack: Vec::new(),
            globals: HashMap::new(),
        }
    }

    pub fn with_chunk(code: Chunk) -> Self {
        Self {
            code,
            pc: 0,
            debug: false,
            stack: Vec::new(),
            globals: HashMap::new(),
        }
    }

    pub fn with_debug(mut self) -> Self {
        self.debug = true;
        self
    }

    pub fn reset_stack(&mut self) -> &Self {
        let vm = self;
        vm.stack.clear();
        vm
    }

    fn read_byte(&mut self) -> OpCode {
        let byte = self.code.code_at(self.pc);
        if self.pc > 254 {
            panic!("Overflow of chunk")
        }
        self.pc += 1;
        byte
    }

    fn read_const(&mut self) -> Value {
        let byte = self.code.const_at(self.pc);
        if self.pc > 254 {
            panic!("Overflow of chunk")
        }
        self.pc += 1;
        byte
    }

    fn read_index(&mut self) -> u8 {
        let byte = self.code.index_at(self.pc);
        if self.pc > 254 {
            panic!("Overflow of chunk")
        }
        self.pc += 1;
        byte
    }

    fn print_stack(&self) {
        let mut s = String::new();
        for value in &self.stack {
            let _ = write!(&mut s, "[ {} ]", value);
        }
        println!("{s}");
    }

    pub fn compile(&mut self, file_contents: &str) -> miette::Result<()> {
        let mut parser = Parser::new(file_contents, &mut self.code);
        parser.compile()?;
        Ok(())
    }

    pub fn run_once(&mut self) -> miette::Result<bool> {
        if self.debug {
            self.print_stack();
            println!("{:04} {}", self.pc, self.code.code_at(self.pc));
        }
        match self.read_byte() {
            OpCode::OpReturn => return Ok(true),
            OpCode::OpConstant => {
                let value = self.read_const();
                self.stack.push(value);
            }
            OpCode::OpNot => {
                let a = self.stack.last_mut().expect("Stack empty");
                *a = Value::Bool(a.is_falsey());
            }
            OpCode::OpNegate => {
                let a = self.stack.last_mut().expect("Stack empty");
                match a {
                    Value::Number(n) => *n = -*n,
                    _ => return Err(miette::miette!("Can't negate a non numeric value: {a}")),
                }
            }
            OpCode::OpAdd => {
                let a = self.stack.pop().expect("Stack empty");
                let b = self.stack.pop().expect("Stack empty");

                match (a, b) {
                    (Value::Number(a), Value::Number(b)) => self.stack.push(Value::Number(a + b)),
                    (Value::String(a), Value::String(b)) => {
                        let mut b = b.clone(); // Ewwwww why clone
                        b.push_str(&a);
                        self.stack.push(Value::String(b))
                    }
                    (a, b) => {
                        return Err(miette::miette!(
                            "Can't add values of type: {a}, {b} together"
                        ))
                    }
                }
            }
            OpCode::OpSubtract => {
                let a = self.stack.pop().expect("Stack empty");
                let b = self.stack.pop().expect("Stack empty");

                match (a, b) {
                    (Value::Number(a), Value::Number(b)) => self.stack.push(Value::Number(b - a)),
                    (a, b) => {
                        return Err(miette::miette!("Can't divide non numeric values {a}, {b}"))
                    }
                }
            }
            OpCode::OpMultiply => {
                let a = self.stack.pop().expect("Stack empty");
                let b = self.stack.pop().expect("Stack empty");
                match (a, b) {
                    (Value::Number(a), Value::Number(b)) => self.stack.push(Value::Number(a * b)),
                    (a, b) => {
                        return Err(miette::miette!("Can't divide non numeric values {a}, {b}"))
                    }
                }
            }
            OpCode::OpDivide => {
                let a = self.stack.pop().expect("Stack empty");
                let b = self.stack.pop().expect("Stack empty");
                match (a, b) {
                    (Value::Number(a), Value::Number(b)) => self.stack.push(Value::Number(b / a)),
                    (a, b) => {
                        return Err(miette::miette!("Can't divide non numeric values {a}, {b}"))
                    }
                }
            }
            OpCode::OpNil => self.stack.push(Value::Nil),
            OpCode::OpTrue => self.stack.push(Value::Bool(true)),
            OpCode::OpFalse => self.stack.push(Value::Bool(false)),
            OpCode::OpEq => {
                let a = self.stack.pop().expect("Stack empty");
                let b = self.stack.pop().expect("Stack empty");
                self.stack.push(Value::Bool(a == b));
            }
            OpCode::OpGreater => {
                let a = self.stack.pop().expect("Stack empty");
                let b = self.stack.pop().expect("Stack empty");
                self.stack.push(Value::Bool(b > a));
            }
            OpCode::OpLess => {
                let a = self.stack.pop().expect("Stack empty");
                let b = self.stack.pop().expect("Stack empty");
                self.stack.push(Value::Bool(b < a));
            }
            OpCode::OpPrint => {
                let a = self.stack.pop().expect("Stack empty");
                println!("{a}");
            }
            OpCode::OpPop => {
                let _ = self.stack.pop();
            }
            OpCode::OpDefGlobal => {
                let name = self.read_const();
                let value = self.stack.pop().expect("Stack empty");
                self.globals.insert(name.to_string(), value);
            }
            OpCode::OpGetGlobal => {
                let name = self.read_const();
                let value = self.globals.get(&name.to_string()); // TODO: implement Into<&str> for Value
                match value {
                    Some(value) => self.stack.push(value.clone()),
                    None => return Err(miette::miette!("Global variable {name} not defined")),
                }
            }
            OpCode::OpDefLocal => {
                let index = self.read_index();
                let new_val = self.stack.last().unwrap().clone();
                *self.stack.get_mut(index as usize).unwrap() = new_val;
            }
            OpCode::OpGetLocal => {
                let index = self.read_index();
                self.stack
                    .push(self.stack.get(index as usize).unwrap().clone());
            }
        };
        Ok(false)
    }

    pub fn run_exact(&mut self, ops: usize) -> miette::Result<Value> {
        for _ in 0..ops {
            self.run_once()?;
        }
        self.stack.pop().ok_or(miette::miette!("whatever"))
    }

    pub fn run(&mut self) -> miette::Result<()> {
        while !self.run_once()? {}
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    pub fn string() {
        let mut code = Chunk::new();
        code.push_opconst(Value::String("string".to_string()));
        code.push_opcode(OpCode::OpReturn);
        let mut vm = Vm::with_chunk(code);
        let res = vm.run_exact(1).unwrap();
        assert_eq!(res, Value::String("string".to_string()))
    }

    #[test]
    pub fn simple_return() {
        let mut code = Chunk::new();
        code.push_opconst(Value::Number(1.));
        code.push_opcode(OpCode::OpReturn);
        let mut vm = Vm::with_chunk(code);
        let _ = vm.run().unwrap();
    }

    // These tests rely on the behavior of float operations not changing, which is not a given
    // so they may need to be refactored later
    #[test]
    pub fn simple_addition() {
        let mut code = Chunk::new();
        code.push_opconst(Value::Number(1.));
        code.push_opconst(Value::Number(1.));
        code.push_opcode(OpCode::OpAdd);
        code.push_opcode(OpCode::OpReturn);
        let mut vm = Vm::with_chunk(code);
        let res = vm.run_exact(3).unwrap();
        assert_eq!(res, Value::Number(2.))
    }

    #[test]
    pub fn simple_subtraction() {
        let mut code = Chunk::new();
        code.push_opconst(Value::Number(5.));
        code.push_opconst(Value::Number(2.));
        code.push_opcode(OpCode::OpSubtract);
        code.push_opcode(OpCode::OpReturn);
        let mut vm = Vm::with_chunk(code);
        let res = vm.run_exact(3).unwrap();
        assert_eq!(res, Value::Number(3.))
    }

    #[test]
    pub fn negate() {
        let mut code = Chunk::new();
        code.push_opconst(Value::Number(5.));
        code.push_opcode(OpCode::OpNegate);
        code.push_opcode(OpCode::OpReturn);
        let mut vm = Vm::with_chunk(code);
        let res = vm.run_exact(2).unwrap();
        assert_eq!(res, Value::Number(-5.))
    }

    #[test]
    pub fn simple_multiply() {
        let mut code = Chunk::new();
        code.push_opconst(Value::Number(1.));
        code.push_opconst(Value::Number(10.));
        code.push_opcode(OpCode::OpMultiply);
        code.push_opcode(OpCode::OpReturn);
        let mut vm = Vm::with_chunk(code);
        let res = vm.run_exact(3).unwrap();
        assert_eq!(res, Value::Number(10.))
    }

    #[test]
    pub fn simple_divide() {
        let mut code = Chunk::new();
        code.push_opconst(Value::Number(4.));
        code.push_opconst(Value::Number(2.));
        code.push_opcode(OpCode::OpDivide);
        code.push_opcode(OpCode::OpReturn);
        let mut vm = Vm::with_chunk(code);
        let res = vm.run_exact(3).unwrap();
        assert_eq!(res, Value::Number(2.))
    }

    #[test]
    pub fn equality() {
        let mut code = Chunk::new();
        code.push_opconst(Value::Bool(true));
        code.push_opconst(Value::Bool(true));
        code.push_opcode(OpCode::OpEq);
        code.push_opcode(OpCode::OpReturn);
        let mut vm = Vm::with_chunk(code);
        let res = vm.run_exact(4).unwrap();
        assert_eq!(res, Value::Bool(true))
    }

    #[test]
    pub fn equality_false() {
        let mut code = Chunk::new();
        code.push_opconst(Value::Bool(true));
        code.push_opconst(Value::Bool(false));
        code.push_opcode(OpCode::OpEq);
        code.push_opcode(OpCode::OpReturn);
        let mut vm = Vm::with_chunk(code);
        let res = vm.run_exact(4).unwrap();
        assert_eq!(res, Value::Bool(false))
    }

    #[test]
    pub fn equality_string() {
        let mut code = Chunk::new();
        code.push_opconst(Value::String("string".to_string()));
        code.push_opconst(Value::String("string".to_string()));
        code.push_opcode(OpCode::OpEq);
        code.push_opcode(OpCode::OpReturn);
        let mut vm = Vm::with_chunk(code);
        let res = vm.run_exact(4).unwrap();
        assert_eq!(res, Value::Bool(true))
    }

    #[test]
    pub fn equality_different_types() {
        // Example from the book:
        // !(5 - 4 > 3 * 2 == !nil)
        let mut code = Chunk::new();
        // 5 - 4
        code.push_opconst(Value::Number(5.));
        code.push_opconst(Value::Number(4.));
        code.push_opcode(OpCode::OpSubtract);
        // 3 * 2
        code.push_opconst(Value::Number(3.));
        code.push_opconst(Value::Number(2.));
        code.push_opcode(OpCode::OpMultiply);
        // (5 - 4) > (3 * 2)
        code.push_opcode(OpCode::OpGreater);
        // !nil
        code.push_opcode(OpCode::OpNil);
        code.push_opcode(OpCode::OpNot);
        // (5 - 4) > (3 * 2) == !nil
        code.push_opcode(OpCode::OpEq);
        // !((5 - 4) > (3 * 2) == !nil)
        code.push_opcode(OpCode::OpNot);
        code.push_opcode(OpCode::OpReturn);
        let mut vm = Vm::with_chunk(code);
        let res = vm.run_exact(9).unwrap();
        assert_eq!(res, Value::Bool(true))
    }

    #[test]
    pub fn add_strings() {
        let mut code = Chunk::new();
        code.push_opconst(Value::String("Hello, ".to_string()));
        code.push_opconst(Value::String("world!".to_string()));
        code.push_opcode(OpCode::OpAdd);
        code.push_opcode(OpCode::OpReturn);
        let mut vm = Vm::with_chunk(code);
        let res = vm.run_exact(4).unwrap();
        assert_eq!(res, Value::String("Hello, world!".to_string()))
    }

    #[test]
    pub fn add_string_and_number() {
        let mut code = Chunk::new();
        code.push_opconst(Value::String(", world!".to_string()));
        code.push_opconst(Value::Number(1.));
        code.push_opcode(OpCode::OpAdd);
        code.push_opcode(OpCode::OpReturn);
        let mut vm = Vm::with_chunk(code);
        let res = vm.run_exact(4);
        assert!(res.is_err())
    }

    #[test]
    pub fn var() {
        let mut code = Chunk::new();
        code.push_const(Value::String("myvar".to_string()));
        code.push_opconst(Value::Number(1.));
        code.push_defvar(0, OpCode::OpDefGlobal);
        code.push_opconst(Value::Number(5.));
        code.push_getvar(0, OpCode::OpGetGlobal);
        code.push_opcode(OpCode::OpMultiply);
        code.push_opcode(OpCode::OpReturn);
        let mut vm = Vm::with_chunk(code);
        let res = vm.run_exact(5).unwrap();
        assert_eq!(res, Value::Number(5.));
    }

    #[test]
    pub fn strings_var_add() {
        let mut code = Chunk::new();
        code.push_const(Value::String("myvar".to_string()));
        code.push_opconst(Value::String("hello, ".to_string()));
        code.push_defvar(0, OpCode::OpDefGlobal);

        code.push_const(Value::String("mysecondvar".to_string()));
        code.push_opconst(Value::String("world!".to_string()));
        code.push_defvar(1, OpCode::OpDefGlobal);

        code.push_getvar(0, OpCode::OpGetGlobal);
        code.push_getvar(1, OpCode::OpGetGlobal);
        code.push_opcode(OpCode::OpAdd);
        code.push_opcode(OpCode::OpReturn);
        let mut vm = Vm::with_chunk(code);
        let res = vm.run_exact(8).unwrap();
        assert_eq!(res, Value::String("hello, world!".to_string()));
    }
}
