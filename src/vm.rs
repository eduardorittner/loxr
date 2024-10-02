use crate::chunk::Chunk;
use crate::{OpCode, Parser, Value};
use miette::{IntoDiagnostic, WrapErr};
use std::borrow::BorrowMut;
use std::collections::HashMap;
use std::io;
use std::io::Cursor;
use std::io::{Stdout, Write};
use std::str::SplitAsciiWhitespace;

pub struct Vm<W: io::Write> {
    pub code: Chunk,
    debug: bool,
    stack: Vec<Value>,
    globals: HashMap<String, Value>,
    out: W,
}

impl Vm<Cursor<Vec<u8>>> {
    pub fn test() -> Self {
        Self {
            code: Chunk::new(),
            debug: false,
            stack: Vec::new(),
            globals: HashMap::new(),
            out: Cursor::new(Vec::new()),
        }
    }

    pub fn test_with_chunk(code: Chunk) -> Self {
        Self {
            code,
            debug: false,
            stack: Vec::new(),
            globals: HashMap::new(),
            out: Cursor::new(Vec::new()),
        }
    }

    pub fn test_with_debug() -> Self {
        let mut vm = Vm::test();
        vm.debug = true;
        vm
    }

    pub fn read_out(&mut self) -> String {
        let v = self.out.get_ref().clone();
        String::from_utf8(v).expect("All writes should be valid UTF-8")
    }
}

impl Default for Vm<Stdout> {
    fn default() -> Self {
        Vm::new()
    }
}

impl Vm<Stdout> {
    pub fn new() -> Self {
        let out = std::io::stdout();
        Self {
            code: Chunk::new(),
            debug: false,
            stack: Vec::new(),
            globals: HashMap::new(),
            out,
        }
    }

    pub fn with_chunk(code: Chunk) -> Self {
        Self {
            code,
            debug: false,
            stack: Vec::new(),
            globals: HashMap::new(),
            out: std::io::stdout(),
        }
    }
}

impl<T: Write> Vm<T> {
    pub fn with_debug(mut self) -> Self {
        self.debug = true;
        self
    }

    pub fn reset_stack(&mut self) -> &Self {
        let vm = self;
        vm.stack.clear();
        vm
    }

    fn print_stack(&mut self) {
        for value in &self.stack {
            let _ = self.out.write_fmt(format_args!("[ {} ]", value));
        }
        let _ = self.out.write(b"\n");
    }

    pub fn compile(&mut self, file_contents: &str) -> miette::Result<()> {
        let mut parser = Parser::new(file_contents, &mut self.code);
        parser.compile()?;
        Ok(())
    }

    pub fn run_once(&mut self) -> miette::Result<bool> {
        if self.debug {
            self.print_stack();
            let _ = self.out.write_fmt(format_args!(
                "{:04} {}\n",
                self.code.pc(),
                self.code.current_op()
            ));
        }

        if let Some(op) = self.code.next() {
            match op {
                OpCode::OpReturn => return Ok(true),
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
                        (Value::Number(a), Value::Number(b)) => {
                            self.stack.push(Value::Number(a + b))
                        }
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
                        (Value::Number(a), Value::Number(b)) => {
                            self.stack.push(Value::Number(b - a))
                        }
                        (a, b) => {
                            return Err(miette::miette!("Can't divide non numeric values {a}, {b}"))
                        }
                    }
                }
                OpCode::OpMultiply => {
                    let a = self.stack.pop().expect("Stack empty");
                    let b = self.stack.pop().expect("Stack empty");
                    match (a, b) {
                        (Value::Number(a), Value::Number(b)) => {
                            self.stack.push(Value::Number(a * b))
                        }
                        (a, b) => {
                            return Err(miette::miette!("Can't divide non numeric values {a}, {b}"))
                        }
                    }
                }
                OpCode::OpDivide => {
                    let a = self.stack.pop().expect("Stack empty");
                    let b = self.stack.pop().expect("Stack empty");
                    match (a, b) {
                        (Value::Number(a), Value::Number(b)) => {
                            self.stack.push(Value::Number(b / a))
                        }
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
                    let _ = self.out.write_fmt(format_args!("{a}\n"));
                }
                OpCode::OpPop => {
                    let _ = self.stack.pop();
                }
                OpCode::OpConstant(i) => {
                    if let Some(value) = self.code.get_const(i) {
                        self.stack.push(value);
                    } else {
                        return Err(miette::miette!("No Value at index: {}", i));
                    }
                }
                OpCode::OpDefGlobal(i) => {
                    if let Some(name) = self.code.get_const(i) {
                        let value = self.stack.pop().expect("Stack empty");
                        self.globals.insert(name.to_string(), value);
                    } else {
                        return Err(miette::miette!("No const at index: {}", i));
                    }
                }
                OpCode::OpGetGlobal(i) => {
                    if let Some(name) = self.code.get_const(i) {
                        if let Some(value) = self.globals.get(&name.to_string()) {
                            self.stack.push(value.clone());
                        } else {
                            return Err(miette::miette!("Global variable {name} not defined"));
                        }
                    } else {
                        return Err(miette::miette!("No const at index: {}", i));
                    }
                }
                OpCode::OpDefLocal(i) => {
                    if let Some(new_val) = self.stack.last() {
                        *self.stack.get_mut(i as usize).unwrap() = new_val.clone();
                    } else {
                        return Err(miette::miette!(
                            "Stack is empty when trying to define a new local variable."
                        ));
                    }
                }
                OpCode::OpGetLocal(i) => {
                    self.stack.push(self.stack.get(i as usize).unwrap().clone());
                }
                OpCode::OpJumpIfFalse(jump) => {
                    let cond = self.stack.last().unwrap();
                    if cond.is_falsey() {
                        self.code.jump(jump)
                    }
                }
                OpCode::OpJump(jump) => self.code.jump(jump),
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
        code.push_const(Value::String("string".to_string()));
        code.push_opcode(OpCode::OpReturn);
        let mut vm = Vm::with_chunk(code);
        let res = vm.run_exact(1).unwrap();
        assert_eq!(res, Value::String("string".to_string()))
    }

    #[test]
    pub fn simple_return() {
        let mut code = Chunk::new();
        code.push_const(Value::Number(1.));
        code.push_opcode(OpCode::OpReturn);
        let mut vm = Vm::with_chunk(code);
        let _ = vm.run().unwrap();
    }

    // These tests rely on the behavior of float operations not changing, which is not a given
    // so they may need to be refactored later
    #[test]
    pub fn simple_addition() {
        let mut code = Chunk::new();
        code.push_const(Value::Number(1.));
        code.push_const(Value::Number(1.));
        code.push_opcode(OpCode::OpAdd);
        code.push_opcode(OpCode::OpReturn);
        let mut vm = Vm::with_chunk(code);
        let res = vm.run_exact(3).unwrap();
        assert_eq!(res, Value::Number(2.))
    }

    #[test]
    pub fn simple_subtraction() {
        let mut code = Chunk::new();
        code.push_const(Value::Number(5.));
        code.push_const(Value::Number(2.));
        code.push_opcode(OpCode::OpSubtract);
        code.push_opcode(OpCode::OpReturn);
        let mut vm = Vm::with_chunk(code);
        let res = vm.run_exact(3).unwrap();
        assert_eq!(res, Value::Number(3.))
    }

    #[test]
    pub fn negate() {
        let mut code = Chunk::new();
        code.push_const(Value::Number(5.));
        code.push_opcode(OpCode::OpNegate);
        code.push_opcode(OpCode::OpReturn);
        let mut vm = Vm::with_chunk(code);
        let res = vm.run_exact(2).unwrap();
        assert_eq!(res, Value::Number(-5.))
    }

    #[test]
    pub fn simple_multiply() {
        let mut code = Chunk::new();
        code.push_const(Value::Number(1.));
        code.push_const(Value::Number(10.));
        code.push_opcode(OpCode::OpMultiply);
        code.push_opcode(OpCode::OpReturn);
        let mut vm = Vm::with_chunk(code);
        let res = vm.run_exact(3).unwrap();
        assert_eq!(res, Value::Number(10.))
    }

    #[test]
    pub fn simple_divide() {
        let mut code = Chunk::new();
        code.push_const(Value::Number(4.));
        code.push_const(Value::Number(2.));
        code.push_opcode(OpCode::OpDivide);
        code.push_opcode(OpCode::OpReturn);
        let mut vm = Vm::with_chunk(code);
        let res = vm.run_exact(3).unwrap();
        assert_eq!(res, Value::Number(2.))
    }

    #[test]
    pub fn equality() {
        let mut code = Chunk::new();
        code.push_const(Value::Bool(true));
        code.push_const(Value::Bool(true));
        code.push_opcode(OpCode::OpEq);
        code.push_opcode(OpCode::OpReturn);
        let mut vm = Vm::with_chunk(code);
        let res = vm.run_exact(4).unwrap();
        assert_eq!(res, Value::Bool(true))
    }

    #[test]
    pub fn equality_false() {
        let mut code = Chunk::new();
        code.push_const(Value::Bool(true));
        code.push_const(Value::Bool(false));
        code.push_opcode(OpCode::OpEq);
        code.push_opcode(OpCode::OpReturn);
        let mut vm = Vm::with_chunk(code);
        let res = vm.run_exact(4).unwrap();
        assert_eq!(res, Value::Bool(false))
    }

    #[test]
    pub fn equality_string() {
        let mut code = Chunk::new();
        code.push_const(Value::String("string".to_string()));
        code.push_const(Value::String("string".to_string()));
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
        code.push_const(Value::Number(5.));
        code.push_const(Value::Number(4.));
        code.push_opcode(OpCode::OpSubtract);
        // 3 * 2
        code.push_const(Value::Number(3.));
        code.push_const(Value::Number(2.));
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
        code.push_const(Value::String("Hello, ".to_string()));
        code.push_const(Value::String("world!".to_string()));
        code.push_opcode(OpCode::OpAdd);
        code.push_opcode(OpCode::OpReturn);
        let mut vm = Vm::with_chunk(code);
        let res = vm.run_exact(4).unwrap();
        assert_eq!(res, Value::String("Hello, world!".to_string()))
    }

    #[test]
    pub fn add_string_and_number() {
        let mut code = Chunk::new();
        code.push_const(Value::String(", world!".to_string()));
        code.push_const(Value::Number(1.));
        code.push_opcode(OpCode::OpAdd);
        code.push_opcode(OpCode::OpReturn);
        let mut vm = Vm::with_chunk(code);
        let res = vm.run_exact(4);
        assert!(res.is_err())
    }

    #[test]
    pub fn var() {
        let mut code = Chunk::new();
        code.push_value(Value::String("myvar".to_string()));
        code.push_const(Value::Number(1.));
        code.push_defvar(OpCode::OpDefGlobal(0));
        code.push_const(Value::Number(5.));
        code.push_getvar(OpCode::OpGetGlobal(0));
        code.push_opcode(OpCode::OpMultiply);
        code.push_opcode(OpCode::OpReturn);
        let mut vm = Vm::with_chunk(code);
        let res = vm.run_exact(5).unwrap();
        assert_eq!(res, Value::Number(5.));
    }

    #[test]
    pub fn strings_var_add() {
        let mut code = Chunk::new();
        code.push_value(Value::String("myvar".to_string()));
        code.push_const(Value::String("hello, ".to_string()));
        code.push_defvar(OpCode::OpDefGlobal(0));

        code.push_value(Value::String("mysecondvar".to_string()));
        code.push_const(Value::String("world!".to_string()));
        code.push_defvar(OpCode::OpDefGlobal(1));

        code.push_getvar(OpCode::OpGetGlobal(0));
        code.push_getvar(OpCode::OpGetGlobal(1));
        code.push_opcode(OpCode::OpAdd);
        code.push_opcode(OpCode::OpPrint);
        code.push_opcode(OpCode::OpReturn);
        let mut vm = Vm::test_with_chunk(code);
        let _ = vm.run().unwrap();
        let res = vm.read_out();
        let res = res.trim_end();
        assert_eq!(res, "hello, world!".to_string());
    }

    #[test]
    pub fn local_var() {
        let mut code = Chunk::new();
        code.push_const(Value::Number(10.));
        code.push_getvar(OpCode::OpGetLocal(0));
        code.push_const(Value::Number(2.));
        code.push_opcode(OpCode::OpMultiply);
        code.push_opcode(OpCode::OpReturn);
        let mut vm = Vm::with_chunk(code);
        let res = vm.run_exact(4).unwrap();
        assert_eq!(res, Value::Number(20.));
    }

    #[test]
    pub fn local_vars() {
        let mut code = Chunk::new();
        code.push_const(Value::Number(10.));
        code.push_const(Value::Number(2.));
        code.push_getvar(OpCode::OpGetLocal(1));
        code.push_getvar(OpCode::OpGetLocal(0));
        code.push_opcode(OpCode::OpDivide);

        code.push_const(Value::Number(2.));
        code.push_const(Value::Number(10.));
        code.push_opcode(OpCode::OpDivide);
        code.push_opcode(OpCode::OpEq);
        code.push_opcode(OpCode::OpReturn);
        let mut vm = Vm::with_chunk(code);
        let res = vm.run_exact(9).unwrap();
        assert_eq!(res, Value::Bool(true));
    }
}
