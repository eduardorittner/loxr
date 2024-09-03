use crate::chunk::Chunk;
use crate::{Lexer, OpCode, Parser, Value};
use miette::{IntoDiagnostic, WrapErr};
use std::fmt::Write;
use std::fs;

pub struct Vm {
    pub code: Chunk,
    pc: u8,
    debug: bool,
    stack: Vec<Value>,
}

impl Vm {
    pub fn new() -> Self {
        Self {
            code: Chunk::new(),
            pc: 0,
            debug: false,
            stack: Vec::new(),
        }
    }

    pub fn with_chunk(code: Chunk) -> Self {
        Self {
            code,
            pc: 0,
            debug: false,
            stack: Vec::new(),
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

    fn print_stack(&self) {
        let mut s = String::new();
        for value in &self.stack {
            let _ = write!(&mut s, "[ {} ]", value);
        }
        println!("{s}");
    }

    pub fn compile(&mut self, file_contents: &String) -> miette::Result<()> {
        let mut parser = Parser::new(&file_contents, &mut self.code);
        parser.compile()?;
        Ok(())
    }

    pub fn run(&mut self) -> miette::Result<Value> {
        loop {
            if self.debug {
                self.print_stack();
                println!("{:04} {}", self.pc, self.code.code_at(self.pc));
            }
            match self.read_byte() {
                OpCode::OpReturn => {
                    let value = self
                        .stack
                        .pop()
                        .expect("Stack should not be empty at the end of the program");
                    println!("{}", value);
                    return Ok(value);
                }
                OpCode::OpConstant => {
                    let value = self.read_const();
                    self.stack.push(value);
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
                        (a, b) => {
                            return Err(miette::miette!("Can't add non numeric values: {a}, {b}"))
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
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

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
        let res = vm.run().unwrap();
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
        let res = vm.run().unwrap();
        assert_eq!(res, Value::Number(3.))
    }

    #[test]
    pub fn simple_multiply() {
        let mut code = Chunk::new();
        code.push_const(Value::Number(1.));
        code.push_const(Value::Number(10.));
        code.push_opcode(OpCode::OpMultiply);
        code.push_opcode(OpCode::OpReturn);
        let mut vm = Vm::with_chunk(code);
        let res = vm.run().unwrap();
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
        let res = vm.run().unwrap();
        assert_eq!(res, Value::Number(2.))
    }
}
