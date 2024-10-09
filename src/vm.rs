use crate::chunk::Chunk;
use crate::value::Function;
use crate::{OpCode, Parser, Value};
use std::collections::HashMap;
use std::io;
use std::io::Cursor;
use std::io::{Stdout, Write};

pub struct Vm<W: io::Write> {
    call_stack: CallStack,
    debug: bool,
    stack: Vec<Value>,
    globals: HashMap<String, Value>,
    out: W,
}

struct CallFrame {
    fun: Function,
    pc: usize,
    frame: usize,
}

impl CallFrame {
    pub fn new(fun: Function, frame: usize) -> Self {
        Self { fun, pc: 0, frame }
    }
}

struct CallStack {
    stack: Vec<CallFrame>,
}

impl CallStack {
    pub fn new() -> Self {
        Self { stack: Vec::new() }
    }

    pub fn push(&mut self, frame: CallFrame) {
        self.stack.push(frame);
    }

    pub fn step(&mut self) -> OpCode {
        let cur = self.stack.last_mut().unwrap();
        let code = cur.fun.code.get_op(cur.pc);
        cur.pc += 1;
        code
    }

    pub fn pc(&self) -> usize {
        self.stack.last().unwrap().pc
    }

    pub fn current_op(&self) -> OpCode {
        self.stack.last().unwrap().fun.code.get_op(self.pc())
    }

    pub fn get_const(&self, i: u32) -> Option<Value> {
        let last = self.stack.last()?;
        last.fun.code.get_const(i)
    }

    pub fn jump(&mut self, jump: i32) {
        let cur = self.stack.last_mut().unwrap();
        if jump < 0 {
            // Jump from the last executed instruction
            cur.pc -= 1 + jump.unsigned_abs() as usize;
        } else {
            cur.pc += jump as usize;
        }
    }
}

impl<T> Vm<T>
where
    T: std::io::Write,
{
    pub fn code(&self) -> &Chunk {
        &self.call_stack.stack.last().unwrap().fun.code
    }
}

impl Vm<Cursor<Vec<u8>>> {
    pub fn test() -> Self {
        let stack = vec![Value::Nil]; // Reserved for the vm
        Self {
            call_stack: CallStack::new(),
            debug: false,
            stack,
            globals: HashMap::new(),
            out: Cursor::new(Vec::new()),
        }
    }

    pub fn test_with_fun(fun: Function) -> Self {
        let mut call_stack = CallStack::new();
        call_stack.push(CallFrame {
            fun,
            pc: 0,
            frame: 0,
        });
        Self {
            call_stack,
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
        let mut call_stack = CallStack::new();
        call_stack.push(CallFrame {
            fun: Function::new(),
            pc: 0,
            frame: 0,
        });
        let stack = vec![Value::Nil]; // Reserved value
        Self {
            call_stack,
            debug: false,
            stack,
            globals: HashMap::new(),
            out,
        }
    }

    pub fn with_fun(fun: Function) -> Self {
        let mut call_stack = CallStack::new();
        call_stack.push(CallFrame {
            fun,
            pc: 0,
            frame: 0,
        });
        Self {
            call_stack,
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
        let mut fun = Function::new_script();
        let mut parser = Parser::new(file_contents, &mut fun);
        parser.compile()?;
        self.call_stack.push(CallFrame {
            fun: fun.clone(),
            pc: 0,
            frame: 0,
        });
        self.call_fun(Value::Fun(fun), 0)
    }

    pub fn run_once(&mut self) -> miette::Result<Option<Value>> {
        if self.debug {
            self.print_stack();
            let _ = self.out.write_fmt(format_args!(
                "{:04} {}\n",
                self.call_stack.pc(),
                self.call_stack.current_op()
            ));
        }

        match self.call_stack.step() {
            OpCode::OpReturn => match self.stack.pop() {
                Some(v) => return Ok(Some(v)),
                None => return Ok(Some(Value::Nil)),
            },
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
                let _ = self.out.write_fmt(format_args!("{a}\n"));
            }
            OpCode::OpPop => {
                let _ = self.stack.pop();
            }
            OpCode::OpCall(i) => {
                if let Some(fun) = self.stack.get(self.stack.len() - i as usize) {
                    self.call_fun(fun.clone(), i)?;
                } else {
                    return Err(miette::miette!("No function call operand"));
                }
            }
            OpCode::OpConstant(i) => {
                if let Some(value) = self.call_stack.get_const(i) {
                    self.stack.push(value);
                } else {
                    return Err(miette::miette!("No Value at index: {}", i));
                }
            }
            OpCode::OpDefGlobal(i) => {
                if let Some(name) = self.call_stack.get_const(i) {
                    let value = self.stack.pop().expect("Stack empty");
                    self.globals.insert(name.to_string(), value);
                } else {
                    return Err(miette::miette!("No const at index: {}", i));
                }
            }
            OpCode::OpGetGlobal(i) => {
                if let Some(name) = self.call_stack.get_const(i) {
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
                self.stack.push(
                    self.stack
                        .get(i as usize)
                        .expect("Stack empty when trying to access a local variable")
                        .clone(),
                );
            }
            OpCode::OpJumpIfFalse(jump) => {
                let cond = self.stack.last().unwrap();
                if cond.is_falsey() {
                    self.call_stack.jump(jump)
                }
            }
            OpCode::OpJump(jump) => self.call_stack.jump(jump),
        }
        Ok(None)
    }

    /// Used for testing infinite loops
    ///
    /// Instead of running the test for a defined ammount of time, just run N steps where
    /// N is very large and see if it terminates before that
    pub fn run_exact(&mut self, ops: usize) -> miette::Result<Value> {
        for _ in 0..ops {
            self.run_once()?;
        }
        self.stack.pop().ok_or(miette::miette!("whatever"))
    }

    pub fn run(&mut self) -> miette::Result<Value> {
        loop {
            match self.run_once() {
                Ok(Some(v)) => return Ok(v),
                Ok(None) => (),
                Err(e) => {
                    self.stack_trace();
                    return Err(e);
                }
            }
        }
    }

    pub fn stack_trace(&mut self) {
        for stack in self.call_stack.stack.iter() {
            let instr = stack.pc;
            // TODO keep a instruction index -> source code line map so we can print
            // which line the error was on
            if stack.fun.code.is_empty() {
                eprint!("instruction: {} in ", stack.fun.code.get_op(instr));
                match &stack.fun.name {
                    Some(name) => eprintln!("{}", name),
                    None => eprintln!("script"),
                }
            }
        }
    }

    pub fn call_fun(&mut self, callee: Value, n_args: u8) -> miette::Result<()> {
        match callee {
            Value::Fun(f) => {
                if f.arity != n_args {
                    return Err(miette::miette!(
                        "Function: {:?} expected {} args, got {}",
                        f.name,
                        f.arity,
                        n_args
                    ));
                }
                let frame = CallFrame::new(f, self.stack.len().saturating_sub(1 + n_args as usize));
                self.call_stack.push(frame);
                Ok(())
            }
            value => Err(miette::miette!(
                "Can't call a non-function value: {:?}",
                value
            )),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    pub fn string() {
        let mut fun = Function::new();
        fun.code.push_const(Value::String("string".to_string()));
        fun.code.push_opcode(OpCode::OpReturn);
        let mut vm = Vm::with_fun(fun);
        let res = vm.run().unwrap();
        assert_eq!(res, Value::String("string".to_string()))
    }

    #[test]
    pub fn simple_return() {
        let mut fun = Function::new();
        fun.code.push_const(Value::Number(1.));
        fun.code.push_opcode(OpCode::OpReturn);
        let mut vm = Vm::with_fun(fun);
        let _ = vm.run().unwrap();
    }

    // These tests rely on the behavior of float operations not changing, which is not a given
    // so they may need to be refactored later
    #[test]
    pub fn simple_addition() {
        let mut fun = Function::new();
        fun.code.push_const(Value::Number(1.));
        fun.code.push_const(Value::Number(1.));
        fun.code.push_opcode(OpCode::OpAdd);
        fun.code.push_opcode(OpCode::OpReturn);
        let mut vm = Vm::with_fun(fun);
        let res = vm.run().unwrap();
        assert_eq!(res, Value::Number(2.))
    }

    #[test]
    pub fn simple_subtraction() {
        let mut fun = Function::new();
        fun.code.push_const(Value::Number(5.));
        fun.code.push_const(Value::Number(2.));
        fun.code.push_opcode(OpCode::OpSubtract);
        fun.code.push_opcode(OpCode::OpReturn);
        let mut vm = Vm::with_fun(fun);
        let res = vm.run().unwrap();
        assert_eq!(res, Value::Number(3.))
    }

    #[test]
    pub fn negate() {
        let mut fun = Function::new();
        fun.code.push_const(Value::Number(5.));
        fun.code.push_opcode(OpCode::OpNegate);
        fun.code.push_opcode(OpCode::OpReturn);
        let mut vm = Vm::with_fun(fun);
        let res = vm.run().unwrap();
        assert_eq!(res, Value::Number(-5.))
    }

    #[test]
    pub fn simple_multiply() {
        let mut fun = Function::new();
        fun.code.push_const(Value::Number(1.));
        fun.code.push_const(Value::Number(10.));
        fun.code.push_opcode(OpCode::OpMultiply);
        fun.code.push_opcode(OpCode::OpReturn);
        let mut vm = Vm::with_fun(fun);
        let res = vm.run().unwrap();
        assert_eq!(res, Value::Number(10.))
    }

    #[test]
    pub fn simple_divide() {
        let mut fun = Function::new();
        fun.code.push_const(Value::Number(4.));
        fun.code.push_const(Value::Number(2.));
        fun.code.push_opcode(OpCode::OpDivide);
        fun.code.push_opcode(OpCode::OpReturn);
        let mut vm = Vm::with_fun(fun);
        let res = vm.run().unwrap();
        assert_eq!(res, Value::Number(2.))
    }

    #[test]
    pub fn equality() {
        let mut fun = Function::new();
        fun.code.push_const(Value::Bool(true));
        fun.code.push_const(Value::Bool(true));
        fun.code.push_opcode(OpCode::OpEq);
        fun.code.push_opcode(OpCode::OpReturn);
        let mut vm = Vm::with_fun(fun);
        let res = vm.run().unwrap();
        assert_eq!(res, Value::Bool(true))
    }

    #[test]
    pub fn equality_false() {
        let mut fun = Function::new();
        fun.code.push_const(Value::Bool(true));
        fun.code.push_const(Value::Bool(false));
        fun.code.push_opcode(OpCode::OpEq);
        fun.code.push_opcode(OpCode::OpReturn);
        let mut vm = Vm::with_fun(fun);
        let res = vm.run().unwrap();
        assert_eq!(res, Value::Bool(false))
    }

    #[test]
    pub fn equality_string() {
        let mut fun = Function::new();
        fun.code.push_const(Value::String("string".to_string()));
        fun.code.push_const(Value::String("string".to_string()));
        fun.code.push_opcode(OpCode::OpEq);
        fun.code.push_opcode(OpCode::OpReturn);
        let mut vm = Vm::with_fun(fun);
        let res = vm.run().unwrap();
        assert_eq!(res, Value::Bool(true))
    }

    #[test]
    pub fn equality_different_types() {
        // Example from the book:
        // !(5 - 4 > 3 * 2 == !nil)
        let mut fun = Function::new();
        // 5 - 4
        fun.code.push_const(Value::Number(5.));
        fun.code.push_const(Value::Number(4.));
        fun.code.push_opcode(OpCode::OpSubtract);
        // 3 * 2
        fun.code.push_const(Value::Number(3.));
        fun.code.push_const(Value::Number(2.));
        fun.code.push_opcode(OpCode::OpMultiply);
        // (5 - 4) > (3 * 2)
        fun.code.push_opcode(OpCode::OpGreater);
        // !nil
        fun.code.push_opcode(OpCode::OpNil);
        fun.code.push_opcode(OpCode::OpNot);
        // (5 - 4) > (3 * 2) == !nil
        fun.code.push_opcode(OpCode::OpEq);
        // !((5 - 4) > (3 * 2) == !nil)
        fun.code.push_opcode(OpCode::OpNot);
        fun.code.push_opcode(OpCode::OpReturn);
        let mut vm = Vm::with_fun(fun);
        let res = vm.run().unwrap();
        assert_eq!(res, Value::Bool(true))
    }

    #[test]
    pub fn add_strings() {
        let mut fun = Function::new();
        fun.code.push_const(Value::String("Hello, ".to_string()));
        fun.code.push_const(Value::String("world!".to_string()));
        fun.code.push_opcode(OpCode::OpAdd);
        fun.code.push_opcode(OpCode::OpReturn);
        let mut vm = Vm::with_fun(fun);
        let res = vm.run().unwrap();
        assert_eq!(res, Value::String("Hello, world!".to_string()))
    }

    #[test]
    pub fn add_string_and_number() {
        let mut fun = Function::new();
        fun.code.push_const(Value::String(", world!".to_string()));
        fun.code.push_const(Value::Number(1.));
        fun.code.push_opcode(OpCode::OpAdd);
        fun.code.push_opcode(OpCode::OpReturn);
        let mut vm = Vm::with_fun(fun);
        let res = vm.run();
        assert!(res.is_err())
    }

    #[test]
    pub fn var() {
        let mut fun = Function::new();
        fun.code.push_value(Value::String("myvar".to_string()));
        fun.code.push_const(Value::Number(1.));
        fun.code.push_defvar(OpCode::OpDefGlobal(0));
        fun.code.push_const(Value::Number(5.));
        fun.code.push_getvar(OpCode::OpGetGlobal(0));
        fun.code.push_opcode(OpCode::OpMultiply);
        fun.code.push_opcode(OpCode::OpReturn);
        let mut vm = Vm::with_fun(fun);
        let res = vm.run().unwrap();
        assert_eq!(res, Value::Number(5.));
    }

    #[test]
    pub fn strings_var_add() {
        let mut fun = Function::new();
        fun.code.push_value(Value::String("myvar".to_string()));
        fun.code.push_const(Value::String("hello, ".to_string()));
        fun.code.push_defvar(OpCode::OpDefGlobal(0));

        fun.code
            .push_value(Value::String("mysecondvar".to_string()));
        fun.code.push_const(Value::String("world!".to_string()));
        fun.code.push_defvar(OpCode::OpDefGlobal(1));

        fun.code.push_getvar(OpCode::OpGetGlobal(0));
        fun.code.push_getvar(OpCode::OpGetGlobal(1));
        fun.code.push_opcode(OpCode::OpAdd);
        fun.code.push_opcode(OpCode::OpPrint);
        fun.code.push_opcode(OpCode::OpReturn);
        let mut vm = Vm::test_with_fun(fun);
        let _ = vm.run().unwrap();
        let res = vm.read_out();
        let res = res.trim_end();
        assert_eq!(res, "hello, world!".to_string());
    }

    #[test]
    pub fn local_var() {
        let mut fun = Function::new();
        fun.code.push_const(Value::Number(10.));
        fun.code.push_getvar(OpCode::OpGetLocal(0));
        fun.code.push_const(Value::Number(2.));
        fun.code.push_opcode(OpCode::OpMultiply);
        fun.code.push_opcode(OpCode::OpReturn);
        let mut vm = Vm::with_fun(fun);
        let res = vm.run().unwrap();
        assert_eq!(res, Value::Number(20.));
    }

    #[test]
    pub fn local_vars() {
        let mut fun = Function::new();
        fun.code.push_const(Value::Number(10.));
        fun.code.push_const(Value::Number(2.));
        fun.code.push_getvar(OpCode::OpGetLocal(1));
        fun.code.push_getvar(OpCode::OpGetLocal(0));
        fun.code.push_opcode(OpCode::OpDivide);

        fun.code.push_const(Value::Number(2.));
        fun.code.push_const(Value::Number(10.));
        fun.code.push_opcode(OpCode::OpDivide);
        fun.code.push_opcode(OpCode::OpEq);
        fun.code.push_opcode(OpCode::OpReturn);
        let mut vm = Vm::with_fun(fun);
        let res = vm.run().unwrap();
        assert_eq!(res, Value::Bool(true));
    }
}
