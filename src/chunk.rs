#![allow(clippy::format_in_format_args)]
use crate::Value;

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum OpCode {
    OpReturn,
    OpNegate,
    OpNot,
    OpAdd,
    OpSubtract,
    OpMultiply,
    OpDivide,
    OpNil,
    OpTrue,
    OpEq,
    OpGreater,
    OpLess,
    OpFalse,
    OpPrint,
    OpPop,

    OpCall(u8),
    OpConstant(u32),
    OpDefGlobal(u32),
    OpGetGlobal(u32),
    OpDefLocal(u32),
    OpGetLocal(u32),
    OpJumpIfFalse(i32),
    OpJump(i32),
}

impl std::fmt::Display for OpCode {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            OpCode::OpReturn => write!(f, "OP_RETURN"),
            OpCode::OpNegate => write!(f, "OP_NEGATE"),
            OpCode::OpNot => write!(f, "OP_NOT"),
            OpCode::OpAdd => write!(f, "OP_ADD"),
            OpCode::OpSubtract => write!(f, "OP_SUBTRACT"),
            OpCode::OpMultiply => write!(f, "OP_MULTIPLY"),
            OpCode::OpDivide => write!(f, "OP_DIVIDE"),
            OpCode::OpNil => write!(f, "OP_NIL"),
            OpCode::OpTrue => write!(f, "OP_TRUE"),
            OpCode::OpFalse => write!(f, "OP_FALSE"),
            OpCode::OpEq => write!(f, "OP_EQUAL"),
            OpCode::OpGreater => write!(f, "OP_GREATER"),
            OpCode::OpLess => write!(f, "OP_LESS"),
            OpCode::OpPrint => write!(f, "OP_PRINT"),
            OpCode::OpPop => write!(f, "OP_POP"),
            OpCode::OpCall(_) => write!(f, "OP_CALL"),
            OpCode::OpConstant(_) => write!(f, "OP_CONSTANT"),
            OpCode::OpDefGlobal(_) => write!(f, "OP_DEF_GLOBAL"),
            OpCode::OpGetGlobal(_) => write!(f, "OP_GET_GLOBAL"),
            OpCode::OpDefLocal(_) => write!(f, "OP_DEF_LOCAL"),
            OpCode::OpGetLocal(_) => write!(f, "OP_GET_LOCAL"),
            OpCode::OpJumpIfFalse(_) => write!(f, "OP_JUMP_IF_FALSE"),
            OpCode::OpJump(_) => write!(f, "OP_JUMP"),
        }
    }
}

impl std::fmt::Display for Chunk {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(
            f,
            "{:<4} | {:<20} | {:<10} | VALUE",
            "PC", "OP CODE", "OPERAND"
        )?;

        let code = self.code.clone().into_iter().enumerate();
        for (index, byte) in code {
            let _ = write!(f, "{:04} | ", index);
            let _ = match byte {
                // For some reason the width {:20} format parameter doesn't work with OpCode's
                // Display trait, so we first convert it to a string and then pad that string
                OpCode::OpReturn => {
                    writeln!(f, "{:<20} | {:10} |", format!("{}", OpCode::OpReturn), "")
                }
                OpCode::OpNegate => {
                    writeln!(f, "{:<20} | {:10} |", format!("{}", OpCode::OpNegate), "")
                }
                OpCode::OpNot => writeln!(f, "{:<20} | {:10} |", format!("{}", OpCode::OpNot), ""),
                OpCode::OpAdd => writeln!(f, "{:<20} | {:10} |", format!("{}", OpCode::OpAdd), ""),
                OpCode::OpSubtract => {
                    writeln!(f, "{:<20} | {:10} |", format!("{}", OpCode::OpSubtract), "")
                }
                OpCode::OpMultiply => {
                    writeln!(f, "{:<20} | {:10} |", format!("{}", OpCode::OpMultiply), "")
                }
                OpCode::OpDivide => {
                    writeln!(f, "{:<20} | {:10} |", format!("{}", OpCode::OpDivide), "")
                }
                OpCode::OpNil => writeln!(f, "{:<20} | {:10} |", format!("{}", OpCode::OpNil), ""),
                OpCode::OpTrue => {
                    writeln!(f, "{:<20} | {:10} |", format!("{}", OpCode::OpTrue), "")
                }
                OpCode::OpFalse => {
                    writeln!(f, "{:<20} | {:10} |", format!("{}", OpCode::OpFalse), "")
                }
                OpCode::OpEq => writeln!(f, "{:<20} | {:10} |", format!("{}", OpCode::OpEq), ""),
                OpCode::OpGreater => {
                    writeln!(f, "{:<20} | {:10} |", format!("{}", OpCode::OpGreater), "")
                }
                OpCode::OpLess => {
                    writeln!(f, "{:<20} | {:10} |", format!("{}", OpCode::OpLess), "")
                }
                OpCode::OpPrint => {
                    writeln!(f, "{:<20} | {:10} |", format!("{}", OpCode::OpPrint), "")
                }
                OpCode::OpPop => writeln!(f, "{:<20} | {:10} |", format!("{}", OpCode::OpPop), ""),
                OpCode::OpCall(i) => {
                    writeln!(
                        f,
                        "{:<20} | {:10} |",
                        format!("{}", OpCode::OpCall(i)),
                        format!("{}", i)
                    )
                }
                OpCode::OpConstant(i) => writeln!(
                    f,
                    "{:<20} | {:<10} | {}",
                    format!("{}", OpCode::OpConstant(i)),
                    format!("{}", i),
                    format!("{}", self.values[i as usize])
                ),
                OpCode::OpDefGlobal(i) => writeln!(
                    f,
                    "{:<20} | {:<10} | {}",
                    format!("{}", OpCode::OpDefGlobal(i)),
                    format!("{}", i),
                    format!("{}", self.values[i as usize])
                ),
                OpCode::OpGetGlobal(i) => writeln!(
                    f,
                    "{:<20} | {:<10} | {}",
                    format!("{}", OpCode::OpGetGlobal(i)),
                    format!("{}", i),
                    format!("{}", self.values[i as usize])
                ),
                OpCode::OpDefLocal(i) => writeln!(
                    f,
                    "{:<20} | {:<10} | {}",
                    format!("{}", OpCode::OpDefLocal(i)),
                    format!("{}", i),
                    format!("{}", self.values[i as usize])
                ),
                OpCode::OpGetLocal(i) => writeln!(
                    f,
                    "{:<20} | {:<10} | {}",
                    format!("{}", OpCode::OpGetLocal(i)),
                    format!("{}", i),
                    format!("{}", self.values[i as usize])
                ),
                OpCode::OpJumpIfFalse(i) => {
                    writeln!(
                        f,
                        "{:20} | {:<10} |",
                        format!("{}", OpCode::OpJumpIfFalse(i)),
                        format!("{}", i)
                    )
                }
                OpCode::OpJump(i) => writeln!(
                    f,
                    "{:<20} | {:<10} |",
                    format!("{}", OpCode::OpJump(i)),
                    format!("{}", i)
                ),
            };
        }
        Ok(())
    }
}

#[derive(Clone, PartialEq, Debug)]
pub struct Chunk {
    code: Vec<OpCode>,
    values: Vec<Value>,
}

impl Default for Chunk {
    fn default() -> Self {
        Chunk::new()
    }
}

impl Chunk {
    pub fn new() -> Self {
        let values = vec![Value::Nil];
        Self {
            code: Vec::with_capacity(64),
            values,
        }
    }

    pub fn get_op(&self, pc: usize) -> OpCode {
        self.code[pc]
    }

    pub fn len(&self) -> usize {
        self.code.len()
    }

    pub fn is_empty(&self) -> bool {
        self.code.len() == 0
    }

    pub fn push_opcode(&mut self, c: OpCode) {
        self.code.push(c);
    }

    pub fn push_opcodes(&mut self, c1: OpCode, c2: OpCode) {
        self.code.push(c1);
        self.code.push(c2);
    }

    pub fn patch_jump(&mut self, from: usize, to: usize) {
        assert!(
            to > from,
            "patch_jump must only be called for forward jumps!"
        );
        if let Some(op) = self.code.get_mut(from) {
            *op = match op {
                OpCode::OpJump(_) => OpCode::OpJump((to - from) as i32),
                OpCode::OpJumpIfFalse(_) => OpCode::OpJumpIfFalse((to - from) as i32),
                _ => panic!("You lied to me! This instruction should be a Jump op"),
            }
        } else {
            panic!("Please implement some proper error handling...");
        }
    }

    pub fn push_value(&mut self, global: Value) -> u32 {
        self.values.push(global);
        (self.values.len() - 1) as u32
    }

    pub fn push_const(&mut self, c: Value) {
        let i = self.values.len();
        self.values.push(c);
        self.code.push(OpCode::OpConstant(i as u32));
    }

    // Pushes a jump and returns its index
    pub fn push_jump(&mut self, c: OpCode) -> usize {
        assert!(matches!(c, OpCode::OpJump(_) | OpCode::OpJumpIfFalse(_)));
        self.push_opcode(c);
        self.code.len() - 1
    }

    pub fn get_const(&self, i: u32) -> Option<Value> {
        self.values.get(i as usize).cloned()
    }

    // Maybe return an Option?
    pub fn last_const(&self) -> u32 {
        (self.values.len() - 1) as u32
    }

    pub fn last_op(&self) -> usize {
        self.code.len()
    }

    pub fn next_op(&self) -> usize {
        self.code.len()
    }

    pub fn push_defvar(&mut self, code: OpCode) {
        match code {
            OpCode::OpDefGlobal(index) => self.push_opcode(OpCode::OpDefGlobal(index)),
            OpCode::OpDefLocal(index) => self.push_opcode(OpCode::OpDefLocal(index)),
            _ => unreachable!(),
        }
    }

    pub fn push_getvar(&mut self, code: OpCode) {
        match code {
            OpCode::OpGetGlobal(index) => self.push_opcode(OpCode::OpGetGlobal(index)),
            OpCode::OpGetLocal(index) => self.push_opcode(OpCode::OpGetLocal(index)),
            _ => unreachable!(),
        }
    }

    pub fn push_loop(&mut self, loop_index: usize) {
        let index = self.last_op();
        assert!(
            index > loop_index,
            "Loop index must be smaller than index! Dummy"
        );
        let offset = (index - loop_index) as i32;
        self.push_opcode(OpCode::OpJump(-offset));
    }

    pub fn push_return(&mut self) {
        self.push_opcode(OpCode::OpReturn);
    }
}
