use crate::Value;
use std::fmt;

#[repr(u8)]
#[derive(Debug, Clone, Copy)]
pub enum OpCode {
    OpReturn,
    OpConstant,
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
    OpDefGlobal,
    OpGetGlobal,
    OpDefLocal,
    OpGetLocal,
    OpJumpIfFalse,
    OpJump,
    OpLoop, // A backwards jump
}

#[derive(Clone, Copy)]
pub union ByteCode {
    code: OpCode,
    index: u8,
}

impl std::cmp::PartialEq for ByteCode {
    fn eq(&self, other: &Self) -> bool {
        self.as_index() == other.as_index()
    }
}

impl fmt::Debug for ByteCode {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.as_index())
    }
}

impl ByteCode {
    fn as_op(&self) -> OpCode {
        unsafe { self.code }
    }

    fn as_index(&self) -> u8 {
        unsafe { self.index }
    }

    fn constant(&self, constants: &[Value]) -> Value {
        // This is safe since we only store indices of values that
        // exist, and never mutate any indices
        unsafe {
            let index = self.index;
            constants.get_unchecked(index as usize).clone()
        }
    }
}

impl std::fmt::Display for OpCode {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            OpCode::OpReturn => write!(f, "OP_RETURN"),
            OpCode::OpConstant => write!(f, "OP_CONSTANT"),
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
            OpCode::OpDefGlobal => write!(f, "OP_DEF_GLOBAL"),
            OpCode::OpGetGlobal => write!(f, "OP_GET_GLOBAL"),
            OpCode::OpDefLocal => write!(f, "OP_DEF_LOCAL"),
            OpCode::OpGetLocal => write!(f, "OP_GET_LOCAL"),
            OpCode::OpJumpIfFalse => write!(f, "OP_JUMP_IF_FALSE"),
            OpCode::OpJump => write!(f, "OP_JUMP"),
            OpCode::OpLoop => write!(f, "OP_LOOP"),
        }
    }
}

impl std::fmt::Display for Chunk {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(f, " PC  | OP CODE             | OP   | VALUE",)?;
        // Utility closure to print an instruction and its operand
        let op_and_index =
            |f: &mut fmt::Formatter<'_>, op: OpCode, constant: ByteCode, values| -> fmt::Result {
                let op_string = String::from(op.to_string());
                let len = op_string.len();
                if len >= 20 {
                    panic!("op code name too long, please refactor this code");
                }
                write!(
                    f,
                    "{}{}| {:04} | '{}'",
                    op,
                    " ".repeat(20 - len),
                    constant.as_index(),
                    constant.constant(values)
                )
            };

        let jump =
            |f: &mut fmt::Formatter<'_>, op: OpCode, c1: ByteCode, c2: ByteCode| -> fmt::Result {
                let op_string = String::from(op.to_string());
                let len = op_string.len();
                if len >= 20 {
                    panic!("op code name too long, please refactor this code");
                }
                write!(
                    f,
                    "{}{}| {:08}",
                    op,
                    " ".repeat(20 - len),
                    (c1.as_index() as u16 | (c2.as_index() as u16) << 8),
                )
            };

        let mut code = self.code.clone().into_iter().enumerate();
        while let Some((index, byte)) = code.next() {
            let _ = write!(f, "{:04} | ", index);
            let _ = match byte.as_op() {
                OpCode::OpReturn => write!(f, "{}", OpCode::OpReturn),
                OpCode::OpConstant => {
                    let (_, constant) = code.next().expect("No index following an OP_CONSTANT");
                    op_and_index(f, OpCode::OpConstant, constant, &self.values)
                }
                OpCode::OpNegate => write!(f, "{}", OpCode::OpNegate),
                OpCode::OpNot => write!(f, "{}", OpCode::OpNot),
                OpCode::OpAdd => write!(f, "{}", OpCode::OpAdd),
                OpCode::OpSubtract => write!(f, "{}", OpCode::OpSubtract),
                OpCode::OpMultiply => write!(f, "{}", OpCode::OpMultiply),
                OpCode::OpDivide => write!(f, "{}", OpCode::OpDivide),
                OpCode::OpNil => write!(f, "{}", OpCode::OpNil),
                OpCode::OpTrue => write!(f, "{}", OpCode::OpTrue),
                OpCode::OpFalse => write!(f, "{}", OpCode::OpFalse),
                OpCode::OpEq => write!(f, "{}", OpCode::OpEq),
                OpCode::OpGreater => write!(f, "{}", OpCode::OpGreater),
                OpCode::OpLess => write!(f, "{}", OpCode::OpLess),
                OpCode::OpPrint => write!(f, "{}", OpCode::OpPrint),
                OpCode::OpPop => write!(f, "{}", OpCode::OpPop),
                OpCode::OpDefGlobal => {
                    let (_, constant) = code.next().expect("No index following an OP_DEF_GLOBAL");
                    op_and_index(f, OpCode::OpDefGlobal, constant, &self.values)
                }
                OpCode::OpGetGlobal => {
                    let (_, constant) = code.next().expect("No index following an OP_GET_GLOBAL");
                    op_and_index(f, OpCode::OpGetGlobal, constant, &self.values)
                }
                OpCode::OpDefLocal => {
                    let (_, constant) = code.next().expect("No index following an OP_DEF_LOCAL");
                    op_and_index(f, OpCode::OpDefLocal, constant, &self.values)
                }
                OpCode::OpGetLocal => {
                    let (_, constant) = code.next().expect("No index following an OP_GET_LOCAL");
                    op_and_index(f, OpCode::OpGetLocal, constant, &self.values)
                }
                OpCode::OpJumpIfFalse => {
                    let (_, c1) = code.next().expect("No jump offset after JUMP_IF_FALSE");
                    let (_, c2) = code.next().expect("No jump offset after JUMP_IF_FALSE");
                    jump(f, OpCode::OpJumpIfFalse, c1, c2)
                }
                OpCode::OpJump => {
                    let (_, c1) = code.next().expect("No jump offset after JUMP");
                    let (_, c2) = code.next().expect("No jump offset after JUMP");
                    jump(f, OpCode::OpJump, c1, c2)
                }
                OpCode::OpLoop => {
                    let (_, c1) = code.next().expect("No jump offset after JUMP");
                    let (_, c2) = code.next().expect("No jump offset after JUMP");
                    jump(f, OpCode::OpLoop, c1, c2)
                }
            };
            let _ = writeln!(f);
        }
        Ok(())
    }
}

#[derive(Clone, PartialEq, Debug)]
pub struct Chunk {
    code: Vec<ByteCode>,
    values: Vec<Value>,
}

impl Default for Chunk {
    fn default() -> Self {
        Chunk::new()
    }
}

impl Chunk {
    pub fn new() -> Self {
        Self {
            code: Vec::with_capacity(64),
            values: Vec::with_capacity(16),
        }
    }

    pub fn push_opcode(&mut self, c: OpCode) {
        self.code.push(ByteCode { code: c });
    }

    pub fn push_opcodes(&mut self, c1: OpCode, c2: OpCode) {
        self.code.push(ByteCode { code: c1 });
        self.code.push(ByteCode { code: c2 });
    }

    pub fn patch_jump(&mut self, offset: u16, jump: u16) {
        *self.code.get_mut((offset - 1) as usize).unwrap() = ByteCode { index: jump as u8 };
        *self.code.get_mut(offset as usize).unwrap() = ByteCode {
            index: (jump >> 8) as u8,
        };
    }

    pub fn push_const(&mut self, c: Value) {
        self.values.push(c);
        // TODO: Add OP_CONSTANT_LONG that is followed by
        // 3 bytes to represent the index, so the max size
        // of self.values would be ~16 million
        if self.values.len() > 256 {
            panic!("Overflow in vec of constants!")
        }
    }

    pub fn push_opconst(&mut self, c: Value) {
        self.code.push(ByteCode {
            code: OpCode::OpConstant,
        });
        self.code.push(ByteCode {
            index: self.values.len() as u8,
        });
        self.push_const(c);
    }

    // Maybe return an Option?
    pub fn last_const(&self) -> u8 {
        (self.values.len() - 1) as u8
    }

    pub fn last_op(&self) -> u16 {
        self.code.len() as u16
    }

    pub fn push_defvar(&mut self, index: u8, code: OpCode) {
        match code {
            OpCode::OpDefGlobal => self.push_opcode(OpCode::OpDefGlobal),
            OpCode::OpDefLocal => self.push_opcode(OpCode::OpDefLocal),
            _ => unreachable!(),
        }
        self.code.push(ByteCode { index });
    }

    pub fn push_getvar(&mut self, index: u8, code: OpCode) {
        match code {
            OpCode::OpGetGlobal => self.push_opcode(OpCode::OpGetGlobal),
            OpCode::OpGetLocal => self.push_opcode(OpCode::OpGetLocal),
            _ => unreachable!(),
        }
        self.code.push(ByteCode { index });
    }

    pub fn push_jump(&mut self, code: OpCode) -> u16 {
        self.push_opcode(code);
        self.code.push(ByteCode { index: 0 });
        self.code.push(ByteCode { index: 0 });
        return self.code.len() as u16 - 1;
    }

    pub fn push_loop(&mut self, loop_index: u16) {
        self.push_opcode(OpCode::OpLoop);

        let offset = self.last_op() + 2 - loop_index;
        self.code.push(ByteCode {
            index: offset as u8,
        });
        self.code.push(ByteCode {
            index: (offset >> 8) as u8,
        });
    }

    pub fn push_return(&mut self) {
        self.push_opcode(OpCode::OpReturn);
    }

    pub fn code_at(&self, i: u16) -> OpCode {
        unsafe { self.code.get_unchecked(i as usize).as_op() }
    }

    pub fn index_at(&self, i: u16) -> u8 {
        unsafe { self.code.get_unchecked(i as usize).as_index() }
    }

    pub fn const_at(&self, i: u16) -> Value {
        unsafe { self.code.get_unchecked(i as usize).constant(&self.values) }
    }
}
