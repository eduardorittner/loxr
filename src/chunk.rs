use crate::Value;
use std::fmt;

#[repr(u8)]
#[derive(Debug, Clone, Copy)]
pub enum OpCode {
    OpReturn,
    OpConstant,
    OpNegate,
    OpAdd,
    OpSubtract,
    OpMultiply,
    OpDivide,
    OpNil,
    OpTrue,
    OpFalse,
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

    fn constant(&self, constants: &Vec<Value>) -> Value {
        // This is safe since we only store indices of values that
        // exist, and never mutate any indices
        unsafe {
            let index = self.index;
            *constants.get_unchecked(index as usize)
        }
    }
}

impl std::fmt::Display for OpCode {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            OpCode::OpReturn => write!(f, "OP_RETURN"),
            OpCode::OpConstant => write!(f, "OP_CONSTANT"),
            OpCode::OpNegate => write!(f, "OP_NEGATE"),
            OpCode::OpAdd => write!(f, "OP_ADD"),
            OpCode::OpSubtract => write!(f, "OP_SUBTRACT"),
            OpCode::OpMultiply => write!(f, "OP_MULTIPLY"),
            OpCode::OpDivide => write!(f, "OP_DIVIDE"),
            OpCode::OpNil => write!(f, "OP_NIL"),
            OpCode::OpTrue => write!(f, "OP_TRUE"),
            OpCode::OpFalse => write!(f, "OP_FALSE"),
        }
    }
}

impl std::fmt::Display for Chunk {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut code = self.code.clone().into_iter().enumerate();
        while let Some((index, byte)) = code.next() {
            let _ = write!(f, "{:04} ", index);
            let _ = match byte.as_op() {
                OpCode::OpReturn => write!(f, "{}", OpCode::OpReturn),
                OpCode::OpConstant => {
                    let (_, constant) = code.next().expect("No index following an OP_CONSTANT");
                    write!(
                        f,
                        "{} {:04} '{}'",
                        OpCode::OpConstant,
                        constant.as_index(),
                        constant.constant(&self.values)
                    )
                }
                OpCode::OpNegate => write!(f, "{}", OpCode::OpNegate),
                OpCode::OpAdd => write!(f, "{}", OpCode::OpAdd),
                OpCode::OpSubtract => write!(f, "{}", OpCode::OpSubtract),
                OpCode::OpMultiply => write!(f, "{}", OpCode::OpMultiply),
                OpCode::OpDivide => write!(f, "{}", OpCode::OpDivide),
                OpCode::OpNil => write!(f, "{}", OpCode::OpNil),
                OpCode::OpTrue => write!(f, "{}", OpCode::OpTrue),
                OpCode::OpFalse => write!(f, "{}", OpCode::OpFalse),
            };
            let _ = write!(f, "\n");
        }
        Ok(())
    }
}

#[derive(Clone, PartialEq, Debug)]
pub struct Chunk {
    code: Vec<ByteCode>,
    values: Vec<Value>,
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

    pub fn push_const(&mut self, c: Value) {
        self.code.push(ByteCode {
            code: OpCode::OpConstant,
        });
        self.code.push(ByteCode {
            index: self.values.len() as u8,
        });
        self.values.push(c);

        // TODO: Add OP_CONSTANT_LONG that is followed by
        // 3 bytes to represent the index, so the max size
        // of self.values would be ~16 million
        if self.values.len() > 256 {
            panic!("Overflow in vec of constants!")
        }
    }

    pub fn push_return(&mut self) {
        self.push_opcode(OpCode::OpReturn);
    }

    pub fn code_at(&self, i: u8) -> OpCode {
        unsafe { self.code.get_unchecked(i as usize).as_op() }
    }

    pub fn const_at(&self, i: u8) -> Value {
        unsafe { self.code.get_unchecked(i as usize).constant(&self.values) }
    }
}
