use std::collections::HashMap;

#[derive(PartialEq, Eq, Debug)]
pub enum Instruction {
    Negate,
    Not,

    Add,
    Subtract,
    Multiply,
    Divide,

    Equal,
    Greater,
    Less,

    True,
    False,
    Nil,

    Constant(usize),

    Pop,
}

impl Instruction {}
