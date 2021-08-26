use std::collections::HashMap;

#[derive(PartialEq, Eq, Debug)]
pub enum Instruction {
    Negate,

    Add,
    Subtract,
    Multiply,
    Divide,

    True,
    False,
    Nil,

    Constant(usize),

    Return,
}

impl Instruction {}
