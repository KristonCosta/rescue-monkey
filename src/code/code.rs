use std::collections::HashMap;

#[derive(PartialEq, Eq, Debug)]
pub enum Instruction {
    Constant(usize),
    Add,
    Sub,
    Mul,
    Div,
    Pop,
    Null,
    True,
    False,
    Eq,
    NEq,
    GT,
    Bang,
    Negate,
    JumpIfNot(usize),
    Jump(usize),
}

impl Instruction {}
