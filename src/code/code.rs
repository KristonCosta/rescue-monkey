use std::{collections::HashMap, fmt::Display, intrinsics::discriminant_value};

use crate::bag::Bag;

#[repr(u32)]
#[derive(PartialEq, Eq, Debug)]
pub enum Instruction {
    Negate = 1,
    Not = 2,

    Add = 10,
    Subtract = 11,
    Multiply = 12,
    Divide = 13,

    Equal = 20,
    Greater = 21,
    Less = 22,

    True = 30,
    False = 31,
    Nil = 32,

    Constant(usize) = 40,

    Pop = 100,
}

impl Display for Instruction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Instruction::Constant(val) => write!(f, "{:<15}{}", "OP_CONSTANT", val),
            this => write!(
                f,
                "{}",
                match this {
                    Instruction::Negate => "OP_NEGATE",
                    Instruction::Not => "OP_NOT",
                    Instruction::Add => "OP_ADD",
                    Instruction::Subtract => "OP_SUBTRACT",
                    Instruction::Multiply => "OP_MULTIPLY",
                    Instruction::Divide => "OP_DIVIDE",
                    Instruction::Equal => "OP_EQUAL",
                    Instruction::Greater => "OP_GREATER",
                    Instruction::Less => "OP_LESS",
                    Instruction::Pop => "OP_POP",
                    Instruction::True => "OP_TRUE",
                    Instruction::False => "OP_FALSE",
                    Instruction::Nil => "OP_NIL",
                    _ => unimplemented!("cannot display instruction {:?}", this),
                }
            ),
        }
    }
}
#[derive(Debug)]
pub struct Bytecode {
    pub instructions: Vec<Instruction>,
    pub constants: Vec<Bag>,
    pub lines: Vec<u32>,
}

impl Display for Bytecode {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut line = 0;
        let same_line = "|".to_string();
        let mut current_line_string = "".to_string();
        for (index, instruction) in self.instructions.iter().enumerate() {
            let current_line = self.lines[index];

            let formatted = if current_line != line && current_line != 0 {
                current_line_string = current_line.to_string();
                &current_line_string
            } else {
                &same_line
            };

            write!(
                f,
                "{:0>4}    {}    {}",
                discriminant_value(instruction),
                formatted,
                instruction,
            )?;

            match instruction {
                Instruction::Constant(val) => writeln!(
                    f,
                    "    {}",
                    self.constants
                        .get(*val)
                        .map_or("INVALID".to_string(), |val| { format!("{:?}", val) })
                )?,
                _ => writeln!(f, "")?,
            }
            line = self.lines[index];
        }
        Ok(())
    }
}
