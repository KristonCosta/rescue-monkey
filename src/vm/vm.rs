use std::cmp::Ordering;

use crate::code::Instruction;

use crate::bag::Bag;
use crate::bag::BagError;
use crate::code::Bytecode;

#[derive(Debug)]
pub enum VMError {
    ProgramNotStarted,
    StackOverflow,
    EmptyStackPop,
    InvalidArgument(String, Bag, Option<Bag>),
}

impl From<BagError> for VMError {
    fn from(err: BagError) -> Self {
        match err {
            BagError::ConversionFailure(expected, bag) => {
                VMError::InvalidArgument(expected, bag, None)
            }
            BagError::InvalidInfixForArguments(operation, left, right) => {
                VMError::InvalidArgument(operation, left, Some(right))
            }
            BagError::InvalidPrefixForArgument(operation, left) => {
                VMError::InvalidArgument(operation, left, None)
            }
        }
    }
}

pub struct VM {
    constants: Vec<Bag>,
    instructions: Vec<Instruction>,

    stack: [Bag; 2048],
    stack_pointer: usize,

    pub last_popped: Bag,
}

const STACK_INIT_VAL: Bag = Bag::Nil;

impl VM {
    pub fn new(bytecode: Bytecode) -> Self {
        Self {
            constants: bytecode.constants,
            instructions: bytecode.instructions,

            last_popped: Bag::Nil,

            stack: [STACK_INIT_VAL; 2048],
            stack_pointer: 0,
        }
    }

    pub fn run(&mut self) -> Result<(), VMError> {
        let mut ip = 0 as usize;
        let max_instruction = self.instructions.len();
        while ip < max_instruction {
            let instruction = &self.instructions[ip];
            ip += 1;
            match instruction {
                Instruction::Constant(const_index) => {
                    let constant = self.constants[*const_index as usize].clone();
                    self.push(constant)?;
                }
                Instruction::Add => {
                    let right = self.pop()?;
                    let left = self.pop()?;
                    self.push((left + right)?)?;
                }
                Instruction::Pop => self.last_popped = self.pop()?,
                Instruction::Subtract => {
                    let right = self.pop()?;
                    let left = self.pop()?;
                    self.push((left - right)?)?;
                }
                Instruction::Divide => {
                    let right = self.pop()?;
                    let left = self.pop()?;
                    self.push((left / right)?)?;
                }
                Instruction::Multiply => {
                    let right = self.pop()?;
                    let left = self.pop()?;
                    self.push((left * right)?)?;
                }
                Instruction::True => {
                    self.push(Bag::True)?;
                }
                Instruction::False => {
                    self.push(Bag::False)?;
                }
                Instruction::Equal => {
                    let right = self.pop()?;
                    let left = self.pop()?;
                    self.push(if left == right { Bag::True } else { Bag::False })?;
                }
                Instruction::Greater => self.binary_compare(Ordering::Greater)?,
                Instruction::Less => self.binary_compare(Ordering::Less)?,
                Instruction::Not => {
                    let left = self.pop()?;
                    if left.get_truthy()? {
                        self.push(Bag::False)?;
                    } else {
                        self.push(Bag::True)?;
                    }
                }
                Instruction::Negate => {
                    let left = self.pop()?;
                    self.push((-left)?)?;
                }
                // Instruction::Jump(instr) => {
                //     ip = *instr;
                // }
                // Instruction::JumpIfNot(instr) => {
                //     let target = *instr;
                //     let condition = &self.pop()?;
                //     if !condition.get_truthy()? {
                //         ip = target;
                //     }
                // }
                Instruction::Nil => {
                    self.push(Bag::Nil)?;
                }
            }
        }
        Ok(())
    }

    pub fn push(&mut self, bag: Bag) -> Result<(), VMError> {
        if self.stack_pointer >= 2048 {
            Err(VMError::StackOverflow)
        } else {
            self.stack[self.stack_pointer] = bag;
            self.stack_pointer += 1;
            Ok(())
        }
    }

    pub fn pop(&mut self) -> Result<Bag, VMError> {
        if self.stack_pointer <= 0 {
            Err(VMError::EmptyStackPop)
        } else {
            self.stack_pointer -= 1;
            Ok(std::mem::replace(
                &mut self.stack[self.stack_pointer],
                Bag::Nil,
            ))
        }
    }

    pub fn binary_compare(&mut self, desired_order: Ordering) -> Result<(), VMError> {
        let right = self.pop()?;
        let left = self.pop()?;
        if let Some(order) = left.partial_cmp(&right) {
            self.push(if order == desired_order {
                Bag::True
            } else {
                Bag::False
            })?;
            Ok(())
        } else {
            Err(VMError::InvalidArgument(
                "could not compare bags".to_string(),
                left,
                Some(right),
            ))
        }
    }

    pub fn stack_top(&self) -> Option<&Bag> {
        if self.stack_pointer == 0 {
            return None;
        }
        Some(&self.stack[self.stack_pointer - 1])
    }
}
