use crate::bag::Object;
use crate::code::Instruction;

use crate::bag::Bag;
use crate::bag::BagError;
use crate::compiler::compiler::Bytecode;

#[derive(Debug)]
pub enum VMError {
    ProgramNotStarted,
    StackOverflow,
    EmptyStackPop,
    InvalidArgument(String, Bag),
}

impl From<BagError> for VMError {
    fn from(err: BagError) -> Self {
        match err {
            BagError::ConversionFailure(expected, bag) => VMError::InvalidArgument(expected, bag),
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
                    let right = self.pop()?.get_int()?;
                    let left = self.pop()?.get_int()?;
                    self.push(Bag::Integer(left + right))?;
                }
                Instruction::Pop => self.last_popped = self.pop()?,
                Instruction::Subtract => {
                    let right = self.pop()?.get_int()?;
                    let left = self.pop()?.get_int()?;
                    self.push(Bag::Integer(left - right))?;
                }
                Instruction::Divide => {
                    let right = self.pop()?.get_int()?;
                    let left = self.pop()?.get_int()?;
                    self.push(Bag::Integer(left / right))?;
                }
                Instruction::Multiply => {
                    let right = self.pop()?.get_int()?;
                    let left = self.pop()?.get_int()?;
                    self.push(Bag::Integer(left * right))?;
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
                Instruction::Greater => {
                    let right = self.pop()?;
                    let left = self.pop()?;
                    self.push(if left > right { Bag::True } else { Bag::False })?;
                }
                Instruction::Less => {
                    let right = self.pop()?;
                    let left = self.pop()?;
                    self.push(if left < right { Bag::True } else { Bag::False })?;
                }
                Instruction::Not => {
                    let left = self.pop()?;
                    if left.get_truthy()? {
                        self.push(Bag::False)?;
                    } else {
                        self.push(Bag::True)?;
                    }
                }
                Instruction::Negate => {
                    let left = self.pop()?.get_int()?;
                    self.push(Bag::Integer(-left))?;
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

    pub fn stack_top(&self) -> Option<&Bag> {
        if self.stack_pointer == 0 {
            return None;
        }
        Some(&self.stack[self.stack_pointer - 1])
    }
}
