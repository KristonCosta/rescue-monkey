use crate::code::Instruction;
use crate::compiler::Bytecode;
use crate::object::Bag;
use crate::object::BagError;

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

impl VM {
    pub fn new(bytecode: Bytecode) -> Self {
        Self {
            constants: bytecode.constants,
            instructions: bytecode.instructions,

            last_popped: Bag::Null,

            stack: [Bag::Null; 2048],
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
                Instruction::Sub => {
                    let right = self.pop()?.get_int()?;
                    let left = self.pop()?.get_int()?;
                    self.push(Bag::Integer(left - right))?;
                }
                Instruction::Div => {
                    let right = self.pop()?.get_int()?;
                    let left = self.pop()?.get_int()?;
                    self.push(Bag::Integer(left / right))?;
                }
                Instruction::Mul => {
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
                Instruction::Eq => {
                    let right = self.pop()?;
                    let left = self.pop()?;
                    self.push(if left == right { Bag::True } else { Bag::False })?;
                }
                Instruction::NEq => {
                    let right = self.pop()?;
                    let left = self.pop()?;
                    self.push(if left != right { Bag::True } else { Bag::False })?;
                }
                Instruction::GT => {
                    let right = self.pop()?;
                    let left = self.pop()?;
                    self.push(if left > right { Bag::True } else { Bag::False })?;
                }
                Instruction::Bang => {
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
                Instruction::Jump(instr) => {
                    ip = *instr;
                }
                Instruction::JumpIfNot(instr) => {
                    let target = *instr;
                    let condition = &self.pop()?;
                    if !condition.get_truthy()? {
                        ip = target;
                    }
                }
                Instruction::Null => {
                    self.push(Bag::Null)?;
                }
                _ => unimplemented!(),
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
                Bag::Null,
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

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::Program;
    use crate::compiler::Compiler;
    use crate::parser::parser::Parser;
    use crate::ASTPrinter;
    use crate::{code::Instruction, object::Bag, TokenStream};

    struct TestCase {
        input: String,
        expected: Bag,
    }

    fn get_program(input: &str) -> Program {
        let tokens = TokenStream::from_string(input);
        let parser = &mut Parser::new(tokens);
        let program = parser.parse();
        program
    }

    fn run_tests(tests: Vec<TestCase>) {
        for test in tests {
            let program = get_program(&test.input);
            let mut compiler = Compiler::new();
            let result = compiler.compile(&program);
            assert!(
                result.is_ok(),
                "failed to compile {:?}",
                result.err().unwrap()
            );

            let mut vm = VM::new(compiler.bytecode());
            vm.run();
            let actual = vm.stack_top();
            assert!(
                actual.is_none(),
                "element left on stack {:?}",
                actual.unwrap()
            );
            assert_eq!(test.expected, vm.last_popped, "input: {:?}", &test.input);
        }
    }

    #[test]
    fn test_integer_arithmetic() {
        let tests = vec![
            TestCase {
                input: "1".to_string(),
                expected: Bag::Integer(1),
            },
            TestCase {
                input: "2".to_string(),
                expected: Bag::Integer(2),
            },
            TestCase {
                input: "1 + 2".to_string(),
                expected: Bag::Integer(3),
            },
            TestCase {
                input: "1 - 2".to_string(),
                expected: Bag::Integer(-1),
            },
            TestCase {
                input: "1 * 2".to_string(),
                expected: Bag::Integer(2),
            },
            TestCase {
                input: "4 / 2".to_string(),
                expected: Bag::Integer(2),
            },
            TestCase {
                input: "50 / 2 * 2 + 10 - 5".to_string(),
                expected: Bag::Integer(55),
            },
            TestCase {
                input: "5 * (2 + 10)".to_string(),
                expected: Bag::Integer(60),
            },
        ];
        run_tests(tests);
    }

    #[test]
    fn test_boolean() {
        let tests = vec![
            TestCase {
                input: "true".to_string(),
                expected: Bag::True,
            },
            TestCase {
                input: "false".to_string(),
                expected: Bag::False,
            },
        ];
        run_tests(tests);
    }

    #[test]
    fn test_comparison() {
        let tests = vec![
            TestCase {
                input: "1 < 2".to_string(),
                expected: Bag::True,
            },
            TestCase {
                input: "1 > 2".to_string(),
                expected: Bag::False,
            },
            TestCase {
                input: "1 < 1".to_string(),
                expected: Bag::False,
            },
            TestCase {
                input: "1 > 1".to_string(),
                expected: Bag::False,
            },
            TestCase {
                input: "1 == 1".to_string(),
                expected: Bag::True,
            },
            TestCase {
                input: "1 != 1".to_string(),
                expected: Bag::False,
            },
            TestCase {
                input: "1 == 2".to_string(),
                expected: Bag::False,
            },
            TestCase {
                input: "(1 != 2) == true".to_string(),
                expected: Bag::True,
            },
            TestCase {
                input: "(3 < 1) != true".to_string(),
                expected: Bag::True,
            },
            TestCase {
                input: "(3 > 2) > (3 < 2)".to_string(),
                expected: Bag::True,
            },
            TestCase {
                input: "(1 > 2) == (2 != 2)".to_string(),
                expected: Bag::True,
            },
        ];
        run_tests(tests);
    }

    #[test]
    fn test_prefix_negate() {
        let tests = vec![
            TestCase {
                input: "-5".to_string(),
                expected: Bag::Integer(-5),
            },
            TestCase {
                input: "-10".to_string(),
                expected: Bag::Integer(-10),
            },
            TestCase {
                input: "-50 + 100 + -50".to_string(),
                expected: Bag::Integer(0),
            },
            TestCase {
                input: "(5 + 10 * 2 + 15 / 3) * 2 + -10".to_string(),
                expected: Bag::Integer(50),
            },
        ];
        run_tests(tests);
    }

    #[test]
    fn test_prefix_bang() {
        let tests = vec![
            TestCase {
                input: "!true".to_string(),
                expected: Bag::False,
            },
            TestCase {
                input: "!false".to_string(),
                expected: Bag::True,
            },
            TestCase {
                input: "!5".to_string(),
                expected: Bag::False,
            },
            TestCase {
                input: "!!true".to_string(),
                expected: Bag::True,
            },
            TestCase {
                input: "!!false".to_string(),
                expected: Bag::False,
            },
            TestCase {
                input: "!!5".to_string(),
                expected: Bag::True,
            },
        ];
        run_tests(tests);
    }

    #[test]
    fn test_if_statement() {
        let tests = vec![
            TestCase {
                input: "if (true) { 10 }".to_string(),
                expected: Bag::Integer(10),
            },
            TestCase {
                input: "if (true) { 10 } else {20}".to_string(),
                expected: Bag::Integer(10),
            },
            TestCase {
                input: "if (false) { 10 } else { 20 }".to_string(),
                expected: Bag::Integer(20),
            },
            TestCase {
                input: "if (1) { 10 }".to_string(),
                expected: Bag::Integer(10),
            },
            TestCase {
                input: "if (1 < 2) { 10 } else { 20 }".to_string(),
                expected: Bag::Integer(10),
            },
            TestCase {
                input: "if (1 > 2) { 10 } else { 20 }".to_string(),
                expected: Bag::Integer(20),
            },
            TestCase {
                input: "if (false) { 10; }".to_string(),
                expected: Bag::Null,
            },
            TestCase {
                input: "if (!(if (false) { 11 })) { 10 } else { 20 }".to_string(),
                expected: Bag::Integer(10),
            },
        ];
        run_tests(tests);
    }
}
