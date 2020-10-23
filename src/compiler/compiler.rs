use crate::ast::{BlockStatement, Expression, Statement};
use crate::code::Instruction;
use crate::{
    ast::{ExpressionEnum, Operator, Program, StatementEnum},
    object::Bag,
};

#[derive(Debug)]
pub enum CompilerError {
    InvalidOperator(String),
}

pub struct Bytecode {
    pub instructions: Vec<Instruction>,
    pub constants: Vec<Bag>,
}

pub struct Compiler {
    instructions: Vec<Instruction>,
    constants: Vec<Bag>,
}

impl Compiler {
    pub fn new() -> Self {
        Compiler {
            instructions: Vec::new(),
            constants: Vec::new(),
        }
    }

    pub fn compile(&mut self, program: &Program) -> Result<(), CompilerError> {
        for statement in program.statements.iter() {
            self.compile_statement(statement, true);
        }
        Ok(())
    }

    fn compile_statement(&mut self, statement: &Statement, pop: bool) -> Result<(), CompilerError> {
        match &statement.inner {
            StatementEnum::Return(_) => {}
            StatementEnum::Expression(expr) => {
                if let Some(expr) = expr {
                    self.compile_expression(expr)?;
                    if pop {
                        self.emit(Instruction::Pop);
                    }
                }
            }
            StatementEnum::Let(_) => {}
            StatementEnum::Block(_) => {}
        }
        Ok(())
    }

    fn compile_block_statement(&mut self, block: &BlockStatement) -> Result<(), CompilerError> {
        if block.is_empty() {
            return Ok(());
        }
        let last_statement_index = block.len() - 1;
        for (index, statement) in block.iter().enumerate() {
            self.compile_statement(statement, index != last_statement_index)?;
        }
        Ok(())
    }

    fn compile_expression(&mut self, expression: &Expression) -> Result<(), CompilerError> {
        match &expression.inner {
            ExpressionEnum::IntegerLiteral(value) => {
                let integer = Bag::Integer(value.value);
                let constant = self.add_constant(integer);
                self.emit(Instruction::Constant(constant));
            }
            ExpressionEnum::Infix(infix) => {
                let Operator::Op(op) = &infix.op;
                match op.as_str() {
                    "<" => {
                        self.compile_expression(&infix.right)?;
                        self.compile_expression(&infix.left)?;
                    }
                    _ => {
                        self.compile_expression(&infix.left)?;
                        self.compile_expression(&infix.right)?;
                    }
                }
                let instruction = match op.as_str() {
                    "+" => Instruction::Add,
                    "-" => Instruction::Sub,
                    "*" => Instruction::Mul,
                    "/" => Instruction::Div,
                    "<" => Instruction::GT,
                    ">" => Instruction::GT,
                    "!=" => Instruction::NEq,
                    "==" => Instruction::Eq,
                    _ => return Err(CompilerError::InvalidOperator(op.clone())),
                };
                self.emit(instruction);
            }
            ExpressionEnum::Boolean(value) => {
                if value.value {
                    self.emit(Instruction::True);
                } else {
                    self.emit(Instruction::False);
                }
            }
            ExpressionEnum::Prefix(prefix) => {
                self.compile_expression(&prefix.left)?;
                let Operator::Op(op) = &prefix.operator;
                let instruction = match op.as_str() {
                    "!" => Instruction::Bang,
                    "-" => Instruction::Negate,
                    _ => return Err(CompilerError::InvalidOperator(op.clone())),
                };
                self.emit(instruction);
            }
            ExpressionEnum::If(expr) => {
                self.compile_expression(&expr.condition)?;
                let jump_true = self.emit(Instruction::JumpIfNot(std::usize::MAX));
                self.compile_block_statement(&expr.true_statement)?;
                let jump_false = self.emit(Instruction::Jump(std::usize::MAX));
                self.replace(jump_true, Instruction::JumpIfNot(self.instructions.len()))?;
                if let Some(right) = &expr.false_statement {
                    self.compile_block_statement(&right)?;
                } else {
                    self.emit(Instruction::Null);
                }
                self.replace(jump_false, Instruction::Jump(self.instructions.len()))?;
            }
            _ => {}
        }
        Ok(())
    }

    fn replace(&mut self, pos: usize, instruction: Instruction) -> Result<(), CompilerError> {
        self.instructions[pos] = instruction;
        Ok(())
    }

    fn emit(&mut self, instruction: Instruction) -> usize {
        self.instructions.push(instruction);
        self.instructions.len() - 1
    }

    fn add_constant(&mut self, bag: Bag) -> usize {
        self.constants.push(bag);
        self.constants.len() - 1
    }

    pub fn bytecode(self) -> Bytecode {
        Bytecode {
            instructions: self.instructions,
            constants: self.constants,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parser::parser::Parser;
    use crate::ASTPrinter;
    use crate::TokenStream;

    struct TestCase {
        input: String,
        expected_constants: Vec<Bag>,
        expected_instructions: Vec<Instruction>,
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

            let bytecode = compiler.bytecode();
            test_vec_eq(
                &test.expected_instructions,
                &bytecode.instructions,
                &test.input,
            );
            test_vec_eq(&test.expected_constants, &bytecode.constants, &test.input);
        }
    }

    fn test_vec_eq<T: Eq + std::fmt::Debug>(expected: &Vec<T>, actual: &Vec<T>, input: &str) {
        assert_eq!(
            expected.len(),
            actual.len(),
            "\nwanted: {:?}\ngot: {:?}",
            expected,
            actual
        );
        for (index, instruction) in actual.iter().enumerate() {
            assert_eq!(instruction, &expected[index], "{:?}", input)
        }
    }

    #[test]
    fn test_basic_compile() {
        let tests = vec![TestCase {
            input: "1 + 2".to_string(),
            expected_constants: vec![Bag::Integer(1), Bag::Integer(2)],
            expected_instructions: vec![
                Instruction::Constant(0),
                Instruction::Constant(1),
                Instruction::Add,
                Instruction::Pop,
            ],
        }];
        run_tests(tests);
    }

    fn test_integer_arithmetic() {
        let tests = vec![
            TestCase {
                input: "1 - 2".to_string(),
                expected_constants: vec![Bag::Integer(1), Bag::Integer(2)],
                expected_instructions: vec![
                    Instruction::Constant(0),
                    Instruction::Constant(1),
                    Instruction::Sub,
                    Instruction::Pop,
                ],
            },
            TestCase {
                input: "1 * 2".to_string(),
                expected_constants: vec![Bag::Integer(1), Bag::Integer(2)],
                expected_instructions: vec![
                    Instruction::Constant(0),
                    Instruction::Constant(1),
                    Instruction::Mul,
                    Instruction::Pop,
                ],
            },
            TestCase {
                input: "1 / 2".to_string(),
                expected_constants: vec![Bag::Integer(1), Bag::Integer(2)],
                expected_instructions: vec![
                    Instruction::Constant(0),
                    Instruction::Constant(1),
                    Instruction::Div,
                    Instruction::Pop,
                ],
            },
        ];
        run_tests(tests);
    }

    #[test]
    fn test_boolean() {
        let tests = vec![
            TestCase {
                input: "true".to_string(),
                expected_constants: vec![],
                expected_instructions: vec![Instruction::True, Instruction::Pop],
            },
            TestCase {
                input: "false".to_string(),
                expected_constants: vec![],
                expected_instructions: vec![Instruction::False, Instruction::Pop],
            },
        ];
        run_tests(tests);
    }

    #[test]
    fn test_comparison() {
        let tests = vec![
            TestCase {
                input: "1 < 2".to_string(),
                expected_constants: vec![Bag::Integer(2), Bag::Integer(1)],
                expected_instructions: vec![
                    Instruction::Constant(0),
                    Instruction::Constant(1),
                    Instruction::GT,
                    Instruction::Pop,
                ],
            },
            TestCase {
                input: "1 > 2".to_string(),
                expected_constants: vec![Bag::Integer(1), Bag::Integer(2)],
                expected_instructions: vec![
                    Instruction::Constant(0),
                    Instruction::Constant(1),
                    Instruction::GT,
                    Instruction::Pop,
                ],
            },
            TestCase {
                input: "1 == 2".to_string(),
                expected_constants: vec![Bag::Integer(1), Bag::Integer(2)],
                expected_instructions: vec![
                    Instruction::Constant(0),
                    Instruction::Constant(1),
                    Instruction::Eq,
                    Instruction::Pop,
                ],
            },
            TestCase {
                input: "1 != 2".to_string(),
                expected_constants: vec![Bag::Integer(1), Bag::Integer(2)],
                expected_instructions: vec![
                    Instruction::Constant(0),
                    Instruction::Constant(1),
                    Instruction::NEq,
                    Instruction::Pop,
                ],
            },
        ];
        run_tests(tests);
    }

    #[test]
    fn test_prefix_fn() {
        let tests = vec![
            TestCase {
                input: "!1".to_string(),
                expected_constants: vec![Bag::Integer(1)],
                expected_instructions: vec![
                    Instruction::Constant(0),
                    Instruction::Bang,
                    Instruction::Pop,
                ],
            },
            TestCase {
                input: "!!2".to_string(),
                expected_constants: vec![Bag::Integer(2)],
                expected_instructions: vec![
                    Instruction::Constant(0),
                    Instruction::Bang,
                    Instruction::Bang,
                    Instruction::Pop,
                ],
            },
            TestCase {
                input: "-2".to_string(),
                expected_constants: vec![Bag::Integer(2)],
                expected_instructions: vec![
                    Instruction::Constant(0),
                    Instruction::Negate,
                    Instruction::Pop,
                ],
            },
        ];
        run_tests(tests);
    }
    #[test]
    fn test_if_statement() {
        let tests = vec![
            TestCase {
                input: "if (true) { 10 }; 3333;".to_string(),
                expected_constants: vec![Bag::Integer(10), Bag::Integer(3333)],
                expected_instructions: vec![
                    Instruction::True,
                    Instruction::JumpIfNot(4),
                    Instruction::Constant(0),
                    Instruction::Jump(5),
                    Instruction::Null,
                    Instruction::Pop,
                    Instruction::Constant(1),
                    Instruction::Pop,
                ],
            },
            TestCase {
                input: "if (true) { 10 } else { 20 }; 3333;".to_string(),
                expected_constants: vec![Bag::Integer(10), Bag::Integer(20), Bag::Integer(3333)],
                expected_instructions: vec![
                    Instruction::True,
                    Instruction::JumpIfNot(4),
                    Instruction::Constant(0),
                    Instruction::Jump(5),
                    Instruction::Constant(1),
                    Instruction::Pop,
                    Instruction::Constant(2),
                    Instruction::Pop,
                ],
            },
        ];
        run_tests(tests);
    }
}
