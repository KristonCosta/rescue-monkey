use crate::bag::{Bag, Object};
use crate::code::{Bytecode, Instruction};
use crate::lexer::Scanner;
use crate::token::{Token, TokenType};

#[derive(Debug)]
pub enum ParserError {
    ExpectPeek {
        token: Option<Token>,
        expected: TokenType,
    },
    TypeParse {
        token: Option<Token>,
        desired_type: String,
    },
    InvalidValueToken {
        token: Option<Token>,
        parse_type: String,
    },
    EoF {
        token: Option<Token>,
        description: String,
    },
    InvalidPrefixFn {
        token: Option<Token>,
    },
    InvalidInfixFn {
        token: Option<Token>,
    },
    UnexpectedRule {
        token: Option<Token>,
        rule_type: String,
    },
}

pub struct Rule {
    precedence: Precedence,
    prefix: Option<ParseFn>,
    infix: Option<ParseFn>,
}

impl Default for Rule {
    fn default() -> Self {
        Self {
            precedence: Precedence::Bottom,
            prefix: Default::default(),
            infix: Default::default(),
        }
    }
}

#[derive(Debug)]
pub enum ParseFn {
    Grouping,
    Infix,
    Prefix,
    Number,
    Literal,
    String,
}

#[derive(Debug, Default)]
pub struct Chunk {
    instructions: Vec<Instruction>,
    lines: Vec<u32>,
    constants: Vec<Bag>,
}

#[derive(Debug)]
pub enum CompilerError {
    InvalidOperator(String),
}

pub struct Compiler {
    tokens: Scanner,
    chunk: Chunk,

    errors: Vec<ParserError>,
}

impl Compiler {
    pub fn new(tokens: Scanner) -> Self {
        Self {
            tokens,
            errors: vec![],
            chunk: Default::default(),
        }
    }

    fn replace(&mut self, pos: usize, instruction: Instruction) -> Result<(), CompilerError> {
        self.chunk.instructions[pos] = instruction;
        Ok(())
    }

    fn emit(&mut self, instruction: Instruction, line: u32) -> usize {
        self.chunk.instructions.push(instruction);
        self.chunk.lines.push(line);
        self.chunk.instructions.len() - 1
    }

    fn add_constant(&mut self, bag: Bag) -> usize {
        self.chunk.constants.push(bag);
        self.chunk.constants.len() - 1
    }

    fn emit_constant(&mut self, bag: Bag, line: u32) -> usize {
        let index = self.add_constant(bag);
        self.emit(Instruction::Constant(index), line)
    }

    fn bytecode(&mut self) -> Bytecode {
        let mut chunk = Default::default();
        std::mem::swap(&mut self.chunk, &mut chunk);
        let Chunk {
            instructions,
            constants,
            lines,
        } = chunk;
        Bytecode {
            instructions,
            constants,
            lines,
        }
    }

    pub fn errors(&self) -> &Vec<ParserError> {
        &self.errors
    }

    fn expect_peek(&mut self, token_type: &TokenType) -> bool {
        if self.tokens.peek_is(token_type) {
            self.tokens.next();
            true
        } else {
            self.errors.push(ParserError::ExpectPeek {
                token: Some(self.tokens.peek().clone()),
                expected: token_type.clone(),
            });
            false
        }
    }

    fn expect_peek_and_consume(&mut self, token_type: &TokenType) -> bool {
        if self.expect_peek(token_type) {
            self.tokens.next();
            true
        } else {
            false
        }
    }

    fn advance(&mut self) -> Token {
        self.tokens.next()
    }

    fn expect(&mut self, token_type: &TokenType) -> bool {
        self.tokens.current_is(token_type)
    }

    fn consume(&mut self, token_type: &TokenType) -> bool {
        if self.expect(token_type) {
            self.tokens.next();
            true
        } else {
            false
        }
    }

    fn try_eat_semicolon(&mut self) {
        if self.tokens.peek_is(&TokenType::Semicolon) {
            self.tokens.next();
        }
    }

    pub fn compile(&mut self) -> Bytecode {
        self.compile_expression();
        self.expect_peek_and_consume(&TokenType::EndOfFile);
        self.emit(Instruction::Pop, 0);
        self.bytecode()
    }

    fn compile_expression(&mut self) {
        self.parse_precedence(Precedence::Bottom);
    }

    fn compile_grouping(&mut self) {
        if self.consume(&TokenType::LeftParen) {
            self.compile_expression();
            self.consume(&TokenType::RightParen);
        }
    }

    fn compile_infix(&mut self) {
        let operator = self.tokens.next();
        let rule = get_rule(operator.token_type());

        self.parse_precedence(rule.precedence.next());

        match operator.token_type() {
            TokenType::Plus => self.emit(Instruction::Add, operator.line),
            TokenType::Minus => self.emit(Instruction::Subtract, operator.line),
            TokenType::Star => self.emit(Instruction::Multiply, operator.line),
            TokenType::Slash => self.emit(Instruction::Divide, operator.line),
            TokenType::BangEqual => {
                self.emit(Instruction::Equal, operator.line);
                self.emit(Instruction::Not, operator.line)
            }
            TokenType::EqualEqual => self.emit(Instruction::Equal, operator.line),
            TokenType::Greater => self.emit(Instruction::Greater, operator.line),
            TokenType::Less => self.emit(Instruction::Less, operator.line),
            TokenType::GreaterEqual => {
                self.emit(Instruction::Less, operator.line);
                self.emit(Instruction::Not, operator.line)
            }
            TokenType::LessEqual => {
                self.emit(Instruction::Greater, operator.line);
                self.emit(Instruction::Not, operator.line)
            }
            _ => {
                self.errors.push(ParserError::InvalidInfixFn {
                    token: Some(operator),
                });
                return;
            }
        };
    }

    fn compile_prefix(&mut self) {
        let operator = self.tokens.next();

        self.parse_precedence(Precedence::Prefix);

        match operator.token_type() {
            TokenType::Minus => {
                self.emit(Instruction::Negate, operator.line);
            }
            TokenType::Bang => {
                self.emit(Instruction::Not, operator.line);
            }
            _ => self.errors.push(ParserError::InvalidPrefixFn {
                token: Some(operator),
            }),
        }
    }

    fn compile_literal(&mut self) {
        let instruction = match self.tokens.current().token_type() {
            TokenType::True => Instruction::True,
            TokenType::False => Instruction::False,
            TokenType::Nil => Instruction::Nil,
            _ => return,
        };
        self.emit(instruction, self.tokens.current().line);
        self.advance();
    }

    fn parse_precedence(&mut self, precedence: Precedence) {
        let tok = self.tokens.current();
        let rule = get_rule(tok.token_type());

        if let Some(prefix_fn) = rule.prefix {
            self.handle_parse_fn(prefix_fn);
        } else {
            self.errors.push(ParserError::UnexpectedRule {
                token: Some(tok.clone()),
                rule_type: "prefix".to_string(),
            });
            return;
        }

        let mut next_rule = get_rule(self.tokens.current().token_type());
        while precedence <= next_rule.precedence {
            if let Some(infix_fn) = next_rule.infix {
                self.handle_parse_fn(infix_fn);
            } else {
                break;
            }
            next_rule = get_rule(self.tokens.current().token_type());
        }
    }

    fn handle_parse_fn(&mut self, parse_fn: ParseFn) {
        match parse_fn {
            ParseFn::Grouping => self.compile_grouping(),
            ParseFn::Infix => self.compile_infix(),
            ParseFn::Prefix => self.compile_prefix(),
            ParseFn::Number => {
                if self.parse_integer_literal() {
                    self.advance();
                }
            }
            ParseFn::Literal => self.compile_literal(),
            ParseFn::String => {
                if self.parse_string() {
                    self.advance();
                }
            }
        }
    }

    fn verify_value_type(&self, tok: &Token, expected: &str) -> Result<(), ParserError> {
        if !tok.is_value_type() {
            Err(ParserError::InvalidValueToken {
                token: Some(tok.clone()),
                parse_type: expected.to_string(),
            })
        } else {
            Ok(())
        }
    }

    fn parse_integer_literal(&mut self) -> bool {
        let tok = self.tokens.current();
        match self.verify_value_type(tok, "parse_integer_literal") {
            Ok(_) => {
                let value = tok.value().unwrap();
                match value.parse::<i64>() {
                    Ok(n) => {
                        self.emit_constant(Bag::Integer(n), tok.line);
                        return true;
                    }
                    Err(_e) => {
                        self.errors.push(ParserError::TypeParse {
                            desired_type: "i64".to_string(),
                            token: Some(tok.clone()),
                        });
                    }
                }
            }
            Err(e) => self.errors.push(e),
        }

        false
    }

    fn parse_string(&mut self) -> bool {
        let tok = self.tokens.current();
        match self.verify_value_type(tok, "parse_string") {
            Ok(_) => {
                let value = tok.value().unwrap().clone();
                self.emit_constant(Bag::Obj(Object::String(value)), tok.line);
                return true;
            }
            Err(e) => self.errors.push(e),
        };
        false
    }
}

#[derive(Eq, PartialEq)]
enum Precedence {
    Bottom,
    Assignment,
    Or,
    And,
    Equality,
    Comparison,
    Sum,
    Product,
    Prefix,
    Call,
    Top,
}

impl Into<usize> for &Precedence {
    fn into(self) -> usize {
        match self {
            Precedence::Bottom => 1,
            Precedence::Assignment => 2,
            Precedence::Or => 3,
            Precedence::And => 4,
            Precedence::Equality => 5,
            Precedence::Comparison => 6,
            Precedence::Sum => 7,
            Precedence::Product => 8,
            Precedence::Prefix => 9,
            Precedence::Call => 10,
            Precedence::Top => 11,
        }
    }
}

impl Precedence {
    pub fn next(&self) -> Precedence {
        match self {
            Precedence::Bottom => Precedence::Assignment,
            Precedence::Assignment => Precedence::Or,
            Precedence::Or => Precedence::And,
            Precedence::And => Precedence::Equality,
            Precedence::Equality => Precedence::Comparison,
            Precedence::Comparison => Precedence::Sum,
            Precedence::Sum => Precedence::Product,
            Precedence::Product => Precedence::Prefix,
            Precedence::Prefix => Precedence::Call,
            Precedence::Call => Precedence::Top,
            Precedence::Top => Precedence::Top,
        }
    }
}

impl Ord for Precedence {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        let x: usize = self.into();
        x.cmp(&other.into())
    }
}

impl PartialOrd for Precedence {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

fn get_rule(token_type: &TokenType) -> Rule {
    // TODO: Make this a table lookup
    match token_type {
        TokenType::LeftParen => Rule {
            prefix: Some(ParseFn::Grouping),
            ..Default::default()
        },
        TokenType::Minus => Rule {
            precedence: Precedence::Sum,
            prefix: Some(ParseFn::Prefix),
            infix: Some(ParseFn::Infix),
        },
        TokenType::Plus => Rule {
            precedence: Precedence::Sum,
            infix: Some(ParseFn::Infix),
            ..Default::default()
        },
        TokenType::Slash => Rule {
            precedence: Precedence::Product,
            infix: Some(ParseFn::Infix),
            ..Default::default()
        },
        TokenType::Star => Rule {
            precedence: Precedence::Product,
            infix: Some(ParseFn::Infix),
            ..Default::default()
        },
        TokenType::Number(_) => Rule {
            prefix: Some(ParseFn::Number),
            ..Default::default()
        },
        TokenType::True => Rule {
            prefix: Some(ParseFn::Literal),
            ..Default::default()
        },
        TokenType::False => Rule {
            prefix: Some(ParseFn::Literal),
            ..Default::default()
        },
        TokenType::Nil => Rule {
            prefix: Some(ParseFn::Literal),
            ..Default::default()
        },
        TokenType::Bang => Rule {
            prefix: Some(ParseFn::Prefix),
            ..Default::default()
        },
        TokenType::BangEqual => Rule {
            infix: Some(ParseFn::Infix),
            precedence: Precedence::Equality,
            ..Default::default()
        },
        TokenType::EqualEqual => Rule {
            infix: Some(ParseFn::Infix),
            precedence: Precedence::Equality,
            ..Default::default()
        },
        TokenType::Greater => Rule {
            infix: Some(ParseFn::Infix),
            precedence: Precedence::Comparison,
            ..Default::default()
        },
        TokenType::GreaterEqual => Rule {
            infix: Some(ParseFn::Infix),
            precedence: Precedence::Comparison,
            ..Default::default()
        },
        TokenType::Less => Rule {
            infix: Some(ParseFn::Infix),
            precedence: Precedence::Comparison,
            ..Default::default()
        },
        TokenType::LessEqual => Rule {
            infix: Some(ParseFn::Infix),
            precedence: Precedence::Comparison,
            ..Default::default()
        },
        TokenType::String(_) => Rule {
            prefix: Some(ParseFn::String),
            ..Default::default()
        },
        _ => Rule::default(),
    }
}
