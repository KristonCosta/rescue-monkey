use crate::bag::{Bag, Object};
use crate::code::Instruction;
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

#[derive(Debug)]
pub struct Chunk {
    instructions: Vec<Instruction>,
    constants: Vec<Bag>,
}

#[derive(Debug)]
pub enum CompilerError {
    InvalidOperator(String),
}

#[derive(Debug)]
pub struct Bytecode {
    pub instructions: Vec<Instruction>,
    pub constants: Vec<Bag>,
}

pub struct Compiler {
    tokens: Scanner,
    chunk: Chunk,

    errors: Vec<ParserError>,
}

impl Compiler {
    pub fn new(tokens: Scanner) -> Self {
        Self {
            chunk: Chunk {
                instructions: Vec::new(),
                constants: Vec::new(),
            },

            tokens,
            errors: Vec::new(),
        }
    }

    fn replace(&mut self, pos: usize, instruction: Instruction) -> Result<(), CompilerError> {
        self.chunk.instructions[pos] = instruction;
        Ok(())
    }

    fn emit(&mut self, instruction: Instruction) -> usize {
        self.chunk.instructions.push(instruction);
        self.chunk.instructions.len() - 1
    }

    fn add_constant(&mut self, bag: Bag) -> usize {
        self.chunk.constants.push(bag);
        self.chunk.constants.len() - 1
    }

    fn emit_constant(&mut self, bag: Bag) -> usize {
        let index = self.add_constant(bag);
        self.emit(Instruction::Constant(index))
    }

    fn bytecode(&mut self) -> Bytecode {
        let mut chunk = Chunk {
            instructions: Vec::new(),
            constants: Vec::new(),
        };
        std::mem::swap(&mut self.chunk, &mut chunk);
        let Chunk {
            instructions,
            constants,
        } = chunk;
        Bytecode {
            instructions,
            constants,
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
        self.emit(Instruction::Pop);
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
            TokenType::Plus => self.emit(Instruction::Add),
            TokenType::Minus => self.emit(Instruction::Subtract),
            TokenType::Star => self.emit(Instruction::Multiply),
            TokenType::Slash => self.emit(Instruction::Divide),
            TokenType::BangEqual => {
                self.emit(Instruction::Equal);
                self.emit(Instruction::Not)
            }
            TokenType::EqualEqual => self.emit(Instruction::Equal),
            TokenType::Greater => self.emit(Instruction::Greater),
            TokenType::Less => self.emit(Instruction::Less),
            TokenType::GreaterEqual => {
                self.emit(Instruction::Less);
                self.emit(Instruction::Not)
            }
            TokenType::LessEqual => {
                self.emit(Instruction::Greater);
                self.emit(Instruction::Not)
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
                self.emit(Instruction::Negate);
            }
            TokenType::Bang => {
                self.emit(Instruction::Not);
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
        self.emit(instruction);
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
                        self.emit_constant(Bag::Integer(n));
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
                self.emit_constant(Bag::Obj(Object::String(value)));
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

//     fn parse_statement(&mut self) -> Option<Statement> {
//         match self.tokens.current() {
//             Some(token) => match token.token_type() {
//                 TokenType::Let => self.parse_let(),
//                 TokenType::Return => self.parse_return(),
//                 _ => self.parse_expression_statement(),
//             },
//             None => None,
//         }
//     }

//     fn parse_let(&mut self) -> Option<Statement> {
//         let tok = match self.tokens.current() {
//             Some(tok) => tok.clone(),
//             None => return None,
//         };

//         if !self.expect_peek(TokenType::Ident) {
//             return None;
//         }

//         let name = match self.tokens.current() {
//             None => return None,
//             Some(tok) => tok,
//         };

//         let name = Identifier::new(name.literal().to_string());

//         if !self.expect_peek_and_consume(TokenType::Assign) {
//             return None;
//         }

//         let expression = self.parse_expression(Precedence::Bottom);

//         self.try_eat_semicolon();

//         Some(Statement::new(
//             tok,
//             StatementEnum::Let(Let::new(name, expression)),
//         ))
//     }

//     fn parse_return(&mut self) -> Option<Statement> {
//         let tok = match self.tokens.current() {
//             Some(tok) => tok.clone(),
//             None => return None,
//         };

//         if self.tokens.peek_is(TokenType::RBrace) {
//             return Some(Statement::new(tok, StatementEnum::Return(None)));
//         }
//         // pop the current token
//         self.tokens.next();

//         if self.tokens.current_is(TokenType::Semicolon) {
//             return Some(Statement::new(tok, StatementEnum::Return(None)));
//         }

//         let expression = self.parse_expression(Precedence::Bottom);

//         self.try_eat_semicolon();

//         return Some(Statement::new(tok, StatementEnum::Return(expression)));
//     }

//     fn parse_expression_statement(&mut self) -> Option<Statement> {
//         let tok = match self.tokens.current() {
//             Some(tok) => tok.clone(),
//             None => return None,
//         };
//         let expression = self.parse_expression(Precedence::Bottom);

//         self.try_eat_semicolon();

//         return Some(Statement::new(tok, StatementEnum::Expression(expression)));
//     }

//     fn parse_expression(&mut self, precedence: Precedence) -> Option<Expression> {
//         let tok = match self.tokens.current() {
//             None => return None,
//             Some(tok) => tok.clone(),
//         };
//         let mut prefix_fn = match self.parse_prefix_fn(tok) {
//             None => return None,
//             Some(prefix_fn) => prefix_fn,
//         };

//         loop {
//             let peek_tok = match self.tokens.peek() {
//                 None => return None,
//                 Some(tok) => tok.clone(),
//             };
//             let peek_prec = Precedence::from_token(&peek_tok);
//             if peek_tok.token_type() == &TokenType::Semicolon || precedence >= peek_prec {
//                 break;
//             }
//             if !self.is_infix_fn(&peek_tok) {
//                 return Some(prefix_fn);
//             }
//             self.tokens.next();
//             prefix_fn = match self.parse_infix_fn(peek_tok, prefix_fn) {
//                 None => return None,
//                 Some(infix_fn) => infix_fn,
//             }
//         }
//         Some(prefix_fn)
//     }

//     fn parse_block_statement(&mut self) -> Option<BlockStatement> {
//         let tok = match self.tokens.current() {
//             Some(tok) => tok.clone(),
//             None => return None,
//         };
//         let mut statements = Vec::new();
//         self.tokens.next();
//         loop {
//             if self.tokens.current_is(TokenType::RBrace) {
//                 break;
//             }
//             if self.tokens.current().is_none() {
//                 self.errors.push(ParserError::EoF {
//                     description: "couldn't find matching } for block".to_string(),
//                     token: Some(tok),
//                 });
//                 return None;
//             }
//             statements.push(self.parse_statement());
//             self.tokens.next();
//         }
//         return Some(statements.into_iter().filter_map(|x| x).collect());
//     }

//     fn is_infix_fn(&mut self, tok: &Token) -> bool {
//         match tok.token_type() {
//             TokenType::Plus => true,
//             TokenType::Minus => true,
//             TokenType::Slash => true,
//             TokenType::Asterisk => true,
//             TokenType::EQ => true,
//             TokenType::NE => true,
//             TokenType::LT => true,
//             TokenType::GT => true,
//             TokenType::LParen => true,
//             TokenType::LBracket => true,
//             _ => false,
//         }
//     }

//     fn parse_infix_fn(&mut self, tok: Token, left: Expression) -> Option<Expression> {
//         match tok.token_type() {
//             TokenType::Plus => self.parse_infix_expression(tok, left),
//             TokenType::Minus => self.parse_infix_expression(tok, left),
//             TokenType::Slash => self.parse_infix_expression(tok, left),
//             TokenType::Asterisk => self.parse_infix_expression(tok, left),
//             TokenType::EQ => self.parse_infix_expression(tok, left),
//             TokenType::NE => self.parse_infix_expression(tok, left),
//             TokenType::LT => self.parse_infix_expression(tok, left),
//             TokenType::GT => self.parse_infix_expression(tok, left),
//             TokenType::LParen => self.parse_call_expression(tok, left),
//             TokenType::LBracket => self.parse_index_expression(tok, left),
//             _ => {
//                 self.errors.push(ParserError::InvalidInfixFn {
//                     name: tok.literal().to_string(),
//                     token: Some(tok),
//                 });
//                 None
//             }
//         }
//     }

//     fn parse_parameters(&mut self, end_token: TokenType) -> Option<Vec<Expression>> {
//         self.tokens.next();
//         if self.tokens.current_is(end_token) {
//             return Some(Vec::new());
//         }
//         let mut args = Vec::new();
//         args.push(self.parse_expression(Precedence::Bottom));
//         loop {
//             if !self.tokens.peek_is(TokenType::Comma) {
//                 break;
//             };
//             self.tokens.next();
//             self.tokens.next();
//             args.push(self.parse_expression(Precedence::Bottom));
//         }
//         if !self.expect_peek(end_token) {
//             return None;
//         }
//         return Some(args.into_iter().filter_map(|x| x).collect());
//     }

//     fn parse_call_expression(&mut self, tok: Token, left: Expression) -> Option<Expression> {
//         let params = match self.parse_parameters(TokenType::RParen) {
//             None => return None,
//             Some(params) => params,
//         };

//         Some(Expression::new(
//             tok.clone(),
//             ast::ExpressionEnum::Call(Call::new(
//                 params.into_iter().map(|x| Box::new(x)).collect(),
//                 Box::new(left),
//             )),
//         ))
//     }

//     fn parse_index_expression(&mut self, tok: Token, left: Expression) -> Option<Expression> {
//         self.tokens.next();
//         let index = match self.parse_expression(Precedence::Bottom) {
//             None => return None,
//             Some(index) => index,
//         };

//         if !self.expect_peek(TokenType::RBracket) {
//             return None;
//         }

//         Some(Expression::new(
//             tok.clone(),
//             ast::ExpressionEnum::Index(Index::new(Box::new(left), Box::new(index))),
//         ))
//     }

//     fn parse_infix_expression(&mut self, tok: Token, left: Expression) -> Option<Expression> {
//         let prec = Precedence::from_token(&tok);
//         self.tokens.next();
//         let right = match self.parse_expression(prec) {
//             None => return None,
//             Some(expr) => expr,
//         };
//         Some(Expression::new(
//             tok.clone(),
//             ast::ExpressionEnum::Infix(Infix::new(
//                 Box::new(left),
//                 Operator::parse(tok.literal().to_string()),
//                 Box::new(right),
//             )),
//         ))
//     }

//     fn parse_prefix_fn(&mut self, tok: Token) -> Option<Expression> {
//         match tok.token_type() {
//             TokenType::Ident => self.parse_identifier(tok),
//             TokenType::Integer => self.parse_integer_literal(tok),
//             TokenType::Bang => self.parse_prefix_expression(tok),
//             TokenType::Minus => self.parse_prefix_expression(tok),
//             TokenType::True => self.parse_boolean(tok),
//             TokenType::False => self.parse_boolean(tok),
//             TokenType::LParen => self.parse_grouped_expression(),
//             TokenType::If => self.parse_if_expression(tok),
//             TokenType::While => self.parse_while_expression(tok),
//             TokenType::Function => self.parse_function_literal(tok),
//             TokenType::String => self.parse_string(tok),
//             TokenType::LBrace => self.parse_hash_literal(tok),
//             TokenType::LBracket => self.parse_array_literal(tok),
//             _ => {
//                 self.errors.push(ParserError::InvalidPrefixFn {
//                     name: tok.literal().to_string(),
//                     token: Some(tok),
//                 });
//                 None
//             }
//         }
//     }

//     pub fn parse_identifier(&mut self, tok: Token) -> Option<Expression> {
//         Some(Expression::new(
//             tok.clone(),
//             ast::ExpressionEnum::Identifier(Identifier::new(tok.literal().to_string())),
//         ))
//     }

//     pub fn parse_integer_literal(&mut self, tok: Token) -> Option<Expression> {
//         let int_lit = match tok.literal().parse::<i64>() {
//             Ok(n) => n,
//             Err(_e) => {
//                 self.errors.push(ParserError::TypeParse {
//                     literal: tok.literal().to_string(),
//                     desired_type: "i64".to_string(),
//                     token: Some(tok),
//                 });
//                 return None;
//             }
//         };
//         Some(Expression::new(
//             tok.clone(),
//             ast::ExpressionEnum::IntegerLiteral(ValueExpression::new(int_lit)),
//         ))
//     }

//     pub fn parse_string(&mut self, tok: Token) -> Option<Expression> {
//         Some(Expression::new(
//             tok.clone(),
//             ast::ExpressionEnum::StringLiteral(ValueExpression::new(tok.literal().to_string())),
//         ))
//     }

//     pub fn parse_boolean(&mut self, tok: Token) -> Option<Expression> {
//         let bool_lit = match tok.literal().parse::<bool>() {
//             Ok(n) => n,
//             Err(_e) => {
//                 self.errors.push(ParserError::TypeParse {
//                     literal: tok.literal().to_string(),
//                     desired_type: "bool".to_string(),
//                     token: Some(tok),
//                 });
//                 return None;
//             }
//         };
//         Some(Expression::new(
//             tok.clone(),
//             ast::ExpressionEnum::Boolean(ValueExpression::new(bool_lit)),
//         ))
//     }

//     pub fn parse_prefix_expression(&mut self, tok: Token) -> Option<Expression> {
//         self.tokens.next();
//         let right = match self.parse_expression(Precedence::Prefix) {
//             None => return None,
//             Some(exp) => exp,
//         };
//         Some(Expression::new(
//             tok.clone(),
//             ast::ExpressionEnum::Prefix(Prefix::new(
//                 Operator::parse(tok.literal().to_string()),
//                 Box::new(right),
//             )),
//         ))
//     }

//     pub fn parse_if_expression(&mut self, tok: Token) -> Option<Expression> {
//         if !self.expect_peek_and_consume(TokenType::LParen) {
//             return None;
//         }
//         let condition = match self.parse_expression(Precedence::Bottom) {
//             None => return None,
//             Some(expr) => expr,
//         };

//         if !self.expect_peek(TokenType::RParen) || !self.expect_peek(TokenType::LBrace) {
//             return None;
//         }

//         let true_branch = match self.parse_block_statement() {
//             None => return None,
//             Some(stmt) => stmt,
//         };
//         let false_branch = match self.tokens.peek_is(TokenType::Else) {
//             true => {
//                 self.tokens.next();
//                 if !self.expect_peek(TokenType::LBrace) {
//                     None
//                 } else {
//                     self.parse_block_statement()
//                 }
//             }
//             false => None,
//         };
//         Some(Expression::new(
//             tok.clone(),
//             ast::ExpressionEnum::If(If::new(Box::new(condition), true_branch, false_branch)),
//         ))
//     }
//     pub fn parse_grouped_expression(&mut self) -> Option<Expression> {
//         self.tokens.next();
//         let expr = self.parse_expression(Precedence::Bottom);
//         if !self.expect_peek(TokenType::RParen) {
//             return None;
//         }
//         return expr;
//     }
//     pub fn parse_while_expression(&mut self, tok: Token) -> Option<Expression> {
//         if !self.expect_peek(TokenType::LBrace) {
//             return None;
//         }

//         let body = match self.parse_block_statement() {
//             None => return None,
//             Some(x) => x,
//         };
//         Some(Expression::new(
//             tok.clone(),
//             ast::ExpressionEnum::While(body),
//         ))
//     }
//     pub fn parse_function_literal(&mut self, tok: Token) -> Option<Expression> {
//         if !self.expect_peek(TokenType::LParen) {
//             return None;
//         }

//         let args = match self.parse_parameters(TokenType::RParen) {
//             None => return None,
//             Some(x) => x,
//         };
//         let mut identifiers = Vec::with_capacity(args.len());
//         for arg in args.into_iter() {
//             match arg.inner {
//                 ast::ExpressionEnum::Identifier(ident) => identifiers.push(ident),
//                 _ => return None,
//             }
//         }
//         if !self.expect_peek(TokenType::LBrace) {
//             return None;
//         }

//         let body = match self.parse_block_statement() {
//             None => return None,
//             Some(x) => x,
//         };
//         Some(Expression::new(
//             tok.clone(),
//             ast::ExpressionEnum::FunctionLiteral(FunctionLiteral::new(identifiers, body)),
//         ))
//     }

//     pub fn parse_array_literal(&mut self, tok: Token) -> Option<Expression> {
//         let args = match self.parse_parameters(TokenType::RBracket) {
//             None => return None,
//             Some(x) => x,
//         };
//         Some(Expression::new(
//             tok.clone(),
//             ast::ExpressionEnum::ArrayLiteral(ArrayLiteral::new(
//                 args.into_iter().map(|x| Box::new(x)).collect(),
//             )),
//         ))
//     }

//     pub fn parse_hash_literal(&mut self, tok: Token) -> Option<Expression> {
//         let mut pairs = Vec::new();

//         loop {
//             if self.tokens.peek_is(TokenType::RBrace) {
//                 break;
//             }
//             self.tokens.next();
//             let key = match self.parse_expression(Precedence::Bottom) {
//                 None => return None,
//                 Some(x) => x,
//             };
//             if !self.expect_peek_and_consume(TokenType::Colon) {
//                 return None;
//             }

//             let value = match self.parse_expression(Precedence::Bottom) {
//                 None => return None,
//                 Some(x) => x,
//             };

//             pairs.push((key, value));

//             if !self.tokens.peek_is(TokenType::RBrace) && !self.expect_peek(TokenType::Comma) {
//                 return None;
//             }
//         }
//         if !self.expect_peek(TokenType::RBrace) {
//             return None;
//         }

//         Some(Expression::new(
//             tok.clone(),
//             ast::ExpressionEnum::HashLiteral(HashLiteral::new(pairs)),
//         ))
//     }
// }

// #[derive(Eq, PartialEq)]
// enum Precedence {
//     Bottom,
//     Equals,
//     LessGreater,
//     Sum,
//     Product,
//     Prefix,
//     Call,
//     Index,
// }

// impl Into<usize> for &Precedence {
//     fn into(self) -> usize {
//         match self {
//             Precedence::Bottom => 1,
//             Precedence::Equals => 2,
//             Precedence::LessGreater => 3,
//             Precedence::Sum => 4,
//             Precedence::Product => 5,
//             Precedence::Prefix => 6,
//             Precedence::Call => 7,
//             Precedence::Index => 8,
//         }
//     }
// }

// impl Ord for Precedence {
//     fn cmp(&self, other: &Self) -> std::cmp::Ordering {
//         let x: usize = self.into();
//         x.cmp(&other.into())
//     }
// }

// impl PartialOrd for Precedence {
//     fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
//         Some(self.cmp(other))
//     }
// }

// impl Precedence {
//     fn from_token(token: &Token) -> Precedence {
//         match token.token_type() {
//             TokenType::EQ => Precedence::Equals,
//             TokenType::NE => Precedence::Equals,
//             TokenType::Plus => Precedence::Sum,
//             TokenType::Minus => Precedence::Sum,
//             TokenType::Slash => Precedence::Product,
//             TokenType::Asterisk => Precedence::Product,
//             TokenType::LT => Precedence::LessGreater,
//             TokenType::GT => Precedence::LessGreater,
//             TokenType::LParen => Precedence::Call,
//             TokenType::LBracket => Precedence::Index,
//             _ => Precedence::Bottom,
//         }
//     }
// }
