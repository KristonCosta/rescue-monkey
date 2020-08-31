use crate::token::Token;
use std::collections::HashMap;
use std::ops::{Deref, DerefMut};

#[derive(Debug, PartialEq)]
pub enum Operator {
    Op(String),
}

impl Operator {
    pub fn parse(literal: String) -> Self {
        Operator::Op(literal)
    }
}
#[readonly::make]
#[derive(Debug)]
pub struct Program {
    pub statements: Vec<Statement>,
}

impl Program {
    pub fn new() -> Self {
        Program {
            statements: Vec::new(),
        }
    }

    pub fn add_statement(&mut self, statement: Statement) {
        self.statements.push(statement);
    }

    pub fn get_statements(&self) -> &Vec<Statement> {
        &self.statements
    }
}

#[derive(Debug)]
enum NodeEnum {
    Expression(Expression),
    Statement(Statement),
    Program(Program),
}
#[derive(Debug)]
pub struct Expression {
    pub token: Token,
    pub inner: ExpressionEnum,
}

impl Expression {
    pub fn new(token: Token, expression: ExpressionEnum) -> Self {
        Self {
            token,
            inner: expression,
        }
    }
}

impl Deref for Expression {
    type Target = ExpressionEnum;

    fn deref(&self) -> &Self::Target {
        &self.inner
    }
}

impl DerefMut for Expression {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.inner
    }
}

pub type BlockStatement = Vec<Statement>;
pub type Identifier = ValueExpression<String>;
pub type ExpressionArg = Box<Expression>;
pub type Args = Vec<Box<Expression>>;
#[readonly::make]
#[derive(Debug)]
pub struct ValueExpression<T> {
    pub value: T,
}

impl<T> ValueExpression<T> {
    pub fn new(value: T) -> Self {
        ValueExpression { value }
    }
    pub fn value(&self) -> &T {
        &self.value
    }
}

#[derive(Debug)]
pub enum ExpressionEnum {
    Identifier(Identifier),
    IntegerLiteral(ValueExpression<i64>),
    StringLiteral(ValueExpression<String>),
    ArrayLiteral(ArrayLiteral),
    Boolean(ValueExpression<bool>),
    HashLiteral(HashLiteral),
    Prefix(Prefix),
    Infix(Infix),
    Index(Index),
    If(If),
    While(BlockStatement),
    FunctionLiteral(FunctionLiteral),
    Call(Call),
}

impl Expression {
    pub fn get_identifier(&self) -> Option<&Identifier> {
        match &self.inner {
            ExpressionEnum::Identifier(l) => Some(l),
            _ => None,
        }
    }
    pub fn get_integer(&self) -> Option<&ValueExpression<i64>> {
        match &self.inner {
            ExpressionEnum::IntegerLiteral(l) => Some(l),
            _ => None,
        }
    }
    pub fn get_string(&self) -> Option<&ValueExpression<String>> {
        match &self.inner {
            ExpressionEnum::StringLiteral(l) => Some(l),
            _ => None,
        }
    }
    pub fn get_bool(&self) -> Option<&ValueExpression<bool>> {
        match &self.inner {
            ExpressionEnum::Boolean(l) => Some(l),
            _ => None,
        }
    }
    pub fn get_prefix(&self) -> Option<&Prefix> {
        match &self.inner {
            ExpressionEnum::Prefix(l) => Some(l),
            _ => None,
        }
    }
    pub fn get_infix(&self) -> Option<&Infix> {
        match &self.inner {
            ExpressionEnum::Infix(l) => Some(l),
            _ => None,
        }
    }
    pub fn get_index(&self) -> Option<&Index> {
        match &self.inner {
            ExpressionEnum::Index(l) => Some(l),
            _ => None,
        }
    }
    pub fn get_if(&self) -> Option<&If> {
        match &self.inner {
            ExpressionEnum::If(l) => Some(l),
            _ => None,
        }
    }
    pub fn get_while(&self) -> Option<&Vec<Statement>> {
        match &self.inner {
            ExpressionEnum::While(l) => Some(l),
            _ => None,
        }
    }
    pub fn get_function_literal(&self) -> Option<&FunctionLiteral> {
        match &self.inner {
            ExpressionEnum::FunctionLiteral(l) => Some(l),
            _ => None,
        }
    }
    pub fn get_call(&self) -> Option<&Call> {
        match &self.inner {
            ExpressionEnum::Call(l) => Some(l),
            _ => None,
        }
    }
}

#[readonly::make]
#[derive(Debug, new)]
pub struct HashLiteral {
    pub pairs: Vec<(Expression, Expression)>,
}

#[readonly::make]
#[derive(Debug, new)]
pub struct ArrayLiteral {
    pub elements: Vec<Box<Expression>>,
}

#[readonly::make]
#[derive(Debug, new)]
pub struct Prefix {
    pub operator: Operator,
    pub left: Box<Expression>,
}
#[readonly::make]
#[derive(Debug, new)]
pub struct Index {
    pub indexable: Box<Expression>,
    pub index: Box<Expression>,
}
#[readonly::make]
#[derive(Debug, new)]
pub struct Call {
    pub args: Vec<Box<Expression>>,
    pub callable: Box<Expression>,
}
#[readonly::make]
#[derive(Debug, new)]
pub struct FunctionLiteral {
    pub identifiers: Vec<Identifier>,
    pub block: BlockStatement,
}
#[readonly::make]
#[derive(Debug, new)]
pub struct If {
    pub condition: Box<Expression>,
    pub true_statement: BlockStatement,
    pub false_statement: Option<BlockStatement>,
}
#[readonly::make]
#[derive(Debug, new)]
pub struct Infix {
    pub left: Box<Expression>,
    pub op: Operator,
    pub right: Box<Expression>,
}

#[derive(Debug, new)]
pub struct Statement {
    pub token: Token,
    pub inner: StatementEnum,
}

impl Deref for Statement {
    type Target = StatementEnum;

    fn deref(&self) -> &Self::Target {
        &self.inner
    }
}

impl DerefMut for Statement {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.inner
    }
}

impl Statement {
    pub fn get_let(&self) -> Option<&Let> {
        match &self.inner {
            StatementEnum::Let(l) => Some(l),
            _ => None,
        }
    }

    pub fn get_expr(&self) -> Option<&Expression> {
        match &self.inner {
            StatementEnum::Expression(expr) => match expr {
                None => None,
                Some(x) => Some(x),
            },
            _ => None,
        }
    }

    pub fn get_return(&self) -> Option<&Expression> {
        match &self.inner {
            StatementEnum::Return(expr) => match expr {
                None => None,
                Some(x) => Some(x),
            },
            _ => None,
        }
    }

    pub fn get_block(&self) -> Option<&BlockStatement> {
        match &self.inner {
            StatementEnum::Block(expr) => match expr {
                None => None,
                Some(x) => Some(x),
            },
            _ => None,
        }
    }
}

#[derive(Debug)]
pub enum StatementEnum {
    Return(Option<Expression>),
    Expression(Option<Expression>),
    Let(Let),
    Block(Option<BlockStatement>),
}

#[readonly::make]
#[derive(Debug, new)]
pub struct Let {
    pub identifier: Identifier,
    pub expr: Option<Expression>,
}
