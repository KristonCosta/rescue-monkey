use std::{
    ops::{Add, Div, Mul, Neg, Sub},
    process::Output,
};

type Integer = i64;

pub enum BagError {
    ConversionFailure(String, Bag),
    InvalidInfixForArguments(String, Bag, Bag),
    InvalidPrefixForArgument(String, Bag),
}
#[derive(Debug, Eq, PartialEq, Clone)]
pub enum Bag {
    Obj(Object),
    Integer(i64),
    Nil,
    False,
    True,
}

impl PartialOrd for Bag {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        match (self, other) {
            (Bag::Obj(me), Bag::Obj(them)) => me.partial_cmp(them),
            (Bag::Integer(me), Bag::Integer(them)) => me.partial_cmp(them),
            _ => None,
        }
    }
}

impl Bag {
    pub fn get_truthy(&self) -> Result<bool, BagError> {
        match self {
            Bag::Integer(val) => Ok(*val != 0),
            Bag::Nil => Ok(false),
            Bag::True => Ok(true),
            _ => Ok(false),
        }
    }
}

impl Add for Bag {
    type Output = Result<Bag, BagError>;

    fn add(self, other: Self) -> Self::Output {
        match (self, other) {
            (Bag::Obj(me), Bag::Obj(them)) => me + them,
            (Bag::Integer(me), Bag::Integer(them)) => Ok(Bag::Integer(me + them)),
            (me, them) => Err(BagError::InvalidInfixForArguments(
                "addition".to_string(),
                me,
                them,
            )),
        }
    }
}

impl Sub for Bag {
    type Output = Result<Bag, BagError>;

    fn sub(self, other: Self) -> Self::Output {
        match (self, other) {
            (Bag::Integer(me), Bag::Integer(them)) => Ok(Bag::Integer(me - them)),
            (me, them) => Err(BagError::InvalidInfixForArguments(
                "subtraction".to_string(),
                me,
                them,
            )),
        }
    }
}

impl Div for Bag {
    type Output = Result<Bag, BagError>;

    fn div(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Bag::Integer(me), Bag::Integer(them)) => Ok(Bag::Integer(me / them)),
            (me, them) => Err(BagError::InvalidInfixForArguments(
                "division".to_string(),
                me,
                them,
            )),
        }
    }
}

impl Mul for Bag {
    type Output = Result<Bag, BagError>;

    fn mul(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Bag::Integer(me), Bag::Integer(them)) => Ok(Bag::Integer(me * them)),
            (me, them) => Err(BagError::InvalidInfixForArguments(
                "multiplication".to_string(),
                me,
                them,
            )),
        }
    }
}

impl Neg for Bag {
    type Output = Result<Bag, BagError>;

    fn neg(self) -> Self::Output {
        match self {
            Bag::Integer(me) => Ok(Bag::Integer(-me)),
            me => Err(BagError::InvalidPrefixForArgument(
                "negation".to_string(),
                me,
            )),
        }
    }
}

#[derive(Debug, Eq, PartialEq, Clone, Ord, PartialOrd)]
pub enum Object {
    String(String),
}

impl Add for Object {
    type Output = Result<Bag, BagError>;

    fn add(self, other: Self) -> Self::Output {
        match (self, other) {
            (Object::String(me), Object::String(them)) => Ok(Bag::Obj(Object::String(me + &them))),
        }
    }
}
