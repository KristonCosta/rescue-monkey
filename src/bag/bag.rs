type Integer = i64;

pub enum BagError {
    ConversionFailure(String, Bag),
}
#[derive(Debug, Eq, PartialEq, Clone, Ord, PartialOrd)]
pub enum Bag {
    Obj(Object),
    Integer(i64),
    Nil,
    False,
    True,
}

impl Bag {
    pub fn get_int(&self) -> Result<Integer, BagError> {
        match &self {
            Bag::Integer(i) => Ok(*i),
            _ => Err(BagError::ConversionFailure(
                "integer".to_string(),
                self.clone(),
            )),
        }
    }
    pub fn get_truthy(&self) -> Result<bool, BagError> {
        match self {
            Bag::Integer(val) => Ok(*val != 0),
            Bag::Nil => Ok(false),
            Bag::True => Ok(true),
            _ => Ok(false),
        }
    }
}

#[derive(Debug, Eq, PartialEq, Clone, Ord, PartialOrd)]
pub enum Object {
    String(String),
}
