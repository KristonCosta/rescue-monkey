use std::fmt;

#[derive(Debug)]
pub enum TokenError {
    InvalidTokenTypeForValue(TokenType),
}

#[readonly::make]
#[derive(Clone, Debug)]
pub struct Token {
    pub _type: TokenType,
    pub line: u32,
}

impl Token {
    pub fn new(_type: TokenType, line: u32) -> Self {
        Token { _type, line }
    }

    pub fn from_literal(literal: String, line: u32) -> Self {
        let _type = TokenType::from_literal(literal);
        Token { _type, line }
    }

    pub fn token_type(&self) -> &TokenType {
        &self._type
    }

    pub fn is_value_type(&self) -> bool {
        match self._type {
            TokenType::Identifier(_) => true,
            TokenType::String(_) => true,
            TokenType::Number(_) => true,
            _ => false,
        }
    }

    pub fn value(&self) -> Result<&String, TokenError> {
        match &self._type {
            TokenType::Identifier(value) => Ok(value),
            TokenType::String(value) => Ok(value),
            TokenType::Number(value) => Ok(value),
            _ => Err(TokenError::InvalidTokenTypeForValue(self._type.clone())),
        }
    }
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "[token type={}]", self._type)
    }
}

pub struct Location {
    line: i32,
    column: i32,
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum TokenType {
    LeftParen,
    RightParen,
    LeftBrace,
    RightBrace,
    Comma,
    Dot,
    Minus,
    Plus,
    Semicolon,
    Slash,
    Star,
    Bang,
    BangEqual,
    Equal,
    EqualEqual,
    Greater,
    GreaterEqual,
    Less,
    LessEqual,

    Identifier(String),
    String(String),
    Number(String),

    And,
    Class,
    If,
    Else,
    Function,
    For,
    Nil,
    Or,
    Print,
    Return,
    Super,
    This,
    True,
    False,
    Var,
    While,

    EndOfFile,
    Illegal,
    Bottom,
}

impl TokenType {
    fn from_literal(literal: String) -> Self {
        match literal.as_str() {
            "and" => TokenType::And,
            "class" => TokenType::Class,
            "if" => TokenType::If,
            "else" => TokenType::Else,
            "fn" => TokenType::Function,
            "for" => TokenType::For,
            "nil" => TokenType::Nil,
            "or" => TokenType::Or,
            "print" => TokenType::Print,
            "return" => TokenType::Return,
            "super" => TokenType::Super,
            "true" => TokenType::True,
            "false" => TokenType::False,
            "var" => TokenType::Var,
            "this" => TokenType::This,
            "while" => TokenType::While,
            _ => TokenType::Identifier(literal),
        }
    }
}

impl fmt::Display for TokenType {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{:?}", self)
    }
}
