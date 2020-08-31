use crate::token::TokenType::Ident;
use std::collections::HashMap;
use std::fmt;

lazy_static! {
    static ref KEYWORDS: HashMap<String, TokenType> = {
        let mut m = HashMap::new();
        m.insert("fn".to_owned(), TokenType::Function);
        m.insert("let".to_owned(), TokenType::Let);
        m.insert("true".to_owned(), TokenType::True);
        m.insert("false".to_owned(), TokenType::False);
        m.insert("if".to_owned(), TokenType::If);
        m.insert("else".to_owned(), TokenType::Else);
        m.insert("return".to_owned(), TokenType::Return);
        m.insert("while".to_owned(), TokenType::While);
        m
    };
}

#[derive(Clone, Debug)]
pub struct Token {
    _type: TokenType,
    literal: String,
}

impl Token {
    pub fn new(_type: TokenType, literal: String) -> Self {
        Token { _type, literal }
    }

    pub fn from_literal(literal: String) -> Self {
        let _type = TokenType::from_literal(&literal);
        Token { _type, literal }
    }

    pub fn token_type(&self) -> &TokenType {
        &self._type
    }

    pub fn literal(&self) -> &String {
        &self.literal
    }
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "[token type={}, literal={}]", self._type, self.literal)
    }
}

pub struct Location {
    line: i32,
    column: i32,
}

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub enum TokenType {
    // Info types
    Illegal,
    EndOfFile,

    // Identifiers and literals
    Ident,
    Integer,
    String,

    // Ops
    Assign,
    Plus,
    Minus,
    Bang,
    Asterisk,
    Slash,

    LT,
    GT,
    EQ,
    NE,

    // Delimiters
    Comma,
    Semicolon,
    Colon,

    // Syntax
    LParen,
    RParen,
    LBrace,
    RBrace,
    LBracket,
    RBracket,

    // Keywords
    Function,
    Let,
    True,
    False,
    If,
    Else,
    Return,
    While,
}

impl TokenType {
    fn from_literal(literal: &str) -> Self {
        let lowered_literal = &literal.to_ascii_lowercase();
        if KEYWORDS.contains_key(lowered_literal) {
            *KEYWORDS.get(lowered_literal).unwrap()
        } else {
            Ident
        }
    }
}

impl fmt::Display for TokenType {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{:?}", self)
    }
}
