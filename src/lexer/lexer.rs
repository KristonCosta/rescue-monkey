use crate::token::{Token, TokenType};
use std::iter::Peekable;
use std::vec::IntoIter;

struct Lexer {
    chars: Peekable<IntoIter<char>>,
    tok: char,
    line: u32,
}

impl Lexer {
    fn from_string(input: &str) -> Self {
        let mut chars = input.chars().collect::<Vec<_>>().into_iter().peekable();
        let tok = chars.next().unwrap();
        let line = 1;
        Lexer { chars, tok, line }
    }

    fn read(&mut self) -> char {
        let tok = if let Some(tok) = self.chars.next() {
            tok
        } else {
            '\0'
        };
        std::mem::replace(&mut self.tok, tok)
    }

    fn peek(&mut self) -> &char {
        if let Some(tok) = self.chars.peek() {
            tok
        } else {
            &'\0'
        }
    }

    fn make_token(&self, token_type: TokenType) -> Token {
        Token::new(token_type, self.line)
    }

    fn match_read(&mut self, character: char) -> bool {
        if *self.peek() == character {
            self.read();
            true
        } else {
            false
        }
    }

    fn next(&mut self) -> Token {
        self.skip_whitespace();
        let token = match self.tok {
            ';' => self.make_token(TokenType::Semicolon),
            '(' => self.make_token(TokenType::LeftParen),
            ')' => self.make_token(TokenType::RightParen),
            '{' => self.make_token(TokenType::LeftBrace),
            '}' => self.make_token(TokenType::RightBrace),
            ',' => self.make_token(TokenType::Comma),
            '.' => self.make_token(TokenType::Dot),
            '-' => self.make_token(TokenType::Minus),
            '+' => self.make_token(TokenType::Plus),
            '*' => self.make_token(TokenType::Star),
            '=' => {
                if self.match_read('=') {
                    self.make_token(TokenType::EqualEqual)
                } else {
                    self.make_token(TokenType::Equal)
                }
            }
            '!' => {
                if self.match_read('=') {
                    self.make_token(TokenType::BangEqual)
                } else {
                    self.make_token(TokenType::Bang)
                }
            }
            '<' => {
                if self.match_read('=') {
                    self.make_token(TokenType::LessEqual)
                } else {
                    self.make_token(TokenType::Less)
                }
            }
            '>' => {
                if self.match_read('=') {
                    self.make_token(TokenType::GreaterEqual)
                } else {
                    self.make_token(TokenType::Greater)
                }
            }
            '/' => {
                if self.match_read('/') {
                    self.skip_line();
                    self.next()
                } else {
                    self.make_token(TokenType::Slash)
                }
            }
            '\0' => self.make_token(TokenType::EndOfFile),
            '"' => {
                self.read();
                let literal = self.read_string();
                self.make_token(TokenType::String(literal))
            }
            _ => self.make_token(TokenType::Illegal),
        };

        if token._type == TokenType::Illegal {
            if self.tok.is_ascii_alphabetic() {
                let identifier = self.read_identifier();
                Token::from_literal(identifier, self.line)
            } else if self.tok.is_ascii_digit() {
                let number = self.read_number();
                self.make_token(TokenType::Number(number))
            } else {
                self.read();
                token
            }
        } else {
            self.read();
            token
        }
    }

    fn read_identifier(&mut self) -> String {
        self.extract_while(|c: &char| -> bool { c.is_ascii_alphabetic() || *c == '_' })
    }

    fn read_number(&mut self) -> String {
        self.extract_while(|c: &char| -> bool { c.is_ascii_digit() })
    }

    fn read_string(&mut self) -> String {
        self.extract_while(|c: &char| -> bool { *c != '"' })
    }

    fn extract_while(&mut self, condition: fn(c: &char) -> bool) -> String {
        let mut chars = vec![];
        loop {
            if !condition(&self.tok) || self.tok == '\0' {
                break;
            }
            chars.push(self.read());
        }
        chars.into_iter().collect()
    }

    fn skip_whitespace(&mut self) {
        loop {
            if !self.tok.is_whitespace() {
                break;
            }
            if self.tok == '\n' {
                self.line += 1;
            }
            self.read();
        }
    }

    fn skip_line(&mut self) {
        loop {
            if self.tok == '\n' || self.tok == '\0' {
                break;
            }
            self.read();
        }
    }
}

pub struct Scanner {
    lexer: Lexer,
    current: Token,
    next: Token,
    is_done: bool,
}

impl Scanner {
    pub fn from_string(input: &str, line: u32) -> Self {
        let mut lexer = Lexer::from_string(input);
        lexer.line = line;
        let current = lexer.next();
        let next = lexer.next();
        Self {
            lexer,
            current,
            next,
            is_done: false,
        }
    }

    pub fn next(&mut self) -> Token {
        if self.is_done {
            self.current.clone()
        } else {
            let next = self.lexer.next();
            let old_next = std::mem::replace(&mut self.next, next);
            let ret = std::mem::replace(&mut self.current, old_next);
            if *ret.token_type() == TokenType::EndOfFile {
                self.is_done = true;
            }
            ret
        }
    }

    pub fn peek(&self) -> &Token {
        match self.is_done {
            true => &self.current,
            false => &self.next,
        }
    }

    pub fn current(&self) -> &Token {
        &self.current
    }

    pub fn peek_is(&self, token_type: &TokenType) -> bool {
        match self.is_done {
            true => false,
            false => self.next.token_type() == token_type,
        }
    }

    pub fn current_is(&self, token_type: &TokenType) -> bool {
        self.current.token_type() == token_type
    }
}

impl Iterator for Scanner {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        if self.is_done {
            None
        } else {
            Some(self.next())
        }
    }
}
