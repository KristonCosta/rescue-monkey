use crate::token::{Token, TokenType};
use std::iter::Peekable;
use std::vec::IntoIter;

struct Lexer {
    chars: Peekable<IntoIter<char>>,
    tok: char,
}

impl Lexer {
    fn from_string(input: &str) -> Self {
        let mut chars = input.chars().collect::<Vec<_>>().into_iter().peekable();
        let tok = chars.next().unwrap();
        Lexer { chars, tok }
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

    fn next(&mut self) -> Token {
        self.skip_whitespace();
        let token = match self.tok {
            ';' => Token::new(TokenType::Semicolon, ";".into()),
            ':' => Token::new(TokenType::Colon, ":".into()),
            '(' => Token::new(TokenType::LParen, "(".into()),
            ')' => Token::new(TokenType::RParen, ")".into()),
            ',' => Token::new(TokenType::Comma, ",".into()),
            '+' => Token::new(TokenType::Plus, "+".into()),
            '-' => Token::new(TokenType::Minus, "-".into()),
            '*' => Token::new(TokenType::Asterisk, "*".into()),
            '/' => Token::new(TokenType::Slash, "/".into()),
            '<' => Token::new(TokenType::LT, "<".into()),
            '>' => Token::new(TokenType::GT, ">".into()),
            '{' => Token::new(TokenType::LBrace, "{".into()),
            '}' => Token::new(TokenType::RBrace, "}".into()),
            '[' => Token::new(TokenType::LBracket, "[".into()),
            ']' => Token::new(TokenType::RBracket, "]".into()),
            '\0' => Token::new(TokenType::EndOfFile, "\0".into()),
            '=' => {
                if *self.peek() == '=' {
                    self.read();
                    Token::new(TokenType::EQ, "==".into())
                } else {
                    Token::new(TokenType::Assign, "=".into())
                }
            }
            '!' => {
                if *self.peek() == '=' {
                    self.read();
                    Token::new(TokenType::NE, "!=".into())
                } else {
                    Token::new(TokenType::Bang, "!".into())
                }
            }
            '"' => {
                self.read();
                let literal = self.read_string();
                Token::new(TokenType::String, literal)
            }
            _ => Token::new(TokenType::Illegal, self.tok.to_string()),
        };
        if *token.token_type() == TokenType::Illegal {
            if self.tok.is_ascii_alphabetic() {
                let identifier = self.read_identifier();
                Token::from_literal(identifier)
            } else if self.tok.is_ascii_digit() {
                let number = self.read_number();
                Token::new(TokenType::Integer, number)
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
        self.extract_while(|c: &char| -> bool { *c != '\0' && *c != '"' })
    }

    fn extract_while(&mut self, condition: fn(c: &char) -> bool) -> String {
        let mut chars = vec![];
        loop {
            chars.push(self.read());
            if !condition(&self.tok) {
                break;
            }
        }
        chars.into_iter().collect()
    }

    fn skip_whitespace(&mut self) {
        loop {
            if !self.tok.is_whitespace() {
                break;
            }
            self.read();
            self.skip_whitespace();
        }
    }

    fn skip_line(&mut self) {
        unimplemented!();
    }
}

pub struct TokenStream {
    lexer: Lexer,
    current: Token,
    next: Token,
    is_done: bool,
}

impl TokenStream {
    pub fn from_string(input: &str) -> Self {
        let mut lexer = Lexer::from_string(input);
        let current = lexer.next();
        let next = lexer.next();
        TokenStream {
            lexer,
            current,
            next,
            is_done: false,
        }
    }

    pub fn next(&mut self) -> Option<Token> {
        if self.is_done {
            None
        } else {
            let next = self.lexer.next();
            let old_next = std::mem::replace(&mut self.next, next);
            let ret = std::mem::replace(&mut self.current, old_next);
            if *ret.token_type() == TokenType::EndOfFile {
                self.is_done = true;
            }
            Some(ret)
        }
    }

    pub fn peek(&self) -> Option<&Token> {
        match self.is_done {
            true => None,
            false => Some(&self.next),
        }
    }

    pub fn current(&self) -> Option<&Token> {
        match self.is_done {
            true => None,
            false => Some(&self.current),
        }
    }

    pub fn peek_is(&self, token_type: TokenType) -> bool {
        match self.is_done {
            true => false,
            false => self.next.token_type() == &token_type,
        }
    }

    pub fn current_is(&self, token_type: TokenType) -> bool {
        match self.is_done {
            true => false,
            false => self.current.token_type() == &token_type,
        }
    }
}

impl Iterator for TokenStream {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        if self.is_done {
            None
        } else {
            self.next()
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    struct Pair {
        _type: TokenType,
        literal: String,
    }

    fn run_test(input: &str, expected: Vec<Pair>) {
        let tokens = TokenStream::from_string(input).peekable();
        let mut size = 0;
        for (index, token) in tokens.enumerate() {
            if let Some(pair) = expected.get(index) {
                assert_eq!(pair.literal, *token.literal());
                assert_eq!(pair._type, *token.token_type())
            } else {
                assert!(false, "Found more tokens than expected");
            }
            size += 1;
        }

        assert_eq!(size, expected.len());
    }

    #[test]
    fn test_basic() {
        let input = r#"
            let five = 5;
            let ten = 10;

            let add = fn(x, y) { x + y; };
            let result = add(five, ten);
            let x = "test 123 five!";
            [1, 2];
            {"test":"map"};
            while(true);
        "#;

        let expected = vec![
            Pair {
                _type: TokenType::Let,
                literal: "let".to_string(),
            },
            Pair {
                _type: TokenType::Ident,
                literal: "five".to_string(),
            },
            Pair {
                _type: TokenType::Assign,
                literal: "=".to_string(),
            },
            Pair {
                _type: TokenType::Integer,
                literal: "5".to_string(),
            },
            Pair {
                _type: TokenType::Semicolon,
                literal: ";".to_string(),
            },
            Pair {
                _type: TokenType::Let,
                literal: "let".to_string(),
            },
            Pair {
                _type: TokenType::Ident,
                literal: "ten".to_string(),
            },
            Pair {
                _type: TokenType::Assign,
                literal: "=".to_string(),
            },
            Pair {
                _type: TokenType::Integer,
                literal: "10".to_string(),
            },
            Pair {
                _type: TokenType::Semicolon,
                literal: ";".to_string(),
            },
            Pair {
                _type: TokenType::Let,
                literal: "let".to_string(),
            },
            Pair {
                _type: TokenType::Ident,
                literal: "add".to_string(),
            },
            Pair {
                _type: TokenType::Assign,
                literal: "=".to_string(),
            },
            Pair {
                _type: TokenType::Function,
                literal: "fn".to_string(),
            },
            Pair {
                _type: TokenType::LParen,
                literal: "(".to_string(),
            },
            Pair {
                _type: TokenType::Ident,
                literal: "x".to_string(),
            },
            Pair {
                _type: TokenType::Comma,
                literal: ",".to_string(),
            },
            Pair {
                _type: TokenType::Ident,
                literal: "y".to_string(),
            },
            Pair {
                _type: TokenType::RParen,
                literal: ")".to_string(),
            },
            Pair {
                _type: TokenType::LBrace,
                literal: "{".to_string(),
            },
            Pair {
                _type: TokenType::Ident,
                literal: "x".to_string(),
            },
            Pair {
                _type: TokenType::Plus,
                literal: "+".to_string(),
            },
            Pair {
                _type: TokenType::Ident,
                literal: "y".to_string(),
            },
            Pair {
                _type: TokenType::Semicolon,
                literal: ";".to_string(),
            },
            Pair {
                _type: TokenType::RBrace,
                literal: "}".to_string(),
            },
            Pair {
                _type: TokenType::Semicolon,
                literal: ";".to_string(),
            },
            Pair {
                _type: TokenType::Let,
                literal: "let".to_string(),
            },
            Pair {
                _type: TokenType::Ident,
                literal: "result".to_string(),
            },
            Pair {
                _type: TokenType::Assign,
                literal: "=".to_string(),
            },
            Pair {
                _type: TokenType::Ident,
                literal: "add".to_string(),
            },
            Pair {
                _type: TokenType::LParen,
                literal: "(".to_string(),
            },
            Pair {
                _type: TokenType::Ident,
                literal: "five".to_string(),
            },
            Pair {
                _type: TokenType::Comma,
                literal: ",".to_string(),
            },
            Pair {
                _type: TokenType::Ident,
                literal: "ten".to_string(),
            },
            Pair {
                _type: TokenType::RParen,
                literal: ")".to_string(),
            },
            Pair {
                _type: TokenType::Semicolon,
                literal: ";".to_string(),
            },
            Pair {
                _type: TokenType::Let,
                literal: "let".to_string(),
            },
            Pair {
                _type: TokenType::Ident,
                literal: "x".to_string(),
            },
            Pair {
                _type: TokenType::Assign,
                literal: "=".to_string(),
            },
            Pair {
                _type: TokenType::String,
                literal: "test 123 five!".to_string(),
            },
            Pair {
                _type: TokenType::Semicolon,
                literal: ";".to_string(),
            },
            Pair {
                _type: TokenType::LBracket,
                literal: "[".to_string(),
            },
            Pair {
                _type: TokenType::Integer,
                literal: "1".to_string(),
            },
            Pair {
                _type: TokenType::Comma,
                literal: ",".to_string(),
            },
            Pair {
                _type: TokenType::Integer,
                literal: "2".to_string(),
            },
            Pair {
                _type: TokenType::RBracket,
                literal: "]".to_string(),
            },
            Pair {
                _type: TokenType::Semicolon,
                literal: ";".to_string(),
            },
            Pair {
                _type: TokenType::LBrace,
                literal: "{".to_string(),
            },
            Pair {
                _type: TokenType::String,
                literal: "test".to_string(),
            },
            Pair {
                _type: TokenType::Colon,
                literal: ":".to_string(),
            },
            Pair {
                _type: TokenType::String,
                literal: "map".to_string(),
            },
            Pair {
                _type: TokenType::RBrace,
                literal: "}".to_string(),
            },
            Pair {
                _type: TokenType::Semicolon,
                literal: ";".to_string(),
            },
            Pair {
                _type: TokenType::While,
                literal: "while".to_string(),
            },
            Pair {
                _type: TokenType::LParen,
                literal: "(".to_string(),
            },
            Pair {
                _type: TokenType::True,
                literal: "true".to_string(),
            },
            Pair {
                _type: TokenType::RParen,
                literal: ")".to_string(),
            },
            Pair {
                _type: TokenType::Semicolon,
                literal: ";".to_string(),
            },
            Pair {
                _type: TokenType::EndOfFile,
                literal: '\0'.to_string(),
            },
        ];
        run_test(input, expected);
    }

    #[test]
    fn test_advanced_parsing() {
        let input = r#"
            !-/*5;
            5 < 10 > 5;

            if (5 < 10) {
              return true;
            } else {
              return false;
            }
        "#;
        let expected = vec![
            Pair {
                _type: TokenType::Bang,
                literal: "!".to_string(),
            },
            Pair {
                _type: TokenType::Minus,
                literal: "-".to_string(),
            },
            Pair {
                _type: TokenType::Slash,
                literal: "/".to_string(),
            },
            Pair {
                _type: TokenType::Asterisk,
                literal: "*".to_string(),
            },
            Pair {
                _type: TokenType::Integer,
                literal: "5".to_string(),
            },
            Pair {
                _type: TokenType::Semicolon,
                literal: ";".to_string(),
            },
            Pair {
                _type: TokenType::Integer,
                literal: "5".to_string(),
            },
            Pair {
                _type: TokenType::LT,
                literal: "<".to_string(),
            },
            Pair {
                _type: TokenType::Integer,
                literal: "10".to_string(),
            },
            Pair {
                _type: TokenType::GT,
                literal: ">".to_string(),
            },
            Pair {
                _type: TokenType::Integer,
                literal: "5".to_string(),
            },
            Pair {
                _type: TokenType::Semicolon,
                literal: ";".to_string(),
            },
            Pair {
                _type: TokenType::If,
                literal: "if".to_string(),
            },
            Pair {
                _type: TokenType::LParen,
                literal: "(".to_string(),
            },
            Pair {
                _type: TokenType::Integer,
                literal: "5".to_string(),
            },
            Pair {
                _type: TokenType::LT,
                literal: "<".to_string(),
            },
            Pair {
                _type: TokenType::Integer,
                literal: "10".to_string(),
            },
            Pair {
                _type: TokenType::RParen,
                literal: ")".to_string(),
            },
            Pair {
                _type: TokenType::LBrace,
                literal: "{".to_string(),
            },
            Pair {
                _type: TokenType::Return,
                literal: "return".to_string(),
            },
            Pair {
                _type: TokenType::True,
                literal: "true".to_string(),
            },
            Pair {
                _type: TokenType::Semicolon,
                literal: ";".to_string(),
            },
            Pair {
                _type: TokenType::RBrace,
                literal: "}".to_string(),
            },
            Pair {
                _type: TokenType::Else,
                literal: "else".to_string(),
            },
            Pair {
                _type: TokenType::LBrace,
                literal: "{".to_string(),
            },
            Pair {
                _type: TokenType::Return,
                literal: "return".to_string(),
            },
            Pair {
                _type: TokenType::False,
                literal: "false".to_string(),
            },
            Pair {
                _type: TokenType::Semicolon,
                literal: ";".to_string(),
            },
            Pair {
                _type: TokenType::RBrace,
                literal: "}".to_string(),
            },
            Pair {
                _type: TokenType::EndOfFile,
                literal: '\0'.to_string(),
            },
        ];
        run_test(input, expected);
    }

    #[test]
    fn test_multichar_token_parsing() {
        let input = r#"
            let seven = 7;
            10 == seven;
            10 != 9;
        "#;
        let expected = vec![
            Pair {
                _type: TokenType::Let,
                literal: "let".to_string(),
            },
            Pair {
                _type: TokenType::Ident,
                literal: "seven".to_string(),
            },
            Pair {
                _type: TokenType::Assign,
                literal: "=".to_string(),
            },
            Pair {
                _type: TokenType::Integer,
                literal: "7".to_string(),
            },
            Pair {
                _type: TokenType::Semicolon,
                literal: ";".to_string(),
            },
            Pair {
                _type: TokenType::Integer,
                literal: "10".to_string(),
            },
            Pair {
                _type: TokenType::EQ,
                literal: "==".to_string(),
            },
            Pair {
                _type: TokenType::Ident,
                literal: "seven".to_string(),
            },
            Pair {
                _type: TokenType::Semicolon,
                literal: ";".to_string(),
            },
            Pair {
                _type: TokenType::Integer,
                literal: "10".to_string(),
            },
            Pair {
                _type: TokenType::NE,
                literal: "!=".to_string(),
            },
            Pair {
                _type: TokenType::Integer,
                literal: "9".to_string(),
            },
            Pair {
                _type: TokenType::Semicolon,
                literal: ";".to_string(),
            },
            Pair {
                _type: TokenType::EndOfFile,
                literal: '\0'.to_string(),
            },
        ];
        run_test(input, expected);
    }
}
