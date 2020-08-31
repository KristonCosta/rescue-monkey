#[macro_use]
extern crate lazy_static;
#[macro_use]
extern crate derive_new;
#[macro_use]
extern crate readonly;

mod ast;
mod lexer;
mod parser;
mod token;

use lexer::TokenStream;
use std::io;
use std::io::BufRead;

fn main() {
    let stream = io::stdin();
    let mut reader = stream.lock();
    let mut line = String::new();
    let mut running = true;

    while running {
        match reader.read_line(&mut line) {
            Ok(_) => {
                let tokens = TokenStream::from_string(&line);
                let parser = &mut parser::parser::Parser::new(tokens);
                let program = parser.parse();

                for statement in &program.statements {
                    println!("{:#?}", statement);
                }
                for error in parser.errors() {
                    println!("ERROR: {:#?}", error);
                }
                line.pop();
            }
            Err(e) => {
                panic!(e);
            }
        }
    }
}
