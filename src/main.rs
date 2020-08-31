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
use parser::printer::ASTPrinter;
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
                let mut file = Vec::new();
                let printer = ASTPrinter::new(file);
                let res = printer.print(&program);
                let s = String::from_utf8_lossy(res.as_slice());
                println!("{}", s);
                line.pop();
            }
            Err(e) => {
                panic!(e);
            }
        }
    }
}
