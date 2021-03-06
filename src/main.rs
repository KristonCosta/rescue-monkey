#[macro_use]
extern crate lazy_static;
#[macro_use]
extern crate derive_new;

mod ast;
mod code;
mod compiler;
mod lexer;
mod object;
mod parser;
mod token;
mod vm;

use crate::compiler::Compiler;
use crate::vm::VM;
use lexer::TokenStream;
use parser::printer::ASTPrinter;
use std::fs;
use std::io;
use std::io::BufRead;

fn main() {
    let stream = io::stdin();
    let mut reader = stream.lock();
    let mut line = String::new();
    let running = true;

    while running {
        match reader.read_line(&mut line) {
            Ok(_) => {
                let tokens = TokenStream::from_string(&line);
                line.clear();
                let parser = &mut parser::parser::Parser::new(tokens);
                let program = parser.parse();
                if !parser.errors().is_empty() {
                    for error in parser.errors() {
                        println!("Error: {:?}", error)
                    }
                    continue;
                }
                // println!("Pretty: {:#?}", program);
                let mut compiler = Compiler::new();
                compiler.compile(&program).unwrap();
                let mut vm = VM::new(compiler.bytecode());
                let err = vm.run();
                if let Err(e) = err {
                    println!("{:?}", e);
                    continue;
                }
                println!("{:?}", vm.last_popped);
                // let file = Vec::new();
                // let printer = ASTPrinter::new(file);
                // let res = printer.print(&program);
                // let s = String::from_utf8_lossy(res.as_slice());
                // println!("{}", s);
            }
            Err(e) => {
                panic!(e);
            }
        }
    }
}
