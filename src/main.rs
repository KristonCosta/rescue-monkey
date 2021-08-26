mod code;
mod compiler;
mod lexer;
mod object;
mod token;
// mod vm;

use compiler::compiler::Compiler;
use lexer::Scanner;

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
                let tokens = Scanner::from_string(&line);

                line.clear();

                let mut compiler = Compiler::new(tokens);
                let bytecode = compiler.compile();
                println!("{:?}", bytecode);
                println!("{:?}", compiler.errors());
                // let parser = &mut parser::parser::Parser::new(tokens);
                // let program = parser.parse();
                // if !parser.errors().is_empty() {
                //     for error in parser.errors() {
                //         println!("Error: {:?}", error)
                //     }
                //     continue;
                // }
                // // println!("Pretty: {:#?}", program);
                // let mut compiler = Compiler::new();
                // compiler.compile(&program).unwrap();
                // let mut vm = VM::new(compiler.bytecode());
                // let err = vm.run();
                // if let Err(e) = err {
                //     println!("{:?}", e);
                //     continue;
                // }
                // println!("{:?}", vm.last_popped);
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
