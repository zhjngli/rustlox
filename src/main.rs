pub mod environment;
pub mod errors;
pub mod expr;
pub mod interpreter;
pub mod lox;
pub mod parser;
pub mod scanner;
pub mod stmt;
pub mod token;

use std::{
    env, fs,
    io::{stdin, stdout, Error, ErrorKind, Write},
};

use interpreter::Interpreter;
use parser::Parser;
use scanner::Scanner;

fn main() -> Result<(), Error> {
    let mut lox = Lox::new();
    let args: Vec<String> = env::args().collect();
    if args.len() > 2 {
        Err(Error::new(
            ErrorKind::Other,
            "Usage: rlox [script]".to_string(),
        ))
    } else if args.len() == 2 {
        lox.run_file(&args[1])
    } else {
        lox.run_prompt()
    }
}

#[derive(Debug)]
pub struct Lox {}

impl Lox {
    pub fn new() -> Self {
        Lox {}
    }

    pub fn run_file(&self, f: &String) -> Result<(), Error> {
        self.run(fs::read_to_string(f)?)
    }

    pub fn run_prompt(&mut self) -> Result<(), Error> {
        loop {
            print!("> ");
            stdout().flush().unwrap();
            let mut buffer = String::new();
            stdin().read_line(&mut buffer)?;
            if buffer.trim().is_empty() {
                break;
            } else {
                match self.run(buffer.clone()) {
                    Ok(()) => continue,
                    e => return e,
                }
            }
        }
        Ok(())
    }

    fn run(&self, s: String) -> Result<(), Error> {
        println!("string: {:?}\n", s);

        let mut scanner = Scanner::new(&s);
        let tokens = scanner.scan_tokens();
        tokens.iter().for_each(|t| {
            println!("{:?}", t);
        });

        let mut parser = Parser::new(tokens);
        let stmts_result = parser.parse();
        let stmts;
        match stmts_result {
            Ok(r) => stmts = r,
            Err(e) => return Err(Error::new(ErrorKind::Other, format!("{:?}", e))),
        }
        println!();
        stmts.iter().for_each(|s| {
            println!("{:?}", s);
        });

        let mut interpreter = Interpreter::new();
        match interpreter.interpret(&stmts) {
            Ok(()) => Ok(()),
            Err(e) => Err(Error::new(ErrorKind::Other, format!("{:?}", e))),
        }
    }
}
