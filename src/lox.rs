use std::{
    fs,
    io::{stdin, stdout, Error, ErrorKind, Write},
};

use crate::scanner::Scanner;
use crate::{interpreter::Interpreter, parser::Parser, token::Token};

#[derive(Debug, Clone)]
pub enum LoxValue {
    Bool(bool),
    Number(f64),
    String(String),
    Null,
}

#[derive(Debug)]
pub enum Exits {
    RuntimeError(Token, String),
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
        let expr = parser.parse();
        println!();
        println!("{:?}", expr);

        let mut interpreter = Interpreter::new();
        match interpreter.interpret(&expr) {
            Ok(()) => Ok(()),
            Err(e) => Err(Error::new(ErrorKind::Other, format!("{:?}", e))),
        }
    }
}
