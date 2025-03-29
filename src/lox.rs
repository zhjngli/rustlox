use std::{
    fmt::{Display, Formatter, Result as FmtResult},
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

impl Display for LoxValue {
    fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult {
        match self {
            Self::Bool(b) => write!(f, "{}", b),
            Self::Number(n) => write!(f, "{}", n),
            Self::String(s) => write!(f, "{}", s),
            Self::Null => write!(f, "null"),
        }
    }
}

#[derive(Debug)]
pub enum Exits {
    RuntimeError(Token, String),
    // TODO: should ParseError be here?
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
