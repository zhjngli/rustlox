pub mod errors;
pub mod expr;
pub mod parser;
pub mod scanner;
pub mod token;

use std::{
    env, fs,
    io::{stdin, stdout, Error, ErrorKind, Write},
};

use parser::Parser;
use scanner::Scanner;

#[derive(Debug)]
struct Lox {}

impl Lox {
    pub fn new() -> Self {
        Lox {}
    }

    fn run_file(&self, f: &String) -> Result<(), Error> {
        self.run(fs::read_to_string(f)?)
    }

    fn run_prompt(&mut self) -> Result<(), Error> {
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

        Ok(())
    }
}

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
