pub mod environment;
pub mod expr;
pub mod interpreter;
pub mod lox;
pub mod parser;
pub mod resolver;
pub mod scanner;
pub mod stmt;
pub mod token;

use std::{
    env, fs,
    io::{stdin, stdout, Error, Write},
    process,
};

use interpreter::Interpreter;
use lox::InterpreterResult as IR;
use parser::{ParseError, Parser};
use resolver::{Resolver, StaticError};
use scanner::Scanner;

fn main() {
    let lox = Lox::new();
    let args: Vec<String> = env::args().collect();
    if args.len() > 2 {
        eprintln!("Usage: rlox [script]");
        process::exit(64); // EX_USAGE
    }
    match if args.len() == 2 {
        lox.run_file(&args[1])
    } else {
        lox.run_repl()
    } {
        Ok(()) => (),
        Err(Exits::Other(e)) => {
            eprintln!("{}", e);
            process::exit(74); // EX_IOERR
        }
        Err(Exits::PE(e)) => {
            eprintln!("{:?}", e);
            process::exit(65); // EX_DATAERR
        }
        Err(Exits::SE(e)) => {
            eprintln!("{:?}", e);
            process::exit(65); // EX_DATAERR
        }
        Err(Exits::IR(IR::RuntimeError(t, m))) => {
            eprint!("Runtime Error: ");
            report_token_error(&t, &m);
            process::exit(70); // EX_SOFTWARE
        }
        Err(Exits::IR(IR::Return(v))) => println!("result: {:?}", v),
        Err(Exits::IR(IR::BreakException)) => {
            panic!("Should always catch break exception while executing a loop statement.")
        }
    }
}

pub fn report_token_error(token: &crate::token::TokenRef, message: &str) {
    if token.token_type == crate::token::TokenType::Eof {
        print_error(token.line, " at end", message);
    } else {
        print_error(token.line, &format!(" at '{}'", token.lexeme), message);
    }
}

pub fn print_error(line: usize, location: &str, message: &str) {
    eprintln!("[line {}] Error{}: {}", line, location, message);
}

#[derive(Debug)]
enum Exits {
    IR(IR),
    PE(ParseError),
    SE(StaticError),
    Other(Error),
}

#[derive(Debug)]
struct Lox {}

impl Lox {
    fn new() -> Self {
        Lox {}
    }

    fn run_file(&self, f: &String) -> Result<(), Exits> {
        match fs::read_to_string(f) {
            Ok(s) => self.run(s),
            Err(e) => Err(Exits::Other(e)),
        }
    }

    fn run_repl(&self) -> Result<(), Exits> {
        let mut interpreter = Interpreter::new(true);
        loop {
            print!("> ");
            stdout().flush().unwrap();

            // read buffer until parseable
            let mut buffer = String::new();
            let stmts;
            loop {
                match stdin().read_line(&mut buffer) {
                    Ok(_) => (),
                    Err(e) => return Err(Exits::Other(e)),
                };
                let mut scanner = Scanner::new(&buffer);
                let tokens = scanner.scan_tokens();
                let mut parser = Parser::new(tokens, true);
                let stmts_result = parser.parse_repl();
                match stmts_result {
                    Ok(r) => {
                        stmts = r;
                        break;
                    }
                    Err(_) => continue,
                }
            }

            // interpret parsed statements
            let mut resolver = Resolver::new(&mut interpreter);
            match resolver.resolve(&stmts) {
                Ok(()) => (),
                Err(e) => {
                    eprintln!("{:?}", e);
                    continue;
                }
            };

            match interpreter.interpret(&stmts) {
                Ok(()) => (),
                Err(IR::RuntimeError(t, m)) => {
                    eprint!("Runtime Error: ");
                    report_token_error(&t, &m);
                }
                Err(IR::Return(v)) => println!("{}", v),
                Err(IR::BreakException) => {
                    panic!("Should always catch break exception while executing a loop statement.")
                }
            }
        }
    }

    fn run(&self, s: String) -> Result<(), Exits> {
        let mut scanner = Scanner::new(&s);
        let tokens = scanner.scan_tokens();

        let mut parser = Parser::new(tokens, false);
        let stmts_result = parser.parse();
        let stmts;
        match stmts_result {
            Ok(r) => stmts = r,
            Err(e) => return Err(Exits::PE(e)),
        }

        let mut interpreter = Interpreter::new(false);
        let mut resolver = Resolver::new(&mut interpreter);
        match resolver.resolve(&stmts) {
            Ok(()) => (),
            Err(e) => return Err(Exits::SE(e)),
        };

        match interpreter.interpret(&stmts) {
            Ok(()) => Ok(()),
            Err(e) => return Err(Exits::IR(e)),
        }
    }
}
