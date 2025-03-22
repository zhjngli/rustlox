pub mod errors;
pub mod expr;
pub mod interpreter;
pub mod lox;
pub mod parser;
pub mod scanner;
pub mod token;

use std::{
    env,
    io::{Error, ErrorKind},
};

use crate::lox::Lox;

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
