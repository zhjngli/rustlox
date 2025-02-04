use std::{
    env, fs,
    io::{stdin, stdout, Error, ErrorKind, Write},
};

fn main() -> Result<(), Error> {
    let args: Vec<String> = env::args().collect();
    if args.len() > 2 {
        Err(Error::new(
            ErrorKind::Other,
            "Usage: rlox [script]".to_string(),
        ))
    } else if args.len() == 2 {
        run_file(&args[0])
    } else {
        run_prompt()
    }
}

fn run_file(f: &String) -> Result<(), Error> {
    run(fs::read_to_string(f)?)
}

fn run_prompt() -> Result<(), Error> {
    loop {
        print!("> ");
        stdout().flush().unwrap();
        let mut buffer = String::new();
        stdin().read_line(&mut buffer)?;
        if buffer.trim().is_empty() {
            break;
        } else {
            match run(buffer.clone()) {
                Ok(()) => continue,
                e => return e,
            }
        }
    }
    Ok(())
}

fn run(s: String) -> Result<(), Error> {
    println!("{}", s);
    Ok(())
}
