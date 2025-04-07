use crate::token::{Token, TokenType};

pub fn report_lex_error(line: usize, message: &str) {
    print_error(line, "", message)
}

pub fn report_token_error(token: &Token, message: &str) {
    if token.token_type == TokenType::Eof {
        print_error(token.line, " at end", message);
    } else {
        print_error(token.line, &format!(" at '{}'", token.lexeme), message);
    }
}

fn print_error(line: usize, location: &str, message: &str) {
    eprintln!("[line {}] Error{}: {}", line, location, message);
}
