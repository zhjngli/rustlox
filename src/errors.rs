use crate::token::{Token, TokenType};

pub fn lex_error(line: usize, message: &str) {
    report_error(line, "", message)
}

pub fn report_error(line: usize, location: &str, message: &str) {
    eprintln!("[line {}] Error{}: {}", line, location, message);
}

pub fn token_error(token: &Token, message: &str) {
    if token.token_type == TokenType::Eof {
        report_error(token.line, " at end", message);
    } else {
        report_error(token.line, &format!(" at '{}'", token.lexeme), message);
    }
}
