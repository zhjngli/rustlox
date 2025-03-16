use crate::token::{Token, TokenType};

pub fn lex_error(line: usize, message: String) {
    report_error(line, "".to_string(), message)
}

pub fn report_error(line: usize, location: String, message: String) {
    eprintln!("[line {}] Error{}: {}", line, location, message);
}

pub fn token_error(token: &Token, message: String) {
    if token.token_type == TokenType::Eof {
        report_error(token.line, " at end".to_string(), message);
    } else {
        report_error(token.line, format!(" at '{}'", token.lexeme), message);
    }
}
