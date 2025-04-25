use once_cell::sync::Lazy;
use std::{collections::HashMap, iter::Peekable, rc::Rc, str::Chars};

use crate::token::{
    Token,
    TokenLiteral::{Null, NumberLit, StringLit},
    TokenRef,
    TokenType::{
        self, And, Bang, BangEqual, Break, Class, Comma, Dot, Else, Eof, Equal, EqualEqual, False,
        For, Fun, Greater, GreaterEqual, Identifier, If, LeftBrace, LeftParen, Less, LessEqual,
        Minus, Nil, Number, Or, Plus, Print, Return, RightBrace, RightParen, Semicolon, Slash,
        Star, String as TString, Super, This, True, Var, While,
    },
};

static KEYWORDS: Lazy<HashMap<String, TokenType>> = Lazy::new(|| {
    let mut m = HashMap::new();
    m.insert("and".to_string(), And);
    m.insert("break".to_string(), Break);
    m.insert("class".to_string(), Class);
    m.insert("else".to_string(), Else);
    m.insert("false".to_string(), False);
    m.insert("for".to_string(), For);
    m.insert("fun".to_string(), Fun);
    m.insert("if".to_string(), If);
    m.insert("nil".to_string(), Nil);
    m.insert("or".to_string(), Or);
    m.insert("print".to_string(), Print);
    m.insert("return".to_string(), Return);
    m.insert("super".to_string(), Super);
    m.insert("this".to_string(), This);
    m.insert("true".to_string(), True);
    m.insert("var".to_string(), Var);
    m.insert("while".to_string(), While);
    m
});

fn report_lex_error(line: usize, message: &str) {
    crate::print_error(line, "", message)
}

// TODO: scanner error if it doesn't finish parsing a block comment?
#[derive(Debug)]
pub struct Scanner<'a> {
    source: Peekable<Chars<'a>>,
    tokens: Vec<TokenRef>,
    line: usize,
    block_comment: usize,
}

impl<'a> Scanner<'a> {
    pub fn new(s: &'a String) -> Self {
        Scanner {
            source: s.chars().peekable(),
            tokens: Vec::new(),
            line: 1,
            block_comment: 0,
        }
    }

    pub fn scan_tokens(&mut self) -> &Vec<TokenRef> {
        while let Some(_) = self.source.peek() {
            match self.scan_token() {
                Some(t) => {
                    if self.block_comment == 0 {
                        self.tokens.push(Rc::new(t))
                    }
                }
                None => (),
            }
        }

        self.tokens
            .push(Rc::new(Token::new(Eof, "".to_string(), Null, self.line)));
        &self.tokens
    }

    fn scan_token(&mut self) -> Option<Token> {
        let opt_c = self.source.next();
        match opt_c {
            Some(c) => self.token_from_char(c),
            None => panic!("peek and next didn't match"),
        }
    }

    fn token_from_char(&mut self, c: char) -> Option<Token> {
        if self.block_comment == 0 {
            // only look for tokens when we're not in a block comment
            match c {
                '(' => Some(Token::new(LeftParen, c.to_string(), Null, self.line)),
                ')' => Some(Token::new(RightParen, c.to_string(), Null, self.line)),
                '{' => Some(Token::new(LeftBrace, c.to_string(), Null, self.line)),
                '}' => Some(Token::new(RightBrace, c.to_string(), Null, self.line)),
                ',' => Some(Token::new(Comma, c.to_string(), Null, self.line)),
                '.' => Some(Token::new(Dot, c.to_string(), Null, self.line)),
                '-' => Some(Token::new(Minus, c.to_string(), Null, self.line)),
                '+' => Some(Token::new(Plus, c.to_string(), Null, self.line)),
                ';' => Some(Token::new(Semicolon, c.to_string(), Null, self.line)),
                '*' => Some(Token::new(Star, c.to_string(), Null, self.line)),
                '!' => {
                    if self.match_next('=') {
                        Some(Token::new(BangEqual, "!=".to_owned(), Null, self.line))
                    } else {
                        Some(Token::new(Bang, '!'.to_string(), Null, self.line))
                    }
                }
                '=' => {
                    if self.match_next('=') {
                        Some(Token::new(EqualEqual, "==".to_owned(), Null, self.line))
                    } else {
                        Some(Token::new(Equal, '='.to_string(), Null, self.line))
                    }
                }
                '<' => {
                    if self.match_next('=') {
                        Some(Token::new(LessEqual, "<=".to_owned(), Null, self.line))
                    } else {
                        Some(Token::new(Less, '<'.to_string(), Null, self.line))
                    }
                }
                '>' => {
                    if self.match_next('=') {
                        Some(Token::new(GreaterEqual, ">=".to_owned(), Null, self.line))
                    } else {
                        Some(Token::new(Greater, '>'.to_string(), Null, self.line))
                    }
                }
                '/' => {
                    if self.match_next('/') {
                        // comment goes until the end of the line
                        while self.source.next_if(|&c| c != '\n').is_some() {}
                        None
                    } else if self.match_next('*') {
                        self.block_comment += 1;
                        None
                    } else {
                        Some(Token::new(Slash, c.to_string(), Null, self.line))
                    }
                }
                ' ' | '\r' | '\t' => None,
                '\n' => {
                    self.line += 1;
                    None
                }
                '"' => Some(self.string()),
                '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' => Some(self.number(c)),
                _ => {
                    if Scanner::is_alpha(c) {
                        Some(self.identifier(c))
                    } else {
                        report_lex_error(self.line, &format!("Unexpected character: {}", c));
                        None
                    }
                }
            }
        } else {
            // when we're in a block comment, only care about deeper nesting, or exiting the comment
            match c {
                '*' => {
                    if self.match_next('/') {
                        self.block_comment -= 1;
                    }
                }
                '/' => {
                    if self.match_next('*') {
                        self.block_comment += 1;
                    }
                }
                _ => (),
            }
            None
        }
    }

    fn match_next(&mut self, next: char) -> bool {
        match self.source.next_if(|&c| c == next) {
            Some(c) => c == next,
            None => false,
        }
    }

    fn string(&mut self) -> Token {
        let mut s: Vec<char> = Vec::new();
        while let Some(c) = self.source.next_if(|&c| c != '"') {
            if c == '\n' {
                self.line += 1;
            }
            s.push(c);
        }
        if self.source.peek() == None {
            // at end of source
            report_lex_error(self.line, "Unterminated string")
        }
        // the closing '"' char
        self.source.next();

        let lex: String = s.iter().collect();
        Token::new(TString, lex.clone(), StringLit(lex), self.line)
    }

    fn number(&mut self, init: char) -> Token {
        let mut n: Vec<char> = vec![init];
        while let Some(c) = self.source.next_if(|&c| c.is_ascii_digit()) {
            n.push(c);
        }
        // look for a decimal portion
        if self.source.peek() == Some(&'.') {
            // using a cloned peekable to peek twice
            let mut peek_next_source = self.source.clone();
            peek_next_source.next();
            match peek_next_source.peek() {
                Some(c) => {
                    if c.is_ascii_digit() {
                        // the char after the '.' is a digit, so consume the '.'
                        self.source.next();
                        n.push('.');
                        // consume the decimal portion of the number
                        while let Some(c) = self.source.next_if(|&c| c.is_ascii_digit()) {
                            n.push(c);
                        }
                    }
                }
                None => (),
            }
        }

        let s: String = n.iter().collect();
        let result = s.parse::<f64>();
        match result {
            Ok(num) => Token::new(Number, n.iter().collect(), NumberLit(num), self.line),
            Err(e) => panic!("Couldn't parse number from {}: {}", s, e),
        }
    }

    fn is_alpha(c: char) -> bool {
        c.is_ascii_alphabetic() || c == '_'
    }

    fn identifier(&mut self, init: char) -> Token {
        let mut s: Vec<char> = vec![init];
        while let Some(c) = self
            .source
            .next_if(|&c| Scanner::is_alpha(c) || c.is_ascii_digit())
        {
            s.push(c);
        }

        let id = s.iter().collect();
        let token_type = match KEYWORDS.get(&id) {
            Some(tt) => tt.clone(),
            None => Identifier,
        };
        Token::new(token_type, id, Null, self.line)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_identifiers() {
        let source = String::from("foo123 _bar baz_");
        let mut scanner = Scanner::new(&source);
        let tokens = scanner.scan_tokens();

        assert_eq!(tokens[0].token_type, Identifier);
        assert_eq!(tokens[0].lexeme, "foo123");
        assert_eq!(tokens[1].token_type, Identifier);
        assert_eq!(tokens[1].lexeme, "_bar");
        assert_eq!(tokens[2].token_type, Identifier);
        assert_eq!(tokens[2].lexeme, "baz_");
    }

    #[test]
    fn test_keywords() {
        let source = String::from("if while class true false nil");
        let mut scanner = Scanner::new(&source);
        let tokens = scanner.scan_tokens();

        assert_eq!(tokens[0].token_type, If);
        assert_eq!(tokens[1].token_type, While);
        assert_eq!(tokens[2].token_type, Class);
        assert_eq!(tokens[3].token_type, True);
        assert_eq!(tokens[4].token_type, False);
        assert_eq!(tokens[5].token_type, Nil);
    }

    #[test]
    fn test_multiline_strings() {
        let source = String::from("\"hello\nworld\"");
        let mut scanner = Scanner::new(&source);
        let tokens = scanner.scan_tokens();

        assert_eq!(tokens[0].token_type, TString);
        match &tokens[0].literal {
            StringLit(s) => assert_eq!(s, "hello\nworld"),
            _ => panic!("Expected string literal"),
        }
        assert_eq!(tokens[0].line, 2);
        assert_eq!(tokens[1].token_type, Eof);
        assert_eq!(tokens.len(), 2);
    }

    #[test]
    fn test_inlinecomment() {
        let source = String::from("var x = 42; // this is a comment var x = 3;\nvar x = 4;");
        let mut scanner = Scanner::new(&source);
        let tokens = scanner.scan_tokens();

        assert_eq!(tokens[0].token_type, Var);
        assert_eq!(tokens[1].token_type, Identifier);
        assert_eq!(tokens[2].token_type, Equal);
        assert_eq!(tokens[3].token_type, Number);
        match tokens[3].literal {
            NumberLit(n) => assert_eq!(n, 42.0),
            _ => panic!("Expected number literal"),
        }
        assert_eq!(tokens[4].token_type, Semicolon);
        assert_eq!(tokens[5].token_type, Var);
        assert_eq!(tokens[6].token_type, Identifier);
        assert_eq!(tokens[7].token_type, Equal);
        assert_eq!(tokens[8].token_type, Number);
        match tokens[8].literal {
            NumberLit(n) => assert_eq!(n, 4.0),
            _ => panic!("Expected number literal"),
        }
        assert_eq!(tokens[9].token_type, Semicolon);
        assert_eq!(tokens[10].token_type, Eof);
        assert_eq!(tokens.len(), 11);
    }

    #[test]
    fn test_nested_block_comments_with_code() {
        let source = String::from("/* outer ;;12!4 blah aw-1-1**{)/* \" */ comment */ 42");
        let mut scanner = Scanner::new(&source);
        let tokens = scanner.scan_tokens();

        assert_eq!(tokens[0].token_type, Number);
        match tokens[0].literal {
            NumberLit(n) => assert_eq!(n, 42.0),
            _ => panic!("Expected number literal"),
        }
    }

    #[test]
    fn test_complex_expression() {
        let source = String::from("var x = (123 + 456) * (789 / 10);");
        let mut scanner = Scanner::new(&source);
        let tokens = scanner.scan_tokens();

        assert_eq!(tokens[0].token_type, Var);
        assert_eq!(tokens[1].token_type, Identifier);
        assert_eq!(tokens[2].token_type, Equal);
        assert_eq!(tokens[3].token_type, LeftParen);
        assert_eq!(tokens[4].token_type, Number);
        assert_eq!(tokens[5].token_type, Plus);
        assert_eq!(tokens[6].token_type, Number);
        assert_eq!(tokens[7].token_type, RightParen);
        assert_eq!(tokens[8].token_type, Star);
        assert_eq!(tokens[9].token_type, LeftParen);
        assert_eq!(tokens[10].token_type, Number);
        assert_eq!(tokens[11].token_type, Slash);
        assert_eq!(tokens[12].token_type, Number);
        assert_eq!(tokens[13].token_type, RightParen);
        assert_eq!(tokens[14].token_type, Semicolon);
    }

    #[test]
    fn test_number_literals() {
        let source = String::from(".123 456. 789.0123");
        let mut scanner = Scanner::new(&source);
        let tokens = scanner.scan_tokens();

        assert_eq!(tokens[0].token_type, Dot);
        assert_eq!(tokens[1].token_type, Number);
        match tokens[1].literal {
            NumberLit(n) => assert_eq!(n, 123.0),
            _ => panic!("Expected number literal"),
        }

        assert_eq!(tokens[2].token_type, Number);
        match tokens[2].literal {
            NumberLit(n) => assert_eq!(n, 456.0),
            _ => panic!("Expected number literal"),
        }
        assert_eq!(tokens[3].token_type, Dot);

        assert_eq!(tokens[4].token_type, Number);
        match tokens[4].literal {
            NumberLit(n) => assert_eq!(n, 789.0123),
            _ => panic!("Expected number literal"),
        }
        assert_eq!(tokens[5].token_type, Eof);
        assert_eq!(tokens.len(), 6);
    }

    #[test]
    fn test_unterminated_string() {
        let source = String::from("\"hello world");
        let mut scanner = Scanner::new(&source);
        let tokens = scanner.scan_tokens();

        assert_eq!(tokens[0].token_type, TString);
        assert_eq!(tokens[1].token_type, Eof);
        assert_eq!(tokens.len(), 2);
    }

    #[test]
    fn test_unexpected_character() {
        let source = String::from("var x = 42; @");
        let mut scanner = Scanner::new(&source);
        let tokens = scanner.scan_tokens();

        assert_eq!(tokens[0].token_type, Var);
        assert_eq!(tokens[1].token_type, Identifier);
        assert_eq!(tokens[2].token_type, Equal);
        assert_eq!(tokens[3].token_type, Number);
        assert_eq!(tokens[4].token_type, Semicolon);
        assert_eq!(tokens[5].token_type, Eof);
        assert_eq!(tokens.len(), 6);
    }

    #[test]
    fn test_unterminated_block_comment() {
        let source = String::from("/* This is an unterminated block comment");
        let mut scanner = Scanner::new(&source);
        let tokens = scanner.scan_tokens();

        assert_eq!(tokens.len(), 1);
        assert_eq!(tokens[0].token_type, Eof);
    }

    #[test]
    fn test_nested_block_comments() {
        let source = String::from("/* outer /* inner */ still outer */");
        let mut scanner = Scanner::new(&source);
        let tokens = scanner.scan_tokens();

        assert_eq!(tokens.len(), 1);
        assert_eq!(tokens[0].token_type, Eof);
    }

    #[test]
    fn test_mixed_comments_and_code() {
        let source = String::from("var x = 42; /* comment */ var y = 43; // inline comment");
        let mut scanner = Scanner::new(&source);
        let tokens = scanner.scan_tokens();

        assert_eq!(tokens[0].token_type, Var);
        assert_eq!(tokens[1].token_type, Identifier);
        assert_eq!(tokens[2].token_type, Equal);
        assert_eq!(tokens[3].token_type, Number);
        assert_eq!(tokens[4].token_type, Semicolon);
        assert_eq!(tokens[5].token_type, Var);
        assert_eq!(tokens[6].token_type, Identifier);
        assert_eq!(tokens[7].token_type, Equal);
        assert_eq!(tokens[8].token_type, Number);
        assert_eq!(tokens[9].token_type, Semicolon);
        assert_eq!(tokens[10].token_type, Eof);
        assert_eq!(tokens.len(), 11);
    }

    #[test]
    fn test_empty_source() {
        let source = String::from("");
        let mut scanner = Scanner::new(&source);
        let tokens = scanner.scan_tokens();

        assert_eq!(tokens.len(), 1);
        assert_eq!(tokens[0].token_type, Eof);
    }

    #[test]
    fn test_only_whitespace() {
        let source = String::from("   \n\t  ");
        let mut scanner = Scanner::new(&source);
        let tokens = scanner.scan_tokens();

        assert_eq!(tokens.len(), 1);
        assert_eq!(tokens[0].token_type, Eof);
    }

    #[test]
    fn test_invalid_number_format() {
        let source = String::from("123.abc");
        let mut scanner = Scanner::new(&source);
        let tokens = scanner.scan_tokens();

        assert_eq!(tokens[0].token_type, Number);
        match tokens[0].literal {
            NumberLit(n) => assert_eq!(n, 123.0),
            _ => panic!("Expected number literal"),
        }
        assert_eq!(tokens[1].token_type, Dot);
        assert_eq!(tokens[2].token_type, Identifier);
        assert_eq!(tokens[3].token_type, Eof);
        assert_eq!(tokens.len(), 4);
    }
}
