use once_cell::sync::Lazy;
use std::{collections::HashMap, iter::Peekable, str::Chars};

use crate::{
    report,
    token::{Token, TokenLiteral, TokenType},
};

static KEYWORDS: Lazy<HashMap<String, TokenType>> = Lazy::new(|| {
    let mut m = HashMap::new();
    m.insert("and".to_string(), TokenType::And);
    m.insert("class".to_string(), TokenType::Class);
    m.insert("else".to_string(), TokenType::Else);
    m.insert("false".to_string(), TokenType::False);
    m.insert("for".to_string(), TokenType::For);
    m.insert("fun".to_string(), TokenType::Fun);
    m.insert("if".to_string(), TokenType::If);
    m.insert("nil".to_string(), TokenType::Nil);
    m.insert("or".to_string(), TokenType::Or);
    m.insert("print".to_string(), TokenType::Print);
    m.insert("return".to_string(), TokenType::Return);
    m.insert("super".to_string(), TokenType::Super);
    m.insert("this".to_string(), TokenType::This);
    m.insert("true".to_string(), TokenType::True);
    m.insert("var".to_string(), TokenType::Var);
    m.insert("while".to_string(), TokenType::While);
    m
});

#[derive(Debug)]
pub struct Scanner<'a> {
    source: Peekable<Chars<'a>>,
    tokens: Vec<Token>,
    line: usize,
}

impl<'a> Scanner<'a> {
    pub fn new(s: &'a String) -> Self {
        Scanner {
            source: s.chars().peekable(),
            tokens: Vec::new(),
            line: 1,
        }
    }

    pub fn scan_tokens(&mut self) -> &Vec<Token> {
        while let Some(_) = self.source.peek() {
            match self.scan_token() {
                Some(t) => self.tokens.push(t),
                None => (),
            }
        }

        self.tokens.push(Token::new(
            TokenType::Eof,
            "".to_string(),
            TokenLiteral::Null,
            self.line,
        ));
        &self.tokens
    }

    fn scan_token(&mut self) -> Option<Token> {
        let opt_c = self.source.next();
        match opt_c {
            Some(c) => match c {
                '(' => Some(Token::new(
                    TokenType::LeftParen,
                    c.to_string(),
                    TokenLiteral::Null,
                    self.line,
                )),
                ')' => Some(Token::new(
                    TokenType::RightParen,
                    c.to_string(),
                    TokenLiteral::Null,
                    self.line,
                )),
                '{' => Some(Token::new(
                    TokenType::LeftBrace,
                    c.to_string(),
                    TokenLiteral::Null,
                    self.line,
                )),
                '}' => Some(Token::new(
                    TokenType::RightBrace,
                    c.to_string(),
                    TokenLiteral::Null,
                    self.line,
                )),
                ',' => Some(Token::new(
                    TokenType::Comma,
                    c.to_string(),
                    TokenLiteral::Null,
                    self.line,
                )),
                '.' => Some(Token::new(
                    TokenType::Dot,
                    c.to_string(),
                    TokenLiteral::Null,
                    self.line,
                )),
                '-' => Some(Token::new(
                    TokenType::Minus,
                    c.to_string(),
                    TokenLiteral::Null,
                    self.line,
                )),
                '+' => Some(Token::new(
                    TokenType::Plus,
                    c.to_string(),
                    TokenLiteral::Null,
                    self.line,
                )),
                ';' => Some(Token::new(
                    TokenType::Semicolon,
                    c.to_string(),
                    TokenLiteral::Null,
                    self.line,
                )),
                '*' => Some(Token::new(
                    TokenType::Star,
                    c.to_string(),
                    TokenLiteral::Null,
                    self.line,
                )),
                '!' => {
                    if self.match_next('=') {
                        Some(Token::new(
                            TokenType::BangEqual,
                            String::from("!="),
                            TokenLiteral::Null,
                            self.line,
                        ))
                    } else {
                        Some(Token::new(
                            TokenType::Bang,
                            String::from('!'),
                            TokenLiteral::Null,
                            self.line,
                        ))
                    }
                }
                '=' => {
                    if self.match_next('=') {
                        Some(Token::new(
                            TokenType::EqualEqual,
                            String::from("=="),
                            TokenLiteral::Null,
                            self.line,
                        ))
                    } else {
                        Some(Token::new(
                            TokenType::Equal,
                            String::from('='),
                            TokenLiteral::Null,
                            self.line,
                        ))
                    }
                }
                '<' => {
                    if self.match_next('=') {
                        Some(Token::new(
                            TokenType::LessEqual,
                            String::from("<="),
                            TokenLiteral::Null,
                            self.line,
                        ))
                    } else {
                        Some(Token::new(
                            TokenType::Less,
                            String::from('<'),
                            TokenLiteral::Null,
                            self.line,
                        ))
                    }
                }
                '>' => {
                    if self.match_next('=') {
                        Some(Token::new(
                            TokenType::GreaterEqual,
                            String::from(">="),
                            TokenLiteral::Null,
                            self.line,
                        ))
                    } else {
                        Some(Token::new(
                            TokenType::Greater,
                            String::from('>'),
                            TokenLiteral::Null,
                            self.line,
                        ))
                    }
                }
                '/' => {
                    if self.match_next('/') {
                        // comment goes until the end of the line
                        while let Some(c) = self.source.peek() {
                            if *c != '\n' {
                                self.source.next();
                            }
                        }
                        None
                    } else {
                        Some(Token::new(
                            TokenType::Slash,
                            c.to_string(),
                            TokenLiteral::Null,
                            self.line,
                        ))
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
                        report(self.line, format!("Unexpected character: {}", c));
                        None
                    }
                }
            },
            None => panic!("peek and next didn't match"),
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
            report(self.line, "Unterminated string".to_string())
        }
        // the closing '"' char
        self.source.next();

        Token::new(
            TokenType::String,
            s.iter().collect(),
            TokenLiteral::StringLit(s.iter().collect()),
            self.line,
        )
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
            Ok(num) => Token::new(
                TokenType::Number,
                n.iter().collect(),
                TokenLiteral::NumberLit(num),
                self.line,
            ),
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
            None => TokenType::Identifier,
        };
        Token::new(token_type, id, TokenLiteral::Null, self.line)
    }
}
