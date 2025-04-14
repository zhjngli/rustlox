use once_cell::sync::Lazy;
use std::{collections::HashMap, iter::Peekable, str::Chars};

use crate::token::{
    Token,
    TokenLiteral::{Null, NumberLit, StringLit},
    TokenType::{
        self, And, Bang, BangEqual, Class, Comma, Dot, Else, Eof, Equal, EqualEqual, False, For,
        Fun, Greater, GreaterEqual, Identifier, If, LeftBrace, LeftParen, Less, LessEqual, Minus,
        Nil, Number, Or, Plus, Print, Return, RightBrace, RightParen, Semicolon, Slash, Star,
        String as TString, Super, This, True, Var, While,
    },
};

static KEYWORDS: Lazy<HashMap<String, TokenType>> = Lazy::new(|| {
    let mut m = HashMap::new();
    m.insert("and".to_string(), And);
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

        self.tokens
            .push(Token::new(Eof, "".to_string(), Null, self.line));
        &self.tokens
    }

    fn scan_token(&mut self) -> Option<Token> {
        let opt_c = self.source.next();
        match opt_c {
            Some(c) => match c {
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
