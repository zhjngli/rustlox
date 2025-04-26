use std::{
    fmt::{Display, Formatter, Result},
    rc::Rc,
};

#[derive(Debug, Clone, PartialEq)]
pub enum TokenType {
    // Single-character tokens.
    LeftParen,
    RightParen,
    LeftBrace,
    RightBrace,
    LeftBracket,
    RightBracket,
    Comma,
    Dot,
    Minus,
    Plus,
    Semicolon,
    Slash,
    Star,

    // One or two character tokens.
    Bang,
    BangEqual,
    Equal,
    EqualEqual,
    Greater,
    GreaterEqual,
    Less,
    LessEqual,
    Mod,

    // Literals.
    Identifier,
    String,
    Number,

    // Keywords.
    And,
    Break,
    Class,
    Else,
    False,
    Fun,
    For,
    If,
    Nil,
    Or,
    Print,
    Return,
    Super,
    This,
    True,
    Var,
    While,

    Eof,
}

#[derive(Debug, Clone, PartialEq)]
pub enum TokenLiteral {
    NumberLit(f64),
    Bool(bool),
    StringLit(String),
    Null,
}

#[derive(Debug, Clone)]
pub struct Token {
    // as long as Tokens are accessed via TokenRef, you cannot assign data to it
    pub token_type: TokenType,
    pub lexeme: String,
    pub literal: TokenLiteral,
    pub line: usize,
}

pub type TokenRef = Rc<Token>;

impl Token {
    pub fn new(tt: TokenType, lexeme: String, tl: TokenLiteral, line: usize) -> Self {
        Token {
            token_type: tt,
            lexeme: lexeme,
            literal: tl,
            line: line,
        }
    }
}

impl Display for Token {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        write!(
            f,
            "{:?} {} {:?}",
            self.token_type, self.lexeme, self.literal
        )
    }
}
