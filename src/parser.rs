use crate::{
    expr::Expr,
    stmt::Stmt,
    token::{Token, TokenLiteral, TokenType},
};

#[derive(Debug, Clone)]
pub struct Parser<'a> {
    tokens: &'a Vec<Token>,
    current: usize,
}

#[derive(Debug)]
pub struct ParseError;

impl<'a> Parser<'a> {
    pub fn new(tokens: &'a Vec<Token>) -> Self {
        Parser { tokens, current: 0 }
    }

    pub fn parse(&mut self) -> Result<Vec<Stmt>, ParseError> {
        let mut stmts = Vec::new();
        let mut error = false;
        while !self.is_at_end() {
            match self.declaration() {
                Ok(s) => stmts.push(s),
                Err(_) => error = true,
            }
        }
        if error {
            Err(ParseError {})
        } else {
            Ok(stmts)
        }
    }

    fn expression(&mut self) -> Result<Expr, ParseError> {
        self.assignment()
    }

    fn declaration(&mut self) -> Result<Stmt, ParseError> {
        let stmt_result;
        if self.match_next(&[TokenType::Var]) {
            stmt_result = self.var_declaration();
        } else {
            stmt_result = self.statement();
        }
        match stmt_result {
            Ok(s) => Ok(s),
            Err(_) => {
                self.synchronize();
                Err(ParseError {})
            }
        }
    }

    fn statement(&mut self) -> Result<Stmt, ParseError> {
        if self.match_next(&[TokenType::Print]) {
            return self.print_statement();
        }
        if self.match_next(&[TokenType::LeftBrace]) {
            return Ok(Stmt::Block {
                stmts: self.block()?,
            });
        }
        self.expression_statement()
    }

    fn print_statement(&mut self) -> Result<Stmt, ParseError> {
        let value = self.expression()?;
        self.consume(&TokenType::Semicolon, "Expect ';' after value.".to_owned())?;
        Ok(Stmt::Print { expr: value })
    }

    fn var_declaration(&mut self) -> Result<Stmt, ParseError> {
        let name = self
            .consume(&TokenType::Identifier, "Expect variable name.".to_owned())?
            .clone();
        let mut initializer = None;
        if self.match_next(&[TokenType::Equal]) {
            initializer = Some(self.expression()?.clone());
        }
        self.consume(
            &TokenType::Semicolon,
            "Expect ';' after variable declaration.".to_owned(),
        )?;
        Ok(Stmt::Var { name, initializer })
    }

    fn expression_statement(&mut self) -> Result<Stmt, ParseError> {
        let expr = self.expression()?;
        self.consume(
            &TokenType::Semicolon,
            "Expect ';' after expression.".to_owned(),
        )?;
        Ok(Stmt::Expr { expr: expr })
    }

    fn block(&mut self) -> Result<Vec<Stmt>, ParseError> {
        let mut stmts = Vec::new();
        while !self.check(&TokenType::RightBrace) && !self.is_at_end() {
            stmts.push(self.declaration()?);
        }
        self.consume(&TokenType::RightBrace, "Expect '}' after block.".to_owned())?;
        Ok(stmts)
    }

    fn assignment(&mut self) -> Result<Expr, ParseError> {
        let expr = self.equality()?;
        if self.match_next(&[TokenType::Equal]) {
            let equals = self.previous().clone();
            let value = self.assignment()?;

            match expr {
                Expr::Variable { name } => {
                    return Ok(Expr::Assign {
                        name,
                        value: Box::new(value),
                    })
                }
                _ => {
                    self.error(&equals, "Invalid assignment target.".to_owned());
                }
            }
        }
        Ok(expr)
    }

    fn equality(&mut self) -> Result<Expr, ParseError> {
        self.binary(&[TokenType::BangEqual, TokenType::EqualEqual], |p| {
            p.comparison()
        })
    }

    fn comparison(&mut self) -> Result<Expr, ParseError> {
        self.binary(
            &[
                TokenType::Greater,
                TokenType::GreaterEqual,
                TokenType::Less,
                TokenType::LessEqual,
            ],
            |p| p.term(),
        )
    }

    fn term(&mut self) -> Result<Expr, ParseError> {
        self.binary(&[TokenType::Minus, TokenType::Plus], |p| p.factor())
    }

    fn factor(&mut self) -> Result<Expr, ParseError> {
        self.binary(&[TokenType::Slash, TokenType::Star], |p| p.unary())
    }

    fn binary<F>(&mut self, token_types: &[TokenType], mut parse_fn: F) -> Result<Expr, ParseError>
    where
        F: FnMut(&mut Self) -> Result<Expr, ParseError>,
    {
        let mut expr = parse_fn(self)?;
        while self.match_next(token_types) {
            let op = self.previous().clone();
            let right = parse_fn(self)?;
            expr = Expr::Binary {
                left: Box::new(expr),
                op: op,
                right: Box::new(right),
            }
        }
        Ok(expr)
    }

    fn unary(&mut self) -> Result<Expr, ParseError> {
        if self.match_next(&[TokenType::Bang, TokenType::Minus]) {
            let op = self.previous().clone();
            let right = self.unary()?;
            return Ok(Expr::Unary {
                op: op,
                expr: Box::new(right),
            });
        }
        return self.primary();
    }

    fn primary(&mut self) -> Result<Expr, ParseError> {
        if self.match_next(&[TokenType::True]) {
            return Ok(Expr::Literal {
                value: TokenLiteral::Bool(true),
            });
        } else if self.match_next(&[TokenType::False]) {
            return Ok(Expr::Literal {
                value: TokenLiteral::Bool(false),
            });
        } else if self.match_next(&[TokenType::Nil]) {
            return Ok(Expr::Literal {
                value: TokenLiteral::Null,
            });
        } else if self.match_next(&[TokenType::Number, TokenType::String]) {
            return Ok(Expr::Literal {
                value: self.previous().literal.clone(),
            });
        } else if self.match_next(&[TokenType::Identifier]) {
            return Ok(Expr::Variable {
                name: self.previous().clone(),
            });
        } else if self.match_next(&[TokenType::LeftParen]) {
            let expr = self.expression()?;
            self.consume(
                &TokenType::RightParen,
                "Expect ')' after expression.".to_string(),
            )?;
            return Ok(Expr::Grouping {
                expr: Box::new(expr),
            });
        }

        Err(self.error(self.peek(), "Expect expression.".to_string()))
    }

    fn match_next(&mut self, token_types: &[TokenType]) -> bool {
        for tt in token_types {
            if self.check(tt) {
                self.advance();
                return true;
            }
        }
        return false;
    }

    fn consume(&mut self, token_type: &TokenType, message: String) -> Result<&Token, ParseError> {
        if self.check(token_type) {
            Ok(self.advance())
        } else {
            Err(self.error(self.peek(), message))
        }
    }

    fn check(&self, token_type: &TokenType) -> bool {
        if self.is_at_end() {
            return false;
        }
        self.peek().token_type == *token_type
    }

    fn advance(&mut self) -> &Token {
        if !self.is_at_end() {
            self.current += 1;
        }
        self.previous()
    }

    fn is_at_end(&self) -> bool {
        self.peek().token_type == TokenType::Eof
    }

    fn peek(&self) -> &Token {
        &self.tokens[self.current]
    }

    fn previous(&self) -> &Token {
        &self.tokens[self.current - 1]
    }

    fn error(&self, token: &Token, message: String) -> ParseError {
        crate::errors::token_error(token, message);
        ParseError {}
    }

    fn synchronize(&mut self) {
        self.advance();
        while !self.is_at_end() {
            if self.previous().token_type == TokenType::Semicolon {
                return;
            }

            match self.peek().token_type {
                TokenType::Class
                | TokenType::Fun
                | TokenType::Var
                | TokenType::For
                | TokenType::If
                | TokenType::While
                | TokenType::Print
                | TokenType::Return => return,
                _ => (),
            }

            self.advance();
        }
    }
}
