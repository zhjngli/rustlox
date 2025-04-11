use crate::{
    expr::{Expr, ExprKind as E},
    stmt::Stmt,
    token::{
        Token, TokenLiteral,
        TokenType::{
            self, And, Bang, BangEqual, Class, Comma, Dot, Else, Eof, Equal, EqualEqual, False,
            For, Fun, Greater, GreaterEqual, Identifier, If, LeftBrace, LeftParen, Less, LessEqual,
            Minus, Nil, Number, Or, Plus, Print, Return, RightBrace, RightParen, Semicolon, Slash,
            Star, String as TString, True, Var, While,
        },
    },
};

#[derive(Debug, Clone)]
pub struct Parser<'a> {
    tokens: &'a Vec<Token>,
    current: usize,
}

#[derive(Debug)]
pub struct ParseError;

pub fn parse_error(token: &Token, message: &str) -> ParseError {
    crate::errors::report_token_error(token, message);
    ParseError {}
}

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
        let stmt_result = if self.match_next(&[Class]) {
            self.class_declaration()
        } else if self.match_next(&[Fun]) {
            self.function("function")
        } else if self.match_next(&[Var]) {
            self.var_declaration()
        } else {
            self.statement()
        };
        match stmt_result {
            Ok(s) => Ok(s),
            Err(_) => {
                self.synchronize();
                Err(ParseError {})
            }
        }
    }

    fn class_declaration(&mut self) -> Result<Stmt, ParseError> {
        let name = self.consume(&Identifier, "Expect class name.")?.clone();
        self.consume(&LeftBrace, "Expect '{' before class body.")?;

        let mut methods = Vec::new();
        while !self.check(&RightBrace) && !self.is_at_end() {
            methods.push(self.function("method")?);
        }

        self.consume(&RightBrace, "Expect '}' after class body.")?;

        Ok(Stmt::Class { name, methods })
    }

    fn statement(&mut self) -> Result<Stmt, ParseError> {
        if self.match_next(&[For]) {
            return self.for_statement();
        }
        if self.match_next(&[If]) {
            return self.if_statement();
        }
        if self.match_next(&[Print]) {
            return self.print_statement();
        }
        if self.match_next(&[Return]) {
            return self.return_statement();
        }
        if self.match_next(&[While]) {
            return self.while_statement();
        }
        if self.match_next(&[LeftBrace]) {
            return Ok(Stmt::Block {
                stmts: self.block()?,
            });
        }
        self.expression_statement()
    }

    fn for_statement(&mut self) -> Result<Stmt, ParseError> {
        self.consume(&LeftParen, "Expect '(' after 'for'.")?;

        let initializer = if self.match_next(&[Semicolon]) {
            None
        } else if self.match_next(&[Var]) {
            Some(self.var_declaration()?)
        } else {
            Some(self.expression_statement()?)
        };

        let condition = if !self.check(&Semicolon) {
            self.expression()?
        } else {
            Expr::new(E::Literal {
                value: TokenLiteral::Bool(true),
            })
        };
        self.consume(&Semicolon, "Expect ';' after loop condition.")?;

        let increment = if !self.check(&RightParen) {
            Some(self.expression()?)
        } else {
            None
        };
        self.consume(&RightParen, "Expect ')' after for clauses.")?;

        let mut body = self.statement()?;
        if let Some(inc) = increment {
            body = Stmt::Block {
                stmts: vec![body, Stmt::Expr { expr: inc }],
            };
        }
        body = Stmt::While {
            condition,
            body: Box::new(body),
        };
        if let Some(init) = initializer {
            body = Stmt::Block {
                stmts: vec![init, body],
            };
        }

        Ok(body)
    }

    fn if_statement(&mut self) -> Result<Stmt, ParseError> {
        self.consume(&LeftParen, "Expect '(' after 'if'.")?;
        let condition = self.expression()?;
        self.consume(&RightParen, "Expect ')' after if condition.")?;

        let then_branch = Box::new(self.statement()?);
        let mut else_branch = None;
        if self.match_next(&[Else]) {
            else_branch = Some(Box::new(self.statement()?));
        }
        Ok(Stmt::If {
            condition,
            then_branch,
            else_branch,
        })
    }

    fn print_statement(&mut self) -> Result<Stmt, ParseError> {
        let value = self.expression()?;
        self.consume(&Semicolon, "Expect ';' after value.")?;
        Ok(Stmt::Print { expr: value })
    }

    fn return_statement(&mut self) -> Result<Stmt, ParseError> {
        let keyword = self.previous().clone();
        let value = if !self.check(&Semicolon) {
            Some(self.expression()?)
        } else {
            None
        };
        self.consume(&Semicolon, "Expect ';' after return value.")?;
        Ok(Stmt::Return { keyword, value })
    }

    fn var_declaration(&mut self) -> Result<Stmt, ParseError> {
        let name = self.consume(&Identifier, "Expect variable name.")?.clone();
        let initializer = if self.match_next(&[Equal]) {
            Some(self.expression()?.clone())
        } else {
            None
        };
        self.consume(&Semicolon, "Expect ';' after variable declaration.")?;
        Ok(Stmt::Var { name, initializer })
    }

    fn while_statement(&mut self) -> Result<Stmt, ParseError> {
        self.consume(&LeftParen, "Expect '(' after 'while'.")?;
        let condition = self.expression()?;
        self.consume(&RightParen, "Expect ')' after condition")?;
        let body = Box::new(self.statement()?);

        Ok(Stmt::While { condition, body })
    }

    fn expression_statement(&mut self) -> Result<Stmt, ParseError> {
        let expr = self.expression()?;
        self.consume(&Semicolon, "Expect ';' after expression.")?;
        Ok(Stmt::Expr { expr: expr })
    }

    fn function(&mut self, kind: &str) -> Result<Stmt, ParseError> {
        let name = self
            .consume(&Identifier, &format!("Expect {} name.", kind))?
            .clone();

        self.consume(&LeftParen, &format!("Expect '(' after {} name.", kind))?;
        let mut params = Vec::new();
        if !self.check(&RightParen) {
            loop {
                if params.len() >= 255 {
                    return Err(parse_error(
                        self.peek(),
                        "Can't have more than 255 parameters.",
                    ));
                }
                let param = self.consume(&Identifier, "Expect parameter name.")?;
                params.push(param.clone());

                if !self.match_next(&[Comma]) {
                    break;
                }
            }
        }
        self.consume(&RightParen, "Expect ')' after parameters")?;

        self.consume(&LeftBrace, &format!("Expect '{{' before {} body.", kind))?;
        let body = self.block()?;
        Ok(Stmt::Function { name, params, body })
    }

    fn block(&mut self) -> Result<Vec<Stmt>, ParseError> {
        let mut stmts = Vec::new();
        while !self.check(&RightBrace) && !self.is_at_end() {
            stmts.push(self.declaration()?);
        }
        self.consume(&RightBrace, "Expect '}' after block.")?;
        Ok(stmts)
    }

    fn assignment(&mut self) -> Result<Expr, ParseError> {
        let expr = self.or()?;
        if self.match_next(&[Equal]) {
            let equals = self.previous().clone();
            let value = self.assignment()?;

            match expr.kind() {
                E::Variable { name } => {
                    return Ok(Expr::new(E::Assign {
                        name: name.clone(),
                        value: Box::new(value),
                    }))
                }
                E::Get { object, name } => {
                    return Ok(Expr::new(E::Set {
                        object: object.clone(),
                        name: name.clone(),
                        value: Box::new(value),
                    }))
                }
                _ => {
                    parse_error(&equals, "Invalid assignment target.");
                }
            }
        }
        Ok(expr)
    }

    fn or(&mut self) -> Result<Expr, ParseError> {
        let mut expr = self.and()?;
        while self.match_next(&[Or]) {
            let op = self.previous().clone();
            let right = self.and()?;
            expr = Expr::new(E::Logical {
                left: Box::new(expr),
                op,
                right: Box::new(right),
            });
        }
        Ok(expr)
    }

    fn and(&mut self) -> Result<Expr, ParseError> {
        let mut expr = self.equality()?;
        while self.match_next(&[And]) {
            let op = self.previous().clone();
            let right = self.equality()?;
            expr = Expr::new(E::Logical {
                left: Box::new(expr),
                op,
                right: Box::new(right),
            });
        }
        Ok(expr)
    }

    fn equality(&mut self) -> Result<Expr, ParseError> {
        self.binary(&[BangEqual, EqualEqual], |p| p.comparison())
    }

    fn comparison(&mut self) -> Result<Expr, ParseError> {
        self.binary(&[Greater, GreaterEqual, Less, LessEqual], |p| p.term())
    }

    fn term(&mut self) -> Result<Expr, ParseError> {
        self.binary(&[Minus, Plus], |p| p.factor())
    }

    fn factor(&mut self) -> Result<Expr, ParseError> {
        self.binary(&[Slash, Star], |p| p.unary())
    }

    fn binary<F>(&mut self, token_types: &[TokenType], mut parse_fn: F) -> Result<Expr, ParseError>
    where
        F: FnMut(&mut Self) -> Result<Expr, ParseError>,
    {
        let mut expr = parse_fn(self)?;
        while self.match_next(token_types) {
            let op = self.previous().clone();
            let right = parse_fn(self)?;
            expr = Expr::new(E::Binary {
                left: Box::new(expr),
                op: op,
                right: Box::new(right),
            });
        }
        Ok(expr)
    }

    fn unary(&mut self) -> Result<Expr, ParseError> {
        if self.match_next(&[Bang, Minus]) {
            let op = self.previous().clone();
            let right = self.unary()?;
            return Ok(Expr::new(E::Unary {
                op: op,
                expr: Box::new(right),
            }));
        }
        return self.call();
    }

    fn call(&mut self) -> Result<Expr, ParseError> {
        let mut expr = self.primary()?;
        loop {
            if self.match_next(&[LeftParen]) {
                expr = self.finish_call(expr)?;
            } else if self.match_next(&[Dot]) {
                let name = self
                    .consume(&Identifier, "Expect property name after '.'.")?
                    .clone();
                expr = Expr::new(E::Get {
                    object: Box::new(expr),
                    name,
                });
            } else {
                break;
            }
        }
        Ok(expr)
    }

    fn finish_call(&mut self, callee: Expr) -> Result<Expr, ParseError> {
        let mut args = Vec::new();
        if !self.check(&RightParen) {
            args.push(self.expression()?);
            while self.match_next(&[Comma]) {
                if args.len() >= 255 {
                    parse_error(self.peek(), "Can't have more than 255 arguments.");
                }
                args.push(self.expression()?);
            }
        }

        let paren = self
            .consume(&RightParen, "Expect ')' after arguments.")?
            .clone();

        Ok(Expr::new(E::Call {
            callee: Box::new(callee),
            paren,
            args,
        }))
    }

    fn primary(&mut self) -> Result<Expr, ParseError> {
        if self.match_next(&[True]) {
            return Ok(Expr::new(E::Literal {
                value: TokenLiteral::Bool(true),
            }));
        } else if self.match_next(&[False]) {
            return Ok(Expr::new(E::Literal {
                value: TokenLiteral::Bool(false),
            }));
        } else if self.match_next(&[Nil]) {
            return Ok(Expr::new(E::Literal {
                value: TokenLiteral::Null,
            }));
        } else if self.match_next(&[Number, TString]) {
            return Ok(Expr::new(E::Literal {
                value: self.previous().literal.clone(),
            }));
        } else if self.match_next(&[Identifier]) {
            return Ok(Expr::new(E::Variable {
                name: self.previous().clone(),
            }));
        } else if self.match_next(&[LeftParen]) {
            let expr = self.expression()?;
            self.consume(&RightParen, "Expect ')' after expression.")?;
            return Ok(Expr::new(E::Grouping {
                expr: Box::new(expr),
            }));
        }

        Err(parse_error(self.peek(), "Expect expression."))
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

    fn consume(&mut self, token_type: &TokenType, message: &str) -> Result<&Token, ParseError> {
        if self.check(token_type) {
            Ok(self.advance())
        } else {
            Err(parse_error(self.peek(), message))
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
        self.peek().token_type == Eof
    }

    fn peek(&self) -> &Token {
        &self.tokens[self.current]
    }

    fn previous(&self) -> &Token {
        &self.tokens[self.current - 1]
    }

    fn synchronize(&mut self) {
        self.advance();
        while !self.is_at_end() {
            if self.previous().token_type == Semicolon {
                return;
            }

            match self.peek().token_type {
                Class | Fun | Var | For | If | While | Print | Return => return,
                _ => (),
            }

            self.advance();
        }
    }
}
