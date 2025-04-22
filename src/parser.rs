use crate::{
    expr::{
        AssignE, BinaryE, CallE, Expr, GetE, GroupingE, LiteralE, LogicalE, SetE, SuperE, ThisE,
        UnaryE, VariableE,
    },
    stmt::{BlockS, ClassS, ExprS, FunctionS, IfS, PrintS, ReturnS, Stmt, VarS, WhileS},
    token::{
        TokenLiteral, TokenRef,
        TokenType::{
            self, And, Bang, BangEqual, Class, Comma, Dot, Else, Eof, Equal, EqualEqual, False,
            For, Fun, Greater, GreaterEqual, Identifier, If, LeftBrace, LeftParen, Less, LessEqual,
            Minus, Nil, Number, Or, Plus, Print, Return, RightBrace, RightParen, Semicolon, Slash,
            Star, String as TString, Super, This, True, Var, While,
        },
    },
};

#[derive(Debug, Clone)]
pub struct Parser<'a> {
    tokens: &'a Vec<TokenRef>,
    current: usize,
    repl: bool,
}

#[derive(Debug)]
pub struct ParseError;

fn parse_error(token: &TokenRef, message: &str, repl: bool) -> ParseError {
    if !repl {
        crate::report_token_error(token, message)
    };
    ParseError {}
}

impl<'a> Parser<'a> {
    pub fn new(tokens: &'a Vec<TokenRef>, repl: bool) -> Self {
        Parser {
            tokens,
            current: 0,
            repl,
        }
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

    pub fn parse_repl(&mut self) -> Result<Vec<Stmt>, ParseError> {
        let mut stmts = Vec::new();
        let mut error = false;
        while !self.is_at_end() && error == false {
            let prev_i = self.current;
            match self.declaration() {
                Ok(s) => stmts.push(s),
                Err(_) => {
                    self.current = prev_i;
                    match self.expression() {
                        Ok(e) => stmts.push(Stmt::P(PrintS { expr: e })),
                        Err(_) => error = true,
                    }
                }
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
            Ok(Stmt::C(self.class_declaration()?))
        } else if self.match_next(&[Fun]) {
            Ok(Stmt::F(self.function("function")?))
        } else if self.match_next(&[Var]) {
            Ok(Stmt::V(self.var_declaration()?))
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

    fn class_declaration(&mut self) -> Result<ClassS, ParseError> {
        let name = self.consume(&Identifier, "Expect class name.")?.clone();

        let mut superclass = None;
        if self.match_next(&[Less]) {
            self.consume(&Identifier, "Expect superclass name.")?;
            superclass = Some(VariableE::new(self.previous().clone()));
        }

        self.consume(&LeftBrace, "Expect '{' before class body.")?;
        let mut methods = Vec::new();
        while !self.check(&RightBrace) && !self.is_at_end() {
            methods.push(self.function("method")?);
        }
        self.consume(&RightBrace, "Expect '}' after class body.")?;

        Ok(ClassS {
            name,
            superclass,
            methods,
        })
    }

    fn statement(&mut self) -> Result<Stmt, ParseError> {
        if self.match_next(&[For]) {
            return Ok(self.for_statement()?);
        }
        if self.match_next(&[If]) {
            return Ok(Stmt::I(self.if_statement()?));
        }
        if self.match_next(&[Print]) {
            return Ok(Stmt::P(self.print_statement()?));
        }
        if self.match_next(&[Return]) {
            return Ok(Stmt::R(self.return_statement()?));
        }
        if self.match_next(&[While]) {
            return Ok(Stmt::W(self.while_statement()?));
        }
        if self.match_next(&[LeftBrace]) {
            return Ok(Stmt::B(self.block()?));
        }
        Ok(Stmt::E(self.expression_statement()?))
    }

    fn for_statement(&mut self) -> Result<Stmt, ParseError> {
        self.consume(&LeftParen, "Expect '(' after 'for'.")?;

        let initializer = if self.match_next(&[Semicolon]) {
            None
        } else if self.match_next(&[Var]) {
            Some(Stmt::V(self.var_declaration()?))
        } else {
            Some(Stmt::E(self.expression_statement()?))
        };

        let condition = if !self.check(&Semicolon) {
            self.expression()?
        } else {
            Expr::Li(LiteralE::new(TokenLiteral::Bool(true)))
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
            body = Stmt::B(BlockS {
                stmts: vec![body, Stmt::E(ExprS { expr: inc })],
            });
        }
        body = Stmt::W(WhileS {
            condition,
            body: Box::new(body),
        });
        if let Some(init) = initializer {
            body = Stmt::B(BlockS {
                stmts: vec![init, body],
            });
        }

        Ok(body)
    }

    fn if_statement(&mut self) -> Result<IfS, ParseError> {
        self.consume(&LeftParen, "Expect '(' after 'if'.")?;
        let condition = self.expression()?;
        self.consume(&RightParen, "Expect ')' after if condition.")?;

        let then_branch = Box::new(self.statement()?);
        let mut else_branch = None;
        if self.match_next(&[Else]) {
            else_branch = Some(Box::new(self.statement()?));
        }
        Ok(IfS {
            condition,
            then_branch,
            else_branch,
        })
    }

    fn print_statement(&mut self) -> Result<PrintS, ParseError> {
        let value = self.expression()?;
        self.consume(&Semicolon, "Expect ';' after value.")?;
        Ok(PrintS { expr: value })
    }

    fn return_statement(&mut self) -> Result<ReturnS, ParseError> {
        let keyword = self.previous().clone();
        let value = if !self.check(&Semicolon) {
            Some(self.expression()?)
        } else {
            None
        };
        self.consume(&Semicolon, "Expect ';' after return value.")?;
        Ok(ReturnS { keyword, value })
    }

    fn var_declaration(&mut self) -> Result<VarS, ParseError> {
        let name = self.consume(&Identifier, "Expect variable name.")?.clone();
        let initializer = if self.match_next(&[Equal]) {
            Some(self.expression()?.clone())
        } else {
            None
        };
        self.consume(&Semicolon, "Expect ';' after variable declaration.")?;
        Ok(VarS { name, initializer })
    }

    fn while_statement(&mut self) -> Result<WhileS, ParseError> {
        self.consume(&LeftParen, "Expect '(' after 'while'.")?;
        let condition = self.expression()?;
        self.consume(&RightParen, "Expect ')' after condition")?;
        let body = Box::new(self.statement()?);

        Ok(WhileS { condition, body })
    }

    fn expression_statement(&mut self) -> Result<ExprS, ParseError> {
        let expr = self.expression()?;
        self.consume(&Semicolon, "Expect ';' after expression.")?;
        Ok(ExprS { expr })
    }

    fn function(&mut self, kind: &str) -> Result<FunctionS, ParseError> {
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
                        self.repl,
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
        let body = self.block()?.stmts;
        Ok(FunctionS { name, params, body })
    }

    fn block(&mut self) -> Result<BlockS, ParseError> {
        let mut stmts = Vec::new();
        while !self.check(&RightBrace) && !self.is_at_end() {
            stmts.push(self.declaration()?);
        }
        self.consume(&RightBrace, "Expect '}' after block.")?;
        Ok(BlockS { stmts })
    }

    fn assignment(&mut self) -> Result<Expr, ParseError> {
        let expr = self.or()?;
        if self.match_next(&[Equal]) {
            let equals = self.previous().clone();
            let value = self.assignment()?;

            match expr {
                Expr::V(VariableE { name, .. }) => {
                    return Ok(Expr::A(AssignE::new(name.clone(), value)))
                }
                Expr::G(GetE { object, name, .. }) => {
                    return Ok(Expr::S(SetE::new(*object, name.clone(), value)))
                }
                _ => {
                    parse_error(&equals, "Invalid assignment target.", self.repl);
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
            expr = Expr::Lo(LogicalE::new(expr, op, right));
        }
        Ok(expr)
    }

    fn and(&mut self) -> Result<Expr, ParseError> {
        let mut expr = self.equality()?;
        while self.match_next(&[And]) {
            let op = self.previous().clone();
            let right = self.equality()?;
            expr = Expr::Lo(LogicalE::new(expr, op, right));
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
            expr = Expr::B(BinaryE::new(expr, op, right));
        }
        Ok(expr)
    }

    fn unary(&mut self) -> Result<Expr, ParseError> {
        if self.match_next(&[Bang, Minus]) {
            let op = self.previous().clone();
            let right = self.unary()?;
            return Ok(Expr::U(UnaryE::new(op, right)));
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
                expr = Expr::G(GetE::new(expr, name));
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
                    parse_error(
                        self.peek(),
                        "Can't have more than 255 arguments.",
                        self.repl,
                    );
                }
                args.push(self.expression()?);
            }
        }

        let paren = self
            .consume(&RightParen, "Expect ')' after arguments.")?
            .clone();

        Ok(Expr::C(CallE::new(callee, paren, args)))
    }

    fn primary(&mut self) -> Result<Expr, ParseError> {
        if self.match_next(&[True]) {
            return Ok(Expr::Li(LiteralE::new(TokenLiteral::Bool(true))));
        } else if self.match_next(&[False]) {
            return Ok(Expr::Li(LiteralE::new(TokenLiteral::Bool(false))));
        } else if self.match_next(&[Nil]) {
            return Ok(Expr::Li(LiteralE::new(TokenLiteral::Null)));
        } else if self.match_next(&[Number, TString]) {
            return Ok(Expr::Li(LiteralE::new(self.previous().literal.clone())));
        } else if self.match_next(&[Super]) {
            let keyword = self.previous().clone();
            self.consume(&Dot, "Expect '.' after 'super'.")?;
            let method = self
                .consume(&Identifier, "Expect superclass method name.")?
                .clone();
            return Ok(Expr::Su(SuperE::new(keyword, method)));
        } else if self.match_next(&[This]) {
            return Ok(Expr::T(ThisE::new(self.previous().clone())));
        } else if self.match_next(&[Identifier]) {
            return Ok(Expr::V(VariableE::new(self.previous().clone())));
        } else if self.match_next(&[LeftParen]) {
            let expr = self.expression()?;
            self.consume(&RightParen, "Expect ')' after expression.")?;
            return Ok(Expr::Gr(GroupingE::new(expr)));
        }

        Err(parse_error(self.peek(), "Expect expression.", self.repl))
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

    fn consume(&mut self, token_type: &TokenType, message: &str) -> Result<&TokenRef, ParseError> {
        if self.check(token_type) {
            Ok(self.advance())
        } else {
            Err(parse_error(self.peek(), message, self.repl))
        }
    }

    fn check(&self, token_type: &TokenType) -> bool {
        if self.is_at_end() {
            return false;
        }
        self.peek().token_type == *token_type
    }

    fn advance(&mut self) -> &TokenRef {
        if !self.is_at_end() {
            self.current += 1;
        }
        self.previous()
    }

    fn is_at_end(&self) -> bool {
        self.peek().token_type == Eof
    }

    fn peek(&self) -> &TokenRef {
        &self.tokens[self.current]
    }

    fn previous(&self) -> &TokenRef {
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
