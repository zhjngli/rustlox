use std::fmt::{Display, Formatter, Result as DResult};

use crate::{
    expr::{
        AssignE, BinaryE, CallE, Expr, GetE, GroupingE, ListE, ListGetE, ListSetE, LiteralE,
        LogicalE, SetE, SuperE, ThisE, UnaryE, VariableE,
    },
    stmt::{BlockS, BreakS, ClassS, ExprS, FunctionS, IfS, PrintS, ReturnS, Stmt, VarS, WhileS},
    token::{
        TokenLiteral, TokenRef,
        TokenType::{
            self, And, Bang, BangEqual, Break, Class, Comma, Dot, Else, Eof, Equal, EqualEqual,
            False, For, Fun, Greater, GreaterEqual, Identifier, If, LeftBrace, LeftBracket,
            LeftParen, Less, LessEqual, Minus, Mod, Nil, Number, Or, Plus, Print, Return,
            RightBrace, RightBracket, RightParen, Semicolon, Slash, Star, String as TString, Super,
            This, True, Var, While,
        },
    },
};

#[derive(Debug, Clone)]
pub struct Parser<'a> {
    tokens: &'a Vec<TokenRef>,
    current: usize,
    loop_depth: usize,
    repl: bool,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum FunctionKind {
    Function,
    Method,
}

impl Display for FunctionKind {
    fn fmt(&self, f: &mut Formatter<'_>) -> DResult {
        match self {
            FunctionKind::Function => write!(f, "function"),
            FunctionKind::Method => write!(f, "method"),
        }
    }
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
            loop_depth: 0,
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
            Ok(Stmt::F(self.function(FunctionKind::Function)?))
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
        let mut class_methods = Vec::new();
        while !self.check(&RightBrace) && !self.is_at_end() {
            if self.match_next(&[Class]) {
                class_methods.push(self.function(FunctionKind::Method)?);
            } else {
                methods.push(self.function(FunctionKind::Method)?);
            }
        }
        self.consume(&RightBrace, "Expect '}' after class body.")?;

        Ok(ClassS {
            name,
            superclass,
            methods,
            class_methods,
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
        if self.match_next(&[Break]) {
            return Ok(Stmt::Br(self.break_statement()?));
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

        self.loop_depth += 1;
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
        self.loop_depth -= 1;

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

    fn break_statement(&mut self) -> Result<BreakS, ParseError> {
        if self.loop_depth == 0 {
            return Err(parse_error(
                self.peek(),
                "'break' statement not inside a loop.",
                self.repl,
            ));
        }
        let keyword = self.previous().clone();
        self.consume(&Semicolon, "Expect ';' after break.")?;
        Ok(BreakS { keyword })
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

        self.loop_depth += 1;
        let body = Box::new(self.statement()?);
        self.loop_depth -= 1;

        Ok(WhileS { condition, body })
    }

    fn expression_statement(&mut self) -> Result<ExprS, ParseError> {
        let expr = self.expression()?;
        self.consume(&Semicolon, "Expect ';' after expression.")?;
        Ok(ExprS { expr })
    }

    fn function(&mut self, kind: FunctionKind) -> Result<FunctionS, ParseError> {
        let name = self
            .consume(&Identifier, &format!("Expect {} name.", kind))?
            .clone();

        let mut params = None;
        if self.check(&LeftParen) || kind == FunctionKind::Function {
            self.consume(&LeftParen, &format!("Expect '(' after {} name.", kind))?;
            let mut some_params = Vec::new();
            if !self.check(&RightParen) {
                loop {
                    if some_params.len() >= 255 {
                        return Err(parse_error(
                            self.peek(),
                            "Can't have more than 255 parameters.",
                            self.repl,
                        ));
                    }
                    let param = self.consume(&Identifier, "Expect parameter name.")?;
                    some_params.push(param.clone());

                    if !self.match_next(&[Comma]) {
                        break;
                    }
                }
            }
            self.consume(&RightParen, "Expect ')' after parameters")?;
            params = Some(some_params);
        }

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
                Expr::G(GetE { object, name, .. }) => {
                    return Ok(Expr::S(SetE::new(*object, name.clone(), value)))
                }
                Expr::LG(ListGetE {
                    object,
                    bracket,
                    index,
                    ..
                }) => {
                    return Ok(Expr::LS(ListSetE::new(
                        *object,
                        bracket.clone(),
                        *index,
                        value,
                    )))
                }
                Expr::V(VariableE { name, .. }) => {
                    return Ok(Expr::A(AssignE::new(name.clone(), value)))
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
        self.binary(&[Mod, Slash, Star], |p| p.unary())
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
            if self.match_next(&[LeftBracket]) {
                let bracket = self.previous().clone();
                let index = self.expression()?;
                self.consume(&RightBracket, "Expect ']' after index.")?;
                expr = Expr::LG(ListGetE::new(expr, bracket, index));
            } else if self.match_next(&[LeftParen]) {
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
            Ok(Expr::Li(LiteralE::new(TokenLiteral::Bool(true))))
        } else if self.match_next(&[False]) {
            Ok(Expr::Li(LiteralE::new(TokenLiteral::Bool(false))))
        } else if self.match_next(&[Nil]) {
            Ok(Expr::Li(LiteralE::new(TokenLiteral::Null)))
        } else if self.match_next(&[Number, TString]) {
            Ok(Expr::Li(LiteralE::new(self.previous().literal.clone())))
        } else if self.match_next(&[Super]) {
            let keyword = self.previous().clone();
            self.consume(&Dot, "Expect '.' after 'super'.")?;
            let method = self
                .consume(&Identifier, "Expect superclass method name.")?
                .clone();
            Ok(Expr::Su(SuperE::new(keyword, method)))
        } else if self.match_next(&[This]) {
            Ok(Expr::T(ThisE::new(self.previous().clone())))
        } else if self.match_next(&[Identifier]) {
            Ok(Expr::V(VariableE::new(self.previous().clone())))
        } else if self.match_next(&[LeftBracket]) {
            let mut elems = Vec::new();
            if !self.check(&RightBracket) {
                loop {
                    elems.push(self.expression()?);
                    if !self.match_next(&[Comma]) {
                        break;
                    }
                }
            }
            self.consume(&RightBracket, "Expect ']' after list.")?;
            Ok(Expr::L(ListE::new(elems)))
        } else if self.match_next(&[LeftParen]) {
            let expr = self.expression()?;
            self.consume(&RightParen, "Expect ')' after expression.")?;
            Ok(Expr::Gr(GroupingE::new(expr)))
        } else {
            Err(parse_error(self.peek(), "Expect expression.", self.repl))
        }
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

#[cfg(test)]
mod tests {
    use crate::token::Token;

    use super::*;

    fn create_token(
        token_type: TokenType,
        lexeme: &str,
        literal: TokenLiteral,
        line: usize,
    ) -> TokenRef {
        TokenRef::new(Token {
            token_type,
            lexeme: lexeme.to_string(),
            literal,
            line,
        })
    }

    #[test]
    fn test_parse_var_declaration() {
        let tokens = vec![
            create_token(Var, "var", TokenLiteral::Null, 1),
            create_token(Identifier, "x", TokenLiteral::Null, 1),
            create_token(Equal, "=", TokenLiteral::Null, 1),
            create_token(Number, "42", TokenLiteral::NumberLit(42.0), 1),
            create_token(Semicolon, ";", TokenLiteral::Null, 1),
            create_token(Eof, "", TokenLiteral::Null, 1),
        ];

        let mut parser = Parser::new(&tokens, false);
        let result = parser.parse();

        assert!(result.is_ok());
        let stmts = result.unwrap();
        assert_eq!(stmts.len(), 1);

        assert!(matches!(&stmts[0], Stmt::V(_)));
        if let Stmt::V(var_stmt) = &stmts[0] {
            assert_eq!(var_stmt.name.lexeme, "x");

            assert!(var_stmt.initializer.is_some());
            if let Some(Expr::Li(literal)) = &var_stmt.initializer {
                assert_eq!(literal.value, TokenLiteral::NumberLit(42.0));
            }
        }
    }

    #[test]
    fn test_parse_expression_statement() {
        let tokens = vec![
            create_token(Number, "42", TokenLiteral::NumberLit(42.0), 1),
            create_token(Semicolon, ";", TokenLiteral::Null, 1),
            create_token(Eof, "", TokenLiteral::Null, 1),
        ];

        let mut parser = Parser::new(&tokens, false);
        let result = parser.parse();

        assert!(result.is_ok());
        let stmts = result.unwrap();
        assert_eq!(stmts.len(), 1);

        assert!(matches!(&stmts[0], Stmt::E(_)));
        if let Stmt::E(expr_stmt) = &stmts[0] {
            assert!(matches!(expr_stmt.expr, Expr::Li(_)));
            if let Expr::Li(literal) = &expr_stmt.expr {
                assert_eq!(literal.value, TokenLiteral::NumberLit(42.0));
            }
        }
    }

    #[test]
    fn test_parse_if_statement() {
        let tokens = vec![
            create_token(If, "if", TokenLiteral::Null, 1),
            create_token(LeftParen, "(", TokenLiteral::Null, 1),
            create_token(True, "true", TokenLiteral::Bool(true), 1),
            create_token(RightParen, ")", TokenLiteral::Null, 1),
            create_token(Number, "42", TokenLiteral::NumberLit(42.0), 1),
            create_token(Semicolon, ";", TokenLiteral::Null, 1),
            create_token(Else, "else", TokenLiteral::Null, 1),
            create_token(Number, "0", TokenLiteral::NumberLit(0.0), 1),
            create_token(Semicolon, ";", TokenLiteral::Null, 1),
            create_token(Eof, "", TokenLiteral::Null, 1),
        ];

        let mut parser = Parser::new(&tokens, false);
        let result = parser.parse();

        assert!(result.is_ok());
        let stmts = result.unwrap();
        assert_eq!(stmts.len(), 1);

        assert!(matches!(&stmts[0], Stmt::I(_)));
        if let Stmt::I(if_stmt) = &stmts[0] {
            assert!(matches!(if_stmt.condition, Expr::Li(_)));
            if let Expr::Li(condition) = &if_stmt.condition {
                assert_eq!(condition.value, TokenLiteral::Bool(true));
            }

            assert!(matches!(&*if_stmt.then_branch, Stmt::E(_)));
            if let Stmt::E(then_branch) = &*if_stmt.then_branch {
                assert!(matches!(then_branch.expr, Expr::Li(_)));
                if let Expr::Li(literal) = &then_branch.expr {
                    assert_eq!(literal.value, TokenLiteral::NumberLit(42.0));
                }
            }

            assert!(if_stmt.else_branch.is_some());
            if let Some(else_branch) = &if_stmt.else_branch {
                assert!(matches!(&**else_branch, Stmt::E(_)));
                if let Stmt::E(else_stmt) = &**else_branch {
                    assert!(matches!(else_stmt.expr, Expr::Li(_)));
                    if let Expr::Li(literal) = &else_stmt.expr {
                        assert_eq!(literal.value, TokenLiteral::NumberLit(0.0));
                    }
                }
            }
        }
    }

    #[test]
    fn test_parse_while_statement() {
        let tokens = vec![
            create_token(While, "while", TokenLiteral::Null, 1),
            create_token(LeftParen, "(", TokenLiteral::Null, 1),
            create_token(True, "true", TokenLiteral::Bool(true), 1),
            create_token(RightParen, ")", TokenLiteral::Null, 1),
            create_token(Number, "42", TokenLiteral::NumberLit(42.0), 1),
            create_token(Semicolon, ";", TokenLiteral::Null, 1),
            create_token(Eof, "", TokenLiteral::Null, 1),
        ];

        let mut parser = Parser::new(&tokens, false);
        let result = parser.parse();

        assert!(result.is_ok());
        let stmts = result.unwrap();
        assert_eq!(stmts.len(), 1);

        assert!(matches!(&stmts[0], Stmt::W(_)));
        if let Stmt::W(while_stmt) = &stmts[0] {
            assert!(matches!(while_stmt.condition, Expr::Li(_)));
            if let Expr::Li(condition) = &while_stmt.condition {
                assert_eq!(condition.value, TokenLiteral::Bool(true));
            }

            assert!(matches!(&*while_stmt.body, Stmt::E(_)));
            if let Stmt::E(body_stmt) = &*while_stmt.body {
                assert!(matches!(body_stmt.expr, Expr::Li(_)));
                if let Expr::Li(literal) = &body_stmt.expr {
                    assert_eq!(literal.value, TokenLiteral::NumberLit(42.0));
                }
            }
        }
    }

    #[test]
    fn test_parse_block_statement() {
        let tokens = vec![
            create_token(LeftBrace, "{", TokenLiteral::Null, 1),
            create_token(Number, "42", TokenLiteral::NumberLit(42.0), 1),
            create_token(Semicolon, ";", TokenLiteral::Null, 1),
            create_token(RightBrace, "}", TokenLiteral::Null, 1),
            create_token(Eof, "", TokenLiteral::Null, 1),
        ];

        let mut parser = Parser::new(&tokens, false);
        let result = parser.parse();

        assert!(result.is_ok());
        let stmts = result.unwrap();
        assert_eq!(stmts.len(), 1);

        assert!(matches!(&stmts[0], Stmt::B(_)));
        if let Stmt::B(block_stmt) = &stmts[0] {
            assert_eq!(block_stmt.stmts.len(), 1);

            assert!(matches!(&block_stmt.stmts[0], Stmt::E(_)));
            if let Stmt::E(expr_stmt) = &block_stmt.stmts[0] {
                assert!(matches!(expr_stmt.expr, Expr::Li(_)));
                if let Expr::Li(literal) = &expr_stmt.expr {
                    assert_eq!(literal.value, TokenLiteral::NumberLit(42.0));
                }
            }
        }
    }

    #[test]
    fn test_parse_logical_expression() {
        let tokens = vec![
            create_token(True, "true", TokenLiteral::Bool(true), 1),
            create_token(Or, "or", TokenLiteral::Null, 1),
            create_token(False, "false", TokenLiteral::Bool(false), 1),
            create_token(Semicolon, ";", TokenLiteral::Null, 1),
            create_token(Eof, "", TokenLiteral::Null, 1),
        ];

        let mut parser = Parser::new(&tokens, false);
        let result = parser.parse();

        assert!(result.is_ok());
        let stmts = result.unwrap();
        assert_eq!(stmts.len(), 1);

        assert!(matches!(&stmts[0], Stmt::E(_)));
        if let Stmt::E(expr_stmt) = &stmts[0] {
            assert!(matches!(expr_stmt.expr, Expr::Lo(_)));
            if let Expr::Lo(logical_expr) = &expr_stmt.expr {
                assert!(matches!(*logical_expr.left, Expr::Li(_)));
                if let Expr::Li(left_literal) = &*logical_expr.left {
                    assert_eq!(left_literal.value, TokenLiteral::Bool(true));
                }

                assert!(matches!(*logical_expr.right, Expr::Li(_)));
                if let Expr::Li(right_literal) = &*logical_expr.right {
                    assert_eq!(right_literal.value, TokenLiteral::Bool(false));
                }
            }
        }
    }

    #[test]
    fn test_parse_binary_expression() {
        let tokens = vec![
            create_token(Number, "1", TokenLiteral::NumberLit(1.0), 1),
            create_token(Plus, "+", TokenLiteral::Null, 1),
            create_token(Number, "2", TokenLiteral::NumberLit(2.0), 1),
            create_token(Semicolon, ";", TokenLiteral::Null, 1),
            create_token(Eof, "", TokenLiteral::Null, 1),
        ];

        let mut parser = Parser::new(&tokens, false);
        let result = parser.parse();

        assert!(result.is_ok());
        let stmts = result.unwrap();
        assert_eq!(stmts.len(), 1);

        assert!(matches!(&stmts[0], Stmt::E(_)));
        if let Stmt::E(expr_stmt) = &stmts[0] {
            assert!(matches!(expr_stmt.expr, Expr::B(_)));
            if let Expr::B(binary_expr) = &expr_stmt.expr {
                assert!(matches!(*binary_expr.left, Expr::Li(_)));
                if let Expr::Li(left_literal) = &*binary_expr.left {
                    assert_eq!(left_literal.value, TokenLiteral::NumberLit(1.0));
                }

                assert!(matches!(*binary_expr.right, Expr::Li(_)));
                if let Expr::Li(right_literal) = &*binary_expr.right {
                    assert_eq!(right_literal.value, TokenLiteral::NumberLit(2.0));
                }
            }
        }
    }

    #[test]
    fn test_parse_unary_expression() {
        let tokens = vec![
            create_token(Minus, "-", TokenLiteral::Null, 1),
            create_token(Number, "42", TokenLiteral::NumberLit(42.0), 1),
            create_token(Semicolon, ";", TokenLiteral::Null, 1),
            create_token(Eof, "", TokenLiteral::Null, 1),
        ];

        let mut parser = Parser::new(&tokens, false);
        let result = parser.parse();

        assert!(result.is_ok());
        let stmts = result.unwrap();
        assert_eq!(stmts.len(), 1);

        assert!(matches!(&stmts[0], Stmt::E(_)));
        if let Stmt::E(expr_stmt) = &stmts[0] {
            assert!(matches!(expr_stmt.expr, Expr::U(_)));
            if let Expr::U(unary_expr) = &expr_stmt.expr {
                assert!(matches!(*unary_expr.expr, Expr::Li(_)));
                if let Expr::Li(literal) = &*unary_expr.expr {
                    assert_eq!(literal.value, TokenLiteral::NumberLit(42.0));
                }
            }
        }
    }

    #[test]
    fn test_parse_grouping_expression() {
        let tokens = vec![
            create_token(LeftParen, "(", TokenLiteral::Null, 1),
            create_token(Number, "42", TokenLiteral::NumberLit(42.0), 1),
            create_token(RightParen, ")", TokenLiteral::Null, 1),
            create_token(Semicolon, ";", TokenLiteral::Null, 1),
            create_token(Eof, "", TokenLiteral::Null, 1),
        ];

        let mut parser = Parser::new(&tokens, false);
        let result = parser.parse();

        assert!(result.is_ok());
        let stmts = result.unwrap();
        assert_eq!(stmts.len(), 1);

        assert!(matches!(&stmts[0], Stmt::E(_)));
        if let Stmt::E(expr_stmt) = &stmts[0] {
            assert!(matches!(expr_stmt.expr, Expr::Gr(_)));
            if let Expr::Gr(grouping_expr) = &expr_stmt.expr {
                assert!(matches!(*grouping_expr.expr, Expr::Li(_)));
                if let Expr::Li(literal) = &*grouping_expr.expr {
                    assert_eq!(literal.value, TokenLiteral::NumberLit(42.0));
                }
            }
        }
    }

    #[test]
    fn test_parse_assignment_expression() {
        let tokens = vec![
            create_token(Identifier, "x", TokenLiteral::Null, 1),
            create_token(Equal, "=", TokenLiteral::Null, 1),
            create_token(Number, "42", TokenLiteral::NumberLit(42.0), 1),
            create_token(Semicolon, ";", TokenLiteral::Null, 1),
            create_token(Eof, "", TokenLiteral::Null, 1),
        ];

        let mut parser = Parser::new(&tokens, false);
        let result = parser.parse();

        assert!(result.is_ok());
        let stmts = result.unwrap();
        assert_eq!(stmts.len(), 1);

        assert!(matches!(&stmts[0], Stmt::E(_)));
        if let Stmt::E(expr_stmt) = &stmts[0] {
            assert!(matches!(expr_stmt.expr, Expr::A(_)));
            if let Expr::A(assign_expr) = &expr_stmt.expr {
                assert_eq!(assign_expr.name.lexeme, "x");

                assert!(matches!(*assign_expr.value, Expr::Li(_)));
                if let Expr::Li(literal) = &*assign_expr.value {
                    assert_eq!(literal.value, TokenLiteral::NumberLit(42.0));
                }
            }
        }
    }

    #[test]
    fn test_parse_call_expression() {
        let tokens = vec![
            create_token(Identifier, "foo", TokenLiteral::Null, 1),
            create_token(LeftParen, "(", TokenLiteral::Null, 1),
            create_token(Number, "42", TokenLiteral::NumberLit(42.0), 1),
            create_token(RightParen, ")", TokenLiteral::Null, 1),
            create_token(Semicolon, ";", TokenLiteral::Null, 1),
            create_token(Eof, "", TokenLiteral::Null, 1),
        ];

        let mut parser = Parser::new(&tokens, false);
        let result = parser.parse();

        assert!(result.is_ok());
        let stmts = result.unwrap();
        assert_eq!(stmts.len(), 1);

        assert!(matches!(&stmts[0], Stmt::E(_)));
        if let Stmt::E(expr_stmt) = &stmts[0] {
            assert!(matches!(expr_stmt.expr, Expr::C(_)));
            if let Expr::C(call_expr) = &expr_stmt.expr {
                assert!(matches!(*call_expr.callee, Expr::V(_)));
                if let Expr::V(variable) = &*call_expr.callee {
                    assert_eq!(variable.name.lexeme, "foo");
                }

                assert_eq!(call_expr.args.len(), 1);
                assert!(matches!(&call_expr.args[0], Expr::Li(_)));
                if let Expr::Li(literal) = &call_expr.args[0] {
                    assert_eq!(literal.value, TokenLiteral::NumberLit(42.0));
                }
            }
        }
    }

    #[test]
    fn test_parse_class_declaration() {
        let tokens = vec![
            create_token(Class, "class", TokenLiteral::Null, 1),
            create_token(Identifier, "MyClass", TokenLiteral::Null, 1),
            create_token(LeftBrace, "{", TokenLiteral::Null, 1),
            create_token(Identifier, "method", TokenLiteral::Null, 1),
            create_token(LeftParen, "(", TokenLiteral::Null, 1),
            create_token(RightParen, ")", TokenLiteral::Null, 1),
            create_token(LeftBrace, "{", TokenLiteral::Null, 1),
            create_token(RightBrace, "}", TokenLiteral::Null, 1),
            create_token(RightBrace, "}", TokenLiteral::Null, 1),
            create_token(Eof, "", TokenLiteral::Null, 1),
        ];

        let mut parser = Parser::new(&tokens, false);
        let result = parser.parse();

        assert!(result.is_ok());
        let stmts = result.unwrap();
        assert_eq!(stmts.len(), 1);

        assert!(matches!(&stmts[0], Stmt::C(_)));
        if let Stmt::C(class_stmt) = &stmts[0] {
            assert_eq!(class_stmt.name.lexeme, "MyClass");
            assert!(class_stmt.superclass.is_none());
            assert_eq!(class_stmt.methods.len(), 1);

            let method = &class_stmt.methods[0];
            assert_eq!(method.name.lexeme, "method");
            assert!(method
                .params
                .clone()
                .is_some_and(|params| params.is_empty()));
            assert!(method.body.is_empty());
        }
    }

    #[test]
    fn test_parse_super_expression() {
        let tokens = vec![
            create_token(Super, "super", TokenLiteral::Null, 1),
            create_token(Dot, ".", TokenLiteral::Null, 1),
            create_token(Identifier, "method", TokenLiteral::Null, 1),
            create_token(Semicolon, ";", TokenLiteral::Null, 1),
            create_token(Eof, "", TokenLiteral::Null, 1),
        ];

        let mut parser = Parser::new(&tokens, false);
        let result = parser.parse();

        assert!(result.is_ok());
        let stmts = result.unwrap();
        assert_eq!(stmts.len(), 1);

        assert!(matches!(&stmts[0], Stmt::E(_)));
        if let Stmt::E(expr_stmt) = &stmts[0] {
            assert!(matches!(expr_stmt.expr, Expr::Su(_)));
            if let Expr::Su(super_expr) = &expr_stmt.expr {
                assert_eq!(super_expr.method.lexeme, "method");
            }
        }
    }

    #[test]
    fn test_parse_this_expression() {
        let tokens = vec![
            create_token(This, "this", TokenLiteral::Null, 1),
            create_token(Semicolon, ";", TokenLiteral::Null, 1),
            create_token(Eof, "", TokenLiteral::Null, 1),
        ];

        let mut parser = Parser::new(&tokens, false);
        let result = parser.parse();

        assert!(result.is_ok());
        let stmts = result.unwrap();
        assert_eq!(stmts.len(), 1);

        assert!(matches!(&stmts[0], Stmt::E(_)));
        if let Stmt::E(expr_stmt) = &stmts[0] {
            assert!(matches!(expr_stmt.expr, Expr::T(_)));
            if let Expr::T(this_expr) = &expr_stmt.expr {
                assert_eq!(this_expr.keyword.lexeme, "this");
            }
        }
    }

    #[test]
    fn test_parse_empty_block_statement() {
        let tokens = vec![
            create_token(LeftBrace, "{", TokenLiteral::Null, 1),
            create_token(RightBrace, "}", TokenLiteral::Null, 1),
            create_token(Eof, "", TokenLiteral::Null, 1),
        ];

        let mut parser = Parser::new(&tokens, false);
        let result = parser.parse();

        assert!(result.is_ok());
        let stmts = result.unwrap();
        assert_eq!(stmts.len(), 1);

        assert!(matches!(&stmts[0], Stmt::B(_)));
        if let Stmt::B(block_stmt) = &stmts[0] {
            assert!(block_stmt.stmts.is_empty());
        }
    }

    #[test]
    fn test_parse_nested_if_statement() {
        let tokens = vec![
            create_token(If, "if", TokenLiteral::Null, 1),
            create_token(LeftParen, "(", TokenLiteral::Null, 1),
            create_token(True, "true", TokenLiteral::Bool(true), 1),
            create_token(RightParen, ")", TokenLiteral::Null, 1),
            create_token(If, "if", TokenLiteral::Null, 1),
            create_token(LeftParen, "(", TokenLiteral::Null, 1),
            create_token(False, "false", TokenLiteral::Bool(false), 1),
            create_token(RightParen, ")", TokenLiteral::Null, 1),
            create_token(Number, "42", TokenLiteral::NumberLit(42.0), 1),
            create_token(Semicolon, ";", TokenLiteral::Null, 1),
            create_token(Eof, "", TokenLiteral::Null, 1),
        ];

        let mut parser = Parser::new(&tokens, false);
        let result = parser.parse();

        assert!(result.is_ok());
        let stmts = result.unwrap();
        assert_eq!(stmts.len(), 1);

        assert!(matches!(&stmts[0], Stmt::I(_)));
        if let Stmt::I(if_stmt) = &stmts[0] {
            assert!(matches!(if_stmt.condition, Expr::Li(_)));
            if let Expr::Li(condition) = &if_stmt.condition {
                assert_eq!(condition.value, TokenLiteral::Bool(true));
            }

            assert!(matches!(&*if_stmt.then_branch, Stmt::I(_)));
            if let Stmt::I(nested_if_stmt) = &*if_stmt.then_branch {
                assert!(matches!(nested_if_stmt.condition, Expr::Li(_)));
                if let Expr::Li(nested_condition) = &nested_if_stmt.condition {
                    assert_eq!(nested_condition.value, TokenLiteral::Bool(false));
                }

                assert!(matches!(&*nested_if_stmt.then_branch, Stmt::E(_)));
                if let Stmt::E(expr_stmt) = &*nested_if_stmt.then_branch {
                    assert!(matches!(expr_stmt.expr, Expr::Li(_)));
                    if let Expr::Li(literal) = &expr_stmt.expr {
                        assert_eq!(literal.value, TokenLiteral::NumberLit(42.0));
                    }
                }
            }
        }
    }

    #[test]
    fn test_parse_for_statement_with_empty_clauses() {
        let tokens = vec![
            create_token(For, "for", TokenLiteral::Null, 1),
            create_token(LeftParen, "(", TokenLiteral::Null, 1),
            create_token(Semicolon, ";", TokenLiteral::Null, 1),
            create_token(Semicolon, ";", TokenLiteral::Null, 1),
            create_token(RightParen, ")", TokenLiteral::Null, 1),
            create_token(Number, "42", TokenLiteral::NumberLit(42.0), 1),
            create_token(Semicolon, ";", TokenLiteral::Null, 1),
            create_token(Eof, "", TokenLiteral::Null, 1),
        ];

        let mut parser = Parser::new(&tokens, false);
        let result = parser.parse();

        assert!(result.is_ok());
        let stmts = result.unwrap();
        assert_eq!(stmts.len(), 1);

        assert!(matches!(&stmts[0], Stmt::W(_)));
        if let Stmt::W(while_stmt) = &stmts[0] {
            assert!(matches!(while_stmt.condition, Expr::Li(_)));
            if let Expr::Li(condition) = &while_stmt.condition {
                assert_eq!(condition.value, TokenLiteral::Bool(true));
            }

            assert!(matches!(&*while_stmt.body, Stmt::E(_)));
            if let Stmt::E(body_stmt) = &*while_stmt.body {
                assert!(matches!(body_stmt.expr, Expr::Li(_)));
                if let Expr::Li(literal) = &body_stmt.expr {
                    assert_eq!(literal.value, TokenLiteral::NumberLit(42.0));
                }
            }
        }
    }

    #[test]
    fn test_parse_chained_logical_expressions() {
        let tokens = vec![
            create_token(True, "true", TokenLiteral::Bool(true), 1),
            create_token(And, "and", TokenLiteral::Null, 1),
            create_token(False, "false", TokenLiteral::Bool(false), 1),
            create_token(Or, "or", TokenLiteral::Null, 1),
            create_token(True, "true", TokenLiteral::Bool(true), 1),
            create_token(Semicolon, ";", TokenLiteral::Null, 1),
            create_token(Eof, "", TokenLiteral::Null, 1),
        ];

        let mut parser = Parser::new(&tokens, false);
        let result = parser.parse();

        assert!(result.is_ok());
        let stmts = result.unwrap();
        assert_eq!(stmts.len(), 1);

        assert!(matches!(&stmts[0], Stmt::E(_)));
        if let Stmt::E(expr_stmt) = &stmts[0] {
            assert!(matches!(expr_stmt.expr, Expr::Lo(_)));
            if let Expr::Lo(logical_expr) = &expr_stmt.expr {
                assert!(matches!(*logical_expr.left, Expr::Lo(_)));
                if let Expr::Lo(left_expr) = &*logical_expr.left {
                    assert!(matches!(*left_expr.left, Expr::Li(_)));
                    if let Expr::Li(left_literal) = &*left_expr.left {
                        assert_eq!(left_literal.value, TokenLiteral::Bool(true));
                    }

                    assert!(matches!(*left_expr.right, Expr::Li(_)));
                    if let Expr::Li(right_literal) = &*left_expr.right {
                        assert_eq!(right_literal.value, TokenLiteral::Bool(false));
                    }
                }

                assert!(matches!(*logical_expr.right, Expr::Li(_)));
                if let Expr::Li(right_literal) = &*logical_expr.right {
                    assert_eq!(right_literal.value, TokenLiteral::Bool(true));
                }
            }
        }
    }

    #[test]
    fn test_parse_invalid_expression() {
        let tokens = vec![
            create_token(Plus, "+", TokenLiteral::Null, 1),
            create_token(Semicolon, ";", TokenLiteral::Null, 1),
            create_token(Eof, "", TokenLiteral::Null, 1),
        ];

        let mut parser = Parser::new(&tokens, false);
        let result = parser.parse();

        assert!(result.is_err());
    }

    #[test]
    fn test_parse_missing_semicolon() {
        let tokens = vec![
            create_token(Number, "42", TokenLiteral::NumberLit(42.0), 1),
            create_token(Eof, "", TokenLiteral::Null, 1),
        ];

        let mut parser = Parser::new(&tokens, false);
        let result = parser.parse();

        assert!(result.is_err());
    }

    #[test]
    fn test_parse_missing_parentheses_in_function_call() {
        let tokens = vec![
            create_token(Identifier, "foo", TokenLiteral::Null, 1),
            create_token(Number, "42", TokenLiteral::NumberLit(42.0), 1),
            create_token(Semicolon, ";", TokenLiteral::Null, 1),
            create_token(Eof, "", TokenLiteral::Null, 1),
        ];

        let mut parser = Parser::new(&tokens, false);
        let result = parser.parse();

        assert!(result.is_err());
    }

    #[test]
    fn test_parse_missing_braces_in_class_declaration() {
        let tokens = vec![
            create_token(Class, "class", TokenLiteral::Null, 1),
            create_token(Identifier, "MyClass", TokenLiteral::Null, 1),
            create_token(Eof, "", TokenLiteral::Null, 1),
        ];

        let mut parser = Parser::new(&tokens, false);
        let result = parser.parse();

        assert!(result.is_err());
    }

    #[test]
    fn test_parse_missing_right_parenthesis_in_if_statement() {
        let tokens = vec![
            create_token(If, "if", TokenLiteral::Null, 1),
            create_token(LeftParen, "(", TokenLiteral::Null, 1),
            create_token(True, "true", TokenLiteral::Bool(true), 1),
            create_token(Number, "42", TokenLiteral::NumberLit(42.0), 1),
            create_token(Semicolon, ";", TokenLiteral::Null, 1),
            create_token(Eof, "", TokenLiteral::Null, 1),
        ];

        let mut parser = Parser::new(&tokens, false);
        let result = parser.parse();

        assert!(result.is_err());
    }

    #[test]
    fn test_parse_missing_super_method_name() {
        let tokens = vec![
            create_token(Super, "super", TokenLiteral::Null, 1),
            create_token(Dot, ".", TokenLiteral::Null, 1),
            create_token(Semicolon, ";", TokenLiteral::Null, 1),
            create_token(Eof, "", TokenLiteral::Null, 1),
        ];

        let mut parser = Parser::new(&tokens, false);
        let result = parser.parse();

        assert!(result.is_err());
    }

    #[test]
    fn test_parse_missing_variable_name_in_declaration() {
        let tokens = vec![
            create_token(Var, "var", TokenLiteral::Null, 1),
            create_token(Equal, "=", TokenLiteral::Null, 1),
            create_token(Number, "42", TokenLiteral::NumberLit(42.0), 1),
            create_token(Semicolon, ";", TokenLiteral::Null, 1),
            create_token(Eof, "", TokenLiteral::Null, 1),
        ];

        let mut parser = Parser::new(&tokens, false);
        let result = parser.parse();

        assert!(result.is_err());
    }

    #[test]
    fn test_parse_missing_condition_in_while_statement() {
        let tokens = vec![
            create_token(While, "while", TokenLiteral::Null, 1),
            create_token(LeftParen, "(", TokenLiteral::Null, 1),
            create_token(RightParen, ")", TokenLiteral::Null, 1),
            create_token(Number, "42", TokenLiteral::NumberLit(42.0), 1),
            create_token(Semicolon, ";", TokenLiteral::Null, 1),
            create_token(Eof, "", TokenLiteral::Null, 1),
        ];

        let mut parser = Parser::new(&tokens, false);
        let result = parser.parse();

        assert!(result.is_err());
    }

    #[test]
    fn test_parse_break_statement_in_loop() {
        let tokens = vec![
            create_token(While, "while", TokenLiteral::Null, 1),
            create_token(LeftParen, "(", TokenLiteral::Null, 1),
            create_token(True, "true", TokenLiteral::Bool(true), 1),
            create_token(RightParen, ")", TokenLiteral::Null, 1),
            create_token(LeftBrace, "{", TokenLiteral::Null, 1),
            create_token(Break, "break", TokenLiteral::Null, 1),
            create_token(Semicolon, ";", TokenLiteral::Null, 1),
            create_token(RightBrace, "}", TokenLiteral::Null, 1),
            create_token(Eof, "", TokenLiteral::Null, 1),
        ];

        let mut parser = Parser::new(&tokens, false);
        let result = parser.parse();

        assert!(result.is_ok());
        let stmts = result.unwrap();
        assert_eq!(stmts.len(), 1);

        assert!(matches!(&stmts[0], Stmt::W(_)));
        if let Stmt::W(while_stmt) = &stmts[0] {
            assert!(matches!(&*while_stmt.body, Stmt::B(_)));
            if let Stmt::B(block_stmt) = &*while_stmt.body {
                assert_eq!(block_stmt.stmts.len(), 1);
                assert!(matches!(&block_stmt.stmts[0], Stmt::Br(_)));
            }
        }
    }

    #[test]
    fn test_parse_break_statement_outside_loop() {
        let tokens = vec![
            create_token(Break, "break", TokenLiteral::Null, 1),
            create_token(Semicolon, ";", TokenLiteral::Null, 1),
            create_token(Eof, "", TokenLiteral::Null, 1),
        ];

        let mut parser = Parser::new(&tokens, false);
        let result = parser.parse();

        assert!(result.is_err());
    }

    #[test]
    fn test_parse_class_with_getter_method() {
        let tokens = vec![
            create_token(Class, "class", TokenLiteral::Null, 1),
            create_token(Identifier, "MyClass", TokenLiteral::Null, 1),
            create_token(LeftBrace, "{", TokenLiteral::Null, 1),
            create_token(Identifier, "getValue", TokenLiteral::Null, 1),
            create_token(LeftBrace, "{", TokenLiteral::Null, 1),
            create_token(Return, "return", TokenLiteral::Null, 1),
            create_token(Number, "42", TokenLiteral::NumberLit(42.0), 1),
            create_token(Semicolon, ";", TokenLiteral::Null, 1),
            create_token(RightBrace, "}", TokenLiteral::Null, 1),
            create_token(RightBrace, "}", TokenLiteral::Null, 1),
            create_token(Eof, "", TokenLiteral::Null, 1),
        ];

        let mut parser = Parser::new(&tokens, false);
        let result = parser.parse();

        assert!(result.is_ok());
        let stmts = result.unwrap();
        assert_eq!(stmts.len(), 1);

        assert!(matches!(&stmts[0], Stmt::C(_)));
        if let Stmt::C(class_stmt) = &stmts[0] {
            assert_eq!(class_stmt.name.lexeme, "MyClass");
            assert!(class_stmt.superclass.is_none());
            assert_eq!(class_stmt.methods.len(), 1);

            let method = &class_stmt.methods[0];
            assert_eq!(method.name.lexeme, "getValue");
            assert!(method.params.is_none());
            assert_eq!(method.body.len(), 1);

            if let Stmt::R(return_stmt) = &method.body[0] {
                assert!(matches!(return_stmt.value, Some(Expr::Li(_))));
                if let Some(Expr::Li(literal)) = &return_stmt.value {
                    assert_eq!(literal.value, TokenLiteral::NumberLit(42.0));
                }
            }
        }
    }

    #[test]
    fn test_parse_class_with_static_method() {
        let tokens = vec![
            create_token(Class, "class", TokenLiteral::Null, 1),
            create_token(Identifier, "MyClass", TokenLiteral::Null, 1),
            create_token(LeftBrace, "{", TokenLiteral::Null, 1),
            create_token(Class, "class", TokenLiteral::Null, 1),
            create_token(Identifier, "staticMethod", TokenLiteral::Null, 1),
            create_token(LeftParen, "(", TokenLiteral::Null, 1),
            create_token(RightParen, ")", TokenLiteral::Null, 1),
            create_token(LeftBrace, "{", TokenLiteral::Null, 1),
            create_token(Return, "return", TokenLiteral::Null, 1),
            create_token(Number, "42", TokenLiteral::NumberLit(42.0), 1),
            create_token(Semicolon, ";", TokenLiteral::Null, 1),
            create_token(RightBrace, "}", TokenLiteral::Null, 1),
            create_token(RightBrace, "}", TokenLiteral::Null, 1),
            create_token(Eof, "", TokenLiteral::Null, 1),
        ];

        let mut parser = Parser::new(&tokens, false);
        let result = parser.parse();

        assert!(result.is_ok());
        let stmts = result.unwrap();
        assert_eq!(stmts.len(), 1);

        assert!(matches!(&stmts[0], Stmt::C(_)));
        if let Stmt::C(class_stmt) = &stmts[0] {
            assert_eq!(class_stmt.name.lexeme, "MyClass");
            assert!(class_stmt.superclass.is_none());
            assert_eq!(class_stmt.class_methods.len(), 1);

            let static_method = &class_stmt.class_methods[0];
            assert_eq!(static_method.name.lexeme, "staticMethod");
            assert!(static_method
                .params
                .clone()
                .is_some_and(|params| params.is_empty()));
            assert_eq!(static_method.body.len(), 1);

            if let Stmt::R(return_stmt) = &static_method.body[0] {
                assert!(matches!(return_stmt.value, Some(Expr::Li(_))));
                if let Some(Expr::Li(literal)) = &return_stmt.value {
                    assert_eq!(literal.value, TokenLiteral::NumberLit(42.0));
                }
            }
        }
    }

    #[test]
    fn test_parse_list_expression() {
        let tokens = vec![
            create_token(LeftBracket, "[", TokenLiteral::Null, 1),
            create_token(Number, "1", TokenLiteral::NumberLit(1.0), 1),
            create_token(Comma, ",", TokenLiteral::Null, 1),
            create_token(Number, "2", TokenLiteral::NumberLit(2.0), 1),
            create_token(Comma, ",", TokenLiteral::Null, 1),
            create_token(Number, "3", TokenLiteral::NumberLit(3.0), 1),
            create_token(RightBracket, "]", TokenLiteral::Null, 1),
            create_token(Semicolon, ";", TokenLiteral::Null, 1),
            create_token(Eof, "", TokenLiteral::Null, 1),
        ];

        let mut parser = Parser::new(&tokens, false);
        let result = parser.parse();

        assert!(result.is_ok());
        let stmts = result.unwrap();
        assert_eq!(stmts.len(), 1);

        assert!(matches!(&stmts[0], Stmt::E(_)));
        if let Stmt::E(expr_stmt) = &stmts[0] {
            assert!(matches!(expr_stmt.expr, Expr::L(_)));
            if let Expr::L(list_expr) = &expr_stmt.expr {
                assert_eq!(list_expr.elems.len(), 3);

                assert!(matches!(&list_expr.elems[0], Expr::Li(_)));
                if let Expr::Li(literal) = &list_expr.elems[0] {
                    assert_eq!(literal.value, TokenLiteral::NumberLit(1.0));
                }

                assert!(matches!(&list_expr.elems[1], Expr::Li(_)));
                if let Expr::Li(literal) = &list_expr.elems[1] {
                    assert_eq!(literal.value, TokenLiteral::NumberLit(2.0));
                }

                assert!(matches!(&list_expr.elems[2], Expr::Li(_)));
                if let Expr::Li(literal) = &list_expr.elems[2] {
                    assert_eq!(literal.value, TokenLiteral::NumberLit(3.0));
                }
            }
        }
    }

    #[test]
    fn test_parse_list_get_expression() {
        let tokens = vec![
            create_token(Identifier, "myList", TokenLiteral::Null, 1),
            create_token(LeftBracket, "[", TokenLiteral::Null, 1),
            create_token(Number, "1", TokenLiteral::NumberLit(1.0), 1),
            create_token(RightBracket, "]", TokenLiteral::Null, 1),
            create_token(Semicolon, ";", TokenLiteral::Null, 1),
            create_token(Eof, "", TokenLiteral::Null, 1),
        ];

        let mut parser = Parser::new(&tokens, false);
        let result = parser.parse();

        assert!(result.is_ok());
        let stmts = result.unwrap();
        assert_eq!(stmts.len(), 1);

        assert!(matches!(&stmts[0], Stmt::E(_)));
        if let Stmt::E(expr_stmt) = &stmts[0] {
            assert!(matches!(expr_stmt.expr, Expr::LG(_)));
            if let Expr::LG(list_get_expr) = &expr_stmt.expr {
                assert!(matches!(*list_get_expr.object, Expr::V(_)));
                if let Expr::V(variable) = &*list_get_expr.object {
                    assert_eq!(variable.name.lexeme, "myList");
                }

                assert!(matches!(*list_get_expr.index, Expr::Li(_)));
                if let Expr::Li(index_literal) = &*list_get_expr.index {
                    assert_eq!(index_literal.value, TokenLiteral::NumberLit(1.0));
                }
            }
        }
    }

    #[test]
    fn test_parse_list_set_expression() {
        let tokens = vec![
            create_token(Identifier, "myList", TokenLiteral::Null, 1),
            create_token(LeftBracket, "[", TokenLiteral::Null, 1),
            create_token(Number, "1", TokenLiteral::NumberLit(1.0), 1),
            create_token(RightBracket, "]", TokenLiteral::Null, 1),
            create_token(Equal, "=", TokenLiteral::Null, 1),
            create_token(Number, "42", TokenLiteral::NumberLit(42.0), 1),
            create_token(Semicolon, ";", TokenLiteral::Null, 1),
            create_token(Eof, "", TokenLiteral::Null, 1),
        ];

        let mut parser = Parser::new(&tokens, false);
        let result = parser.parse();

        assert!(result.is_ok());
        let stmts = result.unwrap();
        assert_eq!(stmts.len(), 1);

        assert!(matches!(&stmts[0], Stmt::E(_)));
        if let Stmt::E(expr_stmt) = &stmts[0] {
            assert!(matches!(expr_stmt.expr, Expr::LS(_)));
            if let Expr::LS(list_set_expr) = &expr_stmt.expr {
                assert!(matches!(*list_set_expr.object, Expr::V(_)));
                if let Expr::V(variable) = &*list_set_expr.object {
                    assert_eq!(variable.name.lexeme, "myList");
                }

                assert!(matches!(*list_set_expr.index, Expr::Li(_)));
                if let Expr::Li(index_literal) = &*list_set_expr.index {
                    assert_eq!(index_literal.value, TokenLiteral::NumberLit(1.0));
                }

                assert!(matches!(*list_set_expr.value, Expr::Li(_)));
                if let Expr::Li(value_literal) = &*list_set_expr.value {
                    assert_eq!(value_literal.value, TokenLiteral::NumberLit(42.0));
                }
            }
        }
    }

    #[test]
    fn test_parse_mod_binary_expression() {
        let tokens = vec![
            create_token(Number, "10", TokenLiteral::NumberLit(10.0), 1),
            create_token(Mod, "%", TokenLiteral::Null, 1),
            create_token(Number, "3", TokenLiteral::NumberLit(3.0), 1),
            create_token(Semicolon, ";", TokenLiteral::Null, 1),
            create_token(Eof, "", TokenLiteral::Null, 1),
        ];

        let mut parser = Parser::new(&tokens, false);
        let result = parser.parse();

        assert!(result.is_ok());
        let stmts = result.unwrap();
        assert_eq!(stmts.len(), 1);

        assert!(matches!(&stmts[0], Stmt::E(_)));
        if let Stmt::E(expr_stmt) = &stmts[0] {
            assert!(matches!(expr_stmt.expr, Expr::B(_)));
            if let Expr::B(binary_expr) = &expr_stmt.expr {
                assert!(matches!(*binary_expr.left, Expr::Li(_)));
                if let Expr::Li(left_literal) = &*binary_expr.left {
                    assert_eq!(left_literal.value, TokenLiteral::NumberLit(10.0));
                }

                assert!(matches!(*binary_expr.right, Expr::Li(_)));
                if let Expr::Li(right_literal) = &*binary_expr.right {
                    assert_eq!(right_literal.value, TokenLiteral::NumberLit(3.0));
                }
            }
        }
    }
}
