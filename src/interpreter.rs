use std::{cell::RefCell, rc::Rc};

use crate::{
    environment::Environment,
    expr::{Expr, Visitor as EVisitor},
    lox::{
        Exits,
        LoxValue::{self, Bool, Null, Number, String},
    },
    stmt::{Stmt, Visitor as SVisitor},
    token::{TokenLiteral, TokenType},
};

#[derive(Debug)]
pub struct Interpreter {
    environment: Rc<RefCell<Environment>>,
}

impl EVisitor<Result<LoxValue, Exits>> for Interpreter {
    fn visit_expr(&mut self, expr: &Expr) -> Result<LoxValue, Exits> {
        match expr {
            Expr::Literal { value } => match value {
                TokenLiteral::NumberLit(n) => Ok(Number(*n)),
                TokenLiteral::Bool(b) => Ok(Bool(*b)),
                TokenLiteral::StringLit(s) => Ok(String(s.to_owned())),
                TokenLiteral::Null => Ok(Null),
            },
            Expr::Logical { left, op, right } => {
                let left = self.evaluate(left)?;
                match op.token_type {
                    TokenType::Or => {
                        if self.is_truthy(&left) {
                            return Ok(left);
                        }
                    }
                    _ => {
                        if !self.is_truthy(&left) {
                            return Ok(left);
                        }
                    }
                }
                self.evaluate(right)
            }
            Expr::Grouping { expr } => self.evaluate(expr),
            Expr::Unary { op, expr } => {
                let right = self.evaluate(expr)?;
                match op.token_type {
                    TokenType::Bang => return Ok(Bool(!self.is_truthy(&right))),
                    TokenType::Minus => match right {
                        Number(r) => return Ok(Number(-r)),
                        _ => Err(Exits::RuntimeError(
                            op.clone(),
                            format!("Operand of ({:?}) must be a number.", op).to_owned(),
                        )),
                    },
                    _ => Ok(Null),
                }
            }
            Expr::Binary { left, op, right } => {
                let left_val = self.evaluate(left)?;
                let right_val = self.evaluate(right)?;
                match op.token_type {
                    // comparison
                    TokenType::Greater => match (left_val, right_val) {
                        (Number(l), Number(r)) => Ok(Bool(l > r)),
                        _ => Err(Exits::RuntimeError(
                            op.clone(),
                            format!("Operands of ({:?}) must be numbers", op).to_owned(),
                        )),
                    },
                    TokenType::GreaterEqual => match (left_val, right_val) {
                        (Number(l), Number(r)) => Ok(Bool(l >= r)),
                        _ => Err(Exits::RuntimeError(
                            op.clone(),
                            format!("Operands of ({:?}) must be numbers", op).to_owned(),
                        )),
                    },
                    TokenType::Less => match (left_val, right_val) {
                        (Number(l), Number(r)) => Ok(Bool(l < r)),
                        _ => Err(Exits::RuntimeError(
                            op.clone(),
                            format!("Operands of ({:?}) must be numbers", op).to_owned(),
                        )),
                    },
                    TokenType::LessEqual => match (left_val, right_val) {
                        (Number(l), Number(r)) => Ok(Bool(l <= r)),
                        _ => Err(Exits::RuntimeError(
                            op.clone(),
                            format!("Operands of ({:?}) must be numbers", op).to_owned(),
                        )),
                    },
                    TokenType::BangEqual => return Ok(Bool(!self.is_equal(left_val, right_val))),
                    TokenType::EqualEqual => return Ok(Bool(self.is_equal(left_val, right_val))),
                    // arithmetic
                    TokenType::Minus => match (left_val, right_val) {
                        (Number(l), Number(r)) => Ok(Number(l - r)),
                        _ => Err(Exits::RuntimeError(
                            op.clone(),
                            format!("Operands of ({:?}) must be numbers", op).to_owned(),
                        )),
                    },
                    TokenType::Plus => match (left_val, right_val) {
                        (Number(l), Number(r)) => Ok(Number(l + r)),
                        (String(l), String(r)) => {
                            let mut s = l;
                            s.push_str(&r);
                            Ok(String(s))
                        }
                        _ => Err(Exits::RuntimeError(
                            op.clone(),
                            format!("Operands of ({:?}) must be numbers or strings", op).to_owned(),
                        )),
                    },
                    TokenType::Slash => match (left_val, right_val) {
                        (Number(l), Number(r)) => Ok(Number(l / r)),
                        _ => Err(Exits::RuntimeError(
                            op.clone(),
                            format!("Operands of ({:?}) must be numbers", op).to_owned(),
                        )),
                    },
                    TokenType::Star => match (left_val, right_val) {
                        (Number(l), Number(r)) => Ok(Number(l * r)),
                        _ => Err(Exits::RuntimeError(
                            op.clone(),
                            format!("Operands of ({:?}) must be numbers", op).to_owned(),
                        )),
                    },
                    _ => Ok(Null),
                }
            }
            Expr::Variable { name } => self.environment.borrow().get(name.clone()),
            Expr::Assign { name, value } => {
                let val = self.evaluate(value)?;
                self.environment
                    .borrow_mut()
                    .assign(name.clone(), val.clone())?;
                Ok(val)
            }
        }
    }
}

impl SVisitor<Result<(), Exits>> for Interpreter {
    fn visit_stmt(&mut self, stmt: &Stmt) -> Result<(), Exits> {
        match stmt {
            Stmt::Expr { expr } => {
                self.evaluate(expr)?;
                Ok(())
            }
            Stmt::If {
                condition,
                then_branch,
                else_branch,
            } => {
                let cond = self.evaluate(condition)?;
                if self.is_truthy(&cond) {
                    self.execute(&then_branch)
                } else {
                    match else_branch {
                        Some(e) => self.execute(e),
                        None => Ok(()),
                    }
                }
            }
            Stmt::Print { expr } => {
                println!("{}", self.evaluate(expr)?);
                Ok(())
            }
            Stmt::Var { name, initializer } => {
                let value;
                match initializer {
                    Some(e) => value = self.evaluate(e)?,
                    None => value = LoxValue::Null,
                }
                self.environment
                    .borrow_mut()
                    .define(name.lexeme.clone(), value);
                Ok(())
            }
            Stmt::While { condition, body } => {
                let mut cond = self.evaluate(condition)?;
                while self.is_truthy(&cond) {
                    self.execute(body)?;
                    cond = self.evaluate(condition)?;
                }
                Ok(())
            }
            Stmt::Block { stmts } => {
                self.execute_block(stmts, Environment::enclosed(self.environment.clone()))
            }
        }
    }
}

impl Interpreter {
    pub fn new() -> Self {
        Interpreter {
            environment: Rc::new(RefCell::new(Environment::new())),
        }
    }

    pub fn interpret(&mut self, stmts: &Vec<Stmt>) -> Result<(), Exits> {
        for stmt in stmts {
            self.execute(stmt)?;
        }
        Ok(())
    }

    fn execute(&mut self, stmt: &Stmt) -> Result<(), Exits> {
        stmt.accept(self)
    }

    fn execute_block(&mut self, stmts: &Vec<Stmt>, env: Environment) -> Result<(), Exits> {
        let previous = Rc::clone(&self.environment);
        self.environment = Rc::new(RefCell::new(env));
        let result = stmts.iter().try_for_each(|s| self.execute(s));
        self.environment = previous;
        result
    }

    fn evaluate(&mut self, expr: &Expr) -> Result<LoxValue, Exits> {
        expr.accept(self)
    }

    fn is_truthy(&self, lv: &LoxValue) -> bool {
        match lv {
            LoxValue::Bool(b) => *b,
            LoxValue::Null => false,
            _ => true,
        }
    }

    fn is_equal(&self, lv: LoxValue, rv: LoxValue) -> bool {
        match (lv, rv) {
            (Bool(l), Bool(r)) => l == r,
            (Number(l), Number(r)) => l == r,
            (String(l), String(r)) => l == r,
            (Null, Null) => true,
            _ => false,
        }
    }
}
