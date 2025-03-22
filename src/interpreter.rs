use crate::{
    expr::{Expr, Visitor},
    lox::{
        Exits,
        LoxValue::{self, Bool, Null, Number, String},
    },
    token::{TokenLiteral, TokenType},
};

#[derive(Debug)]
pub struct Interpreter {}

impl Visitor<Result<LoxValue, Exits>> for Interpreter {
    fn visit_expr(&mut self, expr: &Expr) -> Result<LoxValue, Exits> {
        match expr {
            Expr::Literal { value } => match value {
                TokenLiteral::NumberLit(n) => Ok(Number(*n)),
                TokenLiteral::Bool(b) => Ok(Bool(*b)),
                TokenLiteral::StringLit(s) => Ok(String(s.to_owned())),
                TokenLiteral::Null => Ok(Null),
            },
            Expr::Grouping { expr } => self.evaluate(expr),
            Expr::Unary { op, expr } => {
                let right = self.evaluate(expr)?;
                match op.token_type {
                    TokenType::Bang => return Ok(Bool(!self.is_truthy(right))),
                    TokenType::Minus => match right {
                        Number(r) => return Ok(Number(-r)),
                        _ => panic!("minus error"),
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
        }
    }
}

impl Interpreter {
    pub fn new() -> Self {
        Interpreter {}
    }

    pub fn interpret(&mut self, expr: &Expr) -> Result<(), Exits> {
        let value = self.evaluate(expr)?;
        println!("{:?}", value);
        Ok(())
    }

    fn evaluate(&mut self, expr: &Expr) -> Result<LoxValue, Exits> {
        expr.accept(self)
    }

    fn is_truthy(&self, lv: LoxValue) -> bool {
        match lv {
            LoxValue::Bool(b) => b,
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
