use crate::token::{Token, TokenLiteral};

#[derive(Debug, Clone)]
pub enum Expr {
    Literal {
        value: TokenLiteral,
    },
    Grouping {
        expr: Box<Expr>,
    },
    Unary {
        op: Token,
        expr: Box<Expr>,
    },
    Binary {
        left: Box<Expr>,
        op: Token,
        right: Box<Expr>,
    },
}

pub trait Visitor<T> {
    fn visit_expr(&mut self, expr: &Expr) -> T;
}

impl Expr {
    pub fn accept<T>(&self, visitor: &mut dyn Visitor<T>) -> T {
        visitor.visit_expr(self)
    }
}
