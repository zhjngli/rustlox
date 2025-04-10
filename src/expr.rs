use std::{
    hash::Hash,
    sync::atomic::{AtomicUsize, Ordering},
};

use crate::token::{Token, TokenLiteral};

static ID: AtomicUsize = AtomicUsize::new(0);

#[derive(Debug, Clone)]
pub struct Expr {
    uid: usize,
    kind: ExprKind,
}

#[derive(Debug, Clone)]
pub enum ExprKind {
    Literal {
        value: TokenLiteral,
    },
    Logical {
        left: Box<Expr>,
        op: Token,
        right: Box<Expr>,
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
    Call {
        callee: Box<Expr>,
        paren: Token,
        args: Vec<Expr>,
    },
    Variable {
        name: Token,
    },
    Assign {
        name: Token,
        value: Box<Expr>,
    },
}

pub trait Visitor<T> {
    fn visit_expr(&mut self, expr: &Expr) -> T;
}

impl Expr {
    pub fn new(kind: ExprKind) -> Self {
        Expr {
            uid: ID.fetch_add(1, Ordering::Relaxed),
            kind,
        }
    }

    pub fn accept<T>(&self, visitor: &mut dyn Visitor<T>) -> T {
        visitor.visit_expr(self)
    }

    pub fn kind(&self) -> &ExprKind {
        &self.kind
    }
}

impl PartialEq for Expr {
    fn eq(&self, other: &Self) -> bool {
        self.uid == other.uid
    }
}

impl Eq for Expr {}

impl Hash for Expr {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.uid.hash(state);
    }
}
