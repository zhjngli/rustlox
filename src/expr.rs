use std::{
    hash::Hash,
    sync::atomic::{AtomicUsize, Ordering},
};

use crate::token::{TokenLiteral, TokenRef};

static ID: AtomicUsize = AtomicUsize::new(0);

#[derive(Debug, Clone)]
pub enum Expr {
    A(AssignE),
    B(BinaryE),
    C(CallE),
    G(GetE),
    Gr(GroupingE),
    Li(LiteralE),
    Lo(LogicalE),
    S(SetE),
    Su(SuperE),
    T(ThisE),
    U(UnaryE),
    V(VariableE),
}

pub trait Visitor<T> {
    fn visit_expr(&mut self, expr: &Expr) -> T;
}

impl Expr {
    pub fn accept<T>(&self, visitor: &mut dyn Visitor<T>) -> T {
        visitor.visit_expr(self)
    }

    fn uid(&self) -> usize {
        match self {
            Expr::A(e) => e.uid,
            Expr::B(e) => e.uid,
            Expr::C(e) => e.uid,
            Expr::G(e) => e.uid,
            Expr::Gr(e) => e.uid,
            Expr::Li(e) => e.uid,
            Expr::Lo(e) => e.uid,
            Expr::S(e) => e.uid,
            Expr::Su(e) => e.uid,
            Expr::T(e) => e.uid,
            Expr::U(e) => e.uid,
            Expr::V(e) => e.uid,
        }
    }
}

impl PartialEq for Expr {
    fn eq(&self, other: &Self) -> bool {
        self.uid() == other.uid()
    }
}

impl Eq for Expr {}

impl Hash for Expr {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.uid().hash(state);
    }
}

#[derive(Debug, Clone)]
pub struct AssignE {
    uid: usize,
    pub name: TokenRef,
    pub value: Box<Expr>,
}

impl AssignE {
    pub fn new(name: TokenRef, value: Expr) -> Self {
        AssignE {
            uid: ID.fetch_add(1, Ordering::Relaxed),
            name,
            value: Box::new(value),
        }
    }
}

#[derive(Debug, Clone)]
pub struct BinaryE {
    uid: usize,
    pub left: Box<Expr>,
    pub op: TokenRef,
    pub right: Box<Expr>,
}

impl BinaryE {
    pub fn new(left: Expr, op: TokenRef, right: Expr) -> Self {
        BinaryE {
            uid: ID.fetch_add(1, Ordering::Relaxed),
            left: Box::new(left),
            op,
            right: Box::new(right),
        }
    }
}

#[derive(Debug, Clone)]
pub struct CallE {
    uid: usize,
    pub callee: Box<Expr>,
    pub paren: TokenRef,
    pub args: Vec<Expr>,
}

impl CallE {
    pub fn new(callee: Expr, paren: TokenRef, args: Vec<Expr>) -> Self {
        CallE {
            uid: ID.fetch_add(1, Ordering::Relaxed),
            callee: Box::new(callee),
            paren,
            args,
        }
    }
}

#[derive(Debug, Clone)]
pub struct GetE {
    uid: usize,
    pub object: Box<Expr>,
    pub name: TokenRef,
}

impl GetE {
    pub fn new(object: Expr, name: TokenRef) -> Self {
        GetE {
            uid: ID.fetch_add(1, Ordering::Relaxed),
            object: Box::new(object),
            name,
        }
    }
}

#[derive(Debug, Clone)]
pub struct GroupingE {
    uid: usize,
    pub expr: Box<Expr>,
}

impl GroupingE {
    pub fn new(expr: Expr) -> Self {
        GroupingE {
            uid: ID.fetch_add(1, Ordering::Relaxed),
            expr: Box::new(expr),
        }
    }
}

#[derive(Debug, Clone)]
pub struct LiteralE {
    uid: usize,
    pub value: TokenLiteral,
}

impl LiteralE {
    pub fn new(value: TokenLiteral) -> Self {
        LiteralE {
            uid: ID.fetch_add(1, Ordering::Relaxed),
            value,
        }
    }
}

#[derive(Debug, Clone)]
pub struct LogicalE {
    uid: usize,
    pub left: Box<Expr>,
    pub op: TokenRef,
    pub right: Box<Expr>,
}

impl LogicalE {
    pub fn new(left: Expr, op: TokenRef, right: Expr) -> Self {
        LogicalE {
            uid: ID.fetch_add(1, Ordering::Relaxed),
            left: Box::new(left),
            op,
            right: Box::new(right),
        }
    }
}

#[derive(Debug, Clone)]
pub struct SetE {
    uid: usize,
    pub object: Box<Expr>,
    pub name: TokenRef,
    pub value: Box<Expr>,
}

impl SetE {
    pub fn new(object: Expr, name: TokenRef, value: Expr) -> Self {
        SetE {
            uid: ID.fetch_add(1, Ordering::Relaxed),
            object: Box::new(object),
            name,
            value: Box::new(value),
        }
    }
}

#[derive(Debug, Clone)]
pub struct SuperE {
    uid: usize,
    pub keyword: TokenRef,
    pub method: TokenRef,
}

impl SuperE {
    pub fn new(keyword: TokenRef, method: TokenRef) -> Self {
        SuperE {
            uid: ID.fetch_add(1, Ordering::Relaxed),
            keyword,
            method,
        }
    }
}

#[derive(Debug, Clone)]
pub struct ThisE {
    uid: usize,
    pub keyword: TokenRef,
}

impl ThisE {
    pub fn new(keyword: TokenRef) -> Self {
        ThisE {
            uid: ID.fetch_add(1, Ordering::Relaxed),
            keyword,
        }
    }
}

#[derive(Debug, Clone)]
pub struct UnaryE {
    uid: usize,
    pub op: TokenRef,
    pub expr: Box<Expr>,
}

impl UnaryE {
    pub fn new(op: TokenRef, expr: Expr) -> Self {
        UnaryE {
            uid: ID.fetch_add(1, Ordering::Relaxed),
            op,
            expr: Box::new(expr),
        }
    }
}

#[derive(Debug, Clone)]
pub struct VariableE {
    uid: usize,
    pub name: TokenRef,
}

impl VariableE {
    pub fn new(name: TokenRef) -> Self {
        VariableE {
            uid: ID.fetch_add(1, Ordering::Relaxed),
            name,
        }
    }
}
