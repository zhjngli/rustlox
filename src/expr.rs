use std::{
    hash::Hash,
    sync::atomic::{AtomicUsize, Ordering},
};

use crate::token::{TokenLiteral, TokenRef};

static ID: AtomicUsize = AtomicUsize::new(0);

#[derive(Debug, Clone)]
pub struct Expr {
    uid: usize,
    kind: ExprKind,
}

#[derive(Debug, Clone)]
pub enum ExprKind {
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

#[derive(Debug, Clone)]
pub struct AssignE {
    pub name: TokenRef,
    pub value: Box<Expr>,
}

#[derive(Debug, Clone)]
pub struct BinaryE {
    pub left: Box<Expr>,
    pub op: TokenRef,
    pub right: Box<Expr>,
}

#[derive(Debug, Clone)]
pub struct CallE {
    pub callee: Box<Expr>,
    pub paren: TokenRef,
    pub args: Vec<Expr>,
}

#[derive(Debug, Clone)]
pub struct GetE {
    pub object: Box<Expr>,
    pub name: TokenRef,
}

#[derive(Debug, Clone)]
pub struct GroupingE {
    pub expr: Box<Expr>,
}

#[derive(Debug, Clone)]
pub struct LiteralE {
    pub value: TokenLiteral,
}

#[derive(Debug, Clone)]
pub struct LogicalE {
    pub left: Box<Expr>,
    pub op: TokenRef,
    pub right: Box<Expr>,
}

#[derive(Debug, Clone)]
pub struct SetE {
    pub object: Box<Expr>,
    pub name: TokenRef,
    pub value: Box<Expr>,
}

#[derive(Debug, Clone)]
pub struct SuperE {
    pub keyword: TokenRef,
    pub method: TokenRef,
}

#[derive(Debug, Clone)]
pub struct ThisE {
    pub keyword: TokenRef,
}

#[derive(Debug, Clone)]
pub struct UnaryE {
    pub op: TokenRef,
    pub expr: Box<Expr>,
}

#[derive(Debug, Clone)]
pub struct VariableE {
    pub name: TokenRef,
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
