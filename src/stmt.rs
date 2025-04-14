use crate::{expr::Expr, token::TokenRef};

// TODO: Stmt and StmtKind? simplifies some statement pattern matching in function resolver
#[derive(Debug, Clone)]
pub enum Stmt {
    Block {
        stmts: Vec<Stmt>,
    },
    Class {
        name: TokenRef,
        superclass: Option<Expr>, // Must be ExprKind::Variable
        methods: Vec<Stmt>,       // Must be Stmt::Function
    },
    Expr {
        expr: Expr,
    },
    Function {
        name: TokenRef,
        params: Vec<TokenRef>,
        body: Vec<Stmt>,
    },
    If {
        condition: Expr,
        then_branch: Box<Stmt>,
        else_branch: Option<Box<Stmt>>,
    },
    Print {
        expr: Expr,
    },
    Return {
        keyword: TokenRef,
        value: Option<Expr>,
    },
    Var {
        name: TokenRef,
        initializer: Option<Expr>,
    },
    While {
        condition: Expr,
        body: Box<Stmt>,
    },
}
pub trait Visitor<T> {
    fn visit_stmt(&mut self, stmt: &Stmt) -> T;
}

impl Stmt {
    pub fn accept<T>(&self, visitor: &mut dyn Visitor<T>) -> T {
        visitor.visit_stmt(self)
    }
}
