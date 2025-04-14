use crate::{expr::Expr, token::Token};

// TODO: Stmt and StmtKind? simplifies some statement pattern matching in function resolver
#[derive(Debug, Clone)]
pub enum Stmt {
    Block {
        stmts: Vec<Stmt>,
    },
    Class {
        name: Token,
        superclass: Option<Expr>, // Must be ExprKind::Variable
        methods: Vec<Stmt>,       // Must be Stmt::Function
    },
    Expr {
        expr: Expr,
    },
    Function {
        name: Token,
        params: Vec<Token>,
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
        keyword: Token,
        value: Option<Expr>,
    },
    Var {
        name: Token,
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
