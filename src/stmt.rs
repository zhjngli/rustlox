use crate::expr::Expr;

#[derive(Debug, Clone)]
pub enum Stmt {
    Expr { expr: Expr },
    Print { expr: Expr },
}
pub trait Visitor<T> {
    fn visit_stmt(&mut self, stmt: &Stmt) -> T;
}

impl Stmt {
    pub fn accept<T>(&self, visitor: &mut dyn Visitor<T>) -> T {
        visitor.visit_stmt(self)
    }
}
