use crate::{
    expr::{Expr, VariableE},
    token::TokenRef,
};

#[derive(Debug, Clone)]
pub enum Stmt {
    B(BlockS),
    Br(BreakS),
    C(ClassS),
    E(ExprS),
    F(FunctionS),
    I(IfS),
    P(PrintS),
    R(ReturnS),
    V(VarS),
    W(WhileS),
}

#[derive(Debug, Clone)]
pub struct BlockS {
    pub stmts: Vec<Stmt>,
}

#[derive(Debug, Clone)]
pub struct BreakS {
    pub keyword: TokenRef,
}

#[derive(Debug, Clone)]
pub struct ClassS {
    pub name: TokenRef,
    pub superclass: Option<VariableE>,
    pub methods: Vec<FunctionS>,
}

#[derive(Debug, Clone)]
pub struct ExprS {
    pub expr: Expr,
}

#[derive(Debug, Clone)]
pub struct FunctionS {
    pub name: TokenRef,
    pub params: Vec<TokenRef>,
    pub body: Vec<Stmt>,
}

#[derive(Debug, Clone)]
pub struct IfS {
    pub condition: Expr,
    pub then_branch: Box<Stmt>,
    pub else_branch: Option<Box<Stmt>>,
}

#[derive(Debug, Clone)]
pub struct PrintS {
    pub expr: Expr,
}

#[derive(Debug, Clone)]
pub struct ReturnS {
    pub keyword: TokenRef,
    pub value: Option<Expr>,
}

#[derive(Debug, Clone)]
pub struct VarS {
    pub name: TokenRef,
    pub initializer: Option<Expr>,
}

#[derive(Debug, Clone)]
pub struct WhileS {
    pub condition: Expr,
    pub body: Box<Stmt>,
}

pub trait Visitor<T> {
    fn visit_stmt(&mut self, stmt: &Stmt) -> T;
}

impl Stmt {
    pub fn accept<T>(&self, visitor: &mut dyn Visitor<T>) -> T {
        visitor.visit_stmt(self)
    }
}
