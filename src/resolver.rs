use std::collections::HashMap;

use crate::{
    expr::{Expr, ExprKind as E, Visitor as EVisitor},
    interpreter::Interpreter,
    parser::{parse_error, ParseError},
    stmt::{Stmt, Visitor as SVisitor},
    token::Token,
};

#[derive(Debug)]
pub struct Resolver<'a> {
    interpreter: &'a mut Interpreter,
    scopes: Vec<HashMap<String, bool>>,
    current_function: FunctionType,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum FunctionType {
    None,
    Function,
}

impl<'a> Resolver<'a> {
    pub fn new(interpreter: &'a mut Interpreter) -> Self {
        Resolver {
            interpreter,
            scopes: Vec::new(),
            current_function: FunctionType::None,
        }
    }

    pub fn resolve(&mut self, stmts: &Vec<Stmt>) -> Result<(), ParseError> {
        stmts.iter().try_for_each(|s| self.resolve_statement(s))
    }

    fn resolve_function(
        &mut self,
        function: &Stmt,
        function_type: FunctionType,
    ) -> Result<(), ParseError> {
        match function {
            Stmt::Function {
                name: _,
                params,
                body,
            } => {
                let enclosing_function = self.current_function;
                self.current_function = function_type;

                self.begin_scope();
                for p in params {
                    self.declare(p)?;
                    self.define(p);
                }
                self.resolve(body)?;
                self.end_scope();

                self.current_function = enclosing_function;
                Ok(())
            }
            _ => panic!("Resolve function called on non-function statement!"),
        }
    }

    fn resolve_statement(&mut self, stmt: &Stmt) -> Result<(), ParseError> {
        stmt.accept(self)
    }

    fn resolve_expression(&mut self, expr: &Expr) -> Result<(), ParseError> {
        expr.accept(self)
    }

    fn begin_scope(&mut self) {
        self.scopes.push(HashMap::new());
    }

    fn end_scope(&mut self) {
        self.scopes.pop();
    }

    fn declare(&mut self, name: &Token) -> Result<(), ParseError> {
        if let Some(scope) = self.scopes.last_mut() {
            if scope.contains_key(&name.lexeme) {
                return Err(parse_error(
                    name,
                    "Already a variable with this name in this scope.",
                ));
            }
            scope.insert(name.lexeme.clone(), false);
        }
        Ok(())
    }

    fn define(&mut self, name: &Token) {
        if let Some(scope) = self.scopes.last_mut() {
            scope.insert(name.lexeme.clone(), true);
        }
    }

    // TODO does this need to return a result? always returns Ok(())
    fn resolve_local(&mut self, expr: &Expr, name: &Token) -> Result<(), ParseError> {
        for (i, s) in self.scopes.iter().enumerate().rev() {
            if s.contains_key(&name.lexeme) {
                self.interpreter
                    .resolve(expr.clone(), self.scopes.len() - 1 - i);
                return Ok(());
            }
        }
        Ok(())
    }
}

impl<'a> EVisitor<Result<(), ParseError>> for Resolver<'a> {
    fn visit_expr(&mut self, expr: &Expr) -> Result<(), ParseError> {
        match expr.kind() {
            E::Assign { name, value } => {
                self.resolve_expression(value)?;
                self.resolve_local(expr, name)?;
                Ok(())
            }
            E::Binary { left, op: _, right } => {
                self.resolve_expression(left)?;
                self.resolve_expression(right)?;
                Ok(())
            }
            E::Call {
                callee,
                paren: _,
                args,
            } => {
                self.resolve_expression(callee)?;
                args.iter().try_for_each(|a| self.resolve_expression(a))?;
                Ok(())
            }
            E::Grouping { expr } => self.resolve_expression(expr),
            E::Literal { value: _ } => Ok(()),
            E::Logical { left, op: _, right } => {
                self.resolve_expression(left)?;
                self.resolve_expression(right)?;
                Ok(())
            }
            E::Unary { op: _, expr } => self.resolve_expression(expr),
            E::Variable { name } => {
                if let Some(scope) = self.scopes.last() {
                    if scope.get(&name.lexeme) == Some(&false) {
                        return Err(parse_error(
                            name,
                            "Can't read local variable in its own initializer.",
                        ));
                    }
                }
                self.resolve_local(expr, name)?;
                Ok(())
            }
        }
    }
}

impl<'a> SVisitor<Result<(), ParseError>> for Resolver<'a> {
    fn visit_stmt(&mut self, stmt: &Stmt) -> Result<(), ParseError> {
        match stmt {
            Stmt::Block { stmts } => {
                self.begin_scope();
                self.resolve(stmts)?;
                self.end_scope();
                Ok(())
            }
            Stmt::Expr { expr } => self.resolve_expression(expr),
            Stmt::Function {
                name,
                params: _,
                body: _,
            } => {
                self.declare(name)?;
                self.define(name);

                self.resolve_function(stmt, FunctionType::Function)?;
                Ok(())
            }
            Stmt::If {
                condition,
                then_branch,
                else_branch,
            } => {
                self.resolve_expression(condition)?;
                self.resolve_statement(then_branch)?;
                if let Some(else_stmt) = else_branch {
                    self.resolve_statement(else_stmt)?;
                }
                Ok(())
            }
            Stmt::Print { expr } => self.resolve_expression(expr),
            Stmt::Return { keyword, value } => {
                if self.current_function == FunctionType::None {
                    return Err(parse_error(keyword, "Can't return from top-level code."));
                }

                if let Some(expr) = value {
                    self.resolve_expression(expr)?;
                }
                Ok(())
            }
            Stmt::Var { name, initializer } => {
                self.declare(name)?;
                if let Some(init) = initializer {
                    self.resolve_expression(init)?;
                }
                self.define(name);
                Ok(())
            }
            Stmt::While { condition, body } => {
                self.resolve_expression(condition)?;
                self.resolve_statement(body)?;
                Ok(())
            }
        }
    }
}
