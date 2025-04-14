use std::collections::HashMap;

use crate::{
    expr::{Expr, ExprKind as E, Visitor as EVisitor},
    interpreter::Interpreter,
    stmt::{Stmt, Visitor as SVisitor},
    token::TokenRef,
};

#[derive(Debug)]
pub struct Resolver<'a> {
    interpreter: &'a mut Interpreter,
    scopes: Vec<HashMap<String, bool>>,
    current_function: FunctionType,
    current_class: ClassType,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum FunctionType {
    None,
    Function,
    Initializer,
    Method,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum ClassType {
    None,
    Class,
    Subclass,
}

#[derive(Debug)]
pub struct StaticError;

fn static_error(token: &TokenRef, message: &str) -> StaticError {
    crate::report_token_error(token, message);
    StaticError {}
}

impl<'a> Resolver<'a> {
    pub fn new(interpreter: &'a mut Interpreter) -> Self {
        Resolver {
            interpreter,
            scopes: Vec::new(),
            current_function: FunctionType::None,
            current_class: ClassType::None,
        }
    }

    pub fn resolve(&mut self, stmts: &Vec<Stmt>) -> Result<(), StaticError> {
        stmts.iter().try_for_each(|s| self.resolve_statement(s))
    }

    fn resolve_function(
        &mut self,
        function: &Stmt,
        function_type: FunctionType,
    ) -> Result<(), StaticError> {
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
            _ => panic!(
                "Resolve function called on non-function statement: {:?}",
                function
            ),
        }
    }

    fn resolve_statement(&mut self, stmt: &Stmt) -> Result<(), StaticError> {
        stmt.accept(self)
    }

    fn resolve_expression(&mut self, expr: &Expr) -> Result<(), StaticError> {
        expr.accept(self)
    }

    fn begin_scope(&mut self) {
        self.scopes.push(HashMap::new());
    }

    fn end_scope(&mut self) {
        self.scopes.pop();
    }

    fn declare(&mut self, name: &TokenRef) -> Result<(), StaticError> {
        if let Some(scope) = self.scopes.last_mut() {
            if scope.contains_key(&name.lexeme) {
                return Err(static_error(
                    name,
                    "Already a variable with this name in this scope.",
                ));
            }
            scope.insert(name.lexeme.clone(), false);
        }
        Ok(())
    }

    fn define(&mut self, name: &TokenRef) {
        if let Some(scope) = self.scopes.last_mut() {
            scope.insert(name.lexeme.clone(), true);
        }
    }

    fn resolve_local(&mut self, expr: &Expr, name: &TokenRef) -> Result<(), StaticError> {
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

impl<'a> EVisitor<Result<(), StaticError>> for Resolver<'a> {
    fn visit_expr(&mut self, expr: &Expr) -> Result<(), StaticError> {
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
            E::Get { object, name: _ } => self.resolve_expression(object),
            E::Grouping { expr } => self.resolve_expression(expr),
            E::Literal { value: _ } => Ok(()),
            E::Logical { left, op: _, right } => {
                self.resolve_expression(left)?;
                self.resolve_expression(right)?;
                Ok(())
            }
            E::Set {
                object,
                name: _,
                value,
            } => {
                self.resolve_expression(value)?;
                self.resolve_expression(object)?;
                Ok(())
            }
            E::Super { keyword, method: _ } => {
                if self.current_class == ClassType::None {
                    return Err(static_error(
                        keyword,
                        "Can't use 'super' outside of a class.",
                    ));
                } else if self.current_class != ClassType::Subclass {
                    return Err(static_error(
                        keyword,
                        "Can't use 'super' in a class with no superclass.",
                    ));
                }
                self.resolve_local(expr, keyword)
            }
            E::This { keyword } => match self.current_class {
                ClassType::None => Err(static_error(
                    keyword,
                    "Can't use 'this' outside of a class.",
                )),
                _ => self.resolve_local(expr, keyword),
            },
            E::Unary { op: _, expr } => self.resolve_expression(expr),
            E::Variable { name } => {
                if let Some(scope) = self.scopes.last() {
                    if scope.get(&name.lexeme) == Some(&false) {
                        return Err(static_error(
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

impl<'a> SVisitor<Result<(), StaticError>> for Resolver<'a> {
    fn visit_stmt(&mut self, stmt: &Stmt) -> Result<(), StaticError> {
        match stmt {
            Stmt::Block { stmts } => {
                self.begin_scope();
                self.resolve(stmts)?;
                self.end_scope();
                Ok(())
            }
            Stmt::Class {
                name,
                superclass,
                methods,
            } => {
                let enclosing_class = self.current_class;
                self.current_class = ClassType::Class;

                self.declare(name)?;
                self.define(name);

                if let Some(s) = superclass {
                    self.current_class = ClassType::Subclass;
                    match s.kind() {
                        E::Variable {
                            name: superclass_name,
                        } => {
                            if superclass_name.lexeme == name.lexeme {
                                return Err(static_error(
                                    superclass_name,
                                    "A class can't inherit from itself.",
                                ));
                            }
                        }
                        _ => panic!("Superclass must be a variable expr: {:?}", s),
                    }
                    self.resolve_expression(s)?;

                    self.begin_scope();
                    if let Some(s) = self.scopes.last_mut() {
                        s.insert("super".to_owned(), true);
                    }
                }

                self.begin_scope();
                if let Some(s) = self.scopes.last_mut() {
                    s.insert("this".to_owned(), true);
                }
                methods.iter().try_for_each(|m| {
                    self.resolve_function(
                        m,
                        if match m {
                            Stmt::Function { name, .. } => name.lexeme == "init",
                            _ => panic!("Class method cannot be a non-function statement: {:?}", m),
                        } {
                            FunctionType::Initializer
                        } else {
                            FunctionType::Method
                        },
                    )
                })?;
                self.end_scope();

                if let Some(_) = superclass {
                    self.end_scope();
                }

                self.current_class = enclosing_class;
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
                    return Err(static_error(keyword, "Can't return from top-level code."));
                }
                if let Some(expr) = value {
                    if self.current_function == FunctionType::Initializer {
                        return Err(static_error(
                            keyword,
                            "Can't return a value from an initializer.",
                        ));
                    }
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
