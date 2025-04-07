use std::{
    cell::RefCell,
    collections::HashMap,
    rc::Rc,
    time::{SystemTime, UNIX_EPOCH},
};

use crate::{
    environment::Environment,
    expr::{Expr, ExprKind as E, Visitor as EVisitor},
    lox::{
        Callable, Exits, Function, LoxCallable,
        LoxValue::{self, Bool, CallableVal, Null, Number, String},
        NativeFunction,
    },
    stmt::{Stmt, Visitor as SVisitor},
    token::{Token, TokenLiteral, TokenType},
};

#[derive(Debug)]
pub struct Interpreter {
    environment: Rc<RefCell<Environment>>,
    pub globals: Rc<RefCell<Environment>>,
    locals: HashMap<Expr, usize>,
}

impl EVisitor<Result<LoxValue, Exits>> for Interpreter {
    fn visit_expr(&mut self, expr: &Expr) -> Result<LoxValue, Exits> {
        match expr.kind() {
            E::Literal { value } => match value {
                TokenLiteral::NumberLit(n) => Ok(Number(*n)),
                TokenLiteral::Bool(b) => Ok(Bool(*b)),
                TokenLiteral::StringLit(s) => Ok(String(s.to_owned())),
                TokenLiteral::Null => Ok(Null),
            },
            E::Logical { left, op, right } => {
                let left = self.evaluate(left)?;
                match op.token_type {
                    TokenType::Or => {
                        if self.is_truthy(&left) {
                            return Ok(left);
                        }
                    }
                    _ => {
                        if !self.is_truthy(&left) {
                            return Ok(left);
                        }
                    }
                }
                self.evaluate(right)
            }
            E::Grouping { expr } => self.evaluate(expr),
            E::Unary { op, expr } => {
                let right = self.evaluate(expr)?;
                match op.token_type {
                    TokenType::Bang => return Ok(Bool(!self.is_truthy(&right))),
                    TokenType::Minus => match right {
                        Number(r) => return Ok(Number(-r)),
                        _ => Err(Exits::RuntimeError(
                            op.clone(),
                            format!("Operand of ({:?}) must be a number.", op).to_owned(),
                        )),
                    },
                    _ => Ok(Null),
                }
            }
            E::Binary { left, op, right } => {
                let left_val = self.evaluate(left)?;
                let right_val = self.evaluate(right)?;
                match op.token_type {
                    // comparison
                    TokenType::Greater => match (left_val, right_val) {
                        (Number(l), Number(r)) => Ok(Bool(l > r)),
                        _ => Err(Exits::RuntimeError(
                            op.clone(),
                            format!("Operands of ({:?}) must be numbers", op).to_owned(),
                        )),
                    },
                    TokenType::GreaterEqual => match (left_val, right_val) {
                        (Number(l), Number(r)) => Ok(Bool(l >= r)),
                        _ => Err(Exits::RuntimeError(
                            op.clone(),
                            format!("Operands of ({:?}) must be numbers", op).to_owned(),
                        )),
                    },
                    TokenType::Less => match (left_val, right_val) {
                        (Number(l), Number(r)) => Ok(Bool(l < r)),
                        _ => Err(Exits::RuntimeError(
                            op.clone(),
                            format!("Operands of ({:?}) must be numbers", op).to_owned(),
                        )),
                    },
                    TokenType::LessEqual => match (left_val, right_val) {
                        (Number(l), Number(r)) => Ok(Bool(l <= r)),
                        _ => Err(Exits::RuntimeError(
                            op.clone(),
                            format!("Operands of ({:?}) must be numbers", op).to_owned(),
                        )),
                    },
                    TokenType::BangEqual => return Ok(Bool(!self.is_equal(left_val, right_val))),
                    TokenType::EqualEqual => return Ok(Bool(self.is_equal(left_val, right_val))),
                    // arithmetic
                    TokenType::Minus => match (left_val, right_val) {
                        (Number(l), Number(r)) => Ok(Number(l - r)),
                        _ => Err(Exits::RuntimeError(
                            op.clone(),
                            format!("Operands of ({:?}) must be numbers", op).to_owned(),
                        )),
                    },
                    TokenType::Plus => match (left_val, right_val) {
                        (Number(l), Number(r)) => Ok(Number(l + r)),
                        (String(l), String(r)) => {
                            let mut s = l;
                            s.push_str(&r);
                            Ok(String(s))
                        }
                        _ => Err(Exits::RuntimeError(
                            op.clone(),
                            format!("Operands of ({:?}) must be numbers or strings", op).to_owned(),
                        )),
                    },
                    TokenType::Slash => match (left_val, right_val) {
                        (Number(l), Number(r)) => Ok(Number(l / r)),
                        _ => Err(Exits::RuntimeError(
                            op.clone(),
                            format!("Operands of ({:?}) must be numbers", op).to_owned(),
                        )),
                    },
                    TokenType::Star => match (left_val, right_val) {
                        (Number(l), Number(r)) => Ok(Number(l * r)),
                        _ => Err(Exits::RuntimeError(
                            op.clone(),
                            format!("Operands of ({:?}) must be numbers", op).to_owned(),
                        )),
                    },
                    _ => Ok(Null),
                }
            }
            E::Call {
                callee,
                paren,
                args,
            } => {
                let callee = self.evaluate(callee)?;
                let args_vals: Vec<LoxValue> = args
                    .into_iter()
                    .map(|a| self.evaluate(a))
                    .collect::<Result<Vec<LoxValue>, Exits>>()?;

                match callee {
                    CallableVal(callable) => {
                        if args_vals.len() != callable.arity() {
                            return Err(Exits::RuntimeError(
                                paren.clone(),
                                format!(
                                    "Expected {} arguments but got {}.",
                                    callable.arity(),
                                    args_vals.len()
                                ),
                            ));
                        }
                        callable.call(self, args_vals)
                    }
                    _ => Err(Exits::RuntimeError(
                        paren.clone(),
                        "Can only call functions and classes".to_owned(),
                    )),
                }
            }
            E::Variable { name } => self.lookup_var(name, expr),
            E::Assign { name, value } => {
                let val = self.evaluate(value)?;
                match self.locals.get(expr) {
                    Some(distance) => Environment::ancestor(Rc::clone(&self.environment), *distance).borrow_mut().assign(name, &val)?,
                    None => self.globals.borrow_mut().assign(name, &val)?,
                }
                Ok(val)
            }
        }
    }
}

impl SVisitor<Result<(), Exits>> for Interpreter {
    fn visit_stmt(&mut self, stmt: &Stmt) -> Result<(), Exits> {
        match stmt {
            Stmt::Expr { expr } => {
                self.evaluate(expr)?;
                Ok(())
            }
            Stmt::Function {
                name,
                params: _,
                body: _,
            } => {
                let function = Function::new(stmt.clone(), Rc::clone(&self.environment));
                self.environment.borrow_mut().define(
                    name.lexeme.clone(),
                    CallableVal(Callable::Function(function)),
                );
                Ok(())
            }
            Stmt::If {
                condition,
                then_branch,
                else_branch,
            } => {
                let cond = self.evaluate(condition)?;
                if self.is_truthy(&cond) {
                    self.execute(&then_branch)
                } else {
                    match else_branch {
                        Some(e) => self.execute(e),
                        None => Ok(()),
                    }
                }
            }
            Stmt::Print { expr } => {
                println!("{}", self.evaluate(expr)?);
                Ok(())
            }
            Stmt::Return { keyword: _, value } => match value {
                Some(v) => Err(Exits::Return(self.evaluate(v)?)),
                None => Err(Exits::Return(Null)),
            },
            Stmt::Var { name, initializer } => {
                let value;
                match initializer {
                    Some(e) => value = self.evaluate(e)?,
                    None => value = Null,
                }
                self.environment
                    .borrow_mut()
                    .define(name.lexeme.clone(), value);
                Ok(())
            }
            Stmt::While { condition, body } => {
                let mut cond = self.evaluate(condition)?;
                while self.is_truthy(&cond) {
                    self.execute(body)?;
                    cond = self.evaluate(condition)?;
                }
                Ok(())
            }
            Stmt::Block { stmts } => {
                self.execute_block(stmts, Environment::enclosed(Rc::clone(&self.environment)))
            }
        }
    }
}

impl Interpreter {
    pub fn new() -> Self {
        let globals = Rc::new(RefCell::new(Environment::new()));
        globals.borrow_mut().define(
            "clock".to_owned(),
            CallableVal(Callable::from(NativeFunction {
                name: "clock".to_owned(),
                arity: 0,
                call: Box::new(|_, _| {
                    let now = SystemTime::now();
                    let duration = now
                        .duration_since(UNIX_EPOCH)
                        .expect("Time went backwards.");
                    let seconds = duration.as_secs() as f64;
                    Ok(Number(seconds))
                }),
            })),
        );
        Interpreter {
            environment: globals.clone(),
            globals: globals,
            locals: HashMap::new(),
        }
    }

    pub fn interpret(&mut self, stmts: &Vec<Stmt>) -> Result<(), Exits> {
        for stmt in stmts {
            self.execute(stmt)?;
        }
        Ok(())
    }

    fn execute(&mut self, stmt: &Stmt) -> Result<(), Exits> {
        stmt.accept(self)
    }

    pub fn resolve(&mut self, expr: Expr, depth: usize) {
        self.locals.insert(expr, depth);
    }

    pub fn execute_block(&mut self, stmts: &Vec<Stmt>, env: Environment) -> Result<(), Exits> {
        let previous = Rc::clone(&self.environment);
        self.environment = Rc::new(RefCell::new(env));
        let result = stmts.iter().try_for_each(|s| self.execute(s));
        self.environment = previous;
        result
    }

    fn lookup_var(&self, name: &Token, expr: &Expr) -> Result<LoxValue, Exits> {
        match self.locals.get(expr) {
            Some(distance) => Environment::ancestor(Rc::clone(&self.environment), *distance).borrow().get(name),
            None => self.globals.borrow().get(name),
        }
    }

    fn evaluate(&mut self, expr: &Expr) -> Result<LoxValue, Exits> {
        expr.accept(self)
    }

    fn is_truthy(&self, lv: &LoxValue) -> bool {
        match lv {
            Bool(b) => *b,
            Null => false,
            _ => true,
        }
    }

    fn is_equal(&self, lv: LoxValue, rv: LoxValue) -> bool {
        match (lv, rv) {
            (Bool(l), Bool(r)) => l == r,
            (Number(l), Number(r)) => l == r,
            (String(l), String(r)) => l == r,
            (Null, Null) => true,
            _ => false,
        }
    }
}

impl Default for Interpreter {
    fn default() -> Self {
        Self::new()
    }
}
