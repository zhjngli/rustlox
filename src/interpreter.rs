use std::{
    cell::RefCell,
    collections::HashMap,
    rc::Rc,
    time::{SystemTime, UNIX_EPOCH},
};

use crate::{
    environment::Environment,
    expr::{
        AssignE, BinaryE, CallE, Expr, GetE, GroupingE, LiteralE, LogicalE, SetE, SuperE, ThisE,
        UnaryE, VariableE, Visitor as EVisitor,
    },
    lox::{
        Callable, InterpreterResult as IR, LoxCallable, LoxClass, LoxFunction,
        LoxValue::{self, Bool, CallVal, ClassInstance, Null, Number, String},
        NativeFunction,
    },
    stmt::{BlockS, ClassS, ExprS, IfS, PrintS, ReturnS, Stmt, VarS, Visitor as SVisitor, WhileS},
    token::{TokenLiteral, TokenRef, TokenType},
};

#[derive(Debug)]
pub struct Interpreter {
    environment: Rc<RefCell<Environment>>,
    pub globals: Rc<RefCell<Environment>>,
    locals: HashMap<Expr, usize>,
}

impl Interpreter {
    pub fn new() -> Self {
        let globals = Rc::new(RefCell::new(Environment::new()));
        globals.borrow_mut().define(
            "clock".to_owned(),
            CallVal(LoxCallable::from(NativeFunction {
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

    pub fn interpret(&mut self, stmts: &Vec<Stmt>) -> Result<(), IR> {
        for stmt in stmts {
            self.execute(stmt)?;
        }
        Ok(())
    }

    fn execute(&mut self, stmt: &Stmt) -> Result<(), IR> {
        stmt.accept(self)
    }

    pub fn resolve(&mut self, expr: Expr, depth: usize) {
        self.locals.insert(expr, depth);
    }

    pub fn execute_block(&mut self, stmts: &Vec<Stmt>, env: Environment) -> Result<(), IR> {
        let previous = Rc::clone(&self.environment);
        self.environment = Rc::new(RefCell::new(env));
        let result = stmts.iter().try_for_each(|s| self.execute(s));
        self.environment = previous;
        result
    }

    fn lookup_var(&self, name: &TokenRef, expr: &Expr) -> Result<LoxValue, IR> {
        match self.locals.get(expr) {
            Some(distance) => Environment::ancestor(Rc::clone(&self.environment), *distance)
                .borrow()
                .get(name),
            None => self.globals.borrow().get(name),
        }
    }

    fn evaluate(&mut self, expr: &Expr) -> Result<LoxValue, IR> {
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

impl EVisitor<Result<LoxValue, IR>> for Interpreter {
    fn visit_expr(&mut self, expr: &Expr) -> Result<LoxValue, IR> {
        match expr {
            Expr::A(AssignE { name, value, .. }) => {
                let val = self.evaluate(value)?;
                match self.locals.get(expr) {
                    Some(distance) => {
                        Environment::ancestor(Rc::clone(&self.environment), *distance)
                            .borrow_mut()
                            .assign(name, &val)?
                    }
                    None => self.globals.borrow_mut().assign(name, &val)?,
                }
                Ok(val)
            }
            Expr::B(BinaryE {
                left, op, right, ..
            }) => {
                let left_val = self.evaluate(left)?;
                let right_val = self.evaluate(right)?;
                match op.token_type {
                    // comparison
                    TokenType::Greater => match (left_val, right_val) {
                        (Number(l), Number(r)) => Ok(Bool(l > r)),
                        _ => Err(IR::RuntimeError(
                            op.clone(),
                            format!("Operands of ({}) must be numbers", op.lexeme).to_owned(),
                        )),
                    },
                    TokenType::GreaterEqual => match (left_val, right_val) {
                        (Number(l), Number(r)) => Ok(Bool(l >= r)),
                        _ => Err(IR::RuntimeError(
                            op.clone(),
                            format!("Operands of ({}) must be numbers", op.lexeme).to_owned(),
                        )),
                    },
                    TokenType::Less => match (left_val, right_val) {
                        (Number(l), Number(r)) => Ok(Bool(l < r)),
                        _ => Err(IR::RuntimeError(
                            op.clone(),
                            format!("Operands of ({}) must be numbers", op.lexeme).to_owned(),
                        )),
                    },
                    TokenType::LessEqual => match (left_val, right_val) {
                        (Number(l), Number(r)) => Ok(Bool(l <= r)),
                        _ => Err(IR::RuntimeError(
                            op.clone(),
                            format!("Operands of ({}) must be numbers", op.lexeme).to_owned(),
                        )),
                    },
                    TokenType::BangEqual => return Ok(Bool(!self.is_equal(left_val, right_val))),
                    TokenType::EqualEqual => return Ok(Bool(self.is_equal(left_val, right_val))),
                    // arithmetic
                    TokenType::Minus => match (left_val, right_val) {
                        (Number(l), Number(r)) => Ok(Number(l - r)),
                        _ => Err(IR::RuntimeError(
                            op.clone(),
                            format!("Operands of ({}) must be numbers", op.lexeme).to_owned(),
                        )),
                    },
                    TokenType::Plus => match (left_val, right_val) {
                        (Number(l), Number(r)) => Ok(Number(l + r)),
                        (String(l), v) => {
                            let mut s = l;
                            s.push_str(&v.to_string());
                            Ok(String(s))
                        }
                        _ => Err(IR::RuntimeError(
                            op.clone(),
                            format!("Operands of ({}) must be numbers or strings", op.lexeme)
                                .to_owned(),
                        )),
                    },
                    TokenType::Slash => match (left_val, right_val) {
                        (Number(_), Number(0.)) => {
                            Err(IR::RuntimeError(op.clone(), "Division by zero".to_owned()))
                        }
                        (Number(l), Number(r)) => Ok(Number(l / r)),
                        _ => Err(IR::RuntimeError(
                            op.clone(),
                            format!("Operands of ({}) must be numbers", op.lexeme).to_owned(),
                        )),
                    },
                    TokenType::Star => match (left_val, right_val) {
                        (Number(l), Number(r)) => Ok(Number(l * r)),
                        _ => Err(IR::RuntimeError(
                            op.clone(),
                            format!("Operands of ({}) must be numbers", op.lexeme).to_owned(),
                        )),
                    },
                    _ => Ok(Null),
                }
            }
            Expr::C(CallE {
                callee,
                paren,
                args,
                ..
            }) => {
                let callee = self.evaluate(callee)?;
                let args_vals: Vec<LoxValue> = args
                    .into_iter()
                    .map(|a| self.evaluate(a))
                    .collect::<Result<Vec<LoxValue>, IR>>()?;

                match callee {
                    CallVal(callable) => {
                        if args_vals.len() != callable.arity() {
                            return Err(IR::RuntimeError(
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
                    _ => Err(IR::RuntimeError(
                        paren.clone(),
                        "Can only call functions and classes".to_owned(),
                    )),
                }
            }
            Expr::G(GetE { object, name, .. }) => {
                let object = self.evaluate(object)?;
                match object {
                    ClassInstance(i) => i.borrow().get(name),
                    _ => Err(IR::RuntimeError(
                        name.clone(),
                        "Only instances have properties.".to_owned(),
                    )),
                }
            }
            Expr::Gr(GroupingE { expr, .. }) => self.evaluate(expr),
            Expr::Li(LiteralE { value, .. }) => match value {
                TokenLiteral::NumberLit(n) => Ok(Number(*n)),
                TokenLiteral::Bool(b) => Ok(Bool(*b)),
                TokenLiteral::StringLit(s) => Ok(String(s.to_owned())),
                TokenLiteral::Null => Ok(Null),
            },
            Expr::Lo(LogicalE {
                left, op, right, ..
            }) => {
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
            Expr::S(SetE {
                object,
                name,
                value,
                ..
            }) => {
                let instance = self.evaluate(object)?;
                match instance {
                    ClassInstance(i) => {
                        let value = self.evaluate(value)?;
                        i.borrow_mut().set(name, &value);
                        Ok(value)
                    }
                    _ => Err(IR::RuntimeError(
                        name.clone(),
                        "Only instances have fields.".to_owned(),
                    )),
                }
            }
            Expr::Su(SuperE {
                keyword, method, ..
            }) => {
                let distance = match self.locals.get(expr) {
                    Some(d) => d,
                    None => panic!(
                        "Couldn't look up {:?} expr distance when calling super method",
                        expr
                    ),
                };
                let superclass =
                    match Environment::ancestor(Rc::clone(&self.environment), *distance)
                        .borrow()
                        .get(keyword)?
                    {
                        CallVal(LoxCallable::LoxClass(c)) => c,
                        _ => panic!("Did not find class from 'super': {:?}", keyword),
                    };
                let object = match Environment::get_this(&self.environment, distance - 1, keyword)?
                {
                    ClassInstance(i) => i,
                    _ => panic!("Did not find class instance from 'this': {:?}", keyword),
                };
                let method = match superclass.find_method(&method.lexeme) {
                    Some(m) => m,
                    None => {
                        return Err(IR::RuntimeError(
                            method.clone(),
                            format!("Undefined property '{}'.", method.lexeme),
                        ))
                    }
                };
                Ok(CallVal(LoxCallable::LoxFunction(method.bind(object))))
            }
            Expr::T(ThisE { keyword, .. }) => self.lookup_var(keyword, expr),
            Expr::U(UnaryE { op, expr, .. }) => {
                let right = self.evaluate(expr)?;
                match op.token_type {
                    TokenType::Bang => return Ok(Bool(!self.is_truthy(&right))),
                    TokenType::Minus => match right {
                        Number(r) => return Ok(Number(-r)),
                        _ => Err(IR::RuntimeError(
                            op.clone(),
                            format!("Operand of ({}) must be a number.", op.lexeme).to_owned(),
                        )),
                    },
                    _ => Ok(Null),
                }
            }
            Expr::V(VariableE { name, .. }) => self.lookup_var(name, expr),
        }
    }
}

impl SVisitor<Result<(), IR>> for Interpreter {
    fn visit_stmt(&mut self, stmt: &Stmt) -> Result<(), IR> {
        match stmt {
            Stmt::B(BlockS { stmts }) => {
                self.execute_block(stmts, Environment::enclosed(Rc::clone(&self.environment)))
            }
            Stmt::C(ClassS {
                name,
                superclass: superclass_expr,
                methods,
            }) => {
                let superclass = match superclass_expr {
                    Some(s) => {
                        let val = self.evaluate(&Expr::V(s.clone()))?;
                        match val {
                            CallVal(LoxCallable::LoxClass(c)) => Some(Box::new(c)),
                            _ => {
                                return Err(IR::RuntimeError(
                                    s.name.clone(),
                                    "Superclass must be a class.".to_owned(),
                                ))
                            }
                        }
                    }
                    None => None,
                };

                self.environment
                    .borrow_mut()
                    .define(name.lexeme.clone(), Null);

                let prev_env = self.environment.clone();
                if let Some(s) = superclass.clone() {
                    self.environment = Rc::new(RefCell::new(Environment::enclosed(
                        self.environment.clone(),
                    )));
                    self.environment
                        .borrow_mut()
                        .define("super".to_owned(), CallVal(LoxCallable::LoxClass(*s)));
                }

                let mut ms = HashMap::new();
                methods.iter().for_each(|m| {
                    ms.insert(
                        m.name.lexeme.clone(),
                        LoxFunction::new(
                            m.clone(),
                            self.environment.clone(),
                            m.name.lexeme == "init",
                        ),
                    );
                });

                let class = LoxClass::new(name.lexeme.clone(), superclass, ms);

                if let Some(_) = superclass_expr {
                    self.environment = prev_env;
                }

                self.environment
                    .borrow_mut()
                    .assign(name, &CallVal(LoxCallable::LoxClass(class)))?;
                Ok(())
            }
            Stmt::E(ExprS { expr }) => {
                self.evaluate(expr)?;
                Ok(())
            }
            Stmt::F(f) => {
                let function = LoxFunction::new(f.clone(), Rc::clone(&self.environment), false);
                self.environment.borrow_mut().define(
                    f.name.lexeme.clone(),
                    CallVal(LoxCallable::LoxFunction(function)),
                );
                Ok(())
            }
            Stmt::I(IfS {
                condition,
                then_branch,
                else_branch,
            }) => {
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
            Stmt::P(PrintS { expr }) => {
                println!("{}", self.evaluate(expr)?);
                Ok(())
            }
            Stmt::R(ReturnS { keyword: _, value }) => match value {
                Some(v) => Err(IR::Return(self.evaluate(v)?)),
                None => Err(IR::Return(Null)),
            },
            Stmt::V(VarS { name, initializer }) => {
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
            Stmt::W(WhileS { condition, body }) => {
                let mut cond = self.evaluate(condition)?;
                while self.is_truthy(&cond) {
                    self.execute(body)?;
                    cond = self.evaluate(condition)?;
                }
                Ok(())
            }
        }
    }
}
