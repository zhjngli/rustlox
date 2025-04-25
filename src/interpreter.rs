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
    stmt::{
        BlockS, BreakS, ClassS, ExprS, IfS, PrintS, ReturnS, Stmt, VarS, Visitor as SVisitor,
        WhileS,
    },
    token::{TokenLiteral, TokenRef, TokenType},
};

#[derive(Debug)]
pub struct Interpreter {
    environment: Rc<RefCell<Environment>>,
    pub globals: Rc<RefCell<Environment>>,
    locals: HashMap<Expr, usize>,
    repl: bool,
}

impl Interpreter {
    pub fn new(repl: bool) -> Self {
        let globals = Rc::new(RefCell::new(Environment::new()));
        globals.borrow_mut().define(
            "clock".to_owned(),
            Some(CallVal(LoxCallable::from(NativeFunction {
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
            }))),
        );
        Interpreter {
            environment: globals.clone(),
            globals,
            locals: HashMap::new(),
            repl,
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
                    ClassInstance(i) => {
                        let val = i.borrow().get(name)?;
                        match val {
                            CallVal(LoxCallable::LoxFunction(f)) if f.is_getter() => {
                                f.call(self, vec![])
                            }
                            _ => Ok(val),
                        }
                    }
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
            Stmt::Br(BreakS { keyword: _ }) => Err(IR::BreakException),
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
                    .define(name.lexeme.clone(), Some(Null));

                let prev_env = self.environment.clone();
                if let Some(s) = superclass.clone() {
                    self.environment = Rc::new(RefCell::new(Environment::enclosed(
                        self.environment.clone(),
                    )));
                    self.environment
                        .borrow_mut()
                        .define("super".to_owned(), Some(CallVal(LoxCallable::LoxClass(*s))));
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
                let val = self.evaluate(expr)?;
                if self.repl {
                    println!("{}", val);
                }
                Ok(())
            }
            Stmt::F(f) => {
                let function = LoxFunction::new(f.clone(), Rc::clone(&self.environment), false);
                self.environment.borrow_mut().define(
                    f.name.lexeme.clone(),
                    Some(CallVal(LoxCallable::LoxFunction(function))),
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
                    Some(e) => value = Some(self.evaluate(e)?),
                    None => value = None,
                }
                self.environment
                    .borrow_mut()
                    .define(name.lexeme.clone(), value);
                Ok(())
            }
            Stmt::W(WhileS { condition, body }) => {
                let mut cond = self.evaluate(condition)?;
                while self.is_truthy(&cond) {
                    match self.execute(body) {
                        Err(IR::BreakException) => break,
                        Err(e) => return Err(e),
                        Ok(()) => (),
                    }
                    cond = self.evaluate(condition)?;
                }
                Ok(())
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{
        expr::{BinaryE, LiteralE},
        token::{Token, TokenLiteral, TokenType},
    };

    fn create_token(
        token_type: TokenType,
        lexeme: &str,
        literal: TokenLiteral,
        line: usize,
    ) -> TokenRef {
        TokenRef::new(Token {
            token_type,
            lexeme: lexeme.to_string(),
            literal,
            line,
        })
    }

    #[test]
    fn test_interpreter_literal_expression() {
        let mut interpreter = Interpreter::new(false);

        let expr = Expr::Li(LiteralE::new(TokenLiteral::NumberLit(42.0)));

        let result = interpreter.evaluate(&expr).unwrap();
        assert!(matches!(result, LoxValue::Number(_)));
        if let LoxValue::Number(n) = result {
            assert_eq!(n, 42.0);
        }
    }

    #[test]
    fn test_interpreter_binary_expression() {
        let mut interpreter = Interpreter::new(false);

        let expr = Expr::B(BinaryE::new(
            Expr::Li(LiteralE::new(TokenLiteral::NumberLit(10.0))),
            create_token(TokenType::Plus, "+", TokenLiteral::Null, 1),
            Expr::Li(LiteralE::new(TokenLiteral::NumberLit(20.0))),
        ));

        let result = interpreter.evaluate(&expr).unwrap();
        assert!(matches!(result, LoxValue::Number(_)));
        if let LoxValue::Number(n) = result {
            assert_eq!(n, 30.0);
        }
    }

    #[test]
    fn test_interpreter_grouping_expression() {
        let mut interpreter = Interpreter::new(false);

        let expr = Expr::Gr(GroupingE::new(Expr::Li(LiteralE::new(
            TokenLiteral::NumberLit(42.0),
        ))));

        let result = interpreter.evaluate(&expr).unwrap();
        assert!(matches!(result, LoxValue::Number(_)));
        if let LoxValue::Number(n) = result {
            assert_eq!(n, 42.0);
        }
    }

    #[test]
    fn test_interpreter_unary_expression() {
        let mut interpreter = Interpreter::new(false);

        let expr = Expr::U(UnaryE::new(
            create_token(TokenType::Minus, "-", TokenLiteral::Null, 1),
            Expr::Li(LiteralE::new(TokenLiteral::NumberLit(5.0))),
        ));

        let result = interpreter.evaluate(&expr).unwrap();
        assert!(matches!(result, LoxValue::Number(_)));
        if let LoxValue::Number(n) = result {
            assert_eq!(n, -5.0);
        }
    }

    #[test]
    fn test_interpreter_logical_or_expression() {
        let mut interpreter = Interpreter::new(false);

        let expr = Expr::Lo(LogicalE::new(
            Expr::Li(LiteralE::new(TokenLiteral::Bool(false))),
            create_token(TokenType::Or, "or", TokenLiteral::Null, 1),
            Expr::Li(LiteralE::new(TokenLiteral::Bool(true))),
        ));

        let result = interpreter.evaluate(&expr).unwrap();
        assert!(matches!(result, LoxValue::Bool(_)));
        if let LoxValue::Bool(b) = result {
            assert_eq!(b, true);
        }
    }

    #[test]
    fn test_interpreter_logical_and_expression() {
        let mut interpreter = Interpreter::new(false);

        let expr = Expr::Lo(LogicalE::new(
            Expr::Li(LiteralE::new(TokenLiteral::Bool(true))),
            create_token(TokenType::And, "and", TokenLiteral::Null, 1),
            Expr::Li(LiteralE::new(TokenLiteral::Bool(false))),
        ));

        let result = interpreter.evaluate(&expr).unwrap();
        assert!(matches!(result, LoxValue::Bool(_)));
        if let LoxValue::Bool(b) = result {
            assert_eq!(b, false);
        }
    }

    #[test]
    fn test_interpreter_equality_expression() {
        let mut interpreter = Interpreter::new(false);

        let expr = Expr::B(BinaryE::new(
            Expr::Li(LiteralE::new(TokenLiteral::NumberLit(10.0))),
            create_token(TokenType::EqualEqual, "==", TokenLiteral::Null, 1),
            Expr::Li(LiteralE::new(TokenLiteral::NumberLit(10.0))),
        ));

        let result = interpreter.evaluate(&expr).unwrap();
        assert!(matches!(result, LoxValue::Bool(_)));
        if let LoxValue::Bool(b) = result {
            assert_eq!(b, true);
        }
    }

    #[test]
    fn test_interpreter_inequality_expression() {
        let mut interpreter = Interpreter::new(false);

        let expr = Expr::B(BinaryE::new(
            Expr::Li(LiteralE::new(TokenLiteral::NumberLit(10.0))),
            create_token(TokenType::BangEqual, "!=", TokenLiteral::Null, 1),
            Expr::Li(LiteralE::new(TokenLiteral::NumberLit(20.0))),
        ));

        let result = interpreter.evaluate(&expr).unwrap();
        assert!(matches!(result, LoxValue::Bool(_)));
        if let LoxValue::Bool(b) = result {
            assert_eq!(b, true);
        }
    }

    #[test]
    fn test_interpreter_comparison_expression() {
        let mut interpreter = Interpreter::new(false);

        let expr = Expr::B(BinaryE::new(
            Expr::Li(LiteralE::new(TokenLiteral::NumberLit(10.0))),
            create_token(TokenType::Greater, ">", TokenLiteral::Null, 1),
            Expr::Li(LiteralE::new(TokenLiteral::NumberLit(5.0))),
        ));

        let result = interpreter.evaluate(&expr).unwrap();
        assert!(matches!(result, LoxValue::Bool(_)));
        if let LoxValue::Bool(b) = result {
            assert_eq!(b, true);
        }
    }

    #[test]
    fn test_interpreter_variable_expression() {
        let mut interpreter = Interpreter::new(false);

        interpreter
            .environment
            .borrow_mut()
            .define("x".to_owned(), Some(LoxValue::Number(42.0)));

        let expr = Expr::V(VariableE::new(create_token(
            TokenType::Identifier,
            "x",
            TokenLiteral::Null,
            1,
        )));

        let result = interpreter.evaluate(&expr).unwrap();
        assert!(matches!(result, LoxValue::Number(_)));
        if let LoxValue::Number(n) = result {
            assert_eq!(n, 42.0);
        }
    }

    #[test]
    fn test_interpreter_assignment_expression() {
        let mut interpreter = Interpreter::new(false);

        interpreter
            .environment
            .borrow_mut()
            .define("x".to_owned(), Some(LoxValue::Number(10.0)));

        let expr = Expr::A(AssignE::new(
            create_token(TokenType::Identifier, "x", TokenLiteral::Null, 1),
            Expr::Li(LiteralE::new(TokenLiteral::NumberLit(20.0))),
        ));

        let result = interpreter.evaluate(&expr).unwrap();
        assert!(matches!(result, LoxValue::Number(_)));
        if let LoxValue::Number(n) = result {
            assert_eq!(n, 20.0);
        }

        let updated_value = interpreter
            .environment
            .borrow()
            .get(&create_token(
                TokenType::Identifier,
                "x",
                TokenLiteral::Null,
                1,
            ))
            .unwrap();
        assert!(matches!(updated_value, LoxValue::Number(_)));
        if let LoxValue::Number(n) = updated_value {
            assert_eq!(n, 20.0);
        }
    }

    #[test]
    fn test_interpreter_this_expression() {
        let mut interpreter = Interpreter::new(false);

        let class = LoxClass::new("TestClass".to_owned(), None, HashMap::new());
        let instance = class.call(&mut interpreter, vec![]).unwrap();

        interpreter
            .environment
            .borrow_mut()
            .define("this".to_owned(), Some(instance));

        let expr = Expr::T(ThisE::new(create_token(
            TokenType::This,
            "this",
            TokenLiteral::Null,
            1,
        )));

        let result = interpreter.evaluate(&expr).unwrap();
        assert!(matches!(result, LoxValue::ClassInstance(_)));
    }

    #[test]
    fn test_interpreter_var_statement() {
        let mut interpreter = Interpreter::new(false);

        let stmt = Stmt::V(VarS {
            name: create_token(TokenType::Identifier, "x", TokenLiteral::Null, 1),
            initializer: Some(Expr::Li(LiteralE::new(TokenLiteral::NumberLit(42.0)))),
        });

        interpreter.execute(&stmt).unwrap();

        let value = interpreter
            .environment
            .borrow()
            .get(&create_token(
                TokenType::Identifier,
                "x",
                TokenLiteral::Null,
                1,
            ))
            .unwrap();
        assert!(matches!(value, LoxValue::Number(_)));
        if let LoxValue::Number(n) = value {
            assert_eq!(n, 42.0);
        }
    }

    #[test]
    fn test_interpreter_print_statement() {
        let mut interpreter = Interpreter::new(false);

        let stmt = Stmt::P(PrintS {
            expr: Expr::Li(LiteralE::new(TokenLiteral::NumberLit(42.0))),
        });
        let result = interpreter.execute(&stmt);
        assert!(result.is_ok());
    }

    #[test]
    fn test_interpreter_block_statement() {
        let mut interpreter = Interpreter::new(false);

        let stmts = vec![
            Stmt::V(VarS {
                name: create_token(TokenType::Identifier, "x", TokenLiteral::Null, 1),
                initializer: Some(Expr::Li(LiteralE::new(TokenLiteral::NumberLit(10.0)))),
            }),
            Stmt::V(VarS {
                name: create_token(TokenType::Identifier, "y", TokenLiteral::Null, 1),
                initializer: Some(Expr::Li(LiteralE::new(TokenLiteral::NumberLit(20.0)))),
            }),
        ];

        let block_stmt = Stmt::B(BlockS { stmts });

        interpreter.execute(&block_stmt).unwrap();

        let x_value = interpreter.environment.borrow().get(&create_token(
            TokenType::Identifier,
            "x",
            TokenLiteral::Null,
            1,
        ));
        assert!(x_value.is_err()); // x should not exist in the outer environment

        let y_value = interpreter.environment.borrow().get(&create_token(
            TokenType::Identifier,
            "y",
            TokenLiteral::Null,
            1,
        ));
        assert!(y_value.is_err()); // y should not exist in the outer environment
    }

    #[test]
    fn test_interpreter_if_statement() {
        let mut interpreter = Interpreter::new(false);

        let stmt = Stmt::I(IfS {
            condition: Expr::Li(LiteralE::new(TokenLiteral::Bool(true))),
            then_branch: Box::new(Stmt::P(PrintS {
                expr: Expr::Li(LiteralE::new(TokenLiteral::StringLit(
                    "then branch".to_owned(),
                ))),
            })),
            else_branch: Some(Box::new(Stmt::P(PrintS {
                expr: Expr::Li(LiteralE::new(TokenLiteral::StringLit(
                    "else branch".to_owned(),
                ))),
            }))),
        });

        interpreter.execute(&stmt).unwrap();
    }

    #[test]
    fn test_interpreter_while_statement() {
        let mut interpreter = Interpreter::new(false);

        interpreter
            .environment
            .borrow_mut()
            .define("x".to_owned(), Some(LoxValue::Number(0.0)));

        let stmt = Stmt::W(WhileS {
            condition: Expr::B(BinaryE::new(
                Expr::V(VariableE::new(create_token(
                    TokenType::Identifier,
                    "x",
                    TokenLiteral::Null,
                    1,
                ))),
                create_token(TokenType::Less, "<", TokenLiteral::Null, 1),
                Expr::Li(LiteralE::new(TokenLiteral::NumberLit(3.0))),
            )),
            body: Box::new(Stmt::E(ExprS {
                expr: Expr::A(AssignE::new(
                    create_token(TokenType::Identifier, "x", TokenLiteral::Null, 1),
                    Expr::B(BinaryE::new(
                        Expr::V(VariableE::new(create_token(
                            TokenType::Identifier,
                            "x",
                            TokenLiteral::Null,
                            1,
                        ))),
                        create_token(TokenType::Plus, "+", TokenLiteral::Null, 1),
                        Expr::Li(LiteralE::new(TokenLiteral::NumberLit(1.0))),
                    )),
                )),
            })),
        });

        interpreter.execute(&stmt).unwrap();

        let x_value = interpreter
            .environment
            .borrow()
            .get(&create_token(
                TokenType::Identifier,
                "x",
                TokenLiteral::Null,
                1,
            ))
            .unwrap();
        assert!(matches!(x_value, LoxValue::Number(_)));
        if let LoxValue::Number(n) = x_value {
            assert_eq!(n, 3.0);
        }
    }

    #[test]
    fn test_interpreter_while_with_break_statement() {
        let mut interpreter = Interpreter::new(false);

        interpreter
            .environment
            .borrow_mut()
            .define("x".to_owned(), Some(LoxValue::Number(0.0)));

        let stmt = Stmt::W(WhileS {
            condition: Expr::B(BinaryE::new(
                Expr::V(VariableE::new(create_token(
                    TokenType::Identifier,
                    "x",
                    TokenLiteral::Null,
                    1,
                ))),
                create_token(TokenType::Less, "<", TokenLiteral::Null, 1),
                Expr::Li(LiteralE::new(TokenLiteral::NumberLit(5.0))),
            )),
            body: Box::new(Stmt::B(BlockS {
                stmts: vec![
                    Stmt::E(ExprS {
                        expr: Expr::A(AssignE::new(
                            create_token(TokenType::Identifier, "x", TokenLiteral::Null, 1),
                            Expr::B(BinaryE::new(
                                Expr::V(VariableE::new(create_token(
                                    TokenType::Identifier,
                                    "x",
                                    TokenLiteral::Null,
                                    1,
                                ))),
                                create_token(TokenType::Plus, "+", TokenLiteral::Null, 1),
                                Expr::Li(LiteralE::new(TokenLiteral::NumberLit(1.0))),
                            )),
                        )),
                    }),
                    Stmt::Br(BreakS {
                        keyword: create_token(TokenType::Break, "break", TokenLiteral::Null, 1),
                    }),
                ],
            })),
        });

        interpreter.execute(&stmt).unwrap();

        let x_value = interpreter
            .environment
            .borrow()
            .get(&create_token(
                TokenType::Identifier,
                "x",
                TokenLiteral::Null,
                1,
            ))
            .unwrap();
        assert!(matches!(x_value, LoxValue::Number(_)));
        if let LoxValue::Number(n) = x_value {
            assert_eq!(n, 1.0);
        }
    }

    #[test]
    fn test_interpreter_class_with_getter_method() {
        let mut interpreter = Interpreter::new(false);

        let class_name = create_token(TokenType::Identifier, "TestClass", TokenLiteral::Null, 1);
        let getter_name = create_token(TokenType::Identifier, "value", TokenLiteral::Null, 1);

        let getter_method = crate::stmt::FunctionS {
            name: getter_name.clone(),
            params: None,
            body: vec![Stmt::R(crate::stmt::ReturnS {
                keyword: create_token(TokenType::Return, "return", TokenLiteral::Null, 1),
                value: Some(Expr::Li(LiteralE::new(TokenLiteral::NumberLit(42.0)))),
            })],
        };

        let class_stmt = Stmt::C(ClassS {
            name: class_name.clone(),
            superclass: None,
            methods: vec![getter_method],
        });

        interpreter.execute(&class_stmt).unwrap();

        let call_class_expr = Expr::C(CallE::new(
            Expr::V(VariableE::new(class_name.clone())),
            create_token(TokenType::LeftParen, "(", TokenLiteral::Null, 1),
            vec![],
        ));

        let get_value_expr = Expr::G(GetE::new(
            call_class_expr,
            getter_name.clone(),
        ));

        let result = interpreter.evaluate(&get_value_expr).unwrap();
        assert!(matches!(result, LoxValue::Number(_)));
        if let LoxValue::Number(n) = result {
            assert_eq!(n, 42.0);
        }
    }
}
