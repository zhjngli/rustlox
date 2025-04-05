use std::{
    cell::RefCell,
    fmt::{Debug, Display, Formatter, Result as FmtResult},
    rc::Rc,
};

use enum_dispatch::enum_dispatch;

use crate::{environment::Environment, interpreter::Interpreter, stmt::Stmt, token::Token};

#[derive(Debug, Clone)]
pub enum LoxValue {
    Bool(bool),
    Number(f64),
    String(String),
    CallableVal(Callable),
    Null,
}

impl Display for LoxValue {
    fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult {
        match self {
            Self::Bool(b) => write!(f, "{}", b),
            Self::Number(n) => write!(f, "{}", n),
            Self::String(s) => write!(f, "{}", s),
            Self::CallableVal(c) => write!(f, "{:?}", c),
            Self::Null => write!(f, "null"),
        }
    }
}

#[derive(Debug)]
pub enum Exits {
    // TODO: should ParseError be here?
    RuntimeError(Token, String),
    Return(LoxValue),
}

#[enum_dispatch]
pub trait LoxCallable {
    fn call(&self, interpreter: &mut Interpreter, args: Vec<LoxValue>) -> Result<LoxValue, Exits>;

    fn arity(&self) -> usize;
}

#[enum_dispatch(LoxCallable)]
#[derive(Debug, Clone)]
pub enum Callable {
    NativeFunction,
    Function,
}

pub struct NativeFunction {
    pub name: String,
    pub arity: usize,
    pub call: Box<dyn Fn(&mut Interpreter, Vec<LoxValue>) -> Result<LoxValue, Exits>>,
}

impl Display for NativeFunction {
    fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult {
        write!(f, "<native fn {}>", self.name)
    }
}

impl Debug for NativeFunction {
    fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult {
        f.debug_struct("NativeFunction")
            .field("name", &self.name)
            .finish()
    }
}

impl Clone for NativeFunction {
    fn clone(&self) -> Self {
        let name_clone = self.name.clone();
        NativeFunction {
            name: self.name.to_owned(),
            arity: self.arity.clone(),
            call: Box::new(move |_, _| panic!("Can't clone a native function: {}", name_clone)),
        }
    }
}

impl LoxCallable for NativeFunction {
    fn call(&self, interpreter: &mut Interpreter, args: Vec<LoxValue>) -> Result<LoxValue, Exits> {
        (self.call)(interpreter, args)
    }

    fn arity(&self) -> usize {
        self.arity
    }
}

#[derive(Debug, Clone)]
pub struct Function {
    declaration: Stmt,
    closure: Rc<RefCell<Environment>>,
}

fn fn_initialization_panic<T>() -> T {
    panic!("Function not initialized with a function declaration.")
}

impl Function {
    pub fn new(declaration: Stmt, closure: Rc<RefCell<Environment>>) -> Self {
        match &declaration {
            Stmt::Function { .. } => Function {
                declaration,
                closure,
            },
            _ => fn_initialization_panic(),
        }
    }
}

impl Display for Function {
    fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult {
        match &self.declaration {
            Stmt::Function { name, .. } => write!(f, "<fn {}>", name.lexeme),
            _ => fn_initialization_panic(),
        }
    }
}

impl LoxCallable for Function {
    fn call(&self, interpreter: &mut Interpreter, args: Vec<LoxValue>) -> Result<LoxValue, Exits> {
        match &self.declaration {
            Stmt::Function {
                name: _,
                params,
                body,
            } => {
                let mut environment = Environment::enclosed(Rc::clone(&self.closure));
                params.iter().enumerate().for_each(|(i, param)| {
                    environment.define(param.lexeme.clone(), args.get(i).unwrap().clone());
                });
                let result = interpreter.execute_block(body, environment.to_owned());
                match result {
                    Ok(()) => (),
                    Err(Exits::Return(value)) => return Ok(value),
                    Err(runtime_error) => return Err(runtime_error),
                }
            }
            _ => fn_initialization_panic(),
        }
        Ok(LoxValue::Null)
    }

    fn arity(&self) -> usize {
        match &self.declaration {
            Stmt::Function { params, .. } => params.len(),
            _ => fn_initialization_panic(),
        }
    }
}
