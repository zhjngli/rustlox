use std::{
    cell::RefCell,
    collections::HashMap,
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
    ClassInstance(Rc<RefCell<Instance>>),
    Null,
}

impl Display for LoxValue {
    fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult {
        match self {
            Self::Bool(b) => write!(f, "{}", b),
            Self::Number(n) => write!(f, "{}", n),
            Self::String(s) => write!(f, "{}", s),
            Self::CallableVal(c) => write!(f, "{:?}", c),
            Self::ClassInstance(i) => write!(f, "{}", i.borrow()),
            Self::Null => write!(f, "null"),
        }
    }
}

#[derive(Debug)]
pub enum Exits {
    // TODO: add ParseError here?
    // TODO: use new StaticError for Resolver
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
    Class,
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
    is_initializer: bool,
}

fn fn_initialization_panic<T>() -> T {
    panic!("Function not initialized with a function declaration.")
}

impl Function {
    pub fn new(declaration: Stmt, closure: Rc<RefCell<Environment>>, is_initializer: bool) -> Self {
        match &declaration {
            Stmt::Function { .. } => Function {
                declaration,
                closure,
                is_initializer,
            },
            _ => fn_initialization_panic(),
        }
    }

    pub fn bind(&self, instance: Rc<RefCell<Instance>>) -> Function {
        let env = Rc::new(RefCell::new(Environment::enclosed(self.closure.clone())));
        env.borrow_mut()
            .define("this".to_owned(), LoxValue::ClassInstance(instance));
        Function::new(self.declaration.clone(), env, self.is_initializer)
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
            Stmt::Function { name, params, body } => {
                let mut environment = Environment::enclosed(Rc::clone(&self.closure));
                params.iter().enumerate().for_each(|(i, param)| {
                    environment.define(param.lexeme.clone(), args.get(i).unwrap().clone());
                });
                let result = interpreter.execute_block(body, environment.to_owned());
                match result {
                    Err(Exits::Return(value)) => {
                        if self.is_initializer {
                            Environment::get_this(&self.closure, 0, name)
                        } else {
                            Ok(value)
                        }
                    }
                    Ok(()) => {
                        if self.is_initializer {
                            Environment::get_this(&self.closure, 0, name)
                        } else {
                            Ok(LoxValue::Null)
                        }
                    }
                    Err(runtime_error) => Err(runtime_error),
                }
            }
            _ => fn_initialization_panic(),
        }
    }

    fn arity(&self) -> usize {
        match &self.declaration {
            Stmt::Function { params, .. } => params.len(),
            _ => fn_initialization_panic(),
        }
    }
}

#[derive(Debug, Clone)]
pub struct Class {
    name: String,
    superclass: Option<Box<Class>>,
    methods: HashMap<String, Function>,
}

impl Class {
    pub fn new(
        name: String,
        superclass: Option<Box<Class>>,
        methods: HashMap<String, Function>,
    ) -> Self {
        Class {
            name,
            superclass,
            methods,
        }
    }

    pub fn find_method(&self, name: &str) -> Option<&Function> {
        self.methods
            .get(name)
            .or_else(|| self.superclass.as_ref().and_then(|s| s.find_method(name)))
    }
}

impl Display for Class {
    fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult {
        write!(f, "<class {}>", self.name)
    }
}

impl LoxCallable for Class {
    fn call(&self, interpreter: &mut Interpreter, args: Vec<LoxValue>) -> Result<LoxValue, Exits> {
        let instance = Rc::new(RefCell::new(Instance::new(self.clone())));
        if let Some(initializer) = self.find_method("init") {
            initializer.bind(instance.clone()).call(interpreter, args)?;
        }
        Ok(LoxValue::ClassInstance(instance))
    }

    fn arity(&self) -> usize {
        match self.find_method("init") {
            Some(initializer) => initializer.arity(),
            None => 0,
        }
    }
}

#[derive(Debug, Clone)]
pub struct Instance {
    class: Class, // TODO: RC this so it's not deep-cloned every time an instance is created
    fields: HashMap<String, LoxValue>,
}

impl Instance {
    pub fn new(class: Class) -> Self {
        Instance {
            class,
            fields: HashMap::new(),
        }
    }

    pub fn get(&self, name: &Token) -> Result<LoxValue, Exits> {
        if let Some(v) = self.fields.get(&name.lexeme) {
            Ok(v.clone())
        } else if let Some(m) = self.class.find_method(&name.lexeme) {
            Ok(LoxValue::CallableVal(Callable::Function(
                m.bind(Rc::new(RefCell::new(self.clone()))),
            )))
        } else {
            Err(Exits::RuntimeError(
                name.clone(),
                format!("Undefined property '{}'.", name.lexeme),
            ))
        }
    }

    pub fn set(&mut self, name: &Token, value: &LoxValue) {
        self.fields.insert(name.lexeme.clone(), value.clone());
    }
}

impl Display for Instance {
    fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult {
        write!(f, "<instance {}>", self.class.name)
    }
}
