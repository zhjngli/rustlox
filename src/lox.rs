use std::{
    cell::RefCell,
    collections::HashMap,
    fmt::{Debug, Display, Formatter, Result as FmtResult},
    rc::Rc,
};

use enum_dispatch::enum_dispatch;

use crate::{environment::Environment, interpreter::Interpreter, stmt::FunctionS, token::TokenRef};

#[derive(Debug, Clone)]
pub enum LoxValue {
    Bool(bool),
    Number(f64),
    String(String),
    List(Rc<RefCell<LoxList>>),
    CallVal(LoxCallable),
    ClassInstance(Rc<RefCell<LoxInstance>>),
    Null,
}

impl Display for LoxValue {
    fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult {
        match self {
            Self::Bool(b) => write!(f, "{}", b),
            Self::Number(n) => write!(f, "{}", n),
            Self::String(s) => write!(f, "{}", s),
            Self::List(l) => write!(f, "{}", l.borrow()),
            Self::CallVal(c) => match c {
                LoxCallable::NativeFunction(n) => write!(f, "{}", n),
                LoxCallable::LoxFunction(l) => write!(f, "{}", l),
                LoxCallable::LoxClass(c) => write!(f, "{}", c),
            },
            Self::ClassInstance(i) => write!(f, "{}", i.borrow()),
            Self::Null => write!(f, "null"),
        }
    }
}

#[derive(Debug)]
pub enum InterpreterResult {
    BreakException,
    RuntimeError(TokenRef, String),
    Return(LoxValue),
}

type IR = InterpreterResult;

#[enum_dispatch]
pub trait Callable {
    // TODO: wrap args in Rc<RefCell<>>?
    fn call(&self, interpreter: &mut Interpreter, args: Vec<LoxValue>) -> Result<LoxValue, IR>;

    fn arity(&self) -> usize;
}

#[enum_dispatch(Callable)]
#[derive(Debug, Clone)]
pub enum LoxCallable {
    NativeFunction,
    LoxFunction,
    LoxClass,
}

pub struct NativeFunction {
    pub name: String,
    pub arity: usize,
    pub call: Box<dyn Fn(&mut Interpreter, Vec<LoxValue>) -> Result<LoxValue, IR>>,
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

impl Callable for NativeFunction {
    fn call(&self, interpreter: &mut Interpreter, args: Vec<LoxValue>) -> Result<LoxValue, IR> {
        (self.call)(interpreter, args)
    }

    fn arity(&self) -> usize {
        self.arity
    }
}

#[derive(Debug, Clone)]
pub struct LoxFunction {
    declaration: FunctionS,
    closure: Rc<RefCell<Environment>>,
    is_initializer: bool,
}

impl LoxFunction {
    pub fn new(
        declaration: FunctionS,
        closure: Rc<RefCell<Environment>>,
        is_initializer: bool,
    ) -> Self {
        LoxFunction {
            declaration,
            closure,
            is_initializer,
        }
    }

    pub fn bind(&self, instance: Rc<RefCell<LoxInstance>>) -> LoxFunction {
        let env = Rc::new(RefCell::new(Environment::enclosed(self.closure.clone())));
        env.borrow_mut()
            .define("this".to_owned(), Some(LoxValue::ClassInstance(instance)));
        LoxFunction::new(self.declaration.clone(), env, self.is_initializer)
    }

    pub fn is_getter(&self) -> bool {
        self.declaration.params.is_none()
    }
}

impl Display for LoxFunction {
    fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult {
        write!(f, "<fn {}>", self.declaration.name.lexeme)
    }
}

impl Callable for LoxFunction {
    fn call(&self, interpreter: &mut Interpreter, args: Vec<LoxValue>) -> Result<LoxValue, IR> {
        let mut environment = Environment::enclosed(Rc::clone(&self.closure));
        if let Some(params) = &self.declaration.params {
            params.iter().enumerate().for_each(|(i, param)| {
                environment.define(param.lexeme.clone(), Some(args.get(i).unwrap().clone()));
            });
        }
        let result = interpreter.execute_block(&self.declaration.body, environment.to_owned());
        match result {
            Err(IR::Return(value)) => {
                if self.is_initializer {
                    Environment::get_this(&self.closure, 0, &self.declaration.name)
                } else {
                    Ok(value)
                }
            }
            Ok(()) => {
                if self.is_initializer {
                    Environment::get_this(&self.closure, 0, &self.declaration.name)
                } else {
                    Ok(LoxValue::Null)
                }
            }
            Err(runtime_error) => Err(runtime_error),
        }
    }

    fn arity(&self) -> usize {
        match &self.declaration.params {
            Some(params) => params.len(),
            None => 0,
        }
    }
}

#[derive(Debug, Clone)]
pub struct LoxClass {
    name: String,
    metaclass: Option<Rc<RefCell<LoxInstance>>>,
    superclass: Option<Box<LoxClass>>,
    methods: HashMap<String, LoxFunction>,
}

impl LoxClass {
    pub fn new(
        name: String,
        metaclass: Option<LoxClass>,
        superclass: Option<Box<LoxClass>>,
        methods: HashMap<String, LoxFunction>,
    ) -> Self {
        LoxClass {
            name,
            metaclass: metaclass.map(|m| Rc::new(RefCell::new(LoxInstance::new(m)))),
            superclass,
            methods,
        }
    }

    pub fn find_method(&self, name: &str) -> Option<&LoxFunction> {
        self.methods
            .get(name)
            .or_else(|| self.superclass.as_ref().and_then(|s| s.find_method(name)))
    }

    pub fn metaclass(&self) -> Option<Rc<RefCell<LoxInstance>>> {
        self.metaclass.clone()
    }
}

impl Display for LoxClass {
    fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult {
        write!(f, "<class {}>", self.name)
    }
}

impl Callable for LoxClass {
    fn call(&self, interpreter: &mut Interpreter, args: Vec<LoxValue>) -> Result<LoxValue, IR> {
        let instance = Rc::new(RefCell::new(LoxInstance::new(self.clone())));
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

pub trait Instance<T>: Debug + Display {
    fn get(&self, name: &TokenRef, self_ref: Rc<RefCell<T>>) -> Result<LoxValue, IR>;
    fn set(&mut self, name: &TokenRef, value: &LoxValue) -> Result<(), IR>;
}

#[derive(Debug, Clone)]
pub struct LoxInstance {
    class: LoxClass,
    fields: HashMap<String, LoxValue>,
}

impl LoxInstance {
    fn new(class: LoxClass) -> Self {
        LoxInstance {
            class,
            fields: HashMap::new(),
        }
    }
}

impl Instance<LoxInstance> for LoxInstance {
    fn get(&self, name: &TokenRef, self_ref: Rc<RefCell<LoxInstance>>) -> Result<LoxValue, IR> {
        if let Some(v) = self.fields.get(&name.lexeme) {
            Ok(v.clone())
        } else if let Some(m) = self.class.find_method(&name.lexeme) {
            Ok(LoxValue::CallVal(LoxCallable::LoxFunction(
                m.bind(self_ref),
            )))
        } else {
            Err(IR::RuntimeError(
                name.clone(),
                format!("Undefined property '{}'.", name.lexeme),
            ))
        }
    }

    fn set(&mut self, name: &TokenRef, value: &LoxValue) -> Result<(), IR> {
        self.fields.insert(name.lexeme.clone(), value.clone());
        Ok(())
    }
}

impl Display for LoxInstance {
    fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult {
        write!(f, "<instance {}>", self.class.name)
    }
}

#[derive(Debug, Clone)]
pub struct LoxList {
    elems: Rc<RefCell<Vec<LoxValue>>>,
}

impl LoxList {
    pub fn new(elems: Vec<LoxValue>) -> Self {
        LoxList {
            elems: Rc::new(RefCell::new(elems)),
        }
    }

    pub fn elems(&self) -> Rc<RefCell<Vec<LoxValue>>> {
        self.elems.clone()
    }
}

impl Display for LoxList {
    fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult {
        let elems: Vec<String> = self.elems.borrow().iter().map(|e| e.to_string()).collect();
        write!(f, "[{}]", elems.join(", "))
    }
}

impl Instance<LoxList> for LoxList {
    fn get(&self, name: &TokenRef, _self_ref: Rc<RefCell<LoxList>>) -> Result<LoxValue, IR> {
        if name.lexeme == "length" {
            Ok(LoxValue::Number(self.elems.borrow().len() as f64))
        } else if name.lexeme == "append" {
            Ok(LoxValue::CallVal(LoxCallable::from(NativeFunction {
                name: "append".to_owned(),
                arity: 1,
                call: {
                    let elems = self.elems.clone();
                    Box::new(move |_, args| {
                        // arity checked when visiting call expression
                        let value = args[0].clone();
                        elems.borrow_mut().push(value.clone());
                        Ok(value)
                    })
                },
            })))
        } else if name.lexeme == "pop" {
            Ok(LoxValue::CallVal(LoxCallable::from(NativeFunction {
                name: "pop".to_owned(),
                arity: 0,
                call: {
                    let elems = self.elems.clone();
                    Box::new(move |_, _| {
                        // arity checked when visiting call expression
                        Ok(elems.borrow_mut().pop().unwrap_or(LoxValue::Null))
                    })
                },
            })))
        } else {
            Err(IR::RuntimeError(
                name.clone(),
                format!("Undefined property on list '{}'.", name.lexeme),
            ))
        }
    }

    fn set(&mut self, name: &TokenRef, _value: &LoxValue) -> Result<(), IR> {
        Err(IR::RuntimeError(
            name.clone(),
            "Cannot set properties on a list.".to_owned(),
        ))
    }
}
