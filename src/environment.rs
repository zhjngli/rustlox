use std::{cell::RefCell, collections::HashMap};

use crate::{
    lox::{Exits, LoxValue},
    token::Token,
};

#[derive(Debug, Clone)]
pub struct Environment {
    enclosing: Option<Box<RefCell<Environment>>>, // TODO: maybe Rc instead of Box?
    values: HashMap<String, LoxValue>,
}

impl Environment {
    pub fn new() -> Self {
        Environment {
            enclosing: None,
            values: HashMap::new(),
        }
    }

    pub fn enclosed(enclosing: Environment) -> Self {
        Environment {
            enclosing: Some(Box::new(RefCell::new(enclosing))),
            values: HashMap::new(),
        }
    }

    pub fn get(&self, name: Token) -> Result<LoxValue, Exits> {
        let var = name.lexeme.clone();
        if self.values.contains_key(&var) {
            return Ok(self.values.get(&var).unwrap().clone());
        }

        if let Some(e) = &self.enclosing {
            return e.borrow().get(name);
        }

        Err(Exits::RuntimeError(
            name,
            format!("Undefined variable '{}'.", var),
        ))
    }

    pub fn assign(&mut self, name: Token, value: LoxValue) -> Result<(), Exits> {
        let var = name.lexeme.clone();
        if self.values.contains_key(&var) {
            self.values.insert(var, value);
            return Ok(());
        }

        if let Some(e) = &self.enclosing {
            e.borrow_mut().assign(name, value)?;
            return Ok(());
        }

        Err(Exits::RuntimeError(
            name,
            format!("Undefined variable '{}'.", var),
        ))
    }

    pub fn define(&mut self, name: String, value: LoxValue) {
        self.values.insert(name, value);
    }
}
