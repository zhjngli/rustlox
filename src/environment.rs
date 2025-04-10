use std::{cell::RefCell, collections::HashMap, rc::Rc};

use crate::{
    lox::{Exits, LoxValue},
    token::Token,
};

#[derive(Debug, Clone)]
pub struct Environment {
    enclosing: Option<Rc<RefCell<Environment>>>,
    values: HashMap<String, LoxValue>,
}

impl Environment {
    pub fn new() -> Self {
        Environment {
            enclosing: None,
            values: HashMap::new(),
        }
    }

    pub fn enclosed(enclosing: Rc<RefCell<Environment>>) -> Self {
        Environment {
            enclosing: Some(enclosing),
            values: HashMap::new(),
        }
    }

    pub fn ancestor(
        mut env: Rc<RefCell<Environment>>,
        distance: usize,
    ) -> Rc<RefCell<Environment>> {
        for i in 0..distance {
            let enclosing = env.borrow().enclosing.clone();
            match enclosing {
                Some(e) => env = e,
                None => panic!("Couldn't find ancestor at distance {} of {}.", i, distance),
            }
        }
        env
    }

    pub fn define(&mut self, name: String, value: LoxValue) {
        self.values.insert(name, value);
    }

    pub fn get(&self, name: &Token) -> Result<LoxValue, Exits> {
        let var = &name.lexeme;
        if self.values.contains_key(var) {
            return Ok(self.values.get(var).unwrap().clone());
        }

        if let Some(e) = &self.enclosing {
            return e.borrow().get(name);
        }

        Err(Exits::RuntimeError(
            name.clone(),
            format!("Undefined variable '{}'.", var),
        ))
    }

    pub fn assign(&mut self, name: &Token, value: &LoxValue) -> Result<(), Exits> {
        let var = &name.lexeme;
        if self.values.contains_key(var) {
            self.values.insert(var.clone(), value.clone());
            return Ok(());
        }

        if let Some(e) = &self.enclosing {
            e.borrow_mut().assign(name, value)?;
            return Ok(());
        }

        Err(Exits::RuntimeError(
            name.clone(),
            format!("Undefined variable '{}'.", var),
        ))
    }

    // get_at and assign_at can also be used in place of ancestor. I find using ancestor slightly less clunky due to the potentially unsafe unwrap() call
    // pub fn get_at(&self, distance: usize, name: &Token) -> Result<LoxValue, Exits> {
    //     if distance == 0 {
    //         self.get(name)
    //     } else {
    //         self.enclosing
    //             .as_ref()
    //             .unwrap()
    //             .borrow()
    //             .get_at(distance - 1, name)
    //     }
    // }

    // pub fn assign_at(&mut self, distance: usize, name: &Token, value: &LoxValue) -> Result<(), Exits> {
    //     if distance == 0 {
    //         self.define(name.lexeme.clone(), value.clone());
    //         Ok(())
    //     } else {
    //         self.enclosing
    //             .as_ref()
    //             .unwrap()
    //             .borrow_mut()
    //             .assign_at(distance - 1, name, value)
    //     }
    // }
}
