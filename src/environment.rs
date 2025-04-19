use std::{cell::RefCell, collections::HashMap, rc::Rc};

use crate::{
    lox::{InterpreterResult as IR, LoxValue},
    token::{Token, TokenLiteral, TokenRef, TokenType},
};

#[derive(Debug, Clone)]
pub struct Environment {
    enclosing: Option<Rc<RefCell<Environment>>>,
    values: HashMap<String, Option<LoxValue>>,
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

    pub fn get_this(
        env: &Rc<RefCell<Environment>>,
        distance: usize,
        name: &TokenRef,
    ) -> Result<LoxValue, IR> {
        Environment::ancestor(env.clone(), distance)
            .borrow()
            .get(&Rc::new(Token::new(
                TokenType::This,
                "this".to_owned(),
                TokenLiteral::Null,
                name.line,
            )))
    }

    pub fn define(&mut self, name: String, value: Option<LoxValue>) {
        self.values.insert(name, value);
    }

    pub fn get(&self, name: &TokenRef) -> Result<LoxValue, IR> {
        let var = &name.lexeme;
        if self.values.contains_key(var) {
            match self.values.get(var).unwrap() {
                Some(value) => Ok(value.clone()),
                None => Err(IR::RuntimeError(
                    name.clone(),
                    format!("Variable defined but not initialized '{}'.", var),
                )),
            }
        } else {
            match &self.enclosing {
                Some(e) => e.borrow().get(name),
                None => Err(IR::RuntimeError(
                    name.clone(),
                    format!("Undefined variable '{}'.", var),
                )),
            }
        }
    }

    pub fn assign(&mut self, name: &TokenRef, value: &LoxValue) -> Result<(), IR> {
        let var = &name.lexeme;
        if self.values.contains_key(var) {
            self.values.insert(var.clone(), Some(value.clone()));
            return Ok(());
        }

        if let Some(e) = &self.enclosing {
            e.borrow_mut().assign(name, value)?;
            return Ok(());
        }

        Err(IR::RuntimeError(
            name.clone(),
            format!("Undefined variable '{}'.", var),
        ))
    }

    // get_at and assign_at can also be used in place of ancestor. I find using ancestor slightly less clunky because it avoids the potentially unsafe unwrap() call
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
