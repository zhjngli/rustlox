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

#[cfg(test)]
mod tests {
    use super::*;

    fn create_token(name: &str, line: usize) -> TokenRef {
        Rc::new(Token::new(
            TokenType::Identifier,
            name.to_string(),
            TokenLiteral::Null,
            line,
        ))
    }

    #[test]
    fn test_define_and_get() {
        let mut env = Environment::new();
        let token = create_token("x", 1);
        env.define("x".to_string(), Some(LoxValue::Number(42.0)));

        let value = env.get(&token).unwrap();
        assert!(matches!(value, LoxValue::Number(_)));
        if let LoxValue::Number(num) = value {
            assert_eq!(num, 42.0);
        }
    }

    #[test]
    fn test_get_undefined_variable() {
        let env = Environment::new();
        let token = create_token("y", 1);

        let result = env.get(&token);
        assert!(result.is_err());
    }

    #[test]
    fn test_assign_existing_variable() {
        let mut env = Environment::new();
        let token = create_token("z", 1);
        env.define("z".to_string(), Some(LoxValue::Number(10.0)));

        env.assign(&token, &LoxValue::Number(20.0)).unwrap();
        let value = env.get(&token).unwrap();
        assert!(matches!(value, LoxValue::Number(_)));
        if let LoxValue::Number(num) = value {
            assert_eq!(num, 20.0);
        }
    }

    #[test]
    fn test_assign_undefined_variable() {
        let mut env = Environment::new();
        let token = create_token("a", 1);

        let result = env.assign(&token, &LoxValue::Number(15.0));
        assert!(result.is_err());
    }

    #[test]
    fn test_enclosed_environment() {
        let global = Rc::new(RefCell::new(Environment::new()));
        global
            .borrow_mut()
            .define("x".to_string(), Some(LoxValue::Number(1.0)));

        let local = Rc::new(RefCell::new(Environment::enclosed(global.clone())));
        let token = create_token("x", 1);

        let value = local.borrow().get(&token).unwrap();
        assert!(matches!(value, LoxValue::Number(_)));
        if let LoxValue::Number(num) = value {
            assert_eq!(num, 1.0);
        }
    }

    #[test]
    fn test_shadowing_in_enclosed_environment() {
        let global = Rc::new(RefCell::new(Environment::new()));
        global
            .borrow_mut()
            .define("x".to_string(), Some(LoxValue::Number(1.0)));

        let local = Rc::new(RefCell::new(Environment::enclosed(global.clone())));
        local
            .borrow_mut()
            .define("x".to_string(), Some(LoxValue::Number(2.0)));

        let token = create_token("x", 1);

        let value = local.borrow().get(&token).unwrap();
        assert!(matches!(value, LoxValue::Number(_)));
        if let LoxValue::Number(num) = value {
            assert_eq!(num, 2.0);
        }

        let global_value = global.borrow().get(&token).unwrap();
        assert!(matches!(global_value, LoxValue::Number(_)));
        if let LoxValue::Number(num) = global_value {
            assert_eq!(num, 1.0);
        }
    }

    #[test]
    fn test_ancestor() {
        let global = Rc::new(RefCell::new(Environment::new()));
        global
            .borrow_mut()
            .define("x".to_string(), Some(LoxValue::Number(1.0)));

        let middle = Rc::new(RefCell::new(Environment::enclosed(global.clone())));
        middle
            .borrow_mut()
            .define("y".to_string(), Some(LoxValue::Number(2.0)));

        let local = Rc::new(RefCell::new(Environment::enclosed(middle.clone())));
        local
            .borrow_mut()
            .define("z".to_string(), Some(LoxValue::Number(3.0)));

        let ancestor = Environment::ancestor(local.clone(), 1);
        let token_y = create_token("y", 1);
        let value_y = ancestor.borrow().get(&token_y).unwrap();
        assert!(matches!(value_y, LoxValue::Number(_)));
        if let LoxValue::Number(num) = value_y {
            assert_eq!(num, 2.0);
        }

        let ancestor_global = Environment::ancestor(local.clone(), 2);
        let token_x = create_token("x", 1);
        let value_x = ancestor_global.borrow().get(&token_x).unwrap();
        assert!(matches!(value_x, LoxValue::Number(_)));
        if let LoxValue::Number(num) = value_x {
            assert_eq!(num, 1.0);
        }
    }
}
