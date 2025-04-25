use std::collections::HashMap;

use crate::{
    expr::{
        AssignE, BinaryE, CallE, Expr, GetE, GroupingE, LiteralE, LogicalE, SetE, SuperE, ThisE,
        UnaryE, VariableE, Visitor as EVisitor,
    },
    interpreter::Interpreter,
    stmt::{
        BlockS, BreakS, ClassS, ExprS, FunctionS, IfS, PrintS, ReturnS, Stmt, VarS,
        Visitor as SVisitor, WhileS,
    },
    token::TokenRef,
};

#[derive(Debug)]
pub struct Resolver<'a> {
    interpreter: &'a mut Interpreter,
    scopes: Vec<HashMap<String, bool>>,
    current_function: FunctionType,
    current_class: ClassType,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum FunctionType {
    None,
    Function,
    Initializer,
    Method,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum ClassType {
    None,
    Class,
    Subclass,
}

#[derive(Debug)]
pub struct StaticError;

fn static_error(token: &TokenRef, message: &str) -> StaticError {
    crate::report_token_error(token, message);
    StaticError {}
}

impl<'a> Resolver<'a> {
    pub fn new(interpreter: &'a mut Interpreter) -> Self {
        Resolver {
            interpreter,
            scopes: Vec::new(),
            current_function: FunctionType::None,
            current_class: ClassType::None,
        }
    }

    pub fn resolve(&mut self, stmts: &Vec<Stmt>) -> Result<(), StaticError> {
        stmts.iter().try_for_each(|s| self.resolve_statement(s))
    }

    fn resolve_function(
        &mut self,
        function: &FunctionS,
        function_type: FunctionType,
    ) -> Result<(), StaticError> {
        let enclosing_function = self.current_function;
        self.current_function = function_type;

        self.begin_scope();
        for p in function.params.clone() {
            self.declare(&p)?;
            self.define(&p);
        }
        self.resolve(&function.body)?;
        self.end_scope();

        self.current_function = enclosing_function;
        Ok(())
    }

    fn resolve_statement(&mut self, stmt: &Stmt) -> Result<(), StaticError> {
        stmt.accept(self)
    }

    fn resolve_expression(&mut self, expr: &Expr) -> Result<(), StaticError> {
        expr.accept(self)
    }

    fn begin_scope(&mut self) {
        self.scopes.push(HashMap::new());
    }

    fn end_scope(&mut self) {
        self.scopes.pop();
    }

    fn declare(&mut self, name: &TokenRef) -> Result<(), StaticError> {
        if let Some(scope) = self.scopes.last_mut() {
            if scope.contains_key(&name.lexeme) {
                return Err(static_error(
                    name,
                    "Already a variable with this name in this scope.",
                ));
            }
            scope.insert(name.lexeme.clone(), false);
        }
        Ok(())
    }

    fn define(&mut self, name: &TokenRef) {
        if let Some(scope) = self.scopes.last_mut() {
            scope.insert(name.lexeme.clone(), true);
        }
    }

    fn resolve_local(&mut self, expr: &Expr, name: &TokenRef) -> Result<(), StaticError> {
        for (i, s) in self.scopes.iter().enumerate().rev() {
            if s.contains_key(&name.lexeme) {
                self.interpreter
                    .resolve(expr.clone(), self.scopes.len() - 1 - i);
                return Ok(());
            }
        }
        Ok(())
    }
}

impl<'a> EVisitor<Result<(), StaticError>> for Resolver<'a> {
    fn visit_expr(&mut self, expr: &Expr) -> Result<(), StaticError> {
        match expr {
            Expr::A(AssignE { name, value, .. }) => {
                self.resolve_expression(value)?;
                self.resolve_local(expr, name)?;
                Ok(())
            }
            Expr::B(BinaryE {
                left, op: _, right, ..
            }) => {
                self.resolve_expression(left)?;
                self.resolve_expression(right)?;
                Ok(())
            }
            Expr::C(CallE {
                callee,
                paren: _,
                args,
                ..
            }) => {
                self.resolve_expression(callee)?;
                args.iter().try_for_each(|a| self.resolve_expression(a))?;
                Ok(())
            }
            Expr::G(GetE {
                object, name: _, ..
            }) => self.resolve_expression(object),
            Expr::Gr(GroupingE { expr, .. }) => self.resolve_expression(expr),
            Expr::Li(LiteralE { value: _, .. }) => Ok(()),
            Expr::Lo(LogicalE {
                left, op: _, right, ..
            }) => {
                self.resolve_expression(left)?;
                self.resolve_expression(right)?;
                Ok(())
            }
            Expr::S(SetE {
                object,
                name: _,
                value,
                ..
            }) => {
                self.resolve_expression(value)?;
                self.resolve_expression(object)?;
                Ok(())
            }
            Expr::Su(SuperE {
                keyword, method: _, ..
            }) => {
                if self.current_class == ClassType::None {
                    return Err(static_error(
                        keyword,
                        "Can't use 'super' outside of a class.",
                    ));
                } else if self.current_class != ClassType::Subclass {
                    return Err(static_error(
                        keyword,
                        "Can't use 'super' in a class with no superclass.",
                    ));
                }
                self.resolve_local(expr, keyword)
            }
            Expr::T(ThisE { keyword, .. }) => match self.current_class {
                ClassType::None => Err(static_error(
                    keyword,
                    "Can't use 'this' outside of a class.",
                )),
                _ => self.resolve_local(expr, keyword),
            },
            Expr::U(UnaryE { op: _, expr, .. }) => self.resolve_expression(expr),
            Expr::V(VariableE { name, .. }) => {
                if let Some(scope) = self.scopes.last() {
                    if scope.get(&name.lexeme) == Some(&false) {
                        return Err(static_error(
                            name,
                            "Can't read local variable in its own initializer.",
                        ));
                    }
                }
                self.resolve_local(expr, name)?;
                Ok(())
            }
        }
    }
}

impl<'a> SVisitor<Result<(), StaticError>> for Resolver<'a> {
    fn visit_stmt(&mut self, stmt: &Stmt) -> Result<(), StaticError> {
        match stmt {
            Stmt::B(BlockS { stmts }) => {
                self.begin_scope();
                self.resolve(stmts)?;
                self.end_scope();
                Ok(())
            }
            Stmt::Br(BreakS { keyword: _ }) => Ok(()),
            Stmt::C(ClassS {
                name,
                superclass,
                methods,
            }) => {
                let enclosing_class = self.current_class;
                self.current_class = ClassType::Class;

                self.declare(name)?;
                self.define(name);

                if let Some(s) = superclass {
                    self.current_class = ClassType::Subclass;
                    if s.name.lexeme == name.lexeme {
                        return Err(static_error(&s.name, "A class can't inherit from itself."));
                    }
                    self.resolve_expression(&Expr::V(s.clone()))?;

                    self.begin_scope();
                    if let Some(s) = self.scopes.last_mut() {
                        s.insert("super".to_owned(), true);
                    }
                }

                self.begin_scope();
                if let Some(s) = self.scopes.last_mut() {
                    s.insert("this".to_owned(), true);
                }
                methods.iter().try_for_each(|m| {
                    self.resolve_function(
                        m,
                        if m.name.lexeme == "init" {
                            FunctionType::Initializer
                        } else {
                            FunctionType::Method
                        },
                    )
                })?;
                self.end_scope();

                if let Some(_) = superclass {
                    self.end_scope();
                }

                self.current_class = enclosing_class;
                Ok(())
            }
            Stmt::E(ExprS { expr }) => self.resolve_expression(expr),
            Stmt::F(f) => {
                self.declare(&f.name)?;
                self.define(&f.name);

                self.resolve_function(f, FunctionType::Function)?;
                Ok(())
            }
            Stmt::I(IfS {
                condition,
                then_branch,
                else_branch,
            }) => {
                self.resolve_expression(condition)?;
                self.resolve_statement(then_branch)?;
                if let Some(else_stmt) = else_branch {
                    self.resolve_statement(else_stmt)?;
                }
                Ok(())
            }
            Stmt::P(PrintS { expr }) => self.resolve_expression(expr),
            Stmt::R(ReturnS { keyword, value }) => {
                if self.current_function == FunctionType::None {
                    return Err(static_error(keyword, "Can't return from top-level code."));
                }
                if let Some(expr) = value {
                    if self.current_function == FunctionType::Initializer {
                        return Err(static_error(
                            keyword,
                            "Can't return a value from an initializer.",
                        ));
                    }
                    self.resolve_expression(expr)?;
                }
                Ok(())
            }
            Stmt::V(VarS { name, initializer }) => {
                self.declare(name)?;
                if let Some(init) = initializer {
                    self.resolve_expression(init)?;
                }
                self.define(name);
                Ok(())
            }
            Stmt::W(WhileS { condition, body }) => {
                self.resolve_expression(condition)?;
                self.resolve_statement(body)?;
                Ok(())
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{
        expr::{Expr, VariableE},
        stmt::{Stmt, VarS},
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
    fn test_resolve_variable_declaration() {
        let mut interpreter = Interpreter::new(false);
        let mut resolver = Resolver::new(&mut interpreter);

        let var_name = create_token(TokenType::Identifier, "x", TokenLiteral::Null, 1);
        let stmt = Stmt::V(VarS {
            name: var_name.clone(),
            initializer: None,
        });

        let stmts = vec![stmt];
        let result = resolver.resolve(&stmts);

        assert!(result.is_ok());
    }

    #[test]
    fn test_resolve_variable_usage() {
        let mut interpreter = Interpreter::new(false);
        let mut resolver = Resolver::new(&mut interpreter);

        let var_name = create_token(TokenType::Identifier, "x", TokenLiteral::Null, 1);
        let var_decl = Stmt::V(VarS {
            name: var_name.clone(),
            initializer: None,
        });
        let var_usage = Stmt::P(PrintS {
            expr: Expr::V(VariableE::new(var_name.clone())),
        });

        let stmts = vec![var_decl, var_usage];
        let result = resolver.resolve(&stmts);

        assert!(result.is_ok());
    }

    #[test]
    fn test_resolve_function_declaration() {
        let mut interpreter = Interpreter::new(false);
        let mut resolver = Resolver::new(&mut interpreter);

        let func_name = create_token(TokenType::Identifier, "myFunc", TokenLiteral::Null, 1);
        let func_stmt = Stmt::F(FunctionS {
            name: func_name.clone(),
            params: vec![
                create_token(TokenType::Identifier, "a", TokenLiteral::Null, 1),
                create_token(TokenType::Identifier, "b", TokenLiteral::Null, 1),
            ],
            body: vec![Stmt::E(ExprS {
                expr: Expr::V(VariableE::new(create_token(
                    TokenType::Identifier,
                    "a",
                    TokenLiteral::Null,
                    1,
                ))),
            })],
        });

        let stmts = vec![func_stmt];
        let result = resolver.resolve(&stmts);

        assert!(result.is_ok());
    }

    #[test]
    fn test_resolve_class_declaration() {
        let mut interpreter = Interpreter::new(false);
        let mut resolver = Resolver::new(&mut interpreter);

        let class_name = create_token(TokenType::Identifier, "MyClass", TokenLiteral::Null, 1);
        let method_name = create_token(TokenType::Identifier, "method", TokenLiteral::Null, 1);
        let class_stmt = Stmt::C(ClassS {
            name: class_name.clone(),
            superclass: None,
            methods: vec![FunctionS {
                name: method_name.clone(),
                params: vec![],
                body: vec![Stmt::E(ExprS {
                    expr: Expr::T(ThisE::new(create_token(
                        TokenType::This,
                        "this",
                        TokenLiteral::Null,
                        1,
                    ))),
                })],
            }],
        });

        let stmts = vec![class_stmt];
        let result = resolver.resolve(&stmts);

        assert!(result.is_ok());
    }

    #[test]
    fn test_resolve_super_in_subclass() {
        let mut interpreter = Interpreter::new(false);
        let mut resolver = Resolver::new(&mut interpreter);

        let superclass_name =
            create_token(TokenType::Identifier, "SuperClass", TokenLiteral::Null, 1);
        let subclass_name = create_token(TokenType::Identifier, "SubClass", TokenLiteral::Null, 1);
        let method_name = create_token(TokenType::Identifier, "method", TokenLiteral::Null, 1);

        let class_stmt = Stmt::C(ClassS {
            name: subclass_name.clone(),
            superclass: Some(VariableE::new(superclass_name.clone())),
            methods: vec![FunctionS {
                name: method_name.clone(),
                params: vec![],
                body: vec![Stmt::E(ExprS {
                    expr: Expr::Su(SuperE::new(
                        create_token(TokenType::Super, "super", TokenLiteral::Null, 1),
                        create_token(TokenType::Identifier, "method", TokenLiteral::Null, 1),
                    )),
                })],
            }],
        });

        let stmts = vec![class_stmt];
        let result = resolver.resolve(&stmts);

        assert!(result.is_ok());
    }

    #[test]
    fn test_resolve_super_outside_class() {
        let mut interpreter = Interpreter::new(false);
        let mut resolver = Resolver::new(&mut interpreter);

        let stmt = Stmt::E(ExprS {
            expr: Expr::Su(SuperE::new(
                create_token(TokenType::Super, "super", TokenLiteral::Null, 1),
                create_token(TokenType::Identifier, "method", TokenLiteral::Null, 1),
            )),
        });

        let stmts = vec![stmt];
        let result = resolver.resolve(&stmts);

        assert!(result.is_err());
    }
}
