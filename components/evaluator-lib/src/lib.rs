#![feature(box_syntax)]

extern crate gc;
extern crate js_parser;
#[macro_use]
extern crate gc_derive;

#[macro_use]
mod error;
mod console;
mod declaration;
mod expression;
mod function;
mod object;
mod scope;
mod statement;
mod value;

use crate::value::{Completion, CompletionType, ValueData};
use gc::Gc;
use js_parser::node;
use scope::Scope;
use std::ops::Deref;

/// An execution engine
pub trait Executor {
    /// Make a new execution engine
    fn new() -> Self;
    /// Run an expression
    fn run(&mut self, expr: &node::Program);
}

/// A Javascript intepreter
#[derive(Debug)]
pub struct Interpreter {
    scope: Scope,
}

impl Interpreter {
    fn run_node(node: &node::Node, scope: &mut Scope) -> Completion {
        match node {
            node::Node::NumberLiteral(num) => Completion::normal(ValueData::Number(num.0.into())),
            node::Node::StringLiteral(s) => Completion::normal(s.into()),
            node::Node::BooleanLiteral(b) => Completion::normal(b.into()),
            node::Node::NullLiteral(_) => Completion::normal(ValueData::Null),
            node::Node::ExpressionStatement(stmt) => {
                Self::run_node(&stmt.expression.as_ref().clone().into(), scope)
            }
            node::Node::VariableDeclaration(decl) => declaration::variable(decl, scope),
            node::Node::CallExpression(expr) => expression::call_expression(expr, scope),
            node::Node::Identifier(ident) => {
                // Hack because lexer is not recognizing undefined as a keyword
                if ident.name == "undefined" {
                    return Completion::normal(ValueData::Undefined);
                }
                Completion::normal(scope.find(&ident.name.to_string()).get().deref().clone())
            }
            node::Node::ObjectExpression(expr) => expression::object_expression(expr, scope),
            node::Node::ArrayExpression(expr) => expression::array_expression(expr, scope),
            node::Node::AssignmentExpression(expr) => expression::assignment(expr, scope),
            node::Node::FunctionDeclaration(decl) => declaration::function(decl, scope),
            node::Node::ReturnStatement(stmt) => {
                if stmt.argument.is_none() {
                    return Completion::return_empty();
                }
                Self::run_node(&stmt.argument.as_ref().unwrap().clone().into(), scope).returned()
            }
            node::Node::FunctionBody(decl) => {
                for node::FunctionBodyEnum::Statement(stmt) in &decl.body {
                    let res = Self::run_node(&stmt.clone().into(), scope);
                    if res.ty != CompletionType::Normal {
                        return res;
                    }
                }
                Completion::normal_empty()
            }
            node::Node::IfStatement(stmt) => statement::_if(stmt, scope),
            node::Node::BlockStatement(stmt) => statement::block(stmt, scope),
            node::Node::WhileStatement(stmt) => statement::_while(stmt, scope),
            node::Node::BinaryExpression(expr) => expression::binary(expr, scope),
            node::Node::UnaryExpression(expr) => expression::unary(expr, scope),
            node::Node::LogicalExpression(expr) => expression::logical(expr, scope),
            a @ _ => panic!("Unsupported node {:?}", a),
        }
    }
}

impl Executor for Interpreter {
    fn new() -> Self {
        let mut scope = Scope::new(None, false);
        let window = Gc::new(ValueData::Object(Default::default()));
        scope.var(String::from("window"), window.clone());
        scope._let(String::from("this"), window.clone());
        scope.var(String::from("console"), console::init());
        Interpreter { scope }
    }

    fn run(&mut self, program: &node::Program) {
        for body in &program.body {
            if let node::ProgramBody::ProgramStatement(stmt) = body {
                Self::run_node(&stmt.clone().into(), &mut self.scope);
            }
        }
    }
}
