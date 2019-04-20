use crate::{
    js_parser::node,
    scope::{Scope, ScopeWrapper},
    value::{Completion, ValueData},
    Interpreter,
};
use gc::{Gc, GcCell};
use std::ops::Deref;

pub fn variable(node: &node::VariableDeclaration, scope: ScopeWrapper) -> Completion {
    use js_parser::estree::VariableDeclarationKind;

    let kind = &node.kind;
    for decl in &node.declarations {
        if let node::Pattern::Identifier(ident) = &decl.id {
            let val = if let Some(init) = &decl.init {
                Interpreter::run_node(&init.clone().into(), scope.clone())
            } else {
                Completion::normal(ValueData::Null)
            };
            let val = val.value.unwrap().deref().borrow_mut().clone();
            let mut scope = scope.deref().borrow_mut();
            let res = match kind {
                VariableDeclarationKind::Let => scope._let(ident.name.to_string(), val),
                VariableDeclarationKind::Var => scope.var(ident.name.to_string(), val),
                VariableDeclarationKind::Const => scope._const(ident.name.to_string(), val),
            };
            if !res {
                evaluation_error!("Redeclaration of let/const variable `{}`", ident.name);
            }
        } else {
            unimplemented!("unsupported variable declaration {:?}", decl.id);
        }
    }
    Completion::normal_empty()
}

pub fn function(node: &node::FunctionDeclaration, scope: ScopeWrapper) -> Completion {
    use crate::function::{Expr, Function, RegularFunction};
    let inner_scope = Gc::new(GcCell::new(Scope::new(Some(scope.clone()), true)));
    let mut params = vec![];
    for param in &node.params {
        if let node::Pattern::Identifier(ident) = param {
            params.push(ident.name.to_string())
        }
    }
    let fun = RegularFunction {
        scope: inner_scope,
        expr: Expr(node::Node::FunctionBody(node.body.clone())),
        params,
    };
    scope.deref().borrow_mut().var(
        node.id.as_ref().unwrap().name.to_string(),
        ValueData::Function(GcCell::new(Function::RegularFunc(fun))),
    );
    Completion::normal_empty()
}
