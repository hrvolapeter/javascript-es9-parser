use crate::{
    js_parser::node,
    scope::{Scope, ScopeWrapper},
    value::{Completion, CompletionType, ValueData},
    Interpreter,
};
use gc::{Gc, GcCell};
use std::ops::Deref;

pub fn _if(node: &node::IfStatement, scope: ScopeWrapper) -> Completion {
    let test: bool = Interpreter::run_node(&node.test.clone().into(), scope.clone())
        .value
        .unwrap()
        .deref()
        .borrow_mut()
        .deref()
        .into();
    if test {
        Interpreter::run_node(&node.consequent.deref().clone().into(), scope.clone())
    } else if let Some(alt) = &node.alternate {
        Interpreter::run_node(&alt.deref().clone().into(), scope.clone())
    } else {
        Completion::normal_empty()
    }
}

// 13.2.13 https://tc39.github.io/ecma262/#prod-BlockStatement
pub fn block(node: &node::BlockStatement, scope: ScopeWrapper) -> Completion {
    let mut inner_scope = Gc::new(GcCell::new(Scope::new(Some(scope.clone()), false)));
    let mut last = ValueData::Undefined;
    for stmt in &node.body {
        let res = Interpreter::run_node(&stmt.clone().into(), inner_scope.clone());
        if res.ty != CompletionType::Normal {
            return res;
        }
        if let Some(val) = res.value {
            let res = val.deref().borrow_mut().clone();
            if res != ValueData::Undefined {
                last = res;
            }
        }
    }
    Completion::normal(last)
}

// 13.7.3 https://tc39.github.io/ecma262/#sec-while-statement
pub fn _while(node: &node::WhileStatement, scope: ScopeWrapper) -> Completion {
    let mut last = ValueData::Undefined;
    while Interpreter::run_node(&node.test.clone().into(), scope.clone())
        .value
        .unwrap()
        .deref()
        .borrow_mut()
        .deref()
        .into()
    {
        let res = Interpreter::run_node(&node.body.deref().clone().into(), scope.clone());
        if res.ty != CompletionType::Normal {
            return res;
        }
        let res = res.value.unwrap().deref().borrow_mut().clone();
        if res != ValueData::Undefined {
            last = res;
        }
    }
    Completion::normal(last)
}

// 13.7.4 https://tc39.github.io/ecma262/#sec-for-statement
pub fn _for(node: &node::ForStatement, scope: ScopeWrapper) -> Completion {
    let inner_scope = Gc::new(GcCell::new(Scope::new(Some(scope.clone()), false)));
    if let Some(init) = &node.init {
        match init {
            node::ForStatementInit::VariableDeclaration(v) => Interpreter::run_node(
                &node::Node::VariableDeclaration(v.clone()),
                inner_scope.clone(),
            ),
            node::ForStatementInit::Expression(v) => {
                Interpreter::run_node(&v.clone().into(), inner_scope.clone())
            }
        };
    }
    let mut last = ValueData::Undefined;
    while {
        // If test is present evaluate test otherwise infinite loop
        if let Some(test) = &node.test {
            Interpreter::run_node(&test.clone().into(), inner_scope.clone())
                .value
                .unwrap()
                .deref()
                .borrow()
                .deref()
                .into()
        } else {
            true
        }
    } {
        let res = Interpreter::run_node(&node.body.deref().clone().into(), inner_scope.clone());
        if res.ty != CompletionType::Normal {
            return res;
        }
        let res = res.value.unwrap().deref().borrow().deref().clone();
        if res != ValueData::Undefined {
            last = res;
        }
        // Update after evaluation
        if let Some(updt) = &node.update {
            Interpreter::run_node(&updt.clone().into(), inner_scope.clone());
        }
    }
    Completion::normal(last)
}
