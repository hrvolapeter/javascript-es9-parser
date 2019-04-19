use crate::{
    js_parser::node,
    scope::Scope,
    value::{Completion, CompletionType, ValueData},
    Interpreter,
};
use std::ops::Deref;

pub fn _if(node: &node::IfStatement, scope: &mut Scope) -> Completion {
    let test: bool = Interpreter::run_node(&node.test.clone().into(), scope)
        .value
        .unwrap()
        .deref()
        .into();
    if test {
        Interpreter::run_node(&node.consequent.deref().clone().into(), scope)
    } else if let Some(alt) = &node.alternate {
        Interpreter::run_node(&alt.deref().clone().into(), scope)
    } else {
        Completion::normal_empty()
    }
}

// 13.2.13 https://tc39.github.io/ecma262/#prod-BlockStatement
pub fn block(node: &node::BlockStatement, scope: &mut Scope) -> Completion {
    let mut inner_scope = Scope::new(Some(scope as *mut Scope), false);
    let mut last = ValueData::Undefined;
    for stmt in &node.body {
        let res = Interpreter::run_node(&stmt.clone().into(), &mut inner_scope);
        if res.ty != CompletionType::Normal {
            return res;
        }
        if let Some(val) = res.value {
            let res = val.deref().clone();
            if res != ValueData::Undefined {
                last = res;
            }
        }
    }
    Completion::normal(last)
}

// 13.7.3 https://tc39.github.io/ecma262/#sec-while-statement
pub fn _while(node: &node::WhileStatement, scope: &mut Scope) -> Completion {
    let mut last = ValueData::Undefined;
    while Interpreter::run_node(&node.test.clone().into(), scope)
        .value
        .unwrap()
        .deref()
        .into()
    {
        let res = Interpreter::run_node(&node.body.deref().clone().into(), scope);
        if res.ty != CompletionType::Normal {
            return res;
        }
        let res = res.value.unwrap().deref().clone();
        if res != ValueData::Undefined {
            last = res;
        }
    }
    Completion::normal(last)
}

// 13.7.4 https://tc39.github.io/ecma262/#sec-for-statement
pub fn _for(node: &node::ForStatement, scope: &mut Scope) -> Completion {
    let inner_scope = &mut Scope::new(Some(scope as *mut Scope), false);
    if let Some(init) = &node.init {
        match init {
            node::ForStatementInit::VariableDeclaration(v) => {
                Interpreter::run_node(&node::Node::VariableDeclaration(v.clone()), inner_scope)
            }
            node::ForStatementInit::Expression(v) => {
                Interpreter::run_node(&v.clone().into(), inner_scope)
            }
        };
    }
    let mut last = ValueData::Undefined;
    while {
        // If test is present evaluate test otherwise infinite loop
        if let Some(test) = &node.test {
            Interpreter::run_node(&test.clone().into(), inner_scope)
                .value
                .unwrap()
                .deref()
                .into()
        } else {
            true
        }
    } {
        let res = Interpreter::run_node(&node.body.deref().clone().into(), inner_scope);
        if res.ty != CompletionType::Normal {
            return res;
        }
        let res = res.value.unwrap().deref().clone();
        if res != ValueData::Undefined {
            last = res;
        }
        // Update after evaluation
        if let Some(updt) = &node.update {
            Interpreter::run_node(&updt.clone().into(), inner_scope);
        }
    }
    Completion::normal(last)
}
