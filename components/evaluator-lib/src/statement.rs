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
    let mut last = ValueData::Undefined;
    for stmt in &node.body {
        let res = Interpreter::run_node(&stmt.clone().into(), scope);
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

// 13.7.3 https://tc39.github.io/ecma262/#sec-while-statement
pub fn _while(node: &node::WhileStatement, scope: &mut Scope) -> Completion {
    let mut last = ValueData::Undefined;
    while Interpreter::run_node(&node.test.clone().into(), scope)
        .value
        .unwrap()
        .deref()
        .into()
    {
        let res = Interpreter::run_node(&node.body.deref().clone().into(), scope)
            .value
            .unwrap()
            .deref()
            .clone();
        if res != ValueData::Undefined {
            last = res;
        }
    }
    Completion::normal(last)
}
