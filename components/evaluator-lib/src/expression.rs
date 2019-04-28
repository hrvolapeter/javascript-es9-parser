use crate::{
    function::Function,
    js_parser::node::{self, CallExpression},
    scope::{Scope, ScopeWrapper},
    value::{Completion, CompletionType, ValueData},
    Interpreter,
};
use gc::{Gc, GcCell};
use std::ops::Deref;

pub fn call(node: &CallExpression, scope: ScopeWrapper) -> Completion {
    let args: Vec<_> = node
        .arguments
        .iter()
        .map(|a| {
            Interpreter::run_node(&(*a).clone().into(), scope.clone())
                .value
                .unwrap()
        })
        .collect();

    // Build identifier for error reporting
    fn get_identifier(node: &node::Expression) -> String {
        if let node::Expression::MemberExpression(mem) = node {
            if let node::Expression::Identifier(ident) = &*mem.object {
                return format!("{}.{}", ident.name, mem.property.name);
            }
        }
        if let node::Expression::Identifier(ident) = node {
            return format!("{}", ident.name);
        }
        panic!("Unexpected error when building error message, {:?}", node);
    }

    let func = Interpreter::run_node(&(*node.callee).clone().into(), scope.clone())
        .value
        .unwrap();
    let func = &*func.deref().borrow();

    match func {
        ValueData::Function(func) => match &*func.borrow() {
            Function::RegularFunc(func) => {
                for i in 0..func.params.len() {
                    let arg = Interpreter::run_node(
                        &node.arguments.get(i).unwrap().clone().into(),
                        scope.clone(),
                    )
                    .value
                    .unwrap();
                    func.scope.deref().borrow_mut().var(
                        func.params.get(i).unwrap().clone(),
                        arg.borrow_mut().deref().clone(),
                    );
                }
                Interpreter::run_node(&func.expr.0.clone().into(), func.scope.clone())
            }
            Function::NativeFunc(nat) => {
                Completion::normal((nat)(args).deref().borrow_mut().clone())
            }
        },
        _ => evaluation_error!("`{}` is not callable", get_identifier(&*node.callee)),
    }
}

pub fn member(node: &node::MemberExpression, scope: ScopeWrapper) -> Completion {
    let obj = Interpreter::run_node(&(*node.object).clone().into(), scope);
    let key = if node.computed {
        unimplemented!("Computed member not supported")
    } else {
        &node.property.name
    };
    Completion::normal_gc(
        obj.value
            .unwrap()
            .borrow_mut()
            .get_prop(&key.to_string())
            .unwrap()
            .value
            .clone(),
    )
}

pub fn object(node: &node::ObjectExpression, scope: ScopeWrapper) -> Completion {
    let obj = ValueData::Object(Default::default());
    for property in &node.properties {
        if let node::ObjectExpressionProperty::Property(prop) = property {
            let key = if let node::Expression::Identifier(ident) = &prop.key {
                &ident.name
            } else {
                unimplemented!("Only Identifier key is supported")
            };
            let value = Interpreter::run_node(&prop.value.clone().into(), scope.clone())
                .value
                .unwrap();
            obj.set_field(&key.to_string(), &value);
        } else {
            unimplemented!("Unsupported object property")
        }
    }
    Completion::normal(obj)
}

pub fn array(node: &node::ArrayExpression, scope: ScopeWrapper) -> Completion {
    let obj = ValueData::Object(Default::default());
    let mut i = 0;
    for element in &node.elements {
        if let Some(node::ArrayExpressionElement::Expression(expr)) = element {
            let value = Interpreter::run_node(&expr.clone().into(), scope.clone())
                .value
                .unwrap();
            obj.set_field(&i.to_string(), &value);
            i += 1;
        } else {
            unimplemented!("Unsupported array property")
        }
    }
    Completion::normal(obj)
}

pub fn assignment(node: &node::AssignmentExpression, scope: ScopeWrapper) -> Completion {
    use js_parser::estree::AssignmentOperator;
    let key = if let node::Pattern::Identifier(ident) = &*node.left {
        &ident.name
    } else {
        unimplemented!("Only identifier assignemnt is supported")
    };
    let val = Interpreter::run_node(&(*node.right).clone().into(), scope.clone())
        .value
        .as_ref()
        .unwrap()
        .deref()
        .borrow_mut()
        .clone();
    let var = scope.deref().borrow_mut().find(&key.to_string());
    let res = var.get().deref().borrow_mut().clone();
    let res = match node.operator {
        AssignmentOperator::PlusEqual => res + val,
        AssignmentOperator::MinusEqual => res - val,
        AssignmentOperator::Equal => val,
        _ => unimplemented!("Unsupported assignment operator `{:?}`", node.operator),
    };
    var.set(res.clone());
    Completion::normal(res)
}

pub fn binary(node: &node::BinaryExpression, scope: ScopeWrapper) -> Completion {
    use js_parser::estree::BinaryOperator;
    let left = Interpreter::run_node(&(*node.left).clone().into(), scope.clone())
        .value
        .unwrap();
    let right = Interpreter::run_node(&(*node.right).clone().into(), scope)
        .value
        .unwrap();
    match &node.operator {
        BinaryOperator::EqualEqualEqual => {
            Completion::normal(ValueData::Boolean(left.deref() == right.deref()))
        }
        BinaryOperator::ExclamationEqualEqual => {
            Completion::normal(ValueData::Boolean(left.deref() != right.deref()))
        }
        BinaryOperator::Less => {
            Completion::normal(ValueData::Boolean(left.deref() < right.deref()))
        }
        BinaryOperator::LessEqual => {
            Completion::normal(ValueData::Boolean(left.deref() <= right.deref()))
        }
        BinaryOperator::MoreEqual => {
            Completion::normal(ValueData::Boolean(left.deref() >= right.deref()))
        }
        BinaryOperator::More => {
            Completion::normal(ValueData::Boolean(left.deref() > right.deref()))
        }
        BinaryOperator::Plus => Completion::normal(
            left.deref().borrow_mut().clone() + right.deref().borrow_mut().clone(),
        ),
        BinaryOperator::Minus => Completion::normal(
            left.deref().borrow_mut().clone() - right.deref().borrow_mut().clone(),
        ),
        a @ _ => unimplemented!("Unsuported binary operator `{:?}`", a),
    }
}

pub fn unary(node: &node::UnaryExpression, scope: ScopeWrapper) -> Completion {
    use js_parser::estree::UnaryOperator::*;
    let argument = Interpreter::run_node(&(*node.argument).clone().into(), scope)
        .value
        .unwrap()
        .deref()
        .clone();
    match &node.operator {
        Minus => {
            if let ValueData::Number(num) = &*argument.borrow_mut() {
                let mut num = num.clone();
                num.integer *= -1;
                Completion::normal(ValueData::Number(num))
            } else {
                panic!("NaN");
            }
        }
        _ => unimplemented!("Unsuported unary operator `{:?}`", node.operator),
    }
}

pub fn logical(node: &node::LogicalExpression, scope: ScopeWrapper) -> Completion {
    use js_parser::estree::LogicalOperator::*;
    let left = Interpreter::run_node(&(*node.left).clone().into(), scope.clone())
        .value
        .unwrap();
    let right = Interpreter::run_node(&(*node.right).clone().into(), scope)
        .value
        .unwrap();
    match &node.operator {
        And => Completion::normal(ValueData::Boolean(
            left.deref().borrow_mut().deref().into() && right.deref().borrow_mut().deref().into(),
        )),
        Or => Completion::normal(ValueData::Boolean(
            left.deref().borrow_mut().deref().into() || right.deref().borrow_mut().deref().into(),
        )),
        _ => unimplemented!("Unsuported logical operator `{:?}`", node.operator),
    }
}

pub fn function(node: &node::FunctionExpression, scope: ScopeWrapper) -> Completion {
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
    Completion::normal(ValueData::Function(GcCell::new(Function::RegularFunc(fun))))
}

pub fn function_body(node: &node::FunctionBody, scope: ScopeWrapper) -> Completion {
    for node::FunctionBodyEnum::Statement(stmt) in &node.body {
        let res = Interpreter::run_node(&stmt.clone().into(), scope.clone());
        if res.ty != CompletionType::Normal {
            return res;
        }
    }
    Completion::normal_empty()
}

pub fn arrow_function(node: &node::ArrowFunctionExpression, scope: ScopeWrapper) -> Completion {
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
        expr: Expr(node::Node::ArrowFunctionExpressionBody(node.body.clone())),
        params,
    };
    Completion::normal(ValueData::Function(GcCell::new(Function::RegularFunc(fun))))
}

pub fn arrow_function_body(
    node: &node::ArrowFunctionExpressionBody,
    scope: ScopeWrapper,
) -> Completion {
    match node {
        node::ArrowFunctionExpressionBody::FunctionBody(fun) => {
            Interpreter::run_node(&node::Node::FunctionBody(fun.clone()), scope)
        }
        node::ArrowFunctionExpressionBody::Expression(expr) => {
            Interpreter::run_node(&expr.deref().clone().into(), scope)
        }
    }
}
