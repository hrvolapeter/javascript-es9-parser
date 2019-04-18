use crate::{
    function::Function,
    js_parser::node::{self, CallExpression},
    scope::Scope,
    value::{Completion, ValueData},
    Interpreter,
};
use gc::Gc;
use std::ops::Deref;

pub fn call_expression(node: &CallExpression, scope: &mut Scope) -> Completion {
    let args: Vec<_> = node
        .arguments
        .iter()
        .map(|a| {
            Interpreter::run_node(&(*a).clone().into(), scope)
                .value
                .unwrap()
        })
        .collect();
    if let node::Expression::MemberExpression(mem) = &*node.callee {
        let object = member_expression(&mem, scope).value.unwrap();
        // get key
        let key = if mem.computed {
            panic!("Computed not supported")
        } else {
            &mem.property.name
        };

        let dat = object.get_prop(&key.to_string());
        if let ValueData::Function(func) = &*dat.unwrap().value {
            if let Function::NativeFunc(nat) = &*func.borrow() {
                return Completion::normal(
                    (nat.data)(
                        Gc::new(ValueData::Undefined),
                        Gc::new(ValueData::Undefined),
                        args,
                    )
                    .deref()
                    .clone(),
                );
            }
        }
    } else {
        let func = Interpreter::run_node(&(*node.callee).clone().into(), scope)
            .value
            .unwrap();
        if let ValueData::Function(func) = &*func {
            if let Function::RegularFunc(func) = &mut *func.borrow_mut() {
                for i in 0..func.params.len() {
                    let arg = Interpreter::run_node(
                        &node.arguments.get(i).unwrap().clone().into(),
                        scope,
                    )
                    .value
                    .unwrap();
                    func.scope.var(func.params.get(i).unwrap().clone(), arg);
                }
                return Interpreter::run_node(&func.expr.0.clone().into(), &mut func.scope);
            }
        }
    }
    unreachable!()
}

pub fn member_expression(node: &node::MemberExpression, scope: &mut Scope) -> Completion {
    Interpreter::run_node(&(*node.object).clone().into(), scope)
}

pub fn object_expression(node: &node::ObjectExpression, scope: &mut Scope) -> Completion {
    let obj = ValueData::Object(Default::default());
    for property in &node.properties {
        if let node::ObjectExpressionProperty::Property(prop) = property {
            let key = if let node::Expression::Identifier(ident) = &prop.key {
                &ident.name
            } else {
                panic!("Only Identifier key is supported")
            };
            let value = Interpreter::run_node(&prop.value.clone().into(), scope)
                .value
                .unwrap();
            obj.set_field(&key.to_string(), &value);
        } else {
            panic!("Unsupported object property")
        }
    }
    Completion::normal(obj)
}

pub fn array_expression(node: &node::ArrayExpression, scope: &mut Scope) -> Completion {
    let obj = ValueData::Object(Default::default());
    let mut i = 0;
    for element in &node.elements {
        if let Some(node::ArrayExpressionElement::Expression(expr)) = element {
            let value = Interpreter::run_node(&expr.clone().into(), scope)
                .value
                .unwrap();
            obj.set_field(&i.to_string(), &value);
            i += 1;
        } else {
            panic!("Unsupported array property")
        }
    }
    Completion::normal(obj)
}

pub fn assignment(node: &node::AssignmentExpression, scope: &mut Scope) -> Completion {
    use js_parser::estree::AssignmentOperator;
    let key = if let node::Pattern::Identifier(ident) = &*node.left {
        &ident.name
    } else {
        panic!("Only identifier assignemnt is supported")
    };
    let val = Interpreter::run_node(&(*node.right).clone().into(), scope)
        .value
        .as_ref()
        .unwrap()
        .deref()
        .clone();
    let var = scope.find(&key.to_string());
    let res = var.get().deref().clone();
    let res = match node.operator {
        AssignmentOperator::PlusEqual => res + val,
        AssignmentOperator::MinusEqual => res - val,
        AssignmentOperator::Equal => val,
        _ => unimplemented!(),
    };
    var.set(Gc::new(res.clone()));
    Completion::normal(res)
}

pub fn binary(node: &node::BinaryExpression, scope: &mut Scope) -> Completion {
    use js_parser::estree::BinaryOperator;
    let left = Interpreter::run_node(&(*node.left).clone().into(), scope)
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
        _ => panic!("Unsuported operator"),
    }
}

pub fn unary(node: &node::UnaryExpression, scope: &mut Scope) -> Completion {
    use js_parser::estree::UnaryOperator::*;
    let argument = Interpreter::run_node(&(*node.argument).clone().into(), scope)
        .value
        .unwrap()
        .deref()
        .clone();
    match &node.operator {
        Minus => {
            if let ValueData::Number(num) = &argument {
                let mut num = num.clone();
                num.integer *= -1;
                Completion::normal(ValueData::Number(num))
            } else {
                panic!("NaN");
            }
        }
        _ => panic!("Unsuported operator"),
    }
}

pub fn logical(node: &node::LogicalExpression, scope: &mut Scope) -> Completion {
    use js_parser::estree::LogicalOperator::*;
    let left = Interpreter::run_node(&(*node.left).clone().into(), scope)
        .value
        .unwrap();
    let right = Interpreter::run_node(&(*node.right).clone().into(), scope)
        .value
        .unwrap();
    match &node.operator {
        And => Completion::normal(ValueData::Boolean(
            left.deref().into() && right.deref().into(),
        )),
        Or => Completion::normal(ValueData::Boolean(
            left.deref().into() || right.deref().into(),
        )),

        _ => panic!("Unsuported operator"),
    }
}
