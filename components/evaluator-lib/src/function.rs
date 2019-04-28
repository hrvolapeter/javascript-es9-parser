use crate::{object::ObjectData, scope::ScopeWrapper, value::Value};
use gc::{Finalize, Trace};
use js_parser::node;
use std::fmt;

/// fn(arguments)
pub type NativeFunction = fn(Vec<Value>) -> Value;

/// A Javascript function
#[derive(Trace, Finalize, Debug, Clone)]
pub enum Function {
    NativeFunc(NativeFunction),
    RegularFunc(RegularFunction),
}

impl fmt::Display for Function {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Function::NativeFunc(_) => write!(f, "[[nativeMethod]]"),
            Function::RegularFunc(fun) => write!(f, "{:?}", fun),
        }
    }
}

#[derive(Debug, Clone)]
pub struct Expr(pub node::Node);

unsafe impl Trace for Expr {
    gc::unsafe_empty_trace!();
}
impl Finalize for Expr {}

/// Represents a regular javascript function in memory
#[derive(Trace, Finalize, Debug, Clone)]
pub struct RegularFunction {
    /// The fields associated with the function
    pub scope: ScopeWrapper,
    /// This function's expression
    pub expr: Expr,
    /// Name of params for function
    //  Params are added to function scope during call
    pub params: Vec<String>,
}
