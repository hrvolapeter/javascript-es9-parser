use crate::{object::ObjectData, scope::Scope, value::Value};
use gc::{Finalize, Trace};
use js_parser::node;
use std::fmt;

/// fn(this, callee, arguments)
pub type NativeFunctionData = fn(Value, Value, Vec<Value>) -> Value;

/// A Javascript function
#[derive(Trace, Finalize, Debug, Clone)]
pub enum Function {
    /// A native javascript function
    NativeFunc(NativeFunction),
    /// A regular javascript function
    RegularFunc(RegularFunction),
}

impl fmt::Display for Function {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Function::NativeFunc(_) => write!(f, "[[nativeMethod]]"),
            Function::RegularFunc(fu) => write!(f, "{:?}", fu),
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
    pub scope: Scope,
    /// This function's expression
    pub expr: Expr,
    /// Name of params for function
    //  Params are added to function scope during call
    pub params: Vec<String>,
}

#[derive(Trace, Finalize, Debug, Clone)]
/// Represents a native javascript function in memory
pub struct NativeFunction {
    /// The fields associated with the function
    pub object: ObjectData,
    /// The callable function data
    pub data: NativeFunctionData,
}
impl NativeFunction {
    /// Make a new native function with the given function data
    pub fn new(data: NativeFunctionData) -> Self {
        Self {
            object: Default::default(),
            data: data,
        }
    }
}
