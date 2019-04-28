use crate::value::{Value, ValueData};
use gc::{Gc, GcCell};
use std::ops::Deref;

#[derive(PartialEq, Clone, Trace, Finalize, Debug)]
pub enum VarKind {
    Var,
    Let,
    Const,
}

#[derive(Trace, Finalize, Debug, Clone)]
pub struct Var {
    kind: VarKind,
    value: Value,
}

impl Var {
    pub fn new(kind: VarKind, value: ValueData) -> Self {
        Self {
            kind,
            value: Gc::new(GcCell::new(value)),
        }
    }

    pub fn new_value(kind: VarKind, value: Value) -> Self {
        Self { kind, value }
    }

    pub fn get(&self) -> Value {
        self.value.clone()
    }

    pub fn set(&self, value: ValueData) {
        if self.kind == VarKind::Const {
            evaluation_error!("Assignment to constant variable {:?}", value);
        }
        *self.value.deref().borrow_mut() = value;
    }
}
