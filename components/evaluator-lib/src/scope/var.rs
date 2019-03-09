use crate::value::Value;

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
    pub fn new(kind: VarKind, value: Value) -> Self {
        Self { kind, value }
    }

    pub fn get(&self) -> Value {
        self.value.clone()
    }

    pub fn set(&mut self, value: Value) {
        if self.kind == VarKind::Const {
            evaluation_error!("Assignment to constant variable {:?}", value);
        }
        self.value = value;
    }
}
