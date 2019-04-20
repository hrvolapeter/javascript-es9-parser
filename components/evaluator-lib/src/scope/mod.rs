pub mod var;

use crate::value::ValueData;
use gc::{Gc, GcCell};
use std::{cell::Cell, collections::HashMap, ops::Deref};

type VarMap = HashMap<String, var::Var>;
pub type ScopeWrapper = Gc<GcCell<Scope>>;

#[derive(Trace, Finalize, Debug, Clone)]
/// Defines scope that can hold variables
/// There is a special Global scope which is top parent
pub struct Scope {
    /// Parent to previous scope
    parent: Option<ScopeWrapper>,
    /// To distinguish function scope (true) and block scope (false)
    isolated: bool,
    /// Holds values with name
    context: VarMap,
}

impl Scope {
    pub fn new(parent: Option<ScopeWrapper>, isolated: bool) -> Self {
        Self {
            parent,
            isolated,
            context: Default::default(),
        }
    }

    /// Find variable in scope chain
    /// If variable not found try window object
    pub fn find(&mut self, name: &String) -> var::Var {
        if let Some(var) = self.context.get(name) {
            var.clone()
        } else if let Some(parent) = &self.parent {
            parent.deref().borrow_mut().find(name)
        } else {
            evaluation_error!("Reference error: `{}` is not defined", name);
            // let mut scope = self;
            // while let Some(parent) = scope.parent {
            //     scope = unsafe { &mut *parent };
            // }
            // let win = scope.find(&String::from("window"));
            // let res = win.get().get_field(&name);
            // var::Var::new(var::VarKind::Var, res.clone())
        }
    }

    /// Define var variable in the nearest scope
    /// If defined in global scope add variable to window object
    pub fn var(&mut self, name: String, val: ValueData) -> bool {
        if !self.isolated && self.parent.is_some() {
            return self
                .parent
                .as_ref()
                .unwrap()
                .deref()
                .borrow_mut()
                .var(name, val);
        }

        self.context
            .insert(name.clone(), var::Var::new(var::VarKind::Var, val.clone()));

        if self.parent.is_none() {
            let win = self.find(&String::from("window"));
            if name != "window" {
                win.get()
                    .borrow_mut()
                    .set_field(&name, &Gc::new(GcCell::new(val)));
            }
        }

        true
    }

    /// Define let variable in the current scope
    /// Return false if variable with the name exists
    pub fn _let(&mut self, name: String, val: ValueData) -> bool {
        let variable = self.context.get(&name);
        if let Some(_) = variable {
            false
        } else {
            self.context
                .insert(name, var::Var::new(var::VarKind::Let, val));
            true
        }
    }

    /// Define const variable in the current scope
    /// Return false if variable with the name exists
    pub fn _const(&mut self, name: String, val: ValueData) -> bool {
        let variable = self.context.get(&name);
        if let Some(_) = variable {
            false
        } else {
            self.context
                .insert(name, var::Var::new(var::VarKind::Const, val));
            true
        }
    }
}
