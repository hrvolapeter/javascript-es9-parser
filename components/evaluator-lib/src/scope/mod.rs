mod var;

use crate::value::Value;
use std::collections::HashMap;

type VarMap = HashMap<String, var::Var>;

#[derive(Trace, Finalize, Debug, Clone)]
/// Defines scope that can hold variables
/// There is a special Global scope which is top parent
pub struct Scope {
    /// Parent to previous scope
    #[unsafe_ignore_trace]
    parent: Option<*mut Scope>,
    /// To distinguish function scope (true) and block scope (false)
    isolated: bool,
    /// Holds values with name
    context: VarMap,
}

impl Scope {
    pub fn new(parent: Option<*mut Scope>, isolated: bool) -> Self {
        Self {
            parent,
            isolated,
            context: Default::default(),
        }
    }

    /// Find variable in scope chain
    /// If variable not found try window object
    pub fn find(&mut self, name: &String) -> &mut var::Var {
        if let Some(var) = self.context.get_mut(name) {
            var
        } else if let Some(parent) = self.parent {
            unsafe { &mut *parent }.find(name)
        } else {
            evaluation_error!("Reference error: `{}` is not defined", name);
            let mut scope = self;
            while let Some(parent) = scope.parent {
                scope = unsafe { &mut *parent };
            }
            let win = scope.find(&String::from("window"));
            let res = win.get().get_field(&name);
            // var::Var::new(var::VarKind::Var, res.clone())
        }
    }

    /// Define var variable in the nearest scope
    /// If defined in global scope add variable to window object
    pub fn var(&mut self, name: String, val: Value) -> bool {
        let mut scope = self;

        // Find the closest function scope
        while scope.parent.is_some() && !scope.isolated {
            scope = unsafe { &mut *scope.parent.unwrap() };
        }

        scope
            .context
            .insert(name.clone(), var::Var::new(var::VarKind::Var, val.clone()));

        if scope.parent.is_none() {
            let win = scope.find(&String::from("window"));
            if name != "window" {
                win.get().set_field(&name, &val);
            }
        }

        true
    }

    /// Define let variable in the current scope
    /// Return false if variable with the name exists
    pub fn _let(&mut self, name: String, val: Value) -> bool {
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
    pub fn _const(&mut self, name: String, val: Value) -> bool {
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
