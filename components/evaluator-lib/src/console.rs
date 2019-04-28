use crate::{
    function::{Function, NativeFunction},
    value::{Value, ValueData},
};
use gc::{Gc, GcCell};
use std::io::{self, BufRead};

/// Create object with native functions of console
pub fn init() -> ValueData {
    let console = ValueData::Object(Default::default());
    console.set_field(
        &String::from("log"),
        &Gc::new(GcCell::new(ValueData::Function(GcCell::new(
            Function::NativeFunc(log),
        )))),
    );
    console.set_field(
        &String::from("readline"),
        &Gc::new(GcCell::new(ValueData::Function(GcCell::new(
            Function::NativeFunc(readline),
        )))),
    );
    console
}

/// Add console.log(...) function that prints directly to std
/// Accepts unbounded number of arguments, arguments are separated by space
pub fn log(args: Vec<Value>) -> Value {
    let mut first = true;
    for a in args {
        if !first {
            print!(" ");
        }
        first = false;
        print!("{}", *a.borrow_mut());
    }
    print!("\n");
    Gc::new(GcCell::new(ValueData::Undefined))
}

/// Add console.readline() function to read one line from std
/// Function return garbage collected string representation of line
pub fn readline(_: Vec<Value>) -> Value {
    let mut line = String::new();
    let stdin = io::stdin();
    stdin.lock().read_line(&mut line).unwrap();
    Gc::new(GcCell::new(ValueData::String(format!("{:?}", line))))
}
