use crate::{
    function::{Function, NativeFunction},
    value::{Value, ValueData},
};
use gc::{Gc, GcCell};
use std::io::{self, BufRead};

// Initialize console object with native functions
pub fn init() -> Value {
    let console = ValueData::Object(Default::default());
    console.set_field(
        &String::from("log"),
        &Gc::new(ValueData::Function(GcCell::new(Function::NativeFunc(
            NativeFunction::new(log),
        )))),
    );
    console.set_field(
        &String::from("readline"),
        &Gc::new(ValueData::Function(GcCell::new(Function::NativeFunc(
            NativeFunction::new(readline),
        )))),
    );
    Gc::new(console)
}

/// Add console.log(...) function that prints directly to std
/// Accepts unbounded number of arguments, arguments are separated by space
pub fn log(_: Value, _: Value, args: Vec<Value>) -> Value {
    let mut first = true;
    for a in args {
        if !first {
            print!(" ");
        }
        first = false;
        print!("{}", a);
    }
    print!("\n");
    Gc::new(ValueData::Undefined)
}

/// Add console.readline() function to read one line from std
/// Function return garbage collected string representation of line
pub fn readline(_: Value, _: Value, _: Vec<Value>) -> Value {
    let mut line = String::new();
    let stdin = io::stdin();
    stdin.lock().read_line(&mut line).unwrap();
    Gc::new(ValueData::String(format!("{:?}", line)))
}
