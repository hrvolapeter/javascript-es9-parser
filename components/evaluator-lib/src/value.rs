use crate::{function::Function, object::ObjectData};
use gc::{Gc, GcCell};
use js_parser::{javascript_lexer::token, node};
use std::{
    cmp::Ordering,
    fmt,
    ops::{Add, Sub},
};

// 6.2.3 https://tc39.github.io/ecma262/#sec-completion-record-specification-type
#[derive(Debug)]
pub struct Completion {
    pub ty: CompletionType,
    pub value: Option<Value>,
    target: Option<String>,
}

impl Completion {
    pub fn normal(value: ValueData) -> Self {
        Self {
            ty: CompletionType::Normal,
            value: Some(Gc::new(GcCell::new(value))),
            target: None,
        }
    }

    pub fn normal_gc(value: Value) -> Self {
        Self {
            ty: CompletionType::Normal,
            value: Some(value),
            target: None,
        }
    }

    pub fn normal_empty() -> Self {
        Self {
            ty: CompletionType::Normal,
            value: None,
            target: None,
        }
    }

    pub fn return_empty() -> Self {
        Self {
            ty: CompletionType::Return,
            value: Some(Gc::new(GcCell::new(ValueData::Undefined))),
            target: None,
        }
    }

    pub fn returned(self) -> Self {
        Self {
            ty: CompletionType::Return,
            ..self
        }
    }

    pub fn empty(value: ValueData) -> Self {
        Self {
            ty: CompletionType::Return,
            value: Some(Gc::new(GcCell::new(value))),
            target: None,
        }
    }
}

#[derive(PartialEq, Debug)]
pub enum CompletionType {
    Normal,
    Break,
    Continue,
    Return,
    Throw,
}

/// A Garbage-collected Javascript value as represented in the interpreter
pub type Value = Gc<GcCell<ValueData>>;

/// A Javascript value
#[derive(Trace, Finalize, Debug, Clone)]
pub enum ValueData {
    Null,
    Undefined,
    Boolean(bool),
    String(String),
    Number(Number),
    Object(GcCell<ObjectData>),
    Function(GcCell<Function>),
}

impl PartialEq for ValueData {
    fn eq(&self, other: &Self) -> bool {
        use ValueData::*;
        match (self, other) {
            (Number(fst), Number(snd)) => fst.integer == snd.integer && fst.base == snd.base,
            (String(fst), String(snd)) => fst == snd,
            (Boolean(fst), Boolean(snd)) => fst == snd,
            (Null, Null) | (Undefined, Undefined) => true,
            _ => false,
        }
    }
}

impl PartialOrd for ValueData {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        use ValueData::*;
        match (self, other) {
            (Number(fst), Number(snd)) => fst.integer.partial_cmp(&snd.integer),
            (String(fst), String(snd)) => unimplemented!("Missing type coerrection"),
            (Boolean(true), Boolean(false)) => Some(Ordering::Greater),
            (Boolean(false), Boolean(true)) => Some(Ordering::Less),
            (Null, Null) | (Undefined, Undefined) => Some(Ordering::Equal),
            _ => None,
        }
    }
}

impl Add for ValueData {
    type Output = Self;

    fn add(self, other: Self) -> Self {
        use ValueData::*;
        match (&self, &other) {
            (Number(fst), Number(snd)) => ValueData::Number(fst.clone() + snd.clone()),
            _ => unimplemented!(),
        }
    }
}

impl Sub for ValueData {
    type Output = Self;

    fn sub(self, other: Self) -> Self {
        use ValueData::*;
        match (&self, &other) {
            (Number(fst), Number(snd)) => ValueData::Number(fst.clone() - snd.clone()),
            _ => unimplemented!(),
        }
    }
}

impl From<i32> for ValueData {
    fn from(num: i32) -> Self {
        ValueData::Number(Number {
            integer: num,
            decimal: 0,
            exponent: 1,
            base: 10,
        })
    }
}

#[derive(Trace, Finalize, PartialEq, Debug, Clone)]
pub struct Number {
    pub integer: i32,
    pub decimal: u32,
    pub exponent: i64,
    pub base: u8,
}

impl Add for Number {
    type Output = Self;

    fn add(self, other: Self) -> Self {
        assert_eq!(self.exponent, other.exponent); // Nope
        Number {
            integer: self.integer + other.integer,
            decimal: self.decimal + other.decimal,
            exponent: self.exponent,
            base: self.base,
        }
    }
}

impl Sub for Number {
    type Output = Self;

    fn sub(self, other: Self) -> Self {
        assert_eq!(self.exponent, other.exponent); // Nope
        Number {
            integer: self.integer - other.integer,
            decimal: self.decimal - other.decimal,
            exponent: self.exponent,
            base: self.base,
        }
    }
}

impl fmt::Display for Number {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let mut res;
        if self.decimal > 0 {
            res = format!("{}.{}", self.integer, self.decimal);
        } else {
            res = format!("{}", self.integer);
        }
        if self.exponent != 1 {
            res += &format!("e{}", self.exponent);
        }
        write!(f, "{}", res)
    }
}

impl From<token::Number> for Number {
    fn from(num: token::Number) -> Self {
        Self {
            integer: num.integer as i32,
            decimal: num.decimal,
            exponent: num.exponent,
            base: num.base,
        }
    }
}

impl From<&node::StringLiteral> for ValueData {
    fn from(s: &node::StringLiteral) -> Self {
        ValueData::String(s.0.clone())
    }
}

impl From<&node::BooleanLiteral> for ValueData {
    fn from(s: &node::BooleanLiteral) -> Self {
        ValueData::Boolean(s.0.clone())
    }
}

impl fmt::Display for ValueData {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            ValueData::Null => write!(f, "null"),
            ValueData::Undefined => write!(f, "undefined"),
            ValueData::Boolean(b) => write!(f, "{}", b),
            ValueData::String(s) => write!(f, "\"{}\"", s),
            ValueData::Number(n) => write!(f, "{}", n),
            ValueData::Object(o) => {
                let mut res = String::from("{");
                let mut first = true;
                let mut keys = o
                    .borrow()
                    .keys()
                    .map(|x| x.clone())
                    .collect::<Vec<String>>();
                keys.sort_by(|a, b| a.cmp(b));
                for key in keys.iter() {
                    if !first {
                        res += ", ";
                    }
                    first = false;
                    res += &format!("{}: {}", key, o.borrow().get(key).unwrap());
                }
                res += "}";
                write!(f, "{}", res)
            }
            ValueData::Function(fu) => write!(f, "{}", &*fu.borrow()),
        }
    }
}

// 7.1.2 https://tc39.github.io/ecma262/#sec-toboolean
impl Into<bool> for &ValueData {
    fn into(self) -> bool {
        use ValueData::*;
        match self {
            Null => false,
            Undefined => false,
            Boolean(b) => *b,
            Number(n) => n.integer != 0,
            String(s) => s.len() != 0,
            Object(_) | Function(_) => true,
        }
    }
}
