#![feature(rustc_private)]
#![feature(box_patterns)]
#![feature(box_syntax)]
#![recursion_limit = "512"]
#![deny(warnings)]
#![feature(try_from)]

extern crate nom;
extern crate unescape;
#[cfg(test)] // <-- not needed in examples + integration tests
#[macro_use]
extern crate pretty_assertions;
#[macro_use]
extern crate mopa;

pub mod lexer;
pub mod parser;
