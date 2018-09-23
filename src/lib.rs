#![feature(rustc_private)]
#![recursion_limit = "256"]
#![deny(warnings)]

extern crate nom;
extern crate unescape;

pub mod lexer;
pub mod parser;
