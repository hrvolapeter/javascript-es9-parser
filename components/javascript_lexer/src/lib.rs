//! Lexer module consumes text input and produces linear stream of tokens.
//!
//! Tokens are defined in [token](lexer::token)
// Grammar rules are in [div](lexer::div)
//
// Supporting macro in [macros](lexer::macros)
#![feature(const_fn)]
#![feature(box_syntax, pattern)]
#![deny(warnings)]

#[cfg(test)]
#[macro_use]
extern crate pretty_assertions;
use std::str;

pub mod token;
#[macro_use]
mod macros;
mod equivalence;
pub mod error;
pub mod internship {
    extern crate internship;
    pub use internship::*;
}
mod state;
mod state_machine;
use self::{state_machine::parse, token::*};

pub struct Lexer;

impl Lexer {
    pub fn lex_tokens(s: &str) -> Result<Vec<Token>, error::Error> {
        let mut tokens = parse(s)?;
        tokens.push(Token::EOF);
        Ok(tokens)
    }
}
