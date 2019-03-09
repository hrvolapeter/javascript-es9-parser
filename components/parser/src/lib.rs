//! Javascript parser with aim to fully support [Ecma script 2019](http://www.ecma-international.org/ecma-262/9.0/index.html#sec-automatic-semicolon-insertion)
//! and [Estree API](https://github.com/estree/estree). One of the core values of project is to represent ECMAScript standart as close as possible,
//! which should improve redability and maintainability of the code.
//!
//! This project is still a work in progress.
//!
//! Lexer carries out transformation of input to linear stream of tokens. Rules
//! for lexer closely resemble [Lexical grammar in ESCMAScript](http://www.ecma-international.org/ecma-262/9.0/index.html#sec-lexical-grammar).
//! Lexer transform n letters of input to exactly 1 token. This allows better
//! speed optimization due to no dynamic memory allocation.
//!
//! To convert string to stream of tokens we can use code simillar to this
//! ```
//! use javascript_lexer::Lexer;
//!
//! static JS: &str = r#"
//! class A extend B {
//!     get a() {}
//! }
//! "#;
//!
//! let tokens = Lexer::lex_tokens(JS).unwrap();
//! ```
//!
//! Parser constructs Abstract syntax tree (AST) from token stream and then
//! checks AST for violation of static semantics rules. Namely *Early errors*
//! which have to be reported before evaluation.
//!
//! Construct AST from token stream
//! ```
//! use javascript_lexer::Lexer;
//! use js_parser::{estree, Parser};
//!
//! static JS: &str = r#"class A extend B {get a() {}}"#;
//!
//! let tokens = Lexer::lex_tokens(JS).unwrap();
//! Parser::ast_tree(tokens, estree::ProgramSourceType::Script);
//! ```
#![feature(rustc_private)]
#![feature(box_patterns)]
#![feature(box_syntax)]
#![feature(const_fn)]
#![recursion_limit = "256"] // Only macros, doesn't affect function calls
#![deny(warnings)]
#![allow(non_snake_case, unused_must_use, dead_code)]

extern crate nom;
extern crate unescape;
#[cfg(test)]
#[macro_use]
extern crate pretty_assertions;
pub extern crate javascript_lexer;

// This module creates AST from stream of tokens. Constructed AST implements
// ESTree standart.
//
// ESTree interface is defined in [estree](parser::estree).
//
// Structs for representation of AST are defined in [node](parser::node).
// AST is constructed in [statement_declaration](parser::statement_declaration).
//
// Code for checking static semantics is in
// [static_semantics](parser::static_semantics).
//
use crate::{estree::*, javascript_lexer::token::Token};

pub mod estree;
pub mod node;
#[macro_use]
mod macros;
mod expression;
mod input_wrapper;
mod script_module;
mod statement_declaration;
pub mod static_semantics;

pub struct Parser;

impl Parser {
    /// Convert tokens to AST
    pub fn ast_tree(tokens: Vec<Token>, ty: ProgramSourceType) -> node::Program {
        let tokens = tokens
            .into_iter()
            .filter(|t| t != &Token::LineTerminator)
            .collect();
        script_module::parse_script(tokens, ty)
    }
}
