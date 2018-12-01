//! Javascript parser with aim to fully support [Ecma script 2019](http://www.ecma-international.org/ecma-262/9.0/index.html#sec-automatic-semicolon-insertion)
//! and [Estree API](https://github.com/estree/estree). One of the core values of project is to represent ECMAScript standart as close as possible,
//! which should improve redability and maintainability of the code.
//!
//! This project is still a work in progress.
//!
//! Project is organized in two parts:
//! - [lexer](lexer)
//! - [parser](parser)
//!
//! Lexer carries out transformation of input to linear stream of tokens. Rules
//! for lexer closely resemble [Lexical grammar in ESCMAScript](http://www.ecma-international.org/ecma-262/9.0/index.html#sec-lexical-grammar).
//! Lexer transform n letters of input to exactly 1 token. This allows better
//! speed optimization due to no dynamic memory allocation.
//!
//! To convert string to stream of tokens we can use code simillar to this
//! ```
//! use js_parser::lexer::Lexer;
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
//! use js_parser::{
//!     lexer::Lexer,
//!     parser::{estree, Parser},
//! };
//!
//! static JS: &str = r#"
//! class A extend B {
//!     get a() {}
//! }
//! "#;
//!
//! let tokens = Lexer::lex_tokens(JS).unwrap();
//! Parser::ast_tree(tokens, estree::ProgramSourceType::Script);
//! ```
#![feature(rustc_private)]
#![feature(box_patterns)]
#![feature(box_syntax)]
#![recursion_limit = "256"] // Only for macros, not function calls
#![deny(warnings)]

extern crate nom;
extern crate unescape;
#[cfg(test)]
#[macro_use]
extern crate pretty_assertions;

pub mod lexer;
pub mod parser;
