//! This module creates AST from stream of tokens. Constructed AST implements
//! ESTree standart.
//!
//! ESTree interface is defined in [estree](parser::estree).
//!
//! Structs for representation of AST are defined in [node](parser::node).
// AST is constructed in [statement_declaration](parser::statement_declaration).
//
// Code for checking static semantics is in
// [static_semantics](parser::static_semantics).
//
#![allow(warnings)]
use crate::{
    javascript_lexer::token::Token,
    parser::{estree::*, input_wrapper::InputWrapper},
};
use nom::{types::*, *};

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
        script_module::parse_script(tokens, ty)
    }
}