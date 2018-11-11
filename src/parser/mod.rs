#![allow(warnings)]
use crate::{
    lexer::token::Token,
    parser::{estree::*, input_wrapper::InputWrapper},
};
use nom::{types::*, *};

pub mod estree;
pub mod node;
#[macro_use]
mod macros;
mod input_wrapper;
mod script_module;
mod statement_declaration;

pub struct Parser;

impl Parser {
    pub fn ast_tree(tokens: Vec<Token>, ty: ProgramSourceType) -> node::Program {
        script_module::parse_script(tokens, ty)
    }
}

// named_ast!(
//     Declaration: HoistableDeclaration,
//     ClassDeclaration,
//     LexicalDeclaration
// );

// named_ast!(
//     HoistableDeclaration: FunctionDeclaration,
//     GeneratorDeclaration,
//     AsyncFunctionDeclaration,
//     AsyncGeneratorDeclaration,
// );
