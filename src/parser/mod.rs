#![allow(warnings)]
use crate::lexer::token::Token;
use crate::parser::estree::*;
use crate::parser::input_wrapper::InputWrapper;
use nom::types::*;
use nom::*;

pub mod estree;
pub mod node;
#[macro_use]
mod macros;
mod input_wrapper;
mod script_module;
mod statement_declaration;

pub struct Parser;

impl Parser {
    pub fn ast_tree(tokens: Vec<Token>, ty: ProgramSourceType) -> Box<estree::Program> {
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
