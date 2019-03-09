extern crate js_evaluator;
extern crate js_parser;

use js_evaluator::{Executor, Interpreter};
use js_parser::{estree::ProgramSourceType, javascript_lexer::Lexer, Parser};
use std::{env, fs::File, io::prelude::*};

fn main() -> std::result::Result<(), std::io::Error> {
    let mut file = File::open(env::args().nth(1).unwrap())?;
    let mut contents = String::new();
    file.read_to_string(&mut contents)?;

    let tokens = Lexer::lex_tokens(&contents).expect("Lexing successfull");
    let ast = Parser::ast_tree(tokens, ProgramSourceType::Script);
    Interpreter::new().run(&ast);
    Ok(())
}
