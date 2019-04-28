extern crate js_evaluator;
extern crate js_parser;

use js_evaluator::Interpreter;
use js_parser::{estree::ProgramSourceType, javascript_lexer::Lexer, Parser};
use std::{env, fs::File, io::prelude::Read};

fn main() -> std::result::Result<(), std::io::Error> {
    let mut file = File::open(env::args().nth(1).unwrap())?;
    let mut contents = String::new();
    file.read_to_string(&mut contents)?;

    let tokens = Lexer::lex_tokens(&contents).expect("Lexing successfull");
    let ast = Parser::ast_tree(tokens, ProgramSourceType::Script);
    // Catch explicit panic and change error codes because of testing
    // Requirements posed by libarries / rust:
    // 1. panic exists with `101` code (not possible to change)
    // 2. testing expects and checks for `1` code in error state (not possible to
    // change) 3. when exiting Gc need to unwind stack and call dtor, rust
    // offers short exit process::exit which    doesn't call destructor when
    // unwiding stack but calls at_exit and performs basic cleanup
    //    Gc sometimes crashes during basic cleanup.
    //
    // Solution: use `panic!` which performs stack undiwinding catch unwinding after
    // Gc is freed and exit with `1` error code
    let res = ::std::panic::catch_unwind(|| Interpreter::new().run(&ast));
    if res.is_err() {
        ::std::process::exit(1);
    }
    Ok(())
}
