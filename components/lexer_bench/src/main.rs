extern crate javascript_lexer;

use javascript_lexer::Lexer;

use std::alloc::System;

#[global_allocator]
static GLOBAL: System = System;

static JS: &str = include!("core.umd.js");
fn react_build_script() {
    Lexer::lex_tokens(JS).unwrap();
}

fn main() {
    react_build_script();
}
