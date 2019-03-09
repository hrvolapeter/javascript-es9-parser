#![feature(rustc_attrs, test)]

extern crate js_parser;
#[cfg(test)]
#[macro_use]
extern crate pretty_assertions;
extern crate javascript_lexer;
extern crate test;
extern crate test262_parser_tests;
extern crate timebomb;
use javascript_lexer::{error, Lexer};
use js_parser::{estree::ProgramSourceType, Parser};
use std::{
    fs::File,
    io::{self, Read},
};
use test262_parser_tests::{test_early_js, test_explicit_js, test_fail_js, test_pass_js};
use timebomb::timeout_ms;

// test_pass_js!();
// test_fail_js!();
// test_early_js!();
// test_explicit_js!();

fn read_file(path: &str) -> Result<String, io::Error> {
    let file = File::open(path)?;
    let mut buf_reader = io::BufReader::new(file);
    let mut contents = String::new();
    buf_reader.read_to_string(&mut contents)?;
    Ok(contents)
}

fn parse(js: &str) -> Result<(), error::Error> {
    let res = Lexer::lex_tokens(js)?;
    Parser::ast_tree(res, ProgramSourceType::Script);
    Ok(())
}
