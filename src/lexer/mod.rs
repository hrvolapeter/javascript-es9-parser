#![allow(non_snake_case, non_camel_case_types, non_upper_case_globals)]

use nom::types::*;
use nom::*;
use std::str;

pub mod token;
#[macro_use]
mod macros;
mod div;
use self::div::InputElementDiv;
use self::token::*;

pub(crate) type ParseResult<'a> = IResult<CompleteStr<'a>, Token>;

// Illegal tokens
named!(lex_illegal<CompleteStr, Token>,
    do_parse!(take!(1) >> (Token::Illegal))
);

named!(lex_token<CompleteStr, Token>, alt_complete!(InputElementDiv));

named!(lex_tokens<CompleteStr, Vec<Token>>, do_parse!(
    tokens: many0!(lex_token) >>
    (tokens)
));

pub struct Lexer;

impl Lexer {
    pub fn lex_tokens(s: &str) -> IResult<CompleteStr, Vec<Token>> {
        let mut tokens = lex_tokens(CompleteStr(s))?;
        tokens.1 = tokens.1.into_iter().filter(|t| t != &Token::NOP).collect();
        Ok((tokens.0, [tokens.1, vec![Token::EOF]].concat()))
    }
}
