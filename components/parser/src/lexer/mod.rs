//! Lexer module consumes text input and produces linear stream of tokens.
//!
//! Tokens are defined in [token](lexer::token)
//!
// Grammar rules are in [div](lexer::div)
//
// Supporting macro in [macros](lexer::macros)
//
#![allow(non_snake_case, non_camel_case_types, non_upper_case_globals)]

use nom::{types::*, *};
use std::str;

pub mod token;
#[macro_use]
mod macros;
mod div;
pub mod error;
use self::{div::InputElementDiv, token::*};

pub(crate) type ParseResult<'a> = IResult<CompleteStr<'a>, Token>;

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
    pub fn lex_tokens(s: &str) -> Result<Vec<Token>, error::Error> {
        let tokens = lex_tokens(CompleteStr(s));
        if let Ok(ref tok) = tokens {
            if tok.0.len() > 0 {
                #[cfg(debug)]
                eprintln!("Unparsed tokens: {:?}", tok.0);
                return Err(error::Error::ParsingIncomplete);
            }
        } else {
            return Err(error::Error::InternalError);
        };

        // Spaces can be discarded by ECMASript standard
        // New lines can be discareded after semicolon insertion
        let tokens = tokens
            .unwrap()
            .1
            .into_iter()
            .filter(|t| t != &Token::NOP && t != &Token::SP && t != &Token::LF)
            .collect();
        Ok([tokens, vec![Token::EOF]].concat())
    }
}
