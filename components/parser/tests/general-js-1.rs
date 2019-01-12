extern crate js_parser;
#[cfg(test)]
#[macro_use]
extern crate pretty_assertions;

use javascript_lexer::{
    internship,
    token::{Number, Token},
    Lexer,
};

#[test]
fn simple_js_example() {
    let parsed = Lexer::lex_tokens(
        "
    function a(a1, b1) {
        let a = \"adan \\n\";
        const b = \"dada\\b\\t\";
        var e, f, g = 1.2;
        var c = 'adad'
    }

    a(1, \"ddada\");
    ",
    )
    .unwrap();
    assert_eq!(
        parsed,
        vec![
            Token::LineTerminator,
            Token::KFunction,
            Token::IdentifierName(internship::IStr::new("a")),
            Token::LRound,
            Token::IdentifierName(internship::IStr::new("a1")),
            Token::Comma,
            Token::IdentifierName(internship::IStr::new("b1")),
            Token::RRound,
            Token::LCurly,
            Token::LineTerminator,
            Token::KLet,
            Token::IdentifierName(internship::IStr::new("a")),
            Token::Assign,
            Token::StringLiteral(String::from("adan \n")),
            Token::Semicolon,
            Token::LineTerminator,
            Token::KConst,
            Token::IdentifierName(internship::IStr::new("b")),
            Token::Assign,
            Token::StringLiteral(String::from("dada\u{8}\t")),
            Token::Semicolon,
            Token::LineTerminator,
            Token::KVar,
            Token::IdentifierName(internship::IStr::new("e")),
            Token::Comma,
            Token::IdentifierName(internship::IStr::new("f")),
            Token::Comma,
            Token::IdentifierName(internship::IStr::new("g")),
            Token::Assign,
            Token::NumericLiteral(Number::new(1, 2, 1, 10)),
            Token::Semicolon,
            Token::LineTerminator,
            Token::KVar,
            Token::IdentifierName(internship::IStr::new("c")),
            Token::Assign,
            Token::StringLiteral(String::from("adad")),
            Token::LineTerminator,
            Token::RCurly,
            Token::LineTerminator,
            Token::LineTerminator,
            Token::IdentifierName(internship::IStr::new("a")),
            Token::LRound,
            Token::NumericLiteral(Number::new(1, 0, 1, 10)),
            Token::Comma,
            Token::StringLiteral(String::from("ddada")),
            Token::RRound,
            Token::Semicolon,
            Token::LineTerminator,
            Token::EOF
        ]
    );
}
