extern crate js_parser;
#[cfg(test)]
#[macro_use]
extern crate pretty_assertions;

use js_parser::lexer::{
    token::{Number, Token},
    Lexer,
};

#[test]
fn simple_js_example() {
    let parsed = Lexer::lex_tokens(
        r#"
    function a(a1, b1) {
        let a = "ada \n";
        const b = "dada\b\t";
        var e, f, g = 1.2;
        var c = 'adad'
    }

    a(1, "ddada");
    "#,
    )
    .unwrap();
    assert_eq!(
        parsed,
        vec![
            Token::KFunction,
            Token::IdentifierName(String::from("a")),
            Token::LRound,
            Token::IdentifierName(String::from("a1")),
            Token::Comma,
            Token::IdentifierName(String::from("b1")),
            Token::RRound,
            Token::LBrace,
            Token::KLet,
            Token::IdentifierName(String::from("a")),
            Token::Assign,
            Token::StringLiteral(String::from("ada \n")),
            Token::Semicolon,
            Token::KConst,
            Token::IdentifierName(String::from("b")),
            Token::Assign,
            Token::StringLiteral(String::from("dada\u{8}\t")),
            Token::Semicolon,
            Token::KVar,
            Token::IdentifierName(String::from("e")),
            Token::Comma,
            Token::IdentifierName(String::from("f")),
            Token::Comma,
            Token::IdentifierName(String::from("g")),
            Token::Assign,
            Token::NumericLiteral(Number::new(1, 2, 1, 10)),
            Token::Semicolon,
            Token::KVar,
            Token::IdentifierName(String::from("c")),
            Token::Assign,
            Token::StringLiteral(String::from("adad")),
            Token::RBrace,
            Token::IdentifierName(String::from("a")),
            Token::LRound,
            Token::NumericLiteral(Number::new(1, 0, 1, 10)),
            Token::Comma,
            Token::StringLiteral(String::from("ddada")),
            Token::RRound,
            Token::Semicolon,
            Token::EOF
        ]
    );
}
