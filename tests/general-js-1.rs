extern crate js_parser;

use js_parser::lexer::token::{Number, Token};
use js_parser::lexer::Lexer;

#[test]
fn simple_js_example() {
    let (unparsed, parsed) = Lexer::lex_tokens(
        r#"
    function a(a1, b1) {
        let a = "ada \n";
        const b = "dada\b\t";
        var e = 1.2;
        var c = 'adad'
    }

    a(1, "ddada");
    "#,
    ).unwrap();
    assert_eq!(
        unparsed.len(),
        0,
        "unparsed should be empty, contains {}",
        unparsed
    );
    assert_eq!(
        parsed,
        vec![
            Token::LF,
            Token::IdentifierName(String::from("function")),
            Token::IdentifierName(String::from("a")),
            Token::LRound,
            Token::IdentifierName(String::from("a1")),
            Token::Comma,
            Token::IdentifierName(String::from("b1")),
            Token::RRound,
            Token::LBrace,
            Token::LF,
            Token::IdentifierName(String::from("let")),
            Token::IdentifierName(String::from("a")),
            Token::Assign,
            Token::StringLiteral(String::from("ada \n")),
            Token::Semicolon,
            Token::LF,
            Token::IdentifierName(String::from("const")),
            Token::IdentifierName(String::from("b")),
            Token::Assign,
            Token::StringLiteral(String::from("dada\u{8}\t")),
            Token::Semicolon,
            Token::LF,
            Token::IdentifierName(String::from("var")),
            Token::IdentifierName(String::from("e")),
            Token::Assign,
            Token::NumericLiteral(Number::new(1, 2, 1, 10)),
            Token::Semicolon,
            Token::LF,
            Token::IdentifierName(String::from("var")),
            Token::IdentifierName(String::from("c")),
            Token::Assign,
            Token::StringLiteral(String::from("adad")),
            Token::LF,
            Token::RBrace,
            Token::LF,
            Token::LF,
            Token::IdentifierName(String::from("a")),
            Token::LRound,
            Token::NumericLiteral(Number::new(1, 0, 1, 10)),
            Token::Comma,
            Token::StringLiteral(String::from("ddada")),
            Token::RRound,
            Token::Semicolon,
            Token::LF,
            Token::EOF
        ]
    );
}