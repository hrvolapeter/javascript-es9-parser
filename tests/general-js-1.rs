extern crate js_parser;

use js_parser::lexer::token::{Number, Token, UnicodeEscapeSequence};
use js_parser::lexer::Lexer;

#[test]
fn test_js() {
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
            Token::SP,
            Token::SP,
            Token::SP,
            Token::SP,
            Token::IdentifierName(vec![
                UnicodeEscapeSequence::Letter('f'),
                UnicodeEscapeSequence::Letter('u'),
                UnicodeEscapeSequence::Letter('n'),
                UnicodeEscapeSequence::Letter('c'),
                UnicodeEscapeSequence::Letter('t'),
                UnicodeEscapeSequence::Letter('i'),
                UnicodeEscapeSequence::Letter('o'),
                UnicodeEscapeSequence::Letter('n')
            ]),
            Token::SP,
            Token::IdentifierName(vec![UnicodeEscapeSequence::Letter('a'),]),
            Token::LRound,
            Token::IdentifierName(vec![
                UnicodeEscapeSequence::Letter('a'),
                UnicodeEscapeSequence::Letter('1'),
            ]),
            Token::Comma,
            Token::SP,
            Token::IdentifierName(vec![
                UnicodeEscapeSequence::Letter('b'),
                UnicodeEscapeSequence::Letter('1'),
            ]),
            Token::RRound,
            Token::SP,
            Token::LBrace,
            Token::LF,
            Token::SP,
            Token::SP,
            Token::SP,
            Token::SP,
            Token::SP,
            Token::SP,
            Token::SP,
            Token::SP,
            Token::IdentifierName(vec![
                UnicodeEscapeSequence::Letter('l'),
                UnicodeEscapeSequence::Letter('e'),
                UnicodeEscapeSequence::Letter('t'),
            ]),
            Token::SP,
            Token::IdentifierName(vec![UnicodeEscapeSequence::Letter('a'),]),
            Token::SP,
            Token::Assign,
            Token::SP,
            Token::StringLiteral(String::from("ada \n")),
            Token::Semicolon,
            Token::LF,
            Token::SP,
            Token::SP,
            Token::SP,
            Token::SP,
            Token::SP,
            Token::SP,
            Token::SP,
            Token::SP,
            Token::IdentifierName(vec![
                UnicodeEscapeSequence::Letter('c'),
                UnicodeEscapeSequence::Letter('o'),
                UnicodeEscapeSequence::Letter('n'),
                UnicodeEscapeSequence::Letter('s'),
                UnicodeEscapeSequence::Letter('t'),
            ]),
            Token::SP,
            Token::IdentifierName(vec![UnicodeEscapeSequence::Letter('b'),]),
            Token::SP,
            Token::Assign,
            Token::SP,
            Token::StringLiteral(String::from("dada\u{8}\t")),
            Token::Semicolon,
            Token::LF,
            Token::SP,
            Token::SP,
            Token::SP,
            Token::SP,
            Token::SP,
            Token::SP,
            Token::SP,
            Token::SP,
            Token::IdentifierName(vec![
                UnicodeEscapeSequence::Letter('v'),
                UnicodeEscapeSequence::Letter('a'),
                UnicodeEscapeSequence::Letter('r'),
            ]),
            Token::SP,
            Token::IdentifierName(vec![UnicodeEscapeSequence::Letter('e'),]),
            Token::SP,
            Token::Assign,
            Token::SP,
            Token::NumericLiteral(Number::new(1, 2, 1, 10)),
            Token::Semicolon,
            Token::LF,
            Token::SP,
            Token::SP,
            Token::SP,
            Token::SP,
            Token::SP,
            Token::SP,
            Token::SP,
            Token::SP,
            Token::IdentifierName(vec![
                UnicodeEscapeSequence::Letter('v'),
                UnicodeEscapeSequence::Letter('a'),
                UnicodeEscapeSequence::Letter('r'),
            ]),
            Token::SP,
            Token::IdentifierName(vec![UnicodeEscapeSequence::Letter('c'),]),
            Token::SP,
            Token::Assign,
            Token::SP,
            Token::StringLiteral(String::from("adad")),
            Token::LF,
            Token::SP,
            Token::SP,
            Token::SP,
            Token::SP,
            Token::RBrace,
            Token::LF,
            Token::LF,
            Token::SP,
            Token::SP,
            Token::SP,
            Token::SP,
            Token::IdentifierName(vec![UnicodeEscapeSequence::Letter('a'),]),
            Token::LRound,
            Token::NumericLiteral(Number::new(1, 0, 1, 10)),
            Token::Comma,
            Token::SP,
            Token::StringLiteral(String::from("ddada")),
            Token::RRound,
            Token::Semicolon,
            Token::LF,
            Token::SP,
            Token::SP,
            Token::SP,
            Token::SP,
            Token::EOF
        ]
    );
}
