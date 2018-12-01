#![feature(test)]

extern crate js_parser;
extern crate test;

use self::test::Bencher;

use js_parser::{
    lexer::{
        token::{Number, Token},
        Lexer,
    },
    parser::{estree, Parser},
};

static JS: &str = r#"
        class A extend B {
            get a() {
                a;
            }

            get b() {
                b;
            }

            set b(c) {
                c;
            }

            k (a, b) {
                var a = b || c;
                try {
                    debug;
                } catch (a) {

                } finally {

                }
            }
        }
        "#;

#[bench]
fn react_build_script(b: &mut Bencher) {
    b.iter(|| {
        test::black_box({
            let lexer = Lexer::lex_tokens(JS).unwrap();
            Parser::ast_tree(lexer, estree::ProgramSourceType::Script);
        })
    });
}
