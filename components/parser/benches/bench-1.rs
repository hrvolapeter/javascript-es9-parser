#![feature(test)]

extern crate js_parser;
extern crate test;

use self::test::Bencher;

use js_parser::{
    estree,
    javascript_lexer::{
        token::{Number, Token},
        Lexer,
    },
    Parser,
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

        var a = function () {
            var b = new A;
        };
        "#;

#[bench]
fn react_build_script(b: &mut Bencher) {
    let mut s = String::new();
    for _ in 1..100 {
        s += JS;
    }
    b.iter(|| {
        test::black_box({
            let lexer = Lexer::lex_tokens(&s).unwrap();
            Parser::ast_tree(lexer, estree::ProgramSourceType::Script);
        })
    });
}
