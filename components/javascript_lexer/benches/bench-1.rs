#![feature(test)]

extern crate javascript_lexer;
extern crate test;

use self::test::Bencher;

use javascript_lexer::Lexer;

static JS: &str = r#"
class A extend B{get a(){a}
get b(){b}
set b(c){c}
k(a,b){var a=b||c;try{debug}catch(a){}finally{}}}
var a=function(){var b=new A}
        "#;

#[bench]
fn react_build_script(b: &mut Bencher) {
    let mut s = String::new();
    for _ in 1..1000 {
        s += JS;
    }
    println!("Bytes {}", s.bytes().len());
    b.iter(|| {
        test::black_box({
            Lexer::lex_tokens(&s[..]).unwrap();
        })
    });
}
