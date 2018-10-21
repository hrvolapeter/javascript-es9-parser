use crate::lexer::token::Token;
use crate::parser::estree;
use crate::parser::estree::ProgramSourceType;
use crate::parser::input_wrapper::InputWrapper;
use crate::parser::node;
use crate::parser::statement_declaration::ScriptBody;
use nom::types::Input;

pub fn parse_script(tokens: Vec<Token>, ty: ProgramSourceType) -> Box<estree::Program> {
    match ty {
        ProgramSourceType::Script => Box::new(node::Program {
            sourceType: ty,
            body: parse_program(tokens),
        }),
        _ => unimplemented!(),
    }
}

fn parse_program(tokens: Vec<Token>) -> Vec<estree::ProgramBody> {
    let input = Input {
        inner: InputWrapper(&tokens),
        at_eof: true,
    };
    let res = ScriptBody(input).unwrap();
    if res.0.inner.len() != 1 {
        eprintln!("Unparsed tokens: {:?}", res.0.inner);
        unreachable!();
    }

    res.1
}
