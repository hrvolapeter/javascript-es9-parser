use crate::parser::input_wrapper::InputWrapper;
/// Parsing one token by it's name
use nom::types::{Input, *};
use nom::IResult;

macro_rules! is_token (
  ($i:expr, $tag: path) => (
    {
        use nom::Err;

        let input = take!($i, 1)?;
        let res = if let $tag = &(*input.1.inner)[0] {
            Ok((input.0, $tag))
        } else {
            Err(Err::Error(error_position!(
                input.0,
                 ErrorKind::Custom(1)
            )))
        };
        res
    }
  );
);

macro_rules! test (
  ($id: ident, $code:expr, $block: expr, $decl: ident) => (
    #[test]
    fn $id() {
        let res = Parser::ast_tree(
            Lexer::lex_tokens($code)
                .unwrap(),
            estree::ProgramSourceType::Script,
        );
        if let node::ProgramBody::ProgramStatement(ref $decl) = res.get_body()[0] {
            $block
        } else {
            unreachable!();
        }
    }
  );
);

#[inline]
pub fn either<'a, T>(
    res: &IResult<Input<InputWrapper<'a>>, T>,
    or: Input<InputWrapper<'a>>,
) -> Input<InputWrapper<'a>> {
    if let Ok((res, _)) = res {
        return res.clone();
    }
    or
}

#[inline]
pub fn unwrap_or_none<T>(res: IResult<Input<InputWrapper>, T>) -> Option<T> {
    if let Ok((_, res)) = res {
        return Some(res);
    }
    None
}
