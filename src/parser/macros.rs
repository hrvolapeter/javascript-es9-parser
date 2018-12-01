/// Parsing one token by it's name
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
