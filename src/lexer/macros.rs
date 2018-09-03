#[macro_export]
macro_rules! named_js {
    ($name: ident: $($rest:tt)*) => {
        named!(pub $name<CompleteStr, Token>, alt_longest!(
            $($rest)*
        ));
    };
}

#[macro_export]
macro_rules! named_token {
    ($name: ident, $expr: expr, $type: expr) => {
        named!($name<CompleteStr, Token>,
               do_parse!(tag!($expr) >> ($type))
        );
    };

}

#[macro_export]
macro_rules! should {
    ($name: ident, $left:expr, $right: expr) => {
        #[cfg(test)]
        #[test]
        fn $name() {
            use crate::lexer::Lexer;

            let input = &$left[..];
            let (unparsed, result) = Lexer::lex_tokens(input).unwrap();

            let expected_results = $right;
            assert_eq!(
                unparsed,
                CompleteStr(&""[..]),
                "Input was not parsed completly, unparsed: {}",
                unparsed
            );
            assert_eq!(result, expected_results);
        }
    };
}

#[macro_export]
macro_rules! should_ignore {
    ($name: ident, $left:expr, $right: expr) => {
        #[cfg(test)]
        #[test]
        #[ignore]
        fn $name() {
            use crate::lexer::Lexer;

            let input = &$left[..];
            let (unparsed, result) = Lexer::lex_tokens(input).unwrap();

            let expected_results = $right;
            assert_eq!(
                unparsed,
                CompleteStr(&""[..]),
                "Input was not parsed completly, unparsed: {}",
                unparsed
            );
            assert_eq!(result, expected_results);
        }
    };
}

#[macro_export]
macro_rules! should_fail {
    ($name: ident, $left:expr, $right: expr) => {
        #[cfg(test)]
        #[test]
        #[should_panic]
        fn $name() {
            use crate::lexer::Lexer;

            let input = &$left[..];
            let (unparsed, result) = Lexer::lex_tokens(input).unwrap();

            let expected_results = $right;
            assert_eq!(
                unparsed,
                CompleteStr(&""[..]),
                "Input was not parsed completly, unparsed: {}",
                unparsed
            );
            assert_eq!(result, expected_results);
        }
    };
}

#[macro_export]
macro_rules! should_incomplete {
    ($name: ident, $left:expr) => {
        #[cfg(test)]
        #[test]
        #[should_panic]
        fn $name() {
            use crate::lexer::Lexer;

            let input = &$left[..];
            let (unparsed, _result) = Lexer::lex_tokens(input).unwrap();

            assert_eq!(
                unparsed.len(),
                0,
                "Input was not parsed completly, unparsed: {}",
                unparsed
            );
        }
    };
}

#[macro_export]
macro_rules! alt_longest {
  (__impl $i:expr, $e:path, $($rest:tt)* ) => (
    alt_longest!(__impl $i, call!($e) , $($rest)*);
  );
  (__impl $i:expr, $e:path | $($rest:tt)*) => (
    alt_longest!(__impl $i, call!($e) | $($rest)*);
  );

  (__impl $i:expr, $subrule:ident!( $($args:tt)*) | $($rest:tt)*) => (
    {
      use nom::lib::std::result::Result::*;
      use nom::Err;

      fn unify_types<T>(_: &T, _: &T) {}

      let i_ = $i.clone();
      let res = $subrule!(i_, $($args)*);
      let out = alt_longest!(__impl $i, $($rest)*);
      match res {
        Ok(o) => {
            if let Ok(i) = out {
                unify_types(&i, &o);
                if i.0.len() < o.0.len() {
                    return Ok(i);
                }
            }
            Ok(o)
        },
        Err(Err::Error(e))      => {

          // Compile-time hack to ensure that res's E type is not under-specified.
          // This all has no effect at runtime.
          if let Err(Err::Error(ref e2)) = out {
            unify_types(&e, e2);
          }

          out
        },
        Err(e) => Err(e),
      }
    }
  );

  (__impl $i:expr, __end) => (
    {
      use nom::{Err,ErrorKind};
      let e2 = ErrorKind::Alt;
      let err = Err::Error(error_position!($i, e2));

      Err(err)
    }
  );

  ($i:expr, $($rest:tt)*) => (
    {
      alt_longest!(__impl $i, $($rest)* | __end)
    }
  );
}

#[macro_export]
macro_rules! named_token_unicode {
    ($name: ident, $expr: expr, $type: expr) => {
        #[inline]
        fn $name(input: CompleteStr) -> IResult<CompleteStr, Token> {
            let mut chars = input.chars();
            if let Some(c) = chars.next() {
                if c as u32 == $expr {
                    return Ok((CompleteStr(chars.as_str()), $type));
                }
            }
            Err(Err::Error(Context::Code(input, ErrorKind::Tag)))
        }
    };
}
