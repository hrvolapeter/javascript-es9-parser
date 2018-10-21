#[macro_export]
macro_rules! is_token (
  ($i:expr, $tag: path) => (
    {
        let input = take!($i, 1)?;
        let res = if let $tag = &(*input.1.inner)[0] {
            Ok(input)
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
