#[cfg(test)]
macro_rules! should {
    ($name:ident, $left:expr, $right:expr) => {
        #[test]
        fn $name() {
            use crate::Lexer;

            let input = &$left[..];
            let result = Lexer::lex_tokens(input).unwrap();
            assert_eq!(result, $right);
        }
    };
}

macro_rules! Accept {
    ($name:ident) => {
        #[derive(Debug)]
        pub struct $name;
        impl State for $name {
            fn is_final(&self) -> bool {
                true
            }
        }
    };
}

macro_rules! State {
    ($name:ident) => {
        #[derive(Debug)]
        pub struct $name;
        impl State for $name {
            fn is_final(&self) -> bool {
                false
            }
        }
    };
}

macro_rules! Edge {
    ($from:ty, $to:ident) => {
        impl From<StateMachine<$from>> for StateMachine<$to> {
            fn from(_st: StateMachine<$from>) -> Self {
                StateMachine { state: $to }
            }
        }
    };
}
