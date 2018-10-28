use crate::lexer::token::Token;
use nom::{InputIter, InputLength, InputTake};
use std::{
    iter::{Enumerate, Map},
    ops::Deref,
    slice::Iter,
};

#[derive(PartialEq, Clone, Copy, Debug)]
pub struct InputWrapper<'a>(pub &'a [Token]);

impl<'a> Deref for InputWrapper<'a> {
    type Target = &'a [Token];

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl<'a> InputIter for InputWrapper<'a> {
    type Item = Token;
    type Iter = Enumerate<Self::IterElem>;
    type IterElem = Map<Iter<'a, Self::Item>, fn(&Token) -> Token>;
    type RawItem = Token;

    fn iter_indices(&self) -> Self::Iter {
        self.iter_elements().enumerate()
    }

    fn iter_elements(&self) -> Self::IterElem {
        self.0.iter().map(|s| s.clone())
    }

    fn position<P>(&self, predicate: P) -> Option<usize>
    where
        P: Fn(Self::Item) -> bool,
    {
        self.0.iter().position(|b| predicate(b.clone()))
    }

    fn slice_index(&self, count: usize) -> Option<usize> {
        if self.0.len() >= count {
            Some(count)
        } else {
            None
        }
    }
}

impl<'a> InputTake for InputWrapper<'a> {
    #[inline]
    fn take(&self, count: usize) -> Self {
        InputWrapper(&self.0[0..count])
    }

    #[inline]
    fn take_split(&self, count: usize) -> (Self, Self) {
        let (prefix, suffix) = self.0.split_at(count);
        (InputWrapper(suffix), InputWrapper(prefix))
    }
}

impl<'a> InputLength for InputWrapper<'a> {
    #[inline]
    fn input_len(&self) -> usize {
        self.0.len()
    }
}
