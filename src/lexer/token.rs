use nom::*;
use std::{
    iter::Enumerate,
    ops::{Range, RangeFrom, RangeFull, RangeTo},
};

#[derive(PartialEq, Debug, Clone)]
pub struct HexDigit(pub char);

#[derive(PartialEq, Debug, Clone)]
pub struct HexDigits(pub String);

#[derive(PartialEq, Debug, Clone)]
pub struct Number {
    integer: u32,
    decimal: u32,
    exponent: i32,
    base: u8,
}

impl Number {
    pub fn new(integer: u32, decimal: u32, exponent: i32, base: u8) -> Self {
        Self {
            integer,
            decimal,
            exponent,
            base,
        }
    }
}

#[derive(PartialEq, Debug, Clone)]
pub enum Token {
    Illegal,
    EOF,
    Dollar,
    Underscore,
    Backslash,
    IDStart,
    LZero,
    LOne,
    LTwo,
    LThree,
    LFour,
    LFive,
    LSix,
    LSeven,
    LEight,
    LNine,
    LNull,
    IdentifierName(String),
    CodePoint(HexDigits),
    Hex4Digits(HexDigit, HexDigit, HexDigit, HexDigit),
    HexDigit(HexDigit),
    NumericLiteral(Number),
    StringLiteral(String),
    BoolLiteral(bool),
    ZWNJ,
    ZWJ,
    IDContinue,
    TAB,
    VT,
    FF,
    SP,
    NBSP,
    ZWNBSP,
    PS,
    LS,
    CR,
    LF,
    Char(char),
    RBrace,
    Slash,
    SlashEqual,
    LBrace,
    LRound,
    RRound,
    LSquare,
    RSquare,
    Dot,
    TripleDot,
    Semicolon,
    Comma,
    LAngle,
    RAngle,
    LessEqual,
    MoreEqual,
    Equal,
    NotEqual,
    EqualEqual,
    NotEqualEqual,
    Plus,
    Minus,
    Mult,
    Mod,
    DoubleMult,
    DoublePlus,
    DoubleMinus,
    DoubleLAngle,
    DoubleRAngle,
    TripleRAngle,
    Amp,
    Pipe,
    Caret,
    Exclamation,
    Tilde,
    And,
    Or,
    Question,
    Colon,
    Assign,
    PlusAssign,
    MinusAssign,
    MultAssign,
    ModAssign,
    DoubleStarAssign,
    DoubleLArrowAssign,
    DoubleRArrowAssign,
    TripleRArrowAssign,
    AmpAssign,
    PipeAssign,
    CaretAssign,
    EqualArrow,
    NOP,
    KFunction,
    KAsync,
    KClass,
    KLet,
    KIf,
    KElse,
    KDo,
    KWhile,
    KFor,
    KVar,
    KConst,
    KIn,
    KOf,
    KAwait,
    KSwitch,
    KCase,
    KDefault,
    KContinue,
    KBreak,
    KReturn,
    KWith,
    KThrow,
    KTry,
    KCatch,
    KFinally,
    KDebugger,
    KExtend,
    KStatic,
    KGet,
    KSet,
}

#[derive(Clone, Copy, PartialEq, Debug)]
#[repr(C)]
pub struct Tokens<'a> {
    pub tok: &'a [Token],
    pub start: usize,
    pub end: usize,
}

impl<'a> Tokens<'a> {
    pub fn new(vec: &'a [Token]) -> Self {
        Tokens {
            tok: vec,
            start: 0,
            end: vec.len(),
        }
    }
}

impl<'a> InputLength for Tokens<'a> {
    #[inline]
    fn input_len(&self) -> usize {
        self.tok.len()
    }
}

impl<'a> AtEof for Tokens<'a> {
    #[inline]
    fn at_eof(&self) -> bool {
        true
    }
}

impl<'a> InputTake for Tokens<'a> {
    #[inline]
    fn take(&self, count: usize) -> Self {
        Tokens {
            tok: &self.tok[0..count],
            start: 0,
            end: count,
        }
    }

    #[inline]
    fn take_split(&self, count: usize) -> (Self, Self) {
        let (prefix, suffix) = self.tok.split_at(count);
        let first = Tokens {
            tok: prefix,
            start: 0,
            end: prefix.len(),
        };
        let second = Tokens {
            tok: suffix,
            start: 0,
            end: suffix.len(),
        };
        (second, first)
    }
}

impl InputLength for Token {
    #[inline]
    fn input_len(&self) -> usize {
        1
    }
}

impl<'a> Slice<Range<usize>> for Tokens<'a> {
    #[inline]
    fn slice(&self, range: Range<usize>) -> Self {
        Tokens {
            tok: self.tok.slice(range.clone()),
            start: self.start + range.start,
            end: self.start + range.end,
        }
    }
}

impl<'a> Slice<RangeTo<usize>> for Tokens<'a> {
    #[inline]
    fn slice(&self, range: RangeTo<usize>) -> Self {
        self.slice(0..range.end)
    }
}

impl<'a> Slice<RangeFrom<usize>> for Tokens<'a> {
    #[inline]
    fn slice(&self, range: RangeFrom<usize>) -> Self {
        self.slice(range.start..self.end - self.start)
    }
}

impl<'a> Slice<RangeFull> for Tokens<'a> {
    #[inline]
    fn slice(&self, _: RangeFull) -> Self {
        Tokens {
            tok: self.tok,
            start: self.start,
            end: self.end,
        }
    }
}

impl<'a> InputIter for Tokens<'a> {
    type Item = &'a Token;
    type Iter = Enumerate<::std::slice::Iter<'a, Token>>;
    type IterElem = ::std::slice::Iter<'a, Token>;
    type RawItem = Token;

    #[inline]
    fn iter_indices(&self) -> Enumerate<::std::slice::Iter<'a, Token>> {
        self.tok.iter().enumerate()
    }

    #[inline]
    fn iter_elements(&self) -> ::std::slice::Iter<'a, Token> {
        self.tok.iter()
    }

    #[inline]
    fn position<P>(&self, predicate: P) -> Option<usize>
    where
        P: Fn(Self::RawItem) -> bool,
    {
        self.tok.iter().position(|b| predicate(b.clone()))
    }

    #[inline]
    fn slice_index(&self, count: usize) -> Option<usize> {
        if self.tok.len() >= count {
            Some(count)
        } else {
            None
        }
    }
}
