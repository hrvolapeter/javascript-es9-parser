use crate::{
    equivalence::{Equivalence, EQUIVALENCE_CLASS},
    error::Error,
    state::*,
    token::{Number, Token},
};
use internship::IStr;
use std::str;

#[derive(Debug)]
struct StateMachine<S: State> {
    state: S,
}

impl<S: State> StateMachine<S> {
    fn new() -> StateMachine<InputElementDiv> {
        StateMachine {
            state: InputElementDiv,
        }
    }

    fn is_final(&self) -> bool {
        self.state.is_final()
    }
}

#[derive(Debug)]
enum StateMachineWrapper {
    LineTerminator(StateMachine<LineTerminator>),
    WhiteSpace(StateMachine<WhiteSpace>),
    SingleLineCommentAcc(StateMachine<SingleLineCommentAcc>),
    MultiLineCommentAcc(StateMachine<MultiLineCommentAcc>),
    LCurly(StateMachine<LCurly>),
    LRound(StateMachine<LRound>),
    RRound(StateMachine<RRound>),
    LSquare(StateMachine<LSquare>),
    RSquare(StateMachine<RSquare>),
    DotPart(StateMachine<DotPart>),
    Comma(StateMachine<Comma>),
    Semicolon(StateMachine<Semicolon>),
    LesserAcc(StateMachine<LesserAcc>),
    BiggerAcc(StateMachine<BiggerAcc>),
    AssignAcc(StateMachine<AssignAcc>),
    ExclamationAcc(StateMachine<ExclamationAcc>),
    PlusAcc(StateMachine<PlusAcc>),
    MinusAcc(StateMachine<MinusAcc>),
    StarAcc(StateMachine<StarAcc>),
    PercentAcc(StateMachine<PercentAcc>),
    AndAcc(StateMachine<AndAcc>),
    OrAcc(StateMachine<OrAcc>),
    Tilde(StateMachine<Tilde>),
    QuestionMark(StateMachine<QuestionMark>),
    Colon(StateMachine<Colon>),
    CaretAcc(StateMachine<CaretAcc>),
    DoubleString(StateMachine<DoubleString>),
    SingleString(StateMachine<SingleString>),
    BinaryAcc(StateMachine<BinaryAcc>),
    OctalAcc(StateMachine<OctalAcc>),
    HexAcc(StateMachine<HexAcc>),
    DecimalAcc(StateMachine<DecimalAcc>),
    DecimalDigitsAcc(StateMachine<DecimalDigitsAcc>),
    DecimalExponentAcc(StateMachine<DecimalExponentAcc>),
    DecimalExponentSignedAcc(StateMachine<DecimalExponentSignedAcc>),
    Identifier(StateMachine<Identifier>),
    SlashAcc(StateMachine<SlashAcc>),
    RCurly(StateMachine<RCurly>),
    Template(StateMachine<Template>),

    InputElementDiv(StateMachine<InputElementDiv>),
    Slash(StateMachine<Slash>),
    SingleLineComment(StateMachine<SingleLineComment>),
    MultiLineComment(StateMachine<MultiLineComment>),
    MultiLineCommentStar(StateMachine<MultiLineCommentStar>),
    Lesser(StateMachine<Lesser>),
    Bigger(StateMachine<Bigger>),
    Assign(StateMachine<Assign>),
    Exclamation(StateMachine<Exclamation>),
    Plus(StateMachine<Plus>),
    Minus(StateMachine<Minus>),
    Star(StateMachine<Star>),
    Percent(StateMachine<Percent>),
    And(StateMachine<And>),
    Or(StateMachine<Or>),
    Caret(StateMachine<Caret>),
    SawZero(StateMachine<SawZero>),
    Binary(StateMachine<Binary>),
    Octal(StateMachine<Octal>),
    Hex(StateMachine<Hex>),
    Decimal(StateMachine<Decimal>),
    DecimalDigits(StateMachine<DecimalDigits>),
    DecimalExponent(StateMachine<DecimalExponent>),
    DecimalExponentSigned(StateMachine<DecimalExponentSigned>),
}

Edge!(InputElementDiv, LineTerminator);
Edge!(InputElementDiv, WhiteSpace);
Edge!(InputElementDiv, Slash);
Edge!(InputElementDiv, DotPart);
Edge!(Slash, SlashAcc);
Edge!(Slash, SingleLineComment);
Edge!(Slash, MultiLineComment);
Edge!(SingleLineComment, SingleLineCommentAcc);
Edge!(MultiLineComment, MultiLineCommentStar);
Edge!(MultiLineCommentStar, MultiLineCommentAcc);
Edge!(InputElementDiv, Identifier);
Edge!(InputElementDiv, LCurly);
Edge!(InputElementDiv, RCurly);
Edge!(InputElementDiv, LRound);
Edge!(InputElementDiv, RRound);
Edge!(InputElementDiv, LSquare);
Edge!(InputElementDiv, RSquare);
Edge!(InputElementDiv, Semicolon);
Edge!(InputElementDiv, Comma);
Edge!(InputElementDiv, Colon);
Edge!(InputElementDiv, QuestionMark);
Edge!(InputElementDiv, Tilde);
Edge!(InputElementDiv, Lesser);
Edge!(Lesser, LesserAcc);
Edge!(InputElementDiv, Bigger);
Edge!(Bigger, BiggerAcc);
Edge!(InputElementDiv, Assign);
Edge!(Assign, AssignAcc);
Edge!(InputElementDiv, Exclamation);
Edge!(Exclamation, ExclamationAcc);
Edge!(InputElementDiv, Plus);
Edge!(Plus, PlusAcc);
Edge!(InputElementDiv, Star);
Edge!(Star, StarAcc);
Edge!(InputElementDiv, Percent);
Edge!(Percent, PercentAcc);
Edge!(InputElementDiv, Minus);
Edge!(Minus, MinusAcc);
Edge!(InputElementDiv, Or);
Edge!(Or, OrAcc);
Edge!(InputElementDiv, Caret);
Edge!(Caret, CaretAcc);
Edge!(InputElementDiv, SingleString);
Edge!(InputElementDiv, Template);
Edge!(InputElementDiv, DoubleString);
Edge!(InputElementDiv, And);
Edge!(And, AndAcc);
Edge!(InputElementDiv, SawZero);
Edge!(SawZero, Decimal);
Edge!(SawZero, DecimalAcc);
Edge!(InputElementDiv, Decimal);
Edge!(Decimal, DecimalAcc);
Edge!(Decimal, DecimalExponent);
Edge!(DecimalDigits, DecimalExponent);
Edge!(DecimalDigits, DecimalDigitsAcc);
Edge!(DecimalExponent, DecimalExponentSigned);
Edge!(DecimalExponentSigned, DecimalExponentSignedAcc);
Edge!(DecimalExponent, DecimalExponentAcc);
Edge!(Decimal, DecimalDigits);
Edge!(InputElementDiv, Hex);
Edge!(Hex, HexAcc);
Edge!(SawZero, Hex);
Edge!(InputElementDiv, Binary);
Edge!(Binary, BinaryAcc);
Edge!(SawZero, Binary);
Edge!(InputElementDiv, Octal);
Edge!(Octal, OctalAcc);
Edge!(SawZero, Octal);
Edge!(MultiLineCommentStar, MultiLineComment);
Edge!(LineTerminator, HELL);

impl StateMachineWrapper {
    fn step(self, e: Equivalence) -> Self {
        include!("./transitions.rs")
    }

    fn is_final(&self) -> bool {
        match self {
            StateMachineWrapper::LineTerminator(n) => n.is_final(),
            StateMachineWrapper::WhiteSpace(n) => n.is_final(),
            StateMachineWrapper::SingleLineCommentAcc(n) => n.is_final(),
            StateMachineWrapper::MultiLineCommentAcc(n) => n.is_final(),
            StateMachineWrapper::LCurly(n) => n.is_final(),
            StateMachineWrapper::LRound(n) => n.is_final(),
            StateMachineWrapper::RRound(n) => n.is_final(),
            StateMachineWrapper::LSquare(n) => n.is_final(),
            StateMachineWrapper::RSquare(n) => n.is_final(),
            StateMachineWrapper::DotPart(n) => n.is_final(),
            StateMachineWrapper::Comma(n) => n.is_final(),
            StateMachineWrapper::Semicolon(n) => n.is_final(),
            StateMachineWrapper::LesserAcc(n) => n.is_final(),
            StateMachineWrapper::BiggerAcc(n) => n.is_final(),
            StateMachineWrapper::AssignAcc(n) => n.is_final(),
            StateMachineWrapper::ExclamationAcc(n) => n.is_final(),
            StateMachineWrapper::PlusAcc(n) => n.is_final(),
            StateMachineWrapper::MinusAcc(n) => n.is_final(),
            StateMachineWrapper::StarAcc(n) => n.is_final(),
            StateMachineWrapper::PercentAcc(n) => n.is_final(),
            StateMachineWrapper::AndAcc(n) => n.is_final(),
            StateMachineWrapper::OrAcc(n) => n.is_final(),
            StateMachineWrapper::Tilde(n) => n.is_final(),
            StateMachineWrapper::QuestionMark(n) => n.is_final(),
            StateMachineWrapper::Colon(n) => n.is_final(),
            StateMachineWrapper::CaretAcc(n) => n.is_final(),
            StateMachineWrapper::DoubleString(n) => n.is_final(),
            StateMachineWrapper::SingleString(n) => n.is_final(),
            StateMachineWrapper::BinaryAcc(n) => n.is_final(),
            StateMachineWrapper::OctalAcc(n) => n.is_final(),
            StateMachineWrapper::HexAcc(n) => n.is_final(),
            StateMachineWrapper::DecimalAcc(n) => n.is_final(),
            StateMachineWrapper::DecimalDigitsAcc(n) => n.is_final(),
            StateMachineWrapper::DecimalExponentAcc(n) => n.is_final(),
            StateMachineWrapper::DecimalExponentSignedAcc(n) => n.is_final(),
            StateMachineWrapper::Identifier(n) => n.is_final(),
            StateMachineWrapper::SlashAcc(n) => n.is_final(),
            StateMachineWrapper::RCurly(n) => n.is_final(),
            StateMachineWrapper::Template(n) => n.is_final(),

            StateMachineWrapper::InputElementDiv(n) => n.is_final(),
            StateMachineWrapper::Slash(n) => n.is_final(),
            StateMachineWrapper::SingleLineComment(n) => n.is_final(),
            StateMachineWrapper::MultiLineComment(n) => n.is_final(),
            StateMachineWrapper::MultiLineCommentStar(n) => n.is_final(),
            StateMachineWrapper::Lesser(n) => n.is_final(),
            StateMachineWrapper::Bigger(n) => n.is_final(),
            StateMachineWrapper::Assign(n) => n.is_final(),
            StateMachineWrapper::Exclamation(n) => n.is_final(),
            StateMachineWrapper::Plus(n) => n.is_final(),
            StateMachineWrapper::Minus(n) => n.is_final(),
            StateMachineWrapper::Star(n) => n.is_final(),
            StateMachineWrapper::Percent(n) => n.is_final(),
            StateMachineWrapper::And(n) => n.is_final(),
            StateMachineWrapper::Or(n) => n.is_final(),
            StateMachineWrapper::Caret(n) => n.is_final(),
            StateMachineWrapper::SawZero(n) => n.is_final(),
            StateMachineWrapper::Binary(n) => n.is_final(),
            StateMachineWrapper::Octal(n) => n.is_final(),
            StateMachineWrapper::Hex(n) => n.is_final(),
            StateMachineWrapper::Decimal(n) => n.is_final(),
            StateMachineWrapper::DecimalDigits(n) => n.is_final(),
            StateMachineWrapper::DecimalExponent(n) => n.is_final(),
            StateMachineWrapper::DecimalExponentSigned(n) => n.is_final(),
        }
    }
}

pub fn parse(input: &str) -> Result<Vec<Token>, Error> {
    let mut st = StateMachineWrapper::InputElementDiv(StateMachine::<InputElementDiv>::new());
    let input = input.as_bytes();
    let mut tokens = Vec::with_capacity(input.len());

    let mut c_src: usize = 0;
    let mut token_len: u64 = 0;
    while c_src < input.len() {
        while !st.is_final() {
            let ch = unsafe { *input.get_unchecked(c_src) };
            st = st.step(EQUIVALENCE_CLASS[ch as usize]);
            c_src += 1;
            token_len += 1;
        }
        match st {
            StateMachineWrapper::LineTerminator(_) => tokens.push(Token::LineTerminator),
            // LF after comment is not considered to be part of comment
            // and should be left. We can parse it as part of singleline
            // comment and replace commen with line terminator
            StateMachineWrapper::SingleLineCommentAcc(_) => tokens.push(Token::LineTerminator),
            StateMachineWrapper::MultiLineCommentAcc(_) => {}
            StateMachineWrapper::LCurly(_) => tokens.push(Token::LCurly),
            StateMachineWrapper::RCurly(_) => tokens.push(Token::RCurly),
            StateMachineWrapper::LRound(_) => tokens.push(Token::LRound),
            StateMachineWrapper::RRound(_) => tokens.push(Token::RRound),
            StateMachineWrapper::Comma(_) => tokens.push(Token::Comma),
            StateMachineWrapper::LSquare(_) => tokens.push(Token::LSquare),
            StateMachineWrapper::RSquare(_) => tokens.push(Token::RSquare),
            StateMachineWrapper::Colon(_) => tokens.push(Token::Colon),
            StateMachineWrapper::QuestionMark(_) => tokens.push(Token::QuestionMark),
            StateMachineWrapper::Tilde(_) => tokens.push(Token::Tilde),
            StateMachineWrapper::DotPart(_) => tokens.push(parse_dot(input, &mut c_src)),
            StateMachineWrapper::LesserAcc(_) => {
                tokens.push(parse_lesser(input, &mut c_src, token_len))
            }
            StateMachineWrapper::BiggerAcc(_) => {
                tokens.push(parse_bigger(input, &mut c_src, token_len))
            }
            StateMachineWrapper::AssignAcc(_) => {
                tokens.push(parse_assign(input, &mut c_src, token_len))
            }
            StateMachineWrapper::ExclamationAcc(_) => {
                tokens.push(parse_exclamation(input, &mut c_src, token_len))
            }
            StateMachineWrapper::PlusAcc(_) => {
                tokens.push(parse_plus(input, &mut c_src, token_len))
            }
            StateMachineWrapper::MinusAcc(_) => {
                tokens.push(parse_minus(input, &mut c_src, token_len))
            }
            StateMachineWrapper::StarAcc(_) => {
                tokens.push(parse_star(input, &mut c_src, token_len))
            }
            StateMachineWrapper::PercentAcc(_) => {
                tokens.push(parse_percent(input, &mut c_src, token_len))
            }
            StateMachineWrapper::AndAcc(_) => tokens.push(parse_and(input, &mut c_src, token_len)),
            StateMachineWrapper::OrAcc(_) => tokens.push(parse_or(input, &mut c_src, token_len)),
            StateMachineWrapper::CaretAcc(_) => {
                tokens.push(parse_caret(input, &mut c_src, token_len))
            }
            StateMachineWrapper::SlashAcc(_) => {
                tokens.push(parse_slash(input, &mut c_src, token_len))
            }
            StateMachineWrapper::SingleString(_) => {
                tokens.push(parse_single_string(input, &mut c_src))
            }
            StateMachineWrapper::DoubleString(_) => {
                tokens.push(parse_double_string(input, &mut c_src))
            }
            StateMachineWrapper::Template(_) => tokens.push(parse_template(input, &mut c_src)),
            StateMachineWrapper::BinaryAcc(_) => {
                tokens.push(parse_number_radix(input, &mut c_src, token_len, 2)?)
            }
            StateMachineWrapper::OctalAcc(_) => {
                tokens.push(parse_number_radix(input, &mut c_src, token_len, 8)?)
            }
            StateMachineWrapper::HexAcc(_) => {
                tokens.push(parse_number_radix(input, &mut c_src, token_len, 16)?)
            }
            StateMachineWrapper::DecimalAcc(_) => {
                tokens.push(parse_number(input, &mut c_src, token_len)?)
            }
            StateMachineWrapper::DecimalDigitsAcc(_) => {
                tokens.push(parse_number_decimal(input, &mut c_src, token_len)?)
            }
            StateMachineWrapper::DecimalExponentSignedAcc(_) => {
                tokens.push(parse_exponent(input, &mut c_src, token_len)?)
            }
            StateMachineWrapper::DecimalExponentAcc(_) => {
                tokens.push(parse_exponent(input, &mut c_src, token_len)?)
            }
            StateMachineWrapper::Identifier(_) => tokens.push(parse_identifier(input, &mut c_src)),
            StateMachineWrapper::WhiteSpace(_) => {}
            StateMachineWrapper::Semicolon(_) => tokens.push(Token::Semicolon),
            StateMachineWrapper::InputElementDiv(_) => {}
            StateMachineWrapper::Slash(_) => {}
            StateMachineWrapper::SingleLineComment(_) => {}
            StateMachineWrapper::MultiLineComment(_) => {}
            StateMachineWrapper::MultiLineCommentStar(_) => {}
            StateMachineWrapper::Lesser(_) => {}
            StateMachineWrapper::Bigger(_) => {}
            StateMachineWrapper::Assign(_) => {}
            StateMachineWrapper::Exclamation(_) => {}
            StateMachineWrapper::Plus(_) => {}
            StateMachineWrapper::Minus(_) => {}
            StateMachineWrapper::Star(_) => {}
            StateMachineWrapper::Percent(_) => {}
            StateMachineWrapper::And(_) => {}
            StateMachineWrapper::Or(_) => {}
            StateMachineWrapper::Caret(_) => {}
            StateMachineWrapper::SawZero(_) => {}
            StateMachineWrapper::Binary(_) => {}
            StateMachineWrapper::Decimal(_) => {}
            StateMachineWrapper::Octal(_) => {}
            StateMachineWrapper::Hex(_) => {}
            StateMachineWrapper::DecimalDigits(_) => {}
            StateMachineWrapper::DecimalExponent(_) => {}
            StateMachineWrapper::DecimalExponentSigned(_) => {}
        }
        st = StateMachineWrapper::InputElementDiv(StateMachine::<InputElementDiv>::new());
        token_len = 0;
    }
    Ok(tokens)
}

#[inline]
fn is_identifier_part(cp: u8) -> bool {
    cp == 0x24
        || cp == 0x5F
        || (cp >= 0x41 && cp <= 0x5A)
        || (cp >= 0x61 && cp <= 0x7A)
        || (cp >= 0x30 && cp <= 0x39)
        || cp == 0x5C
        || cp >= 0x80
}

#[inline]
fn parse_identifier(input: &[u8], c_src: &mut usize) -> Token {
    let mut it = 0;
    for i in 0..input.len() - *c_src {
        if !unsafe { is_identifier_part(*input.get_unchecked(*c_src + i)) } {
            it = i;
            break;
        }
    }
    let ident = &input[*c_src - 1..*c_src + it];
    *c_src += it;
    let ident = unsafe { str::from_utf8_unchecked(ident) };
    match ident {
        "true" => Token::BoolLiteral(true),
        "false" => Token::BoolLiteral(false),
        "null" => Token::LNull,
        "function" => Token::KFunction,
        "async" => Token::KAsync,
        "class" => Token::KClass,
        "let" => Token::KLet,
        "if" => Token::KIf,
        "else" => Token::KElse,
        "do" => Token::KDo,
        "while" => Token::KWhile,
        "for" => Token::KFor,
        "var" => Token::KVar,
        "const" => Token::KConst,
        "in" => Token::KIn,
        "of" => Token::KOf,
        "await" => Token::KAwait,
        "switch" => Token::KSwitch,
        "case" => Token::KCase,
        "default" => Token::KDefault,
        "continue" => Token::KContinue,
        "break" => Token::KBreak,
        "return" => Token::KReturn,
        "with" => Token::KWith,
        "throw" => Token::KThrow,
        "try" => Token::KTry,
        "catch" => Token::KCatch,
        "finally" => Token::KFinally,
        "debugger" => Token::KDebugger,
        "extend" => Token::KExtend,
        "static" => Token::KStatic,
        "get" => Token::KGet,
        "set" => Token::KSet,
        "this" => Token::KThis,
        "delete" => Token::KDelete,
        "void" => Token::KVoid,
        "typeof" => Token::KTypeof,
        "new" => Token::KNew,
        _ => Token::IdentifierName(IStr::new(ident)),
    }
}

#[inline]
fn parse_dot(input: &[u8], c_src: &mut usize) -> Token {
    let rest_len = input.len() - *c_src;
    if rest_len > 1 && input[*c_src] == '.' as u8 && input[*c_src] == '.' as u8 {
        *c_src += 2;
        return Token::TripleDot;
    }
    Token::Dot
}

#[inline]
fn parse_lesser(input: &[u8], c_src: &mut usize, mut token_len: u64) -> Token {
    let token_start = *c_src - token_len as usize;
    token_len -= 1;
    *c_src -= 1;
    if token_len == 2 && input[token_start + 1] == '=' as u8 {
        return Token::LessEqual;
    }
    if token_len == 2 && input[token_start + 1] == '<' as u8 {
        return Token::DoubleLesser;
    }
    if token_len == 3 && input[token_start + 1] == '<' as u8 && input[token_start + 2] == '=' as u8
    {
        return Token::DoubleLesserEqual;
    }
    Token::Lesser
}

#[inline]
fn parse_bigger(input: &[u8], c_src: &mut usize, mut token_len: u64) -> Token {
    let token_start = *c_src - token_len as usize;
    token_len -= 1;
    *c_src -= 1;
    if token_len == 2 && input[token_start + 1] == '=' as u8 {
        return Token::BiggerEqual;
    }
    if token_len == 2 && input[token_start + 1] == '>' as u8 {
        return Token::DoubleBigger;
    }

    if token_len == 3 && input[token_start + 1] == '>' as u8 && input[token_start + 2] == '=' as u8
    {
        return Token::DoubleBiggerEqual;
    }
    if token_len == 3 && input[token_start + 1] == '>' as u8 && input[token_start + 2] == '>' as u8
    {
        return Token::TripleBigger;
    }
    if token_len == 4
        && input[token_start + 1] == '>' as u8
        && input[token_start + 2] == '>' as u8
        && input[token_start + 3] == '=' as u8
    {
        return Token::TripleBiggerEqual;
    }
    Token::Bigger
}

#[inline]
fn parse_assign(input: &[u8], c_src: &mut usize, mut token_len: u64) -> Token {
    let token_start = *c_src - token_len as usize;
    token_len -= 1;
    *c_src -= 1;
    if token_len == 2 && input[token_start + 1] == '=' as u8 {
        return Token::DoubleAssign;
    }
    if token_len == 2 && input[token_start + 1] == '>' as u8 {
        return Token::AssignBigger;
    }
    if token_len == 3 && input[token_start + 1] == '=' as u8 && input[token_start + 2] == '=' as u8
    {
        return Token::TripleAssign;
    }
    Token::Assign
}

#[inline]
fn parse_exclamation(input: &[u8], c_src: &mut usize, mut token_len: u64) -> Token {
    let token_start = *c_src - token_len as usize;
    token_len -= 1;
    *c_src -= 1;
    if token_len == 2 && input[token_start + 1] == '=' as u8 {
        return Token::ExclamationAssign;
    }
    if token_len == 3 && input[token_start + 1] == '=' as u8 && input[token_start + 2] == '=' as u8
    {
        return Token::ExclamationDoubleAssign;
    }
    // TODO: what about !======
    Token::Exclamation
}

#[inline]
fn parse_plus(input: &[u8], c_src: &mut usize, mut token_len: u64) -> Token {
    let token_start = *c_src - token_len as usize;
    token_len -= 1;
    *c_src -= 1;
    if token_len == 2 && input[token_start + 1] == '+' as u8 {
        return Token::DoublePlus;
    }
    if token_len == 2 && input[token_start + 1] == '=' as u8 {
        return Token::PlusAssign;
    }
    Token::Plus
}

#[inline]
fn parse_minus(input: &[u8], c_src: &mut usize, mut token_len: u64) -> Token {
    let token_start = *c_src - token_len as usize;
    token_len -= 1;
    *c_src -= 1;
    if token_len == 2 && input[token_start + 1] == '-' as u8 {
        return Token::DoubleMinus;
    }
    if token_len == 2 && input[token_start + 1] == '=' as u8 {
        return Token::MinusAssign;
    }
    Token::Minus
}

#[inline]
fn parse_star(input: &[u8], c_src: &mut usize, mut token_len: u64) -> Token {
    let token_start = *c_src - token_len as usize;
    token_len -= 1;
    *c_src -= 1;
    if token_len == 2 && input[token_start + 1] == '*' as u8 {
        return Token::DoubleStar;
    }
    if token_len == 2 && input[token_start + 1] == '=' as u8 {
        return Token::StarAssign;
    }
    if token_len == 3 && input[token_start + 1] == '*' as u8 && input[token_start + 2] == '=' as u8
    {
        return Token::DoubleStarAssign;
    }
    Token::Star
}

#[inline]
fn parse_percent(input: &[u8], c_src: &mut usize, mut token_len: u64) -> Token {
    let token_start = *c_src - token_len as usize;
    token_len -= 1;
    *c_src -= 1;
    if token_len == 2 && input[token_start + 1] == '=' as u8 {
        return Token::PercentAssign;
    }
    Token::Percent
}

#[inline]
fn parse_and(input: &[u8], c_src: &mut usize, mut token_len: u64) -> Token {
    let token_start = *c_src - token_len as usize;
    token_len -= 1;
    *c_src -= 1;
    if token_len == 2 && input[token_start + 1] == '&' as u8 {
        return Token::DoubleAnd;
    }
    if token_len == 2 && input[token_start + 1] == '=' as u8 {
        return Token::AndAssign;
    }
    Token::And
}

#[inline]
fn parse_or(input: &[u8], c_src: &mut usize, mut token_len: u64) -> Token {
    let token_start = *c_src - token_len as usize;
    token_len -= 1;
    *c_src -= 1;
    if token_len == 2 && input[token_start + 1] == '|' as u8 {
        return Token::DoubleOr;
    }
    if token_len == 2 && input[token_start + 1] == '=' as u8 {
        return Token::OrAssign;
    }
    Token::Or
}

#[inline]
fn parse_caret(input: &[u8], c_src: &mut usize, mut token_len: u64) -> Token {
    let token_start = *c_src - token_len as usize;
    token_len -= 1;
    *c_src -= 1;
    if token_len == 2 && input[token_start + 1] == '=' as u8 {
        return Token::CaretAssign;
    }
    Token::Caret
}

#[inline]
fn parse_slash(input: &[u8], c_src: &mut usize, token_len: u64) -> Token {
    let token_start = *c_src - token_len as usize;
    if token_len == 2 && input[token_start + 1] == '=' as u8 {
        return Token::SlashAssign;
    }
    let mut it = 0;
    for i in 0..input.len() - *c_src {
        if !unsafe { *input.get_unchecked(*c_src + i) != ' ' as u8 } {
            it = i;
            break;
        }
    }
    let ident = &input[*c_src - 2..*c_src + it];
    *c_src += it;
    let ident = unsafe { str::from_utf8_unchecked(ident) };
    if ident.len() == 2 {
        return Token::Slash;
    }
    Token::Regex(String::from(ident))
}

#[inline]
fn unescape(c: char) -> char {
    match c {
        'n' => '\n',
        'r' => '\r',
        't' => '\t',
        'b' => '\x08',
        'v' => '\x0B',
        'f' => '\x0C',
        _ => c,
    }
}

fn to_unescaped(input: String) -> String {
    let mut s = String::with_capacity(input.len());
    let b = input.as_bytes();
    let mut escaping = false;
    for i in 0..b.len() {
        let c = unsafe { b.get_unchecked(i) };
        if *c == '\\' as u8 {
            escaping = true;
            continue;
        }
        if escaping {
            escaping = false;
            s.push(unescape(*c as char));
            continue;
        }
        s.push(*c as char);
    }
    s
}

use core::str::pattern::Pattern;
fn replace<'a, P: Pattern<'a>>(s: &'a String, from: P, to: &str) -> String {
    let mut result = String::with_capacity(s.len());
    let mut last_end = 0;
    for (start, part) in s.match_indices(from) {
        result.push_str(unsafe { s.get_unchecked(last_end..start) });
        result.push_str(to);
        last_end = start + part.len();
    }
    result.push_str(unsafe { s.get_unchecked(last_end..s.len()) });
    result
}

#[inline]
fn parse_single_string(input: &[u8], c_src: &mut usize) -> Token {
    let mut token_len = 0;
    while input.len() - 1 > *c_src
        && (input[*c_src] != '\'' as u8 || input[*c_src - 1] == '\\' as u8)
    {
        *c_src += 1;
        token_len += 1;
    }
    let res = unsafe { str::from_utf8_unchecked(&input[*c_src - token_len..*c_src]).to_string() };
    let res = to_unescaped(replace(&res, "\\'", "'"));
    *c_src += 1;
    Token::StringLiteral(res)
}

#[inline]
fn parse_double_string(input: &[u8], c_src: &mut usize) -> Token {
    let mut token_len = 0;
    while input.len() - 1 > *c_src
        && (input[*c_src] != '"' as u8 || input[*c_src - 1] == '\\' as u8)
    {
        *c_src += 1;
        token_len += 1;
    }
    let res = unsafe { str::from_utf8_unchecked(&input[*c_src - token_len..*c_src]).to_string() };
    let res = to_unescaped(replace(&res, r#"\""#, "\""));
    *c_src += 1;
    Token::StringLiteral(res)
}

#[inline]
fn parse_template(input: &[u8], c_src: &mut usize) -> Token {
    let mut token_len = 0;
    while input.len() - 1 > *c_src
        && (input[*c_src] != '`' as u8 || input[*c_src - 1] == '\\' as u8)
    {
        *c_src += 1;
        token_len += 1;
    }
    let res = unsafe { str::from_utf8_unchecked(&input[*c_src - token_len..*c_src]).to_string() };
    let res = to_unescaped(replace(&res, "\\`", "`"));
    *c_src += 1;
    Token::Template(res)
}

#[inline]
fn parse_number_radix(
    input: &[u8],
    c_src: &mut usize,
    token_len: u64,
    base: u8,
) -> Result<Token, Error> {
    let i =
        unsafe { str::from_utf8_unchecked(&input[*c_src - token_len as usize + 2..*c_src - 1]) };
    let i = u32::from_str_radix(i, base as u32)?;
    *c_src -= 1;
    Ok(Token::NumericLiteral(Number::new(i, 0, 1, base)))
}

#[inline]
fn parse_number(input: &[u8], c_src: &mut usize, token_len: u64) -> Result<Token, Error> {
    let i = unsafe { str::from_utf8_unchecked(&input[*c_src - token_len as usize..*c_src - 1]) };
    let i = u32::from_str_radix(i, 10)?;
    *c_src -= 1;
    Ok(Token::NumericLiteral(Number::new(i, 0, 1, 10)))
}

#[inline]
fn parse_number_decimal(input: &[u8], c_src: &mut usize, token_len: u64) -> Result<Token, Error> {
    let mut i_point = 0;
    for i in *c_src - token_len as usize..*c_src - 1 {
        if input[i] == '.' as u8 {
            i_point = i;
            break;
        }
    }
    let integer = unsafe { str::from_utf8_unchecked(&input[*c_src - token_len as usize..i_point]) };
    let integer = u32::from_str_radix(integer, 10)?;

    let decimal = unsafe { str::from_utf8_unchecked(&input[i_point + 1..*c_src - 1]) };
    let decimal = u32::from_str_radix(decimal, 10)?;

    *c_src -= 1;
    Ok(Token::NumericLiteral(Number::new(integer, decimal, 1, 10)))
}

#[inline]
fn parse_exponent(input: &[u8], c_src: &mut usize, token_len: u64) -> Result<Token, Error> {
    let mut i_e = 0;
    let mut i_point = None;
    for i in *c_src - token_len as usize..*c_src - 1 {
        if input[i] == '.' as u8 {
            i_point = Some(i);
        }
        if input[i] == 'e' as u8 || input[i] == 'E' as u8 {
            i_e = i;
            break;
        }
    }

    let (integer, decimal) = if i_point.is_some() {
        let integer = unsafe {
            str::from_utf8_unchecked(&input[*c_src - token_len as usize..i_point.unwrap()])
        };
        let integer = u32::from_str_radix(integer, 10)?;
        let decimal = unsafe { str::from_utf8_unchecked(&input[i_point.unwrap() + 1..i_e]) };
        (integer, u32::from_str_radix(decimal, 10)?)
    } else {
        let integer = unsafe { str::from_utf8_unchecked(&input[*c_src - token_len as usize..i_e]) };
        let integer = u32::from_str_radix(integer, 10)?;
        (integer, 0)
    };

    let exponent = unsafe { str::from_utf8_unchecked(&input[i_e + 1..*c_src - 1]) };
    let exponent = i128::from_str_radix(exponent, 10).unwrap();
    *c_src += 1;
    Ok(Token::NumericLiteral(Number::new(
        integer, decimal, exponent, 10,
    )))
}

#[cfg(test)]
mod tests {
    use super::super::*;

    should!(
        lineterminator_all,
        "\u{000A}\u{000D}",
        vec![Token::LineTerminator, Token::LineTerminator, Token::EOF]
    );

    should!(
        single_comment,
        "// rest // of comment \n",
        vec![Token::LineTerminator, Token::EOF]
    );

    should!(multi_comment, "/** rest // of comment */", vec![Token::EOF]);

    should!(
        multi_comment_with_newline,
        "/** rest // \n of comment */",
        vec![Token::EOF]
    );

    should!(left_curly, "{", vec![Token::LCurly, Token::EOF]);

    should!(right_curly, "}", vec![Token::RCurly, Token::EOF]);

    should!(left_round, "(", vec![Token::LRound, Token::EOF]);

    should!(right_round, ")", vec![Token::RRound, Token::EOF]);

    should!(left_square, "[", vec![Token::LSquare, Token::EOF]);

    should!(right_square, "]", vec![Token::RSquare, Token::EOF]);

    should!(dot, ".", vec![Token::Dot, Token::EOF]);

    should!(twodot, "..", vec![Token::Dot, Token::Dot, Token::EOF]);

    should!(tripledot, "...", vec![Token::TripleDot, Token::EOF]);

    should!(
        fourdot,
        "....",
        vec![Token::TripleDot, Token::Dot, Token::EOF]
    );

    should!(lesser, "< ", vec![Token::Lesser, Token::EOF]);

    should!(lesser_double, "<< ", vec![Token::DoubleLesser, Token::EOF]);

    should!(lesser_equal, "<= ", vec![Token::LessEqual, Token::EOF]);

    should!(
        lesser_equal_double,
        "<<= ",
        vec![Token::DoubleLesserEqual, Token::EOF]
    );

    should!(bigger, "> ", vec![Token::Bigger, Token::EOF]);

    should!(bigger_equal, ">= ", vec![Token::BiggerEqual, Token::EOF]);

    should!(bigger_double, ">> ", vec![Token::DoubleBigger, Token::EOF]);

    should!(bigger_triple, ">>> ", vec![Token::TripleBigger, Token::EOF]);

    should!(
        bigger_double_equal,
        ">>= ",
        vec![Token::DoubleBiggerEqual, Token::EOF]
    );

    should!(
        bigger_triple_equal,
        ">>>= ",
        vec![Token::TripleBiggerEqual, Token::EOF]
    );

    should!(assign, "= ", vec![Token::Assign, Token::EOF]);

    should!(assign_double, "== ", vec![Token::DoubleAssign, Token::EOF]);

    should!(assign_triple, "=== ", vec![Token::TripleAssign, Token::EOF]);

    should!(assign_bigger, "=> ", vec![Token::AssignBigger, Token::EOF]);

    should!(exclamation, "! ", vec![Token::Exclamation, Token::EOF]);

    should!(
        exclamation_assign,
        "!= ",
        vec![Token::ExclamationAssign, Token::EOF]
    );

    should!(
        exclamation_double_assing,
        "!== ",
        vec![Token::ExclamationDoubleAssign, Token::EOF]
    );

    should!(plus, "+ ", vec![Token::Plus, Token::EOF]);

    should!(plus_dobule, "++ ", vec![Token::DoublePlus, Token::EOF]);

    should!(plus_assign, "+= ", vec![Token::PlusAssign, Token::EOF]);

    should!(minus, "- ", vec![Token::Minus, Token::EOF]);

    should!(minus_double, "-- ", vec![Token::DoubleMinus, Token::EOF]);

    should!(minus_assign, "-= ", vec![Token::MinusAssign, Token::EOF]);

    should!(star, "* ", vec![Token::Star, Token::EOF]);

    should!(star_double, "** ", vec![Token::DoubleStar, Token::EOF]);

    should!(star_assign, "*= ", vec![Token::StarAssign, Token::EOF]);

    should!(
        star_double_assign,
        "**= ",
        vec![Token::DoubleStarAssign, Token::EOF]
    );

    should!(percent, "% ", vec![Token::Percent, Token::EOF]);

    should!(
        percent_assign,
        "%= ",
        vec![Token::PercentAssign, Token::EOF]
    );

    should!(and, "& ", vec![Token::And, Token::EOF]);

    should!(and_double, "&& ", vec![Token::DoubleAnd, Token::EOF]);

    should!(and_assign, "&= ", vec![Token::AndAssign, Token::EOF]);

    should!(or, "| ", vec![Token::Or, Token::EOF]);

    should!(or_double, "|| ", vec![Token::DoubleOr, Token::EOF]);

    should!(or_assign, "|= ", vec![Token::OrAssign, Token::EOF]);

    should!(tilde, "~ ", vec![Token::Tilde, Token::EOF]);

    should!(colon, ": ", vec![Token::Colon, Token::EOF]);

    should!(questionmark, "?", vec![Token::QuestionMark, Token::EOF]);

    should!(caret, "^ ", vec![Token::Caret, Token::EOF]);

    should!(caret_assign, "^= ", vec![Token::CaretAssign, Token::EOF]);

    should!(slash_assign, "/= ", vec![Token::SlashAssign, Token::EOF]);

    should!(slash, "/ ", vec![Token::Slash, Token::EOF]);

    should!(
        string_single,
        "'cau'",
        vec![Token::StringLiteral(String::from("cau")), Token::EOF]
    );

    should!(
        string_single_escape,
        "'c\\'au'",
        vec![Token::StringLiteral(String::from("c'au")), Token::EOF]
    );

    should!(
        string_single_unescape,
        "'\t'",
        vec![Token::StringLiteral(String::from("\t")), Token::EOF]
    );

    should!(
        string_double,
        r#""cau""#,
        vec![Token::StringLiteral(String::from("cau")), Token::EOF]
    );

    should!(
        string_double_escape,
        r#""c\"au""#,
        vec![Token::StringLiteral(String::from("c\"au")), Token::EOF]
    );

    should!(
        string_double_unescape,
        "\"\t\"",
        vec![Token::StringLiteral(String::from("\t")), Token::EOF]
    );

    should!(
        template,
        "`cau`",
        vec![Token::Template(String::from("cau")), Token::EOF]
    );

    should!(
        template_escape,
        "`\\``",
        vec![Token::Template(String::from("`")), Token::EOF]
    );

    should!(
        template_unescape,
        "`\t`",
        vec![Token::Template(String::from("\t")), Token::EOF]
    );

    should!(
        binary,
        "0b1 ",
        vec![Token::NumericLiteral(Number::new(1, 0, 1, 2)), Token::EOF]
    );

    should!(
        binary_capital,
        "0b1 ",
        vec![Token::NumericLiteral(Number::new(1, 0, 1, 2)), Token::EOF]
    );

    should!(
        binary_four,
        "0b110 ",
        vec![Token::NumericLiteral(Number::new(6, 0, 1, 2)), Token::EOF]
    );

    should!(
        octal,
        "0o7 ",
        vec![Token::NumericLiteral(Number::new(7, 0, 1, 8)), Token::EOF]
    );

    should!(
        octal_cpaital,
        "0O7 ",
        vec![Token::NumericLiteral(Number::new(7, 0, 1, 8)), Token::EOF]
    );

    should!(
        octal_eight,
        "0O110 ",
        vec![Token::NumericLiteral(Number::new(72, 0, 1, 8)), Token::EOF]
    );

    should!(
        hex,
        "0xa ",
        vec![Token::NumericLiteral(Number::new(10, 0, 1, 16)), Token::EOF]
    );

    should!(
        hex_capital,
        "0Xa ",
        vec![Token::NumericLiteral(Number::new(10, 0, 1, 16)), Token::EOF]
    );

    should!(
        hex_sixteen,
        "0x10 ",
        vec![Token::NumericLiteral(Number::new(16, 0, 1, 16)), Token::EOF]
    );

    should!(
        decimal,
        "01 ",
        vec![Token::NumericLiteral(Number::new(1, 0, 1, 10)), Token::EOF]
    );

    should!(
        decimal_ten,
        "10 ",
        vec![Token::NumericLiteral(Number::new(10, 0, 1, 10)), Token::EOF]
    );

    should!(
        decimaldigits,
        "10.1 ",
        vec![Token::NumericLiteral(Number::new(10, 1, 1, 10)), Token::EOF]
    );

    should!(
        decimaldigits_exponent_signed,
        "10.1e-2 ",
        vec![
            Token::NumericLiteral(Number::new(10, 1, -2, 10)),
            Token::EOF
        ]
    );

    should!(
        decimal_exponent_signed,
        "10e-2 ",
        vec![
            Token::NumericLiteral(Number::new(10, 0, -2, 10)),
            Token::EOF
        ]
    );

    should!(
        decimal_exponent_signed_plus,
        "10e+20 ",
        vec![
            Token::NumericLiteral(Number::new(10, 0, 20, 10)),
            Token::EOF
        ]
    );

    should!(
        decimal_exponent_unsigneds,
        "10e20 ",
        vec![
            Token::NumericLiteral(Number::new(10, 0, 20, 10)),
            Token::EOF
        ]
    );

    should!(keyowrd, "var ", vec![Token::KVar, Token::EOF]);

    should!(
        regex,
        "/a/d ",
        vec![Token::Regex(String::from("/a/d")), Token::EOF]
    );

    should!(comma, ",", vec![Token::Comma, Token::EOF]);

    // should_fail!(string_single, "'cau\n'",
    // vec![Token::StringLiteral(String::from("cau")), Token::EOF]);
}
