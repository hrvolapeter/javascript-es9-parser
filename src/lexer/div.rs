use crate::lexer::{
    token::{self, HexDigit, HexDigits, Number, Token},
    ParseResult,
};
use nom::{types::*, *};
use unescape::unescape;

named_js!(
    InputElementDiv: WhiteSpace
        | LineTerminator
        | Comment
        | CommonToken
        | DivPunctuator
        | RightBracePunctuator
);

named_js!(WhiteSpace: TAB | VT | FF | SP | NBSP | ZWNBSP);
should!(
    whitespace_all,
    "\u{0009}\u{000B}\u{000C}\u{0020}\u{00A0}\u{FEFF}",
    vec![
        Token::TAB,
        Token::VT,
        Token::FF,
        Token::NBSP,
        Token::ZWNBSP,
        Token::EOF
    ]
);

named_token_unicode!(TAB, 0x0009, Token::TAB);
should!(whitespace_tab, "\u{0009}", vec![Token::TAB, Token::EOF]);
named_token_unicode!(VT, 0x000B, Token::VT);
should!(whitespace_vr, "\u{000B}", vec![Token::VT, Token::EOF]);
named_token_unicode!(FF, 0x000C, Token::FF);
should!(whitespace_ff, "\u{000C}", vec![Token::FF, Token::EOF]);
named_token_unicode!(SP, 0x0020, Token::SP);
should!(whitespace_sp, " ", vec![Token::EOF]);
named_token_unicode!(NBSP, 0x00A0, Token::NBSP);
should!(whitespace_nbsp, "\u{00A0}", vec![Token::NBSP, Token::EOF]);
named_token_unicode!(ZWNBSP, 0xFEFF, Token::ZWNBSP);
should!(
    whitespace_zwnbsp,
    "\u{FEFF}",
    vec![Token::ZWNBSP, Token::EOF]
);

named_js!(LineTerminator: LF | CR | LS | PS);
should!(
    lineterminator_all,
    "\u{000A}\u{000D}\u{2028}\u{2029}",
    vec![Token::LF, Token::CR, Token::LS, Token::PS, Token::EOF]
);

named_token_unicode!(LF, 0x000A, Token::LF);
should!(lineterminator_lf, "\u{000A}", vec![Token::LF, Token::EOF]);

named_token_unicode!(CR, 0x000D, Token::CR);
should!(lineterminator_cr, "\u{000D}", vec![Token::CR, Token::EOF]);

named_token_unicode!(LS, 0x2028, Token::LS);
should!(lineterminator_ls, "\u{2028}", vec![Token::LS, Token::EOF]);

named_token_unicode!(PS, 0x2029, Token::PS);
should!(lineterminator_ps, "\u{2029}", vec![Token::PS, Token::EOF]);

named_js!(Comment: MultiLineComment | SingleLineComment);
should!(
    comment_multiline_singleline,
    "/* aa */\n // adwada",
    vec![Token::LF, Token::EOF]
);
should!(
    comment_multiline_singleline_multiline,
    "/* aa */\n // adwada \n /* aa */",
    vec![Token::LF, Token::LF, Token::EOF]
);

named_js!(
    MultiLineComment: do_parse!(tag!("/*") >> MultiLineCommentChars >> tag!("*/") >> (Token::NOP))
);
should!(multilinecomment_1, "/* aa */", vec![Token::EOF]);
should!(multilinecomment_2, "/* aa \n aaa */", vec![Token::EOF]);
should!(multilinecomment_3, "/** aa \n aaa **/", vec![Token::EOF]);
should!(multilinecomment_4, "/** aa  \n* aaa ***/", vec![Token::EOF]);
should!(
    multilinecomment_5,
    "/** aa //  \n* aaa ***/",
    vec![Token::EOF]
);
should!(multilinecomment_6, "/*aa\n* aaa ***/", vec![Token::EOF]);
should!(multilinecomment_7, "/*\naa\n* aaa ***/", vec![Token::EOF]);

named_js!(
    MultiLineCommentChars:
        do_parse!(MultiLineNotAsteriskChar >> opt!(MultiLineCommentChars) >> (Token::NOP))
        | do_parse!(
            tag!("*") >> peek!(none_of!("/")) >> opt!(PostAsteriskCommentChars) >> (Token::NOP)
        )
);
named_js!(MultiLineNotAsteriskChar: do_parse!(none_of!("*") >> (Token::NOP)));
named_js!(MultiLineNotForwardSlashOrAsteriskChar: do_parse!(none_of!("/*") >> (Token::NOP)));

named_js!(
    PostAsteriskCommentChars:
        do_parse!(
            MultiLineNotForwardSlashOrAsteriskChar >> opt!(MultiLineCommentChars) >> (Token::NOP)
        )
        | do_parse!(
            tag!("*") >> peek!(none_of!("/")) >> opt!(PostAsteriskCommentChars) >> (Token::NOP)
        )
);

named_js!(SingleLineComment: do_parse!(tag!("//") >> opt!(SingleLineCommentChars) >> (Token::NOP)));
should!(singlelinecomment_1, "//", vec![Token::EOF]);
should!(singlelinecomment_2, "// ", vec![Token::EOF]);
should!(singlelinecomment_3, "// test", vec![Token::EOF]);

named_js!(SingleLineCommentChars: do_parse!(many1!(SingleLineCommentChar) >> (Token::NOP)));

named_js!(SingleLineCommentChar: do_parse!(not!(LineTerminator) >> take!(1) >> (Token::NOP)));

named_js!(CommonToken: IdentifierName | Punctuator | NumericLiteral | StringLiteral);

#[inline]
fn IdentifierName(input: CompleteStr) -> ParseResult {
    let identifierStart = IdentifierStart(input)?;
    let identifierRest = many0!(identifierStart.0, IdentifierPart)?;
    let len = input.len() - identifierRest.0.len();
    let ident = &(*input)[..len];
    let ident = match ident {
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
        _ => Token::IdentifierName(String::from(ident)),
    };
    Ok((identifierRest.0, ident))
}
should!(
    identifiername_1,
    "\\u{1111Aa}",
    vec![
        Token::IdentifierName(String::from("\\u{1111Aa}")),
        Token::EOF
    ]
);
should!(
    identifiername_2,
    "\\u{1111Aa}\\u{12}",
    vec![
        Token::IdentifierName(String::from("\\u{1111Aa}\\u{12}")),
        Token::EOF
    ]
);
should!(
    identifiername_3,
    "$\\u{1111Aa}",
    vec![
        Token::IdentifierName(String::from("$\\u{1111Aa}")),
        Token::EOF
    ]
);
should!(
    identifiername_4,
    "_\\u{1111Aa}",
    vec![
        Token::IdentifierName(String::from("_\\u{1111Aa}")),
        Token::EOF
    ]
);
should!(
    identifiername_5,
    "\u{1E943}\\u{1111Aa}",
    vec![
        Token::IdentifierName(String::from("\u{1E943}\\u{1111Aa}")),
        Token::EOF
    ]
);

should!(
    identifiername_6,
    "$$\\u{1111Aa}",
    vec![
        Token::IdentifierName(String::from("$$\\u{1111Aa}")),
        Token::EOF
    ]
);
should!(
    identifiername_7,
    "\u{1E943}\u{1E959}\\u{1111Aa}",
    vec![
        Token::IdentifierName(String::from("\u{1E943}\u{1E959}\\u{1111Aa}")),
        Token::EOF
    ]
);
should!(
    identifiername_8,
    "$\u{200C}\u{200D}\\u{1111Aa}",
    vec![
        Token::IdentifierName(String::from("$\u{200C}\u{200D}\\u{1111Aa}")),
        Token::EOF
    ]
);
should_fail!(
    identifiername_9,
    "\u{200C}\u{200D}\\u{1111Aa}",
    vec![
        Token::IdentifierName(String::from("\u{200C}\u{200D}\\u{1111Aa}")),
        Token::EOF
    ]
);
should!(
    identifiername_10,
    r"\u11Aa",
    vec![Token::IdentifierName(String::from(r"\u11Aa")), Token::EOF]
);

#[derive(PartialEq, Debug, Clone)]
pub enum UnicodeEscapeSequence {
    HexDigits(HexDigits),
    Hex4Digits(HexDigit, HexDigit, HexDigit, HexDigit),
    NOP,
    Letter(char),
}

fn nop<T>(_: T) -> UnicodeEscapeSequence {
    UnicodeEscapeSequence::NOP
}

named!(IdentifierStart<CompleteStr, UnicodeEscapeSequence>, alt_longest!(
          map!(UnicodeIDStart, UnicodeEscapeSequence::Letter) |
          map!(tag!("$"), nop) |
          map!(tag!("_"), nop) |
          do_parse!(
              backslash_punctuation >>
              ues: unicode_escape_sequence >>
              (ues)
          )
));

named!(IdentifierPart<CompleteStr, UnicodeEscapeSequence>, alt_longest!(
          map!(UnicodeIDContinue, UnicodeEscapeSequence::Letter) |
          map!(tag!("$"), nop) |
          do_parse!(
              backslash_punctuation >>
              ues: unicode_escape_sequence >>
              (ues)
          ) |
          map!(tuple!(ZWNJ, ZWJ), nop)
));

named_token!(dollar_punctuation, "$", Token::Dollar);
named_token!(underscore_punctuation, "_", Token::Underscore);
named_token!(backslash_punctuation, "\\", Token::Backslash);

named_token_unicode!(ZWNJ, 0x200C, Token::ZWNJ);
named_token_unicode!(ZWJ, 0x200D, Token::ZWJ);

named!(unicode_escape_sequence<CompleteStr, UnicodeEscapeSequence>, alt_longest!(
    do_parse!(
        tag!("u") >>
        hex4digits: Hex4Digits >>
        (hex4digits)
    ) |
    do_parse!(
        tag!("u{") >>
        hex: hex_digit1 >>
        tag!("}") >>
        (UnicodeEscapeSequence::HexDigits(token::HexDigits(String::from(*hex))))
    )
));

#[inline]
fn is_hex(chr: char) -> bool {
    (chr >= 'a' && chr <= 'f') || (chr >= '0' && chr <= '9') || (chr >= 'A' && chr <= 'F')
}

#[inline]
fn Hex4Digits(input: CompleteStr) -> IResult<CompleteStr, UnicodeEscapeSequence> {
    let mut i = 0;

    let res = input.chars().take(4).fold(true, |r, c| {
        i += 1;
        is_hex(c) && r
    });
    if i != 4 {
        return Err(Err::Incomplete(Needed::Size(4)));
    }
    if !res {
        return Err(Err::Error(error_position!(input, ErrorKind::Custom(1))));
    }
    let mut c = input.chars();
    let h1 = token::HexDigit(c.next().unwrap());
    let h2 = token::HexDigit(c.next().unwrap());
    let h3 = token::HexDigit(c.next().unwrap());
    let h4 = token::HexDigit(c.next().unwrap());

    Ok((
        take!(input, 4)?.0,
        UnicodeEscapeSequence::Hex4Digits(h1, h2, h3, h4),
    ))
}

#[inline]
fn UnicodeIDStart(input: CompleteStr) -> IResult<CompleteStr, char> {
    let mut chars = input.chars();
    if let Some(c) = chars.next() {
        if c.is_xid_start() {
            return Ok((CompleteStr(chars.as_str()), c));
        }
    }
    Err(Err::Error(Context::Code(input, ErrorKind::Tag)))
}

#[inline]
fn UnicodeIDContinue(input: CompleteStr) -> IResult<CompleteStr, char> {
    let mut chars = input.chars();
    if let Some(c) = chars.next() {
        if c.is_xid_continue() {
            return Ok((CompleteStr(chars.as_str()), c));
        }
    }
    Err(Err::Error(Context::Code(input, ErrorKind::Tag)))
}

named_js!(
    Punctuator: lbrace_punctuation
        | lround_punctuation
        | rround_punctuation
        | lsquare_punctuation
        | rsquare_punctuation
        | dot_punctuation
        | tripledot_punctuation
        | semicolon_punctuation
        | comma_punctuation
        | langle_punctuation
        | rangle_punctuation
        | lessequal_punctuation
        | moreequal_punctuation
        | equal_punctuation
        | notequal_punctuation
        | equalequal_punctuation
        | notequalequal_punctuation
        | plus_punctuation
        | minus_punctuation
        | multiply_punctuation
        | mod_punctuation
        | doublemultiply_punctuation
        | doubleplus_punctuation
        | doubleminus_punctuation
        | doublelangle_punctuation
        | doublerangle_punctuation
        | triplerangle_punctuation
        | amp_punctuation
        | pipe_punctuation
        | caret_punctuation
        | exclamation_punctuation
        | tilde_punctuation
        | and_punctuation
        | or_punctuation
        | question_punctuation
        | colon_punctuation
        | assign_punctuation
        | plusassign_punctuation
        | minusassign_punctuation
        | multiplyassign_punctuation
        | modassign_punctuation
        | doublestarassign_punctuation
        | doublelarrowassign_punctuation
        | doublerarrowassign_punctuation
        | triplelarrowassign_punctuation
        | ampassign_punctuation
        | pipeassign_punctuation
        | caretassign_punctuation
        | equalarrow_punctuation
);

named_token!(lbrace_punctuation, "{", Token::LBrace);
named_token!(lround_punctuation, "(", Token::LRound);
named_token!(rround_punctuation, ")", Token::RRound);
named_token!(lsquare_punctuation, "[", Token::LSquare);
named_token!(rsquare_punctuation, "]", Token::RSquare);
named_token!(dot_punctuation, ".", Token::Dot);
named_token!(tripledot_punctuation, "...", Token::TripleDot);
named_token!(semicolon_punctuation, ";", Token::Semicolon);
named_token!(comma_punctuation, ",", Token::Comma);
named_token!(langle_punctuation, "<", Token::LAngle);
named_token!(rangle_punctuation, ">", Token::RAngle);
named_token!(lessequal_punctuation, "<=", Token::LessEqual);
named_token!(moreequal_punctuation, "=>", Token::MoreEqual);
named_token!(equal_punctuation, "==", Token::Equal);
named_token!(notequal_punctuation, "!=", Token::NotEqual);
named_token!(equalequal_punctuation, "===", Token::EqualEqual);
named_token!(notequalequal_punctuation, "!==", Token::NotEqualEqual);
named_token!(plus_punctuation, "+", Token::Plus);
named_token!(minus_punctuation, "-", Token::Minus);
named_token!(multiply_punctuation, "*", Token::Mult);
named_token!(mod_punctuation, "%", Token::Mod);
named_token!(doublemultiply_punctuation, "**", Token::DoubleMult);
named_token!(doubleplus_punctuation, "++", Token::DoublePlus);
named_token!(doubleminus_punctuation, "--", Token::DoubleMinus);
named_token!(doublelangle_punctuation, "<<", Token::DoubleLAngle);
named_token!(doublerangle_punctuation, ">>", Token::DoubleRAngle);
named_token!(triplerangle_punctuation, ">>>", Token::TripleRAngle);
named_token!(amp_punctuation, "&", Token::Amp);
named_token!(pipe_punctuation, "|", Token::Pipe);
named_token!(caret_punctuation, "^", Token::Caret);
named_token!(exclamation_punctuation, "!", Token::Exclamation);
named_token!(tilde_punctuation, "~", Token::Tilde);
named_token!(and_punctuation, "&&", Token::And);
named_token!(or_punctuation, "||", Token::Or);
named_token!(question_punctuation, "?", Token::Question);
named_token!(colon_punctuation, ":", Token::Colon);
named_token!(assign_punctuation, "=", Token::Assign);
named_token!(plusassign_punctuation, "+=", Token::PlusAssign);
named_token!(minusassign_punctuation, "-=", Token::MinusAssign);
named_token!(multiplyassign_punctuation, "*=", Token::MultAssign);
named_token!(modassign_punctuation, "%=", Token::ModAssign);
named_token!(doublestarassign_punctuation, "**=", Token::DoubleStarAssign);
named_token!(
    doublelarrowassign_punctuation,
    "<<=",
    Token::DoubleLArrowAssign
);
named_token!(
    doublerarrowassign_punctuation,
    ">>=",
    Token::DoubleRArrowAssign
);
named_token!(
    triplelarrowassign_punctuation,
    ">>>=",
    Token::TripleRArrowAssign
);
named_token!(ampassign_punctuation, "&=", Token::AmpAssign);
named_token!(pipeassign_punctuation, "|=", Token::PipeAssign);
named_token!(caretassign_punctuation, "^=", Token::CaretAssign);
named_token!(equalarrow_punctuation, "=>", Token::EqualArrow);

named_js!(
    NumericLiteral: DecimalLiteral | BinaryIntegerLiteral | OctalIntegerLiteral | HexIntegerLiteral
);

named_js!(DecimalLiteral: Decimal | OnlyDecimal | NoDecimal);

should!(
    decimalliteral_1,
    "01",
    vec![Token::NumericLiteral(Number::new(1, 0, 1, 10)), Token::EOF]
);
should!(
    decimalliteral_2,
    "-01",
    vec![
        Token::Minus,
        Token::NumericLiteral(Number::new(1, 0, 1, 10)),
        Token::EOF
    ]
);
should!(
    decimalliteral_3,
    "01.1",
    vec![Token::NumericLiteral(Number::new(1, 1, 1, 10)), Token::EOF]
);
should!(
    decimalliteral_4,
    "-01.1",
    vec![
        Token::Minus,
        Token::NumericLiteral(Number::new(1, 1, 1, 10)),
        Token::EOF
    ]
);
should!(
    decimalliteral_5,
    "-01.1e2",
    vec![
        Token::Minus,
        Token::NumericLiteral(Number::new(1, 1, 2, 10)),
        Token::EOF
    ]
);
should!(
    decimalliteral_6,
    "01.1e2",
    vec![Token::NumericLiteral(Number::new(1, 1, 2, 10)), Token::EOF]
);
should!(
    decimalliteral_7,
    "01.1e-2",
    vec![Token::NumericLiteral(Number::new(1, 1, -2, 10)), Token::EOF]
);
should!(
    decimalliteral_8,
    ".1e2",
    vec![Token::NumericLiteral(Number::new(0, 1, 2, 10)), Token::EOF]
);
should!(
    decimalliteral_9,
    ".1e-2",
    vec![Token::NumericLiteral(Number::new(0, 1, -2, 10)), Token::EOF]
);
should!(
    decimalliteral_10,
    ".1e+2",
    vec![Token::NumericLiteral(Number::new(0, 1, 2, 10)), Token::EOF]
);
should!(
    decimalliteral_11,
    "1e+2",
    vec![Token::NumericLiteral(Number::new(1, 0, 2, 10)), Token::EOF]
);
should!(
    decimalliteral_12,
    "1e-2",
    vec![Token::NumericLiteral(Number::new(1, 0, -2, 10)), Token::EOF]
);

#[inline]
fn Decimal(input: CompleteStr) -> ParseResult {
    let integer = digit1(input)?;
    let separator = tag!(integer.0, ".")?;
    let mut consumed = separator.0;
    let decimal = digit1(separator.0);
    let exponent = if let Ok(decimal) = decimal {
        consumed = decimal.0;
        ExponentPart(decimal.0)
    } else {
        ExponentPart(separator.0)
    };
    if let Ok(exponent) = exponent {
        consumed = exponent.0;
    }
    let num = Number::new(
        (*integer.1).parse().unwrap(),
        decimal.map(|i| (*i.1).parse().unwrap()).unwrap_or(0),
        exponent.map(|i| i.1).unwrap_or(1),
        10,
    );

    Ok((consumed, Token::NumericLiteral(num)))
}

#[inline]
fn OnlyDecimal(input: CompleteStr) -> ParseResult {
    let separator = tag!(input, ".")?;
    let mut consumed = separator.0;
    let decimal = digit1(separator.0);
    let exponent = if let Ok(decimal) = decimal {
        consumed = decimal.0;
        ExponentPart(decimal.0)
    } else {
        ExponentPart(separator.0)
    };
    if let Ok(exponent) = exponent {
        consumed = exponent.0;
    }

    let num = Number::new(
        0,
        decimal.map(|i| (*i.1).parse().unwrap()).unwrap_or(0),
        exponent.map(|i| i.1).unwrap_or(1),
        10,
    );
    Ok((consumed, Token::NumericLiteral(num)))
}

#[inline]
fn NoDecimal(input: CompleteStr) -> ParseResult {
    let integer = digit1(input)?;
    let mut consumed = integer.0;
    let exponent = ExponentPart(integer.0);
    if let Ok(exponent) = exponent {
        consumed = exponent.0;
    }
    let num = Number::new(
        (*integer.1).parse().unwrap(),
        0,
        exponent.map(|i| i.1).unwrap_or(1),
        10,
    );
    Ok((consumed, Token::NumericLiteral(num)))
}

#[inline]
fn ExponentPart(input: CompleteStr) -> IResult<CompleteStr, i32> {
    let indicator = one_of!(input, "eE")?;
    SignedInteger(indicator.0)
}

#[inline]
fn SignedInteger(input: CompleteStr) -> IResult<CompleteStr, i32> {
    let sign = one_of!(input, "+-");
    let integer = if let Ok(sign) = sign {
        digit1(sign.0)?
    } else {
        digit1(input)?
    };
    let mut integer = (integer.0, (*integer.1).parse().unwrap());
    if let Ok(sign) = sign {
        if sign.1 == '-' {
            integer.1 *= -1;
        }
    }
    Ok(integer)
}

#[inline]
fn BinaryIntegerLiteral(input: CompleteStr) -> ParseResult {
    let input = tag!(input, "0")?;
    let input = one_of!(input.0, "bB")?;
    let bin = many1!(input.0, one_of!("01"))?;

    let num = Number::new(
        u32::from_str_radix(&bin.1.into_iter().collect::<String>()[..], 2).unwrap(),
        0,
        1,
        2,
    );
    Ok((bin.0, Token::NumericLiteral(num)))
}
should!(
    binaryliteral_1,
    "0b01",
    vec![Token::NumericLiteral(Number::new(1, 0, 1, 2)), Token::EOF]
);
should!(
    binaryliteral_2,
    "0b101",
    vec![Token::NumericLiteral(Number::new(5, 0, 1, 2)), Token::EOF]
);
should!(
    binaryliteral_3,
    "0B01",
    vec![Token::NumericLiteral(Number::new(1, 0, 1, 2)), Token::EOF]
);

#[inline]
fn OctalIntegerLiteral(input: CompleteStr) -> ParseResult {
    let input = tag!(input, "0")?;
    let input = one_of!(input.0, "oO")?;
    let oct = oct_digit(input.0)?;

    let num = Number::new(u32::from_str_radix(*oct.1, 8).unwrap(), 0, 1, 8);
    Ok((oct.0, Token::NumericLiteral(num)))
}
should!(
    octalliteral_1,
    "0o017",
    vec![Token::NumericLiteral(Number::new(15, 0, 1, 8)), Token::EOF]
);
should!(
    octalliteral_2,
    "0O7",
    vec![Token::NumericLiteral(Number::new(7, 0, 1, 8)), Token::EOF]
);

#[inline]
fn HexIntegerLiteral(input: CompleteStr) -> ParseResult {
    let input = tag!(input, "0")?;
    let input = one_of!(input.0, "xX")?;
    let hex = hex_digit1(input.0)?;

    let num = Number::new(u32::from_str_radix(*hex.1, 16).unwrap(), 0, 1, 16);
    Ok((hex.0, Token::NumericLiteral(num)))
}
should!(
    hexliteral_1,
    "0xC",
    vec![Token::NumericLiteral(Number::new(12, 0, 1, 16)), Token::EOF]
);
should!(
    hexliteral_2,
    "0X0C",
    vec![Token::NumericLiteral(Number::new(12, 0, 1, 16)), Token::EOF]
);
should!(
    hexliteral_3,
    "0x1C",
    vec![Token::NumericLiteral(Number::new(28, 0, 1, 16)), Token::EOF]
);

named_js!(StringLiteral: StringLiteralDouble | StringLiteralSingle);

should!(
    string_literal_1,
    "\"\"",
    vec![Token::StringLiteral(String::new()), Token::EOF]
);
should!(
    string_literal_2,
    "\"ab\"",
    vec![Token::StringLiteral(String::from("ab")), Token::EOF]
);
should_incomplete!(string_literal_3, r#""ab\01""#);
should_ignore!(
    string_literal_4,
    r#""ab\0""#,
    vec![Token::StringLiteral(String::from("ab0")), Token::EOF]
);
should!(
    string_literal_5,
    "\"ab\\n\"",
    vec![Token::StringLiteral(String::from("ab\n")), Token::EOF]
);
should!(
    string_literal_6,
    "'ab'",
    vec![Token::StringLiteral(String::from("ab")), Token::EOF]
);
should_fail!(
    string_literal_7,
    "\"a\"\"",
    vec![Token::StringLiteral(String::from("a")), Token::EOF]
);
should_incomplete!(string_literal_8, "\"\n\"");
should!(
    string_literal_9,
    "\"aa\"",
    vec![Token::StringLiteral(String::from("aa")), Token::EOF]
);
should_incomplete!(string_literal_10, "'\n'");
should!(
    string_literal_11,
    "'aa'",
    vec![Token::StringLiteral(String::from("aa")), Token::EOF]
);
should_incomplete!(string_literal_12, r"'\01'");
should!(
    string_literal_13,
    r"'\x1A'",
    vec![Token::StringLiteral(String::from("\u{001a}")), Token::EOF]
);
should!(
    string_literal_14,
    r#""\x1A""#,
    vec![Token::StringLiteral(String::from("\u{001a}")), Token::EOF]
);
should_ignore!(
    string_literal_15,
    r"'\u{01A}'",
    vec![Token::StringLiteral(String::from("\u{001a}")), Token::EOF]
);
should_ignore!(
    string_literal_16,
    r#""\u{01A}""#,
    vec![Token::StringLiteral(String::from("\u{001a}")), Token::EOF]
);
should!(
    string_literal_17,
    r"'\u001A'",
    vec![Token::StringLiteral(String::from("\u{001a}")), Token::EOF]
);
should!(
    string_literal_18,
    r#""\u001A""#,
    vec![Token::StringLiteral(String::from("\u{001a}")), Token::EOF]
);
should!(
    string_literal_19,
    r#""\'\"\b\f\n\r\t""#,
    vec![
        Token::StringLiteral(String::from("\'\"\u{8}\u{c}\n\r\t")),
        Token::EOF
    ]
);
should_ignore!(
    string_literal_20,
    r"'\v'",
    vec![Token::StringLiteral(String::from("\u{001a}")), Token::EOF]
);
should!(
    string_literal_21,
    r#""number\na"
"#,
    vec![
        Token::StringLiteral(String::from("number\na")),
        Token::LF,
        Token::EOF
    ]
);

#[inline]
fn StringLiteralDouble(input: CompleteStr) -> ParseResult {
    let input = tag!(input, "\"")?;
    let strr = many1!(input.0, DoubleStringCharacter);
    let input = if let Ok((strr, _)) = strr {
        tag!(strr, "\"")?
    } else {
        tag!(input.0, "\"")?
    };

    let strr = if let Ok((_, strr)) = strr {
        strr.into_iter().map(|s| *s).collect::<Vec<&str>>().concat()
    } else {
        String::new()
    };

    Ok((
        input.0,
        Token::StringLiteral(unescape(&strr[..]).expect(&strr[..])),
    ))
}

#[inline]
fn StringLiteralSingle(input: CompleteStr) -> ParseResult {
    let input = tag!(input, "'")?;
    let string = many1!(input.0, SingleStringCharacter);
    let input = if let Ok((string, _)) = string {
        tag!(string, "'")?
    } else {
        tag!(input.0, "'")?
    };

    let string = if let Ok((_, string)) = string {
        string
            .into_iter()
            .map(|s| *s)
            .collect::<Vec<&str>>()
            .concat()
    } else {
        String::new()
    };

    Ok((
        input.0,
        Token::StringLiteral(unescape(&string[..]).unwrap()),
    ))
}

named!(DoubleStringCharacter<CompleteStr, CompleteStr>, alt_longest!(
          DoubleStringCharacter1 |
          DoubleStringCharacter2 |
          LineContinuation
));

named!(SingleStringCharacter<CompleteStr, CompleteStr>, alt_longest!(
          SingleStringCharacter1 |
          DoubleStringCharacter2 |
          LineContinuation
));

#[inline]
fn DoubleStringCharacter1(input: CompleteStr) -> IResult<CompleteStr, CompleteStr> {
    let input = take!(input, 1)?;
    none_of!(input.1, r#""\"#)?;
    not!(input.1, LineTerminator)?;

    Ok((input.0, input.1))
}

#[inline]
fn SingleStringCharacter1(input: CompleteStr) -> IResult<CompleteStr, CompleteStr> {
    let input = take!(input, 1)?;
    none_of!(input.1, r"'\")?;
    not!(input.1, LineTerminator)?;

    Ok((input.0, input.1))
}

#[inline]
fn DoubleStringCharacter2(input: CompleteStr) -> IResult<CompleteStr, CompleteStr> {
    let i = tag!(input, r"\")?;
    let e = EscapeSequence(i.0)?;
    let c = e.1.len() + 1;
    Ok((CompleteStr(&input[c..]), CompleteStr(&input[..c])))
}

#[inline]
fn LineContinuation(input: CompleteStr) -> IResult<CompleteStr, CompleteStr> {
    let input = tag!(input, r"\")?;
    let input = LineTerminatorSequence(input.0)?;

    Ok((input.0, CompleteStr(r"\n")))
}

named!(LineTerminatorSequence<CompleteStr, ()>, alt_longest!(
          LineTerminatorSequence1 |
          LineTerminatorSequence2
));

#[inline]
fn LineTerminatorSequence1(input: CompleteStr) -> IResult<CompleteStr, ()> {
    let input = LF(input)?;
    let input = CR(input.0)?;
    not!(input.0, LF)?;
    Ok((input.0, ()))
}

#[inline]
fn LineTerminatorSequence2(input: CompleteStr) -> IResult<CompleteStr, ()> {
    let input = LS(input)?;
    let input = PS(input.0)?;
    let input = CR(input.0)?;
    let input = LF(input.0)?;
    Ok((input.0, ()))
}

named!(EscapeSequence<CompleteStr, CompleteStr>, alt_longest!(
    CharacterEscapeSequence |
    EscapeSequence0 |
    HexEscapeSequence |
    UnicodeEscapeSequenceStr
));

#[inline]
fn EscapeSequence0(input: CompleteStr) -> IResult<CompleteStr, CompleteStr> {
    let input = tag!(input, "0")?;
    not!(input.0, digit1)?;
    Ok(input)
}

#[inline]
fn HexEscapeSequence(input: CompleteStr) -> IResult<CompleteStr, CompleteStr> {
    tag!(input, "x")?;
    let mut chars = input.chars();
    chars.next(); // x letter
    let c = chars.next();
    if let Some(c) = c {
        let mut byte = [0; 1];
        c.encode_utf8(&mut byte);
        if !is_hex_digit(byte[0]) {
            return Err(Err::Error(error_position!(input, ErrorKind::Custom(1))));
        }
    } else {
        return Err(Err::Incomplete(Needed::Size(2)));
    }
    let c = chars.next();
    if let Some(c) = c {
        let mut byte = [0; 1];
        c.encode_utf8(&mut byte);
        if !is_hex_digit(byte[0]) {
            return Err(Err::Error(error_position!(input, ErrorKind::Custom(1))));
        }
    } else {
        return Err(Err::Incomplete(Needed::Size(1)));
    }

    Ok((CompleteStr(&(*input)[2..]), CompleteStr(&(*input)[..2])))
}

#[inline]
fn UnicodeEscapeSequenceStr(input: CompleteStr) -> IResult<CompleteStr, CompleteStr> {
    let res = unicode_escape_sequence(input)?;
    let i = input.len() - res.0.len();
    Ok((CompleteStr(&input[i..]), CompleteStr(&input[..i])))
}

named!(CharacterEscapeSequence<CompleteStr, CompleteStr>, alt_longest!(
    SingleEscapeCharacter |
    NonEscapeCharacter
));

#[inline]
fn SingleEscapeCharacter(input: CompleteStr) -> IResult<CompleteStr, CompleteStr> {
    let o = one_of!(input, r#"'"\bfnrtv"#)?;
    let c = &(*input)[..1];
    Ok((o.0, CompleteStr(c)))
}

#[inline]
fn NonEscapeCharacter(input: CompleteStr) -> IResult<CompleteStr, CompleteStr> {
    let input = take!(input, 1)?;
    not!(input.1, EscapeCharacter)?;
    not!(input.1, LineTerminator)?;
    Ok((input.0, CompleteStr("")))
}

named!(EscapeCharacter<CompleteStr, CompleteStr>, alt_longest!(
    SingleEscapeCharacter |
    digit1 |
    tag!("x") |
    tag!("u")
));

named_js!(DivPunctuator: slash_punctuation | slashequal_punctuation);
should!(div_punctuator_1, "/", vec![Token::Slash, Token::EOF]);
should!(
    div_punctuator_2,
    "/ /=",
    vec![Token::Slash, Token::SlashEqual, Token::EOF]
);

named_token!(slash_punctuation, "/", Token::Slash);
should!(slash_punctuation_1, "/", vec![Token::Slash, Token::EOF]);
named_token!(slashequal_punctuation, "/=", Token::SlashEqual);
should!(
    slashequal_punctuation_1,
    "/=",
    vec![Token::SlashEqual, Token::EOF]
);

named_js!(RightBracePunctuator: rightbrace_punctuation);

named_token!(rightbrace_punctuation, "}", Token::RBrace);
should!(
    rightbrace_punctuation_1,
    "}",
    vec![Token::RBrace, Token::EOF]
);
