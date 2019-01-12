#[derive(Clone, Copy, Debug)]
pub enum Equivalence {
    HELL,
    White,
    LineTerminator,
    Slash,        // /
    Star,         // *
    LCurly,       // {
    RCurly,       // }
    LRound,       // (
    RRound,       // )
    LSquare,      // [
    RSquare,      // ]
    Dot,          // .
    Semicolon,    // ;
    Comma,        // ,
    Lesser,       // starts with <
    Bigger,       // starts with >
    Exclamation,  // starts with  !
    Assign,       // starts with  =
    Plus,         // starts with  +
    Minus,        // starts with  -
    Percent,      // starts with %
    And,          // &
    Or,           // |
    Caret,        // ^
    Tilde,        // ~
    Questionmark, // ?
    Colon,        // :
    DoubleString, // "
    SingleString, // '
    EightNine,    // 8-9
    Zero,         // 0
    One,          // 1
    Two,          // 2
    Three,        // 3
    Four,         // 4
    Five,         // 5
    Six,          // 6
    Seven,        // 7
    A,            // a A
    B,            // b B
    C,            // c C
    D,            // d D
    E,            // e E
    F,            // f F
    X,            // x X
    O,            // o O
    BackTick,     // `
    Letter,       // A-Za-z_
}
type EquivalenceTable = [Equivalence; 255];
pub const EQUIVALENCE_CLASS: EquivalenceTable = gen_equivalence();
const fn gen_equivalence() -> EquivalenceTable {
    let mut res = [Equivalence::HELL; 255];
    res = ident(res);
    res = white_space(res);
    res = line_terminator(res);
    res['/' as usize] = Equivalence::Slash;
    res['`' as usize] = Equivalence::BackTick;
    res['*' as usize] = Equivalence::Star;
    res = punctuators(res);
    res['\'' as usize] = Equivalence::SingleString;
    res['"' as usize] = Equivalence::DoubleString;
    res = numbers(res);
    res
}

const fn white_space(mut res: EquivalenceTable) -> EquivalenceTable {
    res['\t' as usize] = Equivalence::White;
    res['\u{000B}' as usize] = Equivalence::White;
    res['\u{000C}' as usize] = Equivalence::White;
    res[' ' as usize] = Equivalence::White;
    res['\u{00A0}' as usize] = Equivalence::White;
    res
}

const fn line_terminator(mut res: EquivalenceTable) -> EquivalenceTable {
    res['\u{000A}' as usize] = Equivalence::LineTerminator;
    res['\u{000D}' as usize] = Equivalence::LineTerminator;
    res
}

const fn punctuators(mut res: EquivalenceTable) -> EquivalenceTable {
    res['-' as usize] = Equivalence::Minus;
    res[',' as usize] = Equivalence::Comma;
    res[';' as usize] = Equivalence::Semicolon;
    res[':' as usize] = Equivalence::Colon;
    res['!' as usize] = Equivalence::Exclamation;
    res['?' as usize] = Equivalence::Questionmark;
    res['.' as usize] = Equivalence::Dot;
    res['(' as usize] = Equivalence::LRound;
    res[')' as usize] = Equivalence::RRound;
    res['[' as usize] = Equivalence::LSquare;
    res[']' as usize] = Equivalence::RSquare;
    res['{' as usize] = Equivalence::LCurly;
    res['}' as usize] = Equivalence::RCurly;
    res['*' as usize] = Equivalence::Star;
    res['&' as usize] = Equivalence::And;
    res['%' as usize] = Equivalence::Percent;
    res['^' as usize] = Equivalence::Caret;
    res['+' as usize] = Equivalence::Plus;
    res['<' as usize] = Equivalence::Lesser;
    res['=' as usize] = Equivalence::Assign;
    res['>' as usize] = Equivalence::Bigger;
    res['|' as usize] = Equivalence::Or;
    res['~' as usize] = Equivalence::Tilde;
    res
}

const fn numbers(mut res: EquivalenceTable) -> EquivalenceTable {
    res['0' as usize] = Equivalence::Zero;
    res['1' as usize] = Equivalence::One;
    res['2' as usize] = Equivalence::Two;
    res['3' as usize] = Equivalence::Three;
    res['4' as usize] = Equivalence::Four;
    res['5' as usize] = Equivalence::Five;
    res['6' as usize] = Equivalence::Six;
    res['7' as usize] = Equivalence::Seven;
    res['8' as usize] = Equivalence::EightNine;
    res['9' as usize] = Equivalence::EightNine;
    res['A' as usize] = Equivalence::A;
    res['a' as usize] = Equivalence::A;
    res['B' as usize] = Equivalence::B;
    res['b' as usize] = Equivalence::B;
    res['c' as usize] = Equivalence::C;
    res['C' as usize] = Equivalence::C;
    res['D' as usize] = Equivalence::D;
    res['d' as usize] = Equivalence::D;
    res['e' as usize] = Equivalence::E;
    res['E' as usize] = Equivalence::E;
    res['F' as usize] = Equivalence::F;
    res['f' as usize] = Equivalence::F;
    res['o' as usize] = Equivalence::O;
    res['O' as usize] = Equivalence::O;
    res['X' as usize] = Equivalence::X;
    res['x' as usize] = Equivalence::X;
    res
}

const fn ident(mut res: EquivalenceTable) -> EquivalenceTable {
    res['_' as usize] = Equivalence::Letter;
    res['$' as usize] = Equivalence::Letter;
    res['a' as usize] = Equivalence::Letter;
    res['A' as usize] = Equivalence::Letter;
    res['b' as usize] = Equivalence::Letter;
    res['B' as usize] = Equivalence::Letter;
    res['c' as usize] = Equivalence::Letter;
    res['C' as usize] = Equivalence::Letter;
    res['D' as usize] = Equivalence::Letter;
    res['d' as usize] = Equivalence::Letter;
    res['E' as usize] = Equivalence::Letter;
    res['e' as usize] = Equivalence::Letter;
    res['f' as usize] = Equivalence::Letter;
    res['F' as usize] = Equivalence::Letter;
    res['g' as usize] = Equivalence::Letter;
    res['G' as usize] = Equivalence::Letter;
    res['h' as usize] = Equivalence::Letter;
    res['H' as usize] = Equivalence::Letter;
    res['I' as usize] = Equivalence::Letter;
    res['i' as usize] = Equivalence::Letter;
    res['J' as usize] = Equivalence::Letter;
    res['j' as usize] = Equivalence::Letter;
    res['k' as usize] = Equivalence::Letter;
    res['K' as usize] = Equivalence::Letter;
    res['l' as usize] = Equivalence::Letter;
    res['L' as usize] = Equivalence::Letter;
    res['m' as usize] = Equivalence::Letter;
    res['M' as usize] = Equivalence::Letter;
    res['N' as usize] = Equivalence::Letter;
    res['n' as usize] = Equivalence::Letter;
    res['o' as usize] = Equivalence::Letter;
    res['O' as usize] = Equivalence::Letter;
    res['P' as usize] = Equivalence::Letter;
    res['p' as usize] = Equivalence::Letter;
    res['Q' as usize] = Equivalence::Letter;
    res['q' as usize] = Equivalence::Letter;
    res['r' as usize] = Equivalence::Letter;
    res['R' as usize] = Equivalence::Letter;
    res['s' as usize] = Equivalence::Letter;
    res['S' as usize] = Equivalence::Letter;
    res['T' as usize] = Equivalence::Letter;
    res['t' as usize] = Equivalence::Letter;
    res['U' as usize] = Equivalence::Letter;
    res['u' as usize] = Equivalence::Letter;
    res['v' as usize] = Equivalence::Letter;
    res['V' as usize] = Equivalence::Letter;
    res['x' as usize] = Equivalence::Letter;
    res['X' as usize] = Equivalence::Letter;
    res['y' as usize] = Equivalence::Letter;
    res['Y' as usize] = Equivalence::Letter;
    res['Z' as usize] = Equivalence::Letter;
    res['z' as usize] = Equivalence::Letter;
    res['W' as usize] = Equivalence::Letter;
    res['w' as usize] = Equivalence::Letter;
    res
}
