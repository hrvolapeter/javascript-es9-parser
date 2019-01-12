match (self, e) {
(StateMachineWrapper::InputElementDiv(s), Equivalence::LineTerminator) => {
                StateMachineWrapper::LineTerminator(s.into())
            }
            (StateMachineWrapper::InputElementDiv(s), Equivalence::White) => {
                StateMachineWrapper::WhiteSpace(s.into())
            }
            (StateMachineWrapper::InputElementDiv(s), Equivalence::Slash) => {
                StateMachineWrapper::Slash(s.into())
            } // /
            (StateMachineWrapper::InputElementDiv(s), Equivalence::Dot) => {
                StateMachineWrapper::DotPart(s.into())
            }
            (StateMachineWrapper::Slash(s), Equivalence::Assign) => {
                StateMachineWrapper::SlashAcc(s.into())
            } // /
            (StateMachineWrapper::Slash(s), Equivalence::Slash) => {
                StateMachineWrapper::SingleLineComment(s.into())
            } // //
            (StateMachineWrapper::Slash(s), Equivalence::Star) => {
                StateMachineWrapper::MultiLineComment(s.into())
            }
            (StateMachineWrapper::Slash(s), _) => {
                StateMachineWrapper::SlashAcc(s.into())
            }
            // Singleline comment
            (StateMachineWrapper::SingleLineComment(s), Equivalence::LineTerminator) => {
                StateMachineWrapper::SingleLineCommentAcc(s.into())
            }
            (StateMachineWrapper::SingleLineComment(s), _) => {
                StateMachineWrapper::SingleLineComment(s.into())
            }
            // Multiline comment
            (StateMachineWrapper::MultiLineComment(s), Equivalence::Star) => {
                StateMachineWrapper::MultiLineCommentStar(s.into())
            }
            (StateMachineWrapper::MultiLineCommentStar(s), Equivalence::Star) => {
                StateMachineWrapper::MultiLineCommentStar(s.into())
            }
            (StateMachineWrapper::MultiLineCommentStar(s), Equivalence::Slash) => {
                StateMachineWrapper::MultiLineCommentAcc(s.into())
            }
            (StateMachineWrapper::MultiLineComment(s), _) => {
                StateMachineWrapper::MultiLineComment(s.into())
            }
            (StateMachineWrapper::MultiLineCommentStar(s), _) => {
                StateMachineWrapper::MultiLineComment(s.into())
            }
            // Ident
            (StateMachineWrapper::InputElementDiv(s), Equivalence::Letter) => {
                StateMachineWrapper::Identifier(s.into())
            }
            (StateMachineWrapper::InputElementDiv(s), Equivalence::A) => {
                StateMachineWrapper::Identifier(s.into())
            }
            (StateMachineWrapper::InputElementDiv(s), Equivalence::B) => {
                StateMachineWrapper::Identifier(s.into())
            }
            (StateMachineWrapper::InputElementDiv(s), Equivalence::C) => {
                StateMachineWrapper::Identifier(s.into())
            }
            (StateMachineWrapper::InputElementDiv(s), Equivalence::D) => {
                StateMachineWrapper::Identifier(s.into())
            }
            (StateMachineWrapper::InputElementDiv(s), Equivalence::E) => {
                StateMachineWrapper::Identifier(s.into())
            }
            (StateMachineWrapper::InputElementDiv(s), Equivalence::F) => {
                StateMachineWrapper::Identifier(s.into())
            }
            (StateMachineWrapper::InputElementDiv(s), Equivalence::O) => {
                StateMachineWrapper::Identifier(s.into())
            }
            (StateMachineWrapper::InputElementDiv(s), Equivalence::X) => {
                StateMachineWrapper::Identifier(s.into())
            }
            // Punctuator
            (StateMachineWrapper::InputElementDiv(s), Equivalence::LCurly) => {
                StateMachineWrapper::LCurly(s.into())
            }
            (StateMachineWrapper::InputElementDiv(s), Equivalence::RCurly) => {
                StateMachineWrapper::RCurly(s.into())
            }
            (StateMachineWrapper::InputElementDiv(s), Equivalence::LRound) => {
                StateMachineWrapper::LRound(s.into())
            }
            (StateMachineWrapper::InputElementDiv(s), Equivalence::RRound) => {
                StateMachineWrapper::RRound(s.into())
            }
            (StateMachineWrapper::InputElementDiv(s), Equivalence::LSquare) => {
                StateMachineWrapper::LSquare(s.into())
            }
            (StateMachineWrapper::InputElementDiv(s), Equivalence::RSquare) => {
                StateMachineWrapper::RSquare(s.into())
            }
            (StateMachineWrapper::InputElementDiv(s), Equivalence::Semicolon) => {
                StateMachineWrapper::Semicolon(s.into())
            }
            (StateMachineWrapper::InputElementDiv(s), Equivalence::Comma) => {
                StateMachineWrapper::Comma(s.into())
            }
            (StateMachineWrapper::InputElementDiv(s), Equivalence::Colon) => {
                StateMachineWrapper::Colon(s.into())
            }
            (StateMachineWrapper::InputElementDiv(s), Equivalence::Questionmark) => {
                StateMachineWrapper::QuestionMark(s.into())
            }
            (StateMachineWrapper::InputElementDiv(s), Equivalence::Tilde) => {
                StateMachineWrapper::Tilde(s.into())
            }
            // Lesser
            (StateMachineWrapper::InputElementDiv(s), Equivalence::Lesser) => {
                StateMachineWrapper::Lesser(s.into())
            }
            (StateMachineWrapper::Lesser(s), Equivalence::Assign) => {
                StateMachineWrapper::Lesser(s.into())
            }
            (StateMachineWrapper::Lesser(s), Equivalence::Lesser) => {
                StateMachineWrapper::Lesser(s.into())
            }
            (StateMachineWrapper::Lesser(s), _) => {
                StateMachineWrapper::LesserAcc(s.into())
            }
            // Bigger
            (StateMachineWrapper::InputElementDiv(s), Equivalence::Bigger) => {
                StateMachineWrapper::Bigger(s.into())
            }
            (StateMachineWrapper::Bigger(s), Equivalence::Assign) => {
                StateMachineWrapper::Bigger(s.into())
            }
            (StateMachineWrapper::Bigger(s), Equivalence::Bigger) => {
                StateMachineWrapper::Bigger(s.into())
            }
            (StateMachineWrapper::Bigger(s), _) => {
                StateMachineWrapper::BiggerAcc(s.into())
            }
            // Assign
            (StateMachineWrapper::InputElementDiv(s), Equivalence::Assign) => {
                StateMachineWrapper::Assign(s.into())
            }
            (StateMachineWrapper::Assign(s), Equivalence::Bigger) => {
                StateMachineWrapper::Assign(s.into())
            }
            (StateMachineWrapper::Assign(s), Equivalence::Assign) => {
                StateMachineWrapper::Assign(s.into())
            }
            (StateMachineWrapper::Assign(s), _) => {
                StateMachineWrapper::AssignAcc(s.into())
            }
            // Exclamation
            (StateMachineWrapper::InputElementDiv(s), Equivalence::Exclamation) => {
                StateMachineWrapper::Exclamation(s.into())
            }
            (StateMachineWrapper::Exclamation(s), Equivalence::Assign) => {
                StateMachineWrapper::Exclamation(s.into())
            }
            (StateMachineWrapper::Exclamation(s), _) => {
                StateMachineWrapper::ExclamationAcc(s.into())
            }
            // Plus
            (StateMachineWrapper::InputElementDiv(s), Equivalence::Plus) => {
                StateMachineWrapper::Plus(s.into())
            }
            (StateMachineWrapper::Plus(s), Equivalence::Plus) => {
                StateMachineWrapper::Plus(s.into())
            }
            (StateMachineWrapper::Plus(s), Equivalence::Assign) => {
                StateMachineWrapper::Plus(s.into())
            }
            (StateMachineWrapper::Plus(s), _) => {
                StateMachineWrapper::PlusAcc(s.into())
            }
            // Minus
            (StateMachineWrapper::InputElementDiv(s), Equivalence::Minus) => {
                StateMachineWrapper::Minus(s.into())
            }
            (StateMachineWrapper::Minus(s), Equivalence::Minus) => {
                StateMachineWrapper::Minus(s.into())
            }
            (StateMachineWrapper::Minus(s), Equivalence::Assign) => {
                StateMachineWrapper::Minus(s.into())
            }
            (StateMachineWrapper::Minus(s), _) => {
                StateMachineWrapper::MinusAcc(s.into())
            }
            // Star
            (StateMachineWrapper::InputElementDiv(s), Equivalence::Star) => {
                StateMachineWrapper::Star(s.into())
            }
            (StateMachineWrapper::Star(s), Equivalence::Star) => {
                StateMachineWrapper::Star(s.into())
            }
            (StateMachineWrapper::Star(s), Equivalence::Assign) => {
                StateMachineWrapper::Star(s.into())
            }
            (StateMachineWrapper::Star(s), _) => {
                StateMachineWrapper::StarAcc(s.into())
            }
            // PErcent
            (StateMachineWrapper::InputElementDiv(s), Equivalence::Percent) => {
                StateMachineWrapper::Percent(s.into())
            }
            (StateMachineWrapper::Percent(s), Equivalence::Percent) => {
                StateMachineWrapper::Percent(s.into())
            }
            (StateMachineWrapper::Percent(s), Equivalence::Assign) => {
                StateMachineWrapper::Percent(s.into())
            }
            (StateMachineWrapper::Percent(s), _) => {
                StateMachineWrapper::PercentAcc(s.into())
            }
            // and
            (StateMachineWrapper::InputElementDiv(s), Equivalence::And) => {
                StateMachineWrapper::And(s.into())
            }
            (StateMachineWrapper::And(s), Equivalence::And) => StateMachineWrapper::And(s.into()),
            (StateMachineWrapper::And(s), Equivalence::Assign) => {
                StateMachineWrapper::And(s.into())
            }
            (StateMachineWrapper::And(s), _) => StateMachineWrapper::AndAcc(s.into()),
            // or
            (StateMachineWrapper::InputElementDiv(s), Equivalence::Or) => {
                StateMachineWrapper::Or(s.into())
            }
            (StateMachineWrapper::Or(s), Equivalence::Or) => StateMachineWrapper::Or(s.into()),
            (StateMachineWrapper::Or(s), Equivalence::Assign) => StateMachineWrapper::Or(s.into()),
            (StateMachineWrapper::Or(s), _) => StateMachineWrapper::OrAcc(s.into()),
            // caret
            (StateMachineWrapper::InputElementDiv(s), Equivalence::Caret) => {
                StateMachineWrapper::Caret(s.into())
            }
            (StateMachineWrapper::Caret(s), Equivalence::Caret) => {
                StateMachineWrapper::Caret(s.into())
            }
            (StateMachineWrapper::Caret(s), Equivalence::Assign) => {
                StateMachineWrapper::Caret(s.into())
            }
            (StateMachineWrapper::Caret(s), _) => {
                StateMachineWrapper::CaretAcc(s.into())
            }
            // string
            (StateMachineWrapper::InputElementDiv(s), Equivalence::SingleString) => {
                StateMachineWrapper::SingleString(s.into())
            }
            (StateMachineWrapper::InputElementDiv(s), Equivalence::DoubleString) => {
                StateMachineWrapper::DoubleString(s.into())
            }
            (StateMachineWrapper::InputElementDiv(s), Equivalence::BackTick) => {
                StateMachineWrapper::Template(s.into())
            }
            // numbers
            (StateMachineWrapper::InputElementDiv(s), Equivalence::Zero) => {
                StateMachineWrapper::SawZero(s.into())
            }
            (StateMachineWrapper::InputElementDiv(s), Equivalence::One) => {
                StateMachineWrapper::Decimal(s.into())
            }
            (StateMachineWrapper::InputElementDiv(s), Equivalence::Two) => {
                StateMachineWrapper::Decimal(s.into())
            }
            (StateMachineWrapper::InputElementDiv(s), Equivalence::Three) => {
                StateMachineWrapper::Decimal(s.into())
            }
            (StateMachineWrapper::InputElementDiv(s), Equivalence::Four) => {
                StateMachineWrapper::Decimal(s.into())
            }
            (StateMachineWrapper::InputElementDiv(s), Equivalence::Five) => {
                StateMachineWrapper::Decimal(s.into())
            }
            (StateMachineWrapper::InputElementDiv(s), Equivalence::Six) => {
                StateMachineWrapper::Decimal(s.into())
            }
            (StateMachineWrapper::InputElementDiv(s), Equivalence::Seven) => {
                StateMachineWrapper::Decimal(s.into())
            }
            (StateMachineWrapper::InputElementDiv(s), Equivalence::EightNine) => {
                StateMachineWrapper::Decimal(s.into())
            }

            (StateMachineWrapper::SawZero(s), Equivalence::B) => {
                StateMachineWrapper::Binary(s.into())
            }
            (StateMachineWrapper::SawZero(s), Equivalence::O) => {
                StateMachineWrapper::Octal(s.into())
            }
            (StateMachineWrapper::SawZero(s), Equivalence::X) => StateMachineWrapper::Hex(s.into()),
            (StateMachineWrapper::SawZero(s), Equivalence::One) => {
                StateMachineWrapper::Decimal(s.into())
            }
            (StateMachineWrapper::SawZero(s), Equivalence::Two) => {
                StateMachineWrapper::Decimal(s.into())
            }
            (StateMachineWrapper::SawZero(s), Equivalence::Three) => {
                StateMachineWrapper::Decimal(s.into())
            }
            (StateMachineWrapper::SawZero(s), Equivalence::Four) => {
                StateMachineWrapper::Decimal(s.into())
            }
            (StateMachineWrapper::SawZero(s), Equivalence::Five) => {
                StateMachineWrapper::Decimal(s.into())
            }
            (StateMachineWrapper::SawZero(s), Equivalence::Six) => {
                StateMachineWrapper::Decimal(s.into())
            }
            (StateMachineWrapper::SawZero(s), Equivalence::Seven) => {
                StateMachineWrapper::Decimal(s.into())
            }
            (StateMachineWrapper::SawZero(s), Equivalence::EightNine) => {
                StateMachineWrapper::Decimal(s.into())
            }
            (StateMachineWrapper::SawZero(s), _) => {
                StateMachineWrapper::DecimalAcc(s.into())
            }

            (StateMachineWrapper::Decimal(s), Equivalence::Zero) => {
                StateMachineWrapper::Decimal(s.into())
            }
            (StateMachineWrapper::Decimal(s), Equivalence::One) => {
                StateMachineWrapper::Decimal(s.into())
            }
            (StateMachineWrapper::Decimal(s), Equivalence::Two) => {
                StateMachineWrapper::Decimal(s.into())
            }
            (StateMachineWrapper::Decimal(s), Equivalence::Three) => {
                StateMachineWrapper::Decimal(s.into())
            }
            (StateMachineWrapper::Decimal(s), Equivalence::Four) => {
                StateMachineWrapper::Decimal(s.into())
            }
            (StateMachineWrapper::Decimal(s), Equivalence::Five) => {
                StateMachineWrapper::Decimal(s.into())
            }
            (StateMachineWrapper::Decimal(s), Equivalence::Six) => {
                StateMachineWrapper::Decimal(s.into())
            }
            (StateMachineWrapper::Decimal(s), Equivalence::Seven) => {
                StateMachineWrapper::Decimal(s.into())
            }
            (StateMachineWrapper::Decimal(s), Equivalence::EightNine) => {
                StateMachineWrapper::Decimal(s.into())
            }
            (StateMachineWrapper::Decimal(s), Equivalence::Dot) => {
                StateMachineWrapper::DecimalDigits(s.into())
            }
            (StateMachineWrapper::Decimal(s), Equivalence::E) => {
                StateMachineWrapper::DecimalExponent(s.into())
            }
            (StateMachineWrapper::Decimal(s), _) => {
                StateMachineWrapper::DecimalAcc(s.into())
            }

            (StateMachineWrapper::DecimalDigits(s), Equivalence::Zero) => {
                StateMachineWrapper::DecimalDigits(s.into())
            }
            (StateMachineWrapper::DecimalDigits(s), Equivalence::One) => {
                StateMachineWrapper::DecimalDigits(s.into())
            }
            (StateMachineWrapper::DecimalDigits(s), Equivalence::Two) => {
                StateMachineWrapper::DecimalDigits(s.into())
            }
            (StateMachineWrapper::DecimalDigits(s), Equivalence::Three) => {
                StateMachineWrapper::DecimalDigits(s.into())
            }
            (StateMachineWrapper::DecimalDigits(s), Equivalence::Four) => {
                StateMachineWrapper::DecimalDigits(s.into())
            }
            (StateMachineWrapper::DecimalDigits(s), Equivalence::Five) => {
                StateMachineWrapper::DecimalDigits(s.into())
            }
            (StateMachineWrapper::DecimalDigits(s), Equivalence::Six) => {
                StateMachineWrapper::DecimalDigits(s.into())
            }
            (StateMachineWrapper::DecimalDigits(s), Equivalence::Seven) => {
                StateMachineWrapper::DecimalDigits(s.into())
            }
            (StateMachineWrapper::DecimalDigits(s), Equivalence::EightNine) => {
                StateMachineWrapper::DecimalDigits(s.into())
            }
            (StateMachineWrapper::DecimalDigits(s), Equivalence::E) => {
                StateMachineWrapper::DecimalExponent(s.into())
            }
            (StateMachineWrapper::DecimalDigits(s), _) => {
                StateMachineWrapper::DecimalDigitsAcc(s.into())
            }

            (StateMachineWrapper::DecimalExponent(s), Equivalence::Zero) => {
                StateMachineWrapper::DecimalExponent(s.into())
            }
            (StateMachineWrapper::DecimalExponent(s), Equivalence::One) => {
                StateMachineWrapper::DecimalExponent(s.into())
            }
            (StateMachineWrapper::DecimalExponent(s), Equivalence::Two) => {
                StateMachineWrapper::DecimalExponent(s.into())
            }
            (StateMachineWrapper::DecimalExponent(s), Equivalence::Three) => {
                StateMachineWrapper::DecimalExponent(s.into())
            }
            (StateMachineWrapper::DecimalExponent(s), Equivalence::Four) => {
                StateMachineWrapper::DecimalExponent(s.into())
            }
            (StateMachineWrapper::DecimalExponent(s), Equivalence::Five) => {
                StateMachineWrapper::DecimalExponent(s.into())
            }
            (StateMachineWrapper::DecimalExponent(s), Equivalence::Six) => {
                StateMachineWrapper::DecimalExponent(s.into())
            }
            (StateMachineWrapper::DecimalExponent(s), Equivalence::Seven) => {
                StateMachineWrapper::DecimalExponent(s.into())
            }
            (StateMachineWrapper::DecimalExponent(s), Equivalence::EightNine) => {
                StateMachineWrapper::DecimalExponent(s.into())
            }
            (StateMachineWrapper::DecimalExponent(s), Equivalence::Minus) => {
                StateMachineWrapper::DecimalExponentSigned(s.into())
            }
            (StateMachineWrapper::DecimalExponent(s), Equivalence::Plus) => {
                StateMachineWrapper::DecimalExponentSigned(s.into())
            }
            (StateMachineWrapper::DecimalExponent(s), _) => {
                StateMachineWrapper::DecimalExponentAcc(s.into())
            }

            (StateMachineWrapper::DecimalExponentSigned(s), Equivalence::Zero) => {
                StateMachineWrapper::DecimalExponentSigned(s.into())
            }
            (StateMachineWrapper::DecimalExponentSigned(s), Equivalence::One) => {
                StateMachineWrapper::DecimalExponentSigned(s.into())
            }
            (StateMachineWrapper::DecimalExponentSigned(s), Equivalence::Two) => {
                StateMachineWrapper::DecimalExponentSigned(s.into())
            }
            (StateMachineWrapper::DecimalExponentSigned(s), Equivalence::Three) => {
                StateMachineWrapper::DecimalExponentSigned(s.into())
            }
            (StateMachineWrapper::DecimalExponentSigned(s), Equivalence::Four) => {
                StateMachineWrapper::DecimalExponentSigned(s.into())
            }
            (StateMachineWrapper::DecimalExponentSigned(s), Equivalence::Five) => {
                StateMachineWrapper::DecimalExponentSigned(s.into())
            }
            (StateMachineWrapper::DecimalExponentSigned(s), Equivalence::Six) => {
                StateMachineWrapper::DecimalExponentSigned(s.into())
            }
            (StateMachineWrapper::DecimalExponentSigned(s), Equivalence::Seven) => {
                StateMachineWrapper::DecimalExponentSigned(s.into())
            }
            (StateMachineWrapper::DecimalExponentSigned(s), Equivalence::EightNine) => {
                StateMachineWrapper::DecimalExponentSigned(s.into())
            }
            (StateMachineWrapper::DecimalExponentSigned(s), _) => {
                StateMachineWrapper::DecimalExponentSignedAcc(s.into())
            }

            (StateMachineWrapper::Octal(s), Equivalence::Zero) => {
                StateMachineWrapper::Octal(s.into())
            }
            (StateMachineWrapper::Octal(s), Equivalence::One) => {
                StateMachineWrapper::Octal(s.into())
            }
            (StateMachineWrapper::Octal(s), Equivalence::Two) => {
                StateMachineWrapper::Octal(s.into())
            }
            (StateMachineWrapper::Octal(s), Equivalence::Three) => {
                StateMachineWrapper::Octal(s.into())
            }
            (StateMachineWrapper::Octal(s), Equivalence::Four) => {
                StateMachineWrapper::Octal(s.into())
            }
            (StateMachineWrapper::Octal(s), Equivalence::Five) => {
                StateMachineWrapper::Octal(s.into())
            }
            (StateMachineWrapper::Octal(s), Equivalence::Six) => {
                StateMachineWrapper::Octal(s.into())
            }
            (StateMachineWrapper::Octal(s), Equivalence::Seven) => {
                StateMachineWrapper::Octal(s.into())
            }
            (StateMachineWrapper::Octal(s), _) => {
                StateMachineWrapper::OctalAcc(s.into())
            }

            (StateMachineWrapper::Hex(s), Equivalence::Zero) => StateMachineWrapper::Hex(s.into()),
            (StateMachineWrapper::Hex(s), Equivalence::One) => StateMachineWrapper::Hex(s.into()),
            (StateMachineWrapper::Hex(s), Equivalence::Two) => StateMachineWrapper::Hex(s.into()),
            (StateMachineWrapper::Hex(s), Equivalence::Three) => StateMachineWrapper::Hex(s.into()),
            (StateMachineWrapper::Hex(s), Equivalence::Four) => StateMachineWrapper::Hex(s.into()),
            (StateMachineWrapper::Hex(s), Equivalence::Five) => StateMachineWrapper::Hex(s.into()),
            (StateMachineWrapper::Hex(s), Equivalence::Six) => StateMachineWrapper::Hex(s.into()),
            (StateMachineWrapper::Hex(s), Equivalence::Seven) => StateMachineWrapper::Hex(s.into()),
            (StateMachineWrapper::Hex(s), Equivalence::EightNine) => {
                StateMachineWrapper::Hex(s.into())
            }
            (StateMachineWrapper::Hex(s), Equivalence::A) => StateMachineWrapper::Hex(s.into()),
            (StateMachineWrapper::Hex(s), Equivalence::B) => StateMachineWrapper::Hex(s.into()),
            (StateMachineWrapper::Hex(s), Equivalence::C) => StateMachineWrapper::Hex(s.into()),
            (StateMachineWrapper::Hex(s), Equivalence::D) => StateMachineWrapper::Hex(s.into()),
            (StateMachineWrapper::Hex(s), Equivalence::E) => StateMachineWrapper::Hex(s.into()),
            (StateMachineWrapper::Hex(s), Equivalence::F) => StateMachineWrapper::Hex(s.into()),
            (StateMachineWrapper::Hex(s), _) => StateMachineWrapper::HexAcc(s.into()),

            (StateMachineWrapper::Binary(s), Equivalence::One) => StateMachineWrapper::Binary(s.into()),
            (StateMachineWrapper::Binary(s), Equivalence::Zero) => StateMachineWrapper::Binary(s.into()),
            (StateMachineWrapper::Binary(s), _) => StateMachineWrapper::BinaryAcc(s.into()),
            (_, Equivalence::HELL) => StateMachineWrapper::InputElementDiv(StateMachine::<InputElementDiv>::new()),
            a@_ => unreachable!("Invalid state: {:?}", a)
}