use crate::{
    generated::token_enum_registration::{Group1, Group2},
    source::{NextPolicy, NextPolicyBits::RETURN_TOPLEVELNEWLINE},
    tokenize::TokenKind,
};

//
// All group closers
//
#[allow(non_camel_case_types)]
#[repr(u8)]
#[derive(Debug, Copy, Clone, PartialEq)]
pub enum Closer {
    BarGreater,
    CloseCurly,
    CloseParen,
    CloseSquare,
    LongName_CloseCurlyDoubleQuote,
    LongName_CloseCurlyQuote,
    LongName_RightAngleBracket,
    LongName_RightAssociation,
    LongName_RightBracketingBar,
    LongName_RightCeiling,
    LongName_RightDoubleBracket,
    LongName_RightDoubleBracketingBar,
    LongName_RightFloor,
    // UNUSED
    AssertFalse,
}

impl TokenKind {
    pub const fn bits(self) -> u16 {
        let bits: u16 = self as u16;
        return bits;
    }

    // TODO: This is only used with TOKEN_COUNT -- remove this?
    pub(crate) const fn id(self) -> u16 {
        let value: u16 = self as u16;
        return value & 0x1ff;
    }

    /// Returns either [`TokenKind::ToplevelNewline`] or [`TokenKind::InternalNewline`]
    pub(crate) fn newline_with_policy(policy: NextPolicy) -> Self {
        if policy.contains(RETURN_TOPLEVELNEWLINE) {
            return TokenKind::ToplevelNewline;
        } else {
            TokenKind::InternalNewline
        }
    }

    //
    // All trivia matches: 0b0_0000_1xxx (x is unknown)
    //
    //         Mask off 0b1_1111_1000 (0x1f8)
    // And test against 0b0_0000_1000 (0x08)
    //
    pub const fn isTrivia(self) -> bool {
        return (self.bits() & 0x1f8) == 0x08;
    }

    //
    // All trivia but ToplevelNewline matches: 0b0_0000_10xx (x is unknown)
    //
    //         Mask off 0b1_1111_1100 (0x1fc)
    // And test against 0b0_0000_1000 (0x08)
    //
    pub const fn isTriviaButNotToplevelNewline(self) -> bool {
        return (self.bits() & 0x1fc) == 0x08;
    }

    pub const fn isPossibleBeginning(self) -> bool {
        return self.bits() & Group1::MASK == Group1::PossibleBeginning as u16;
    }

    pub const fn isCloser(self) -> bool {
        return self.bits() & Group1::MASK == Group1::Closer as u16;
    }

    pub const fn isError(self) -> bool {
        return self.bits() & Group1::MASK == Group1::Error as u16;
    }

    pub const fn isUnterminated(self) -> bool {
        return self.bits() & Group2::MASK == Group2::Unterminated as u16;
    }

    pub const fn isEmpty(self) -> bool {
        return self.bits() & Group2::MASK == Group2::Empty as u16;
    }
}


pub(crate) const fn GroupOpenerToCloser(token: TokenKind) -> Closer {
    match token {
        TokenKind::ColonColonOpenSquare => Closer::CloseSquare,
        TokenKind::LongName_LeftAngleBracket => {
            Closer::LongName_RightAngleBracket
        },
        TokenKind::LongName_LeftAssociation => {
            Closer::LongName_RightAssociation
        },
        TokenKind::LongName_LeftBracketingBar => {
            Closer::LongName_RightBracketingBar
        },
        TokenKind::LongName_LeftCeiling => Closer::LongName_RightCeiling,
        TokenKind::LongName_LeftDoubleBracket => {
            Closer::LongName_RightDoubleBracket
        },
        TokenKind::LongName_LeftDoubleBracketingBar => {
            Closer::LongName_RightDoubleBracketingBar
        },
        TokenKind::LongName_LeftFloor => Closer::LongName_RightFloor,
        TokenKind::LessBar => Closer::BarGreater,
        TokenKind::OpenCurly => Closer::CloseCurly,
        TokenKind::LongName_OpenCurlyDoubleQuote => {
            Closer::LongName_CloseCurlyDoubleQuote
        },
        TokenKind::LongName_OpenCurlyQuote => Closer::LongName_CloseCurlyQuote,
        TokenKind::OpenParen => Closer::CloseParen,
        TokenKind::OpenSquare => Closer::CloseSquare,
        _ => panic!("Unhandled token"),
    }
}

pub(crate) fn TokenToCloser(token: TokenKind) -> Closer {
    match token {
        TokenKind::BarGreater => Closer::BarGreater,
        TokenKind::CloseCurly => Closer::CloseCurly,
        TokenKind::LongName_CloseCurlyDoubleQuote => {
            Closer::LongName_CloseCurlyDoubleQuote
        },
        TokenKind::LongName_CloseCurlyQuote => Closer::LongName_CloseCurlyQuote,
        TokenKind::CloseParen => Closer::CloseParen,
        TokenKind::CloseSquare => Closer::CloseSquare,
        TokenKind::LongName_RightAngleBracket => {
            Closer::LongName_RightAngleBracket
        },
        TokenKind::LongName_RightAssociation => {
            Closer::LongName_RightAssociation
        },
        TokenKind::LongName_RightBracketingBar => {
            Closer::LongName_RightBracketingBar
        },
        TokenKind::LongName_RightCeiling => Closer::LongName_RightCeiling,
        TokenKind::LongName_RightDoubleBracket => {
            Closer::LongName_RightDoubleBracket
        },
        TokenKind::LongName_RightDoubleBracketingBar => {
            Closer::LongName_RightDoubleBracketingBar
        },
        TokenKind::LongName_RightFloor => Closer::LongName_RightFloor,
        _ => Closer::AssertFalse,
    }
}

//======================================
// Verify some TokenKind properties
//======================================

const _: () = assert!(TokenKind::EndOfFile.isEmpty());

#[test]
fn test_newline_policy() {
    assert_eq!(
        TokenKind::newline_with_policy(RETURN_TOPLEVELNEWLINE),
        TokenKind::ToplevelNewline
    );
}
