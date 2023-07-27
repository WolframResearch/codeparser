use crate::{
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
    pub(crate) const fn value(self) -> u16 {
        let value: u16 = self as u16;
        return value & 0x1ff;
    }

    pub(crate) fn with_policy(self, policy: NextPolicy) -> Self {
        if self == TokenKind::InternalNewline
            && policy & RETURN_TOPLEVELNEWLINE != 0
        {
            return TokenKind::ToplevelNewline;
        }

        self
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

    //
    // Group 1 matches: 0b0000_0xx0_0000_0000 (x is unknown)
    //
    //         Mask off 0b0000_0110_0000_0000 (0x600)
    // And test against 0b0000_0010_0000_0000 (0x200)
    //
    pub const fn isPossibleBeginning(self) -> bool {
        return (self.bits() & 0x600) == 0x200;
    }

    //
    // Group 1 matches: 0b0000_0xx0_0000_0000 (x is unknown)
    //
    //         Mask off 0b0000_0110_0000_0000 (0x600)
    // And test against 0b0000_0100_0000_0000 (0x400)
    //
    pub const fn isCloser(self) -> bool {
        return (self.bits() & 0x600) == 0x400;
    }

    //
    // Group 1 matches: 0b0000_0xx0_0000_0000 (x is unknown)
    //
    //         Mask off 0b0000_0110_0000_0000 (0x600)
    // And test against 0b0000_0110_0000_0000 (0x600)
    //
    pub const fn isError(self) -> bool {
        return (self.bits() & 0x600) == 0x600;
    }

    //
    // isUnterminated value matches: 0b0000_000x_xxxx_xxxx (x is unknown)
    //
    // Only valid if already checked isError
    //
    //         Mask off 0b0000_0000_0001_1100 (0x1c)
    // And test against 0b0000_0000_0001_1100 (0x1c)
    //
    pub const fn isUnterminated(self) -> bool {
        return (self.bits() & 0x1c) == 0x1c;
    }

    //
    // Group 2 matches: 0b000x_x000_0000_0000 (x is unknown)
    //
    //         Mask off 0b0001_1000_0000_0000 (0x1800)
    // And test against 0b0000_1000_0000_0000 (0x0800)
    //
    pub const fn isEmpty(self) -> bool {
        return (self.bits() & 0x1800) == 0x0800;
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
        TokenKind::InternalNewline.with_policy(RETURN_TOPLEVELNEWLINE),
        TokenKind::ToplevelNewline
    );
}
