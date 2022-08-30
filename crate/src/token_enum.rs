use crate::source::{NextPolicy, NextPolicyBits::RETURN_TOPLEVELNEWLINE};
pub(crate) use crate::token_enum_registration::TokenEnum;

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

impl TokenEnum {
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
        if self == TokenEnum::TOKEN_INTERNALNEWLINE && policy & RETURN_TOPLEVELNEWLINE != 0 {
            return TokenEnum::TOKEN_TOPLEVELNEWLINE;
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


pub(crate) const fn GroupOpenerToCloser(token: TokenEnum) -> Closer {
    match token {
        TokenEnum::TOKEN_COLONCOLONOPENSQUARE => Closer::CloseSquare,
        TokenEnum::TOKEN_LONGNAME_LEFTANGLEBRACKET => Closer::LongName_RightAngleBracket,
        TokenEnum::TOKEN_LONGNAME_LEFTASSOCIATION => Closer::LongName_RightAssociation,
        TokenEnum::TOKEN_LONGNAME_LEFTBRACKETINGBAR => Closer::LongName_RightBracketingBar,
        TokenEnum::TOKEN_LONGNAME_LEFTCEILING => Closer::LongName_RightCeiling,
        TokenEnum::TOKEN_LONGNAME_LEFTDOUBLEBRACKET => Closer::LongName_RightDoubleBracket,
        TokenEnum::TOKEN_LONGNAME_LEFTDOUBLEBRACKETINGBAR => {
            Closer::LongName_RightDoubleBracketingBar
        },
        TokenEnum::TOKEN_LONGNAME_LEFTFLOOR => Closer::LongName_RightFloor,
        TokenEnum::TOKEN_LESSBAR => Closer::BarGreater,
        TokenEnum::TOKEN_OPENCURLY => Closer::CloseCurly,
        TokenEnum::TOKEN_LONGNAME_OPENCURLYDOUBLEQUOTE => Closer::LongName_CloseCurlyDoubleQuote,
        TokenEnum::TOKEN_LONGNAME_OPENCURLYQUOTE => Closer::LongName_CloseCurlyQuote,
        TokenEnum::TOKEN_OPENPAREN => Closer::CloseParen,
        TokenEnum::TOKEN_OPENSQUARE => Closer::CloseSquare,
        _ => panic!("Unhandled token"),
    }
}

pub(crate) fn TokenToCloser(token: TokenEnum) -> Closer {
    match token {
        TokenEnum::TOKEN_BARGREATER => Closer::BarGreater,
        TokenEnum::TOKEN_CLOSECURLY => Closer::CloseCurly,
        TokenEnum::TOKEN_LONGNAME_CLOSECURLYDOUBLEQUOTE => Closer::LongName_CloseCurlyDoubleQuote,
        TokenEnum::TOKEN_LONGNAME_CLOSECURLYQUOTE => Closer::LongName_CloseCurlyQuote,
        TokenEnum::TOKEN_CLOSEPAREN => Closer::CloseParen,
        TokenEnum::TOKEN_CLOSESQUARE => Closer::CloseSquare,
        TokenEnum::TOKEN_LONGNAME_RIGHTANGLEBRACKET => Closer::LongName_RightAngleBracket,
        TokenEnum::TOKEN_LONGNAME_RIGHTASSOCIATION => Closer::LongName_RightAssociation,
        TokenEnum::TOKEN_LONGNAME_RIGHTBRACKETINGBAR => Closer::LongName_RightBracketingBar,
        TokenEnum::TOKEN_LONGNAME_RIGHTCEILING => Closer::LongName_RightCeiling,
        TokenEnum::TOKEN_LONGNAME_RIGHTDOUBLEBRACKET => Closer::LongName_RightDoubleBracket,
        TokenEnum::TOKEN_LONGNAME_RIGHTDOUBLEBRACKETINGBAR => {
            Closer::LongName_RightDoubleBracketingBar
        },
        TokenEnum::TOKEN_LONGNAME_RIGHTFLOOR => Closer::LongName_RightFloor,
        _ => Closer::AssertFalse,
    }
}

//======================================
// Verify some TokenEnum properties
//======================================

const _: () = assert!(TokenEnum::TOKEN_ENDOFFILE.isEmpty());

#[test]
fn test_newline_policy() {
    assert_eq!(
        TokenEnum::TOKEN_INTERNALNEWLINE.with_policy(RETURN_TOPLEVELNEWLINE),
        TokenEnum::TOKEN_TOPLEVELNEWLINE
    );
}
