use std::cmp::Ordering;


// Validate that Rust is doing enum size optimization, by recognizing that the
// values of the CodePoint::{Char, ..} variants are distinct.
//
// This optimization decreases the "tokenize Boxes.wl" benchmark by -14.8%.
const _: () = assert!(std::mem::size_of::<CodePoint>() == 4);

/// Extended code points.
///
/// # Non-`char` code points
///
/// Sentinel value code points, that are not valid Unicode code points.
///
/// Unicode code points are all positive integers. These special code points
/// are all negative values.
#[allow(non_camel_case_types)]
#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
#[rustfmt::skip]
pub enum CodePoint {
    /// A valid unicode scalar value code point.
    Char(char),

    //==================================
    // Special code points
    //==================================

    /// Special end of file character.
    EndOfFile, // = -1

    // /// Should never be used
    // AssertFalse, // = -2

    /// There is an inconsistency in WL, such that LINEARSYNTAX_SPACE does not
    /// have a dedicated code point. So invent one here.
    LinearSyntax_Space, // = -3

    //
    // The string meta characters \< and \> will have code points here, but they are not actual characters and do not have real code points
    // The string meta characters \" and \\ will have code points here, but they are not actual characters and do not have real code points
    // Assign codepoints to \b \f \n \r \t
    // These WLCharacters may only appear in strings
    //
    StringMeta_Open,           // = -4
    StringMeta_Close,          // = -5
    StringMeta_DoubleQuote,    // = -6
    StringMeta_Backslash,      // = -7
    StringMeta_Backspace,      // = -8
    StringMeta_FormFeed,       // = -9
    StringMeta_LineFeed,       // = -10
    StringMeta_CarriageReturn, // = -11
    StringMeta_Tab,            // = -12

    /// \r\n is a single SourceCharacter
    ///
    /// There is a mnemonic here: \r is 13 and CODEPOINT_CRLF is -13
    CRLF, // = -13

    //
    // The return value of ByteDecoder with unsafe input:
    // incomplete sequence
    // stray surrogate
    // BOM
    //
    // CODEPOINT_UNSAFE_1_BYTE_UTF8_SEQUENCE represents unsafe input of 1 byte
    // CODEPOINT_UNSAFE_2_BYTE_UTF8_SEQUENCE represents unsafe input of 2 bytes
    // CODEPOINT_UNSAFE_3_BYTE_UTF8_SEQUENCE represents unsafe input of 3 bytes
    //
    Unsafe1ByteUtf8Sequence, // = -14
    Unsafe2ByteUtf8Sequence, // = -15
    Unsafe3ByteUtf8Sequence, // = -16

    LineContinuation_LineFeed,       // = -17
    LineContinuation_CarriageReturn, // = -18
    LineContinuation_CRLF,           // = -19
}

impl PartialOrd for CodePoint {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for CodePoint {
    fn cmp(&self, other: &Self) -> Ordering {
        use CodePoint::Char;

        match (self, other) {
            (Char(a), Char(b)) => a.cmp(b),
            (Char(_), _) => Ordering::Greater,
            (_, Char(_)) => Ordering::Less,
            (a, b) => a.as_i32().cmp(&b.as_i32()),
        }
    }
}

impl From<char> for CodePoint {
    fn from(c: char) -> Self {
        CodePoint::Char(c)
    }
}

impl From<u8> for CodePoint {
    fn from(b: u8) -> Self {
        CodePoint::Char(char::from(b))
    }
}

impl CodePoint {
    // TODO(cleanup): Remove this function?
    pub(crate) fn from_i32(value: i32) -> Option<Self> {
        if value.is_negative() {
            todo!("unable to construct CodePoint from non-char value: {value}");
        }

        if let Ok(value_u32) = u32::try_from(value) {
            if let Some(c) = char::from_u32(value_u32) {
                // Sanity check that there aren't any i32 values that are both
                // valid Unicode 'char's, and valid SpecialCodePoint values.
                /*
                debug_assert!(SpecialCodePoint::try_from(value).is_err());
                */

                return Some(CodePoint::Char(c));
            }
        }

        /*
        if let Ok(special) = SpecialCodePoint::try_from(value) {
            return Some(CodePoint::Special(special));
        }
        */

        None
    }

    pub fn from_u8(value: u8) -> Self {
        CodePoint::Char(char::from(value))
    }

    pub fn as_char(self) -> Option<char> {
        match self {
            CodePoint::Char(c) => Some(c),
            _ => None,
        }
    }

    // TODO(cleanup): Remove this function?
    pub(crate) fn as_i32(self) -> i32 {
        use self::CodePoint::*;

        let special: i32 = match self {
            Char(c) => {
                let as_u32 = u32::from(c);
                return i32::try_from(as_u32).expect("unable to convert char to i32");
            },
            // Special code points
            EndOfFile => -1,
            LinearSyntax_Space => -3,
            StringMeta_Open => -4,
            StringMeta_Close => -5,
            StringMeta_DoubleQuote => -6,
            StringMeta_Backslash => -7,
            StringMeta_Backspace => -8,
            StringMeta_FormFeed => -9,
            StringMeta_LineFeed => -10,
            StringMeta_CarriageReturn => -11,
            StringMeta_Tab => -12,
            CRLF => -13,

            Unsafe1ByteUtf8Sequence => -14,
            Unsafe2ByteUtf8Sequence => -15,
            Unsafe3ByteUtf8Sequence => -16,

            LineContinuation_LineFeed => -17,
            LineContinuation_CarriageReturn => -18,
            LineContinuation_CRLF => -19,
        };

        debug_assert!(special.is_negative());

        special
    }

    /// Returns true if this source character is a line continuation special
    /// character.
    pub(crate) fn is_line_continuation(&self) -> bool {
        let point = self;

        CodePoint::LineContinuation_LineFeed.as_i32() >= point.as_i32()
            && point.as_i32() >= CodePoint::LineContinuation_CRLF.as_i32()
    }
}

#[test]
fn test_code_point_is_line_continuation() {
    assert!(!CodePoint::Char('a').is_line_continuation());
    assert!(!CodePoint::Char('\\').is_line_continuation());
    assert!(!CodePoint::Char('\n').is_line_continuation());

    assert!(CodePoint::LineContinuation_LineFeed.is_line_continuation());
    assert!(CodePoint::LineContinuation_CarriageReturn.is_line_continuation());
    assert!(CodePoint::LineContinuation_CRLF.is_line_continuation());
}

impl PartialEq<char> for CodePoint {
    fn eq(&self, c: &char) -> bool {
        match self.as_char() {
            Some(self_c) => self_c == *c,
            // If this CodePoint is not a valid Unicode character, then these
            // can't be equal.
            None => false,
        }
    }
}

//
// ASCII codepoints
//
pub const CODEPOINT_BEL: char = '\x07';
pub const CODEPOINT_ESC: char = '\x1b';
pub const CODEPOINT_ACTUAL_DOUBLEQUOTE: char = '\x22';
pub const CODEPOINT_ACTUAL_BACKSLASH: char = '\x5c';
pub const CODEPOINT_DEL: char = '\x7f';

pub const CODEPOINT_ZEROWIDTHSPACE: char = '\u{200b}';
pub const CODEPOINT_FUNCTIONAPPLICATION: char = '\u{2061}';
pub const CODEPOINT_INVISIBLESEPARATOR: char = '\u{2063}';
pub const CODEPOINT_INVISIBLEPLUS: char = '\u{2064}';
pub const CODEPOINT_TRIANGLEHEADEDRIGHTWARDSARROW: char = '\u{279D}';
pub const CODEPOINT_RULEDELAYED: char = '\u{29F4}';

//
// These are the actual WL code points for linear syntax characters
//
pub const CODEPOINT_LINEARSYNTAX_CLOSEPAREN: char = '\u{f7c0}';
pub const CODEPOINT_LINEARSYNTAX_BANG: char = '\u{f7c1}';
pub const CODEPOINT_LINEARSYNTAX_AT: char = '\u{f7c2}';
//UNUSED: constexpr codepoint CODEPOINT_LINEARSYNTAX_HASH(0xf7c3);
//UNUSED: constexpr codepoint CODEPOINT_LINEARSYNTAX_DOLLAR(0xf7c4);
pub const CODEPOINT_LINEARSYNTAX_PERCENT: char = '\u{f7c5}';
pub const CODEPOINT_LINEARSYNTAX_CARET: char = '\u{f7c6}';
pub const CODEPOINT_LINEARSYNTAX_AMP: char = '\u{f7c7}';
pub const CODEPOINT_LINEARSYNTAX_STAR: char = '\u{f7c8}';
pub const CODEPOINT_LINEARSYNTAX_OPENPAREN: char = '\u{f7c9}';
pub const CODEPOINT_LINEARSYNTAX_UNDER: char = '\u{f7ca}';
pub const CODEPOINT_LINEARSYNTAX_PLUS: char = '\u{f7cb}';
pub const CODEPOINT_LINEARSYNTAX_SLASH: char = '\u{f7cc}';
pub const CODEPOINT_LINEARSYNTAX_BACKTICK: char = '\u{f7cd}';

//
// Used because MathLink does not transmit BOM when it is first character
//
// Related bugs: 366106
//
pub const CODEPOINT_BOM: char = '\u{feff}';

//==========================================================
// Special code points
//==========================================================

//
// These are used by long_names_registration.rs
//

// TODO(cleanup): Remove these constants, just name the enum value directly.
pub const CODEPOINT_CRLF: CodePoint = CodePoint::CRLF;
pub const CODEPOINT_STRINGMETA_DOUBLEQUOTE: CodePoint = CodePoint::StringMeta_DoubleQuote;
pub const CODEPOINT_STRINGMETA_BACKSLASH: CodePoint = CodePoint::StringMeta_Backslash;
