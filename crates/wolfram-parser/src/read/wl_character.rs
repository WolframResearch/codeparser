use std::fmt::{self, Debug, Display};

use crate::{
    generated::long_names_registration::*,
    long_names::{self as LongNames, codepoint_to_longname},
    read::code_point::{
        CodePoint::{self, Char, *},
        *,
    },
    source::SourceCharacter,
    tokenize::tokenizer::{ASCII_FORM_FEED, ASCII_VTAB},
};

/// The 8 styles of character escapes
///
/// * [`None`][Escape::None]: just regular characters: a, b, c, etc.
/// * [`Raw`][Escape::Raw]: Using the `\[Raw]` style: `\[RawWedge]`, `\[RawAt]`, etc.
/// * [`Single`][Escape::Single]: A single backslash: `\n`, `\t`, `\r`, etc.
/// * [`Hex2`][Escape::Hex2]: `\.xx` style
/// * [`Hex4`][Escape::Hex4]: `\:xxxx` style
/// * [`Hex6`][Escape::Hex6]: `\|xxxxxx` style
/// * [`Octal`][Escape::Octal]: `\xxx` style
/// * [`LongName`][Escape::LongName]: Using `\[XX]` style: `\[Alpha]`, `\[Beta]`, etc.
#[derive(Debug, Copy, Clone, PartialEq)]
pub enum Escape {
    None,
    Raw,
    /// Allowed only for characters inside strings or linear syntax.
    Single,
    Hex2,
    Hex4,
    Hex6,
    Octal,
    /// Named character.
    ///
    /// See also: [Listing of Named Characters](https://reference.wolfram.com/language/guide/ListingOfNamedCharacters.html)
    LongName,
}

/// A single WL character
///
/// The text `\[Alpha]` would be 1 `WLCharacter`, with [`Escape::LongName`]
#[derive(Copy, Clone, PartialEq)]
pub struct WLCharacter {
    pub(crate) point: CodePoint,
    escape: Escape,
    //
    // valBits: i32, // uint32_t valBits : 21;
    // signBit: bool, // uint8_t signBit : 1;
    // escapeBits: u8, // uint8_t escapeBits : 3;
    //
    // constexpr bool operator==(const WLCharacter& o) const {
    //     return valBits == o.valBits &&
    //         signBit == o.signBit &&
    //         escapeBits == o.escapeBits;
    // }
}

impl Debug for WLCharacter {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let WLCharacter { point, escape } = *self;

        match escape {
            Escape::None => write!(f, "WLCharacter({point:?})"),
            Escape::Raw
            | Escape::Single
            | Escape::Hex2
            | Escape::Hex4
            | Escape::Hex6
            | Escape::Octal
            | Escape::LongName => {
                write!(f, "WLCharacter({point:?}, {escape:?})")
            },
        }
    }
}

impl WLCharacter {
    pub fn new<T: Into<CodePoint>>(val: T) -> Self {
        WLCharacter::new_with_escape(val.into(), Escape::None)
    }

    /// Construct a [`WLCharacter`] that was escaped in the input.
    ///
    /// # Example
    ///
    /// Show different ways of escaping the same character for input:
    ///
    /// ```
    /// use wolfram_parser::read::{WLCharacter, Escape};
    ///
    /// assert_eq!(WLCharacter::new('π').to_string(), "π");
    /// assert_eq!(WLCharacter::escaped('π', Escape::LongName).to_string(), "\\[Pi]");
    /// assert_eq!(WLCharacter::escaped('π', Escape::Hex4).to_string(), "\\:03c0");
    ///
    /// assert_eq!(WLCharacter::new('\t').to_string(), "	");
    /// // ASCII 0x09
    /// assert_eq!(WLCharacter::escaped('\t', Escape::Hex2).to_string(), "\\.09");
    /// assert_eq!(WLCharacter::escaped('\t', Escape::Raw).to_string(), "\t");
    /// assert_eq!(WLCharacter::escaped('\t', Escape::LongName).to_string(), "\\[RawTab]");
    /// ```
    pub fn escaped<C: Into<CodePoint>>(point: C, escape: Escape) -> Self {
        Self {
            point: point.into(),
            escape,
        }
    }

    // TODO(cleanup): Remove this method in favor of using `escaped()`
    //                everywhere.
    pub(crate) fn new_with_escape<T: Into<CodePoint>>(
        val: T,
        escape: Escape,
    ) -> Self {
        Self {
            point: val.into(),
            // signBit: val < 0,
            // escapeBits: escape,
            escape,
        }
    }

    // constexpr codepoint to_point() const {
    //     //
    //     // Sign extend the value
    //     //
    //     return signBit ? (valBits | -0x200000) : valBits;
    // }
    // pub(crate) fn to_point(&self) -> codepoint {
    //     if self.signBit {
    //         self.valBits as i32 | -0x20_00_00
    //     } else {
    //         self.valBits as i32
    //     }
    // }

    pub(crate) fn to_point(&self) -> CodePoint {
        self.point
    }

    // pub(crate) fn as_char(&self) -> char {
    //     let val = self.to_point();

    //     let val = u32::try_from(val).expect("unable to convert SourceCharacter to u32");

    //     char::from_u32(val).expect("unable to convert SourceCharacter u32 to char")
    // }

    pub(crate) fn escape(&self) -> Escape {
        // self.escapeBits as Escape
        self.escape
    }
}

//
// Sizes of structs with bit-fields are implementation-dependent
//
// PRE_COMMIT: Reenable
// const _: () = assert!(std::mem::size_of::<WLCharacter>() == 4, "Check your assumptions");


//
// Respect the actual escape style
//
impl Display for WLCharacter {
    fn fmt(&self, s: &mut fmt::Formatter) -> fmt::Result {
        let graphical_flag = s.alternate();

        let mut i: CodePoint = self.to_point();

        let mut format_char = |source_char: SourceCharacter| {
            if graphical_flag {
                // Pass down the graphical flag as an '#' ("alternate") flag.
                write!(s, "{:#}", source_char)
            } else {
                write!(s, "{}", source_char)
            }
        };

        match self.escape() {
            Escape::None | Escape::Raw => {
                return format_char(SourceCharacter::from(i));
            },
            Escape::Single => {
                format_char(SourceCharacter::from('\\'))?;

                let source_char: char = match i {
                    CodePoint::StringMeta_Backspace => 'b',
                    CodePoint::StringMeta_FormFeed => 'f',
                    CodePoint::StringMeta_LineFeed => 'n',
                    CodePoint::StringMeta_CarriageReturn => 'r',
                    CodePoint::StringMeta_Tab => 't',
                    CodePoint::StringMeta_Open => '<',
                    CodePoint::StringMeta_Close => '>',
                    CodePoint::StringMeta_DoubleQuote => '"',
                    CodePoint::StringMeta_Backslash => '\\',
                    CodePoint::LineContinuation_LineFeed => '\n',
                    CodePoint::LineContinuation_CarriageReturn => '\r',
                    CodePoint::LineContinuation_CRLF => {
                        if graphical_flag {
                            write!(s, "{:#}", SourceCharacter::from('\r'))?;
                            write!(s, "{:#}", SourceCharacter::from('\n'))?;
                        } else {
                            write!(s, "{}", SourceCharacter::from('\r'))?;
                            write!(s, "{}", SourceCharacter::from('\n'))?;
                        }

                        return Ok(());
                    },
                    Char(CODEPOINT_LINEARSYNTAX_BANG) => '!',
                    Char(CODEPOINT_LINEARSYNTAX_PERCENT) => '%',
                    Char(CODEPOINT_LINEARSYNTAX_AMP) => '&',
                    Char(CODEPOINT_LINEARSYNTAX_OPENPAREN) => '(',
                    Char(CODEPOINT_LINEARSYNTAX_CLOSEPAREN) => ')',
                    Char(CODEPOINT_LINEARSYNTAX_STAR) => '*',
                    Char(CODEPOINT_LINEARSYNTAX_PLUS) => '+',
                    Char(CODEPOINT_LINEARSYNTAX_SLASH) => '/',
                    Char(CODEPOINT_LINEARSYNTAX_AT) => '@',
                    Char(CODEPOINT_LINEARSYNTAX_CARET) => '^',
                    Char(CODEPOINT_LINEARSYNTAX_UNDER) => '_',
                    Char(CODEPOINT_LINEARSYNTAX_BACKTICK) => '`',
                    CodePoint::LinearSyntax_Space => ' ',
                    _ => panic!("Unable to format code point as Escape::Single: `{i:?}`"),
                };

                let source_char = SourceCharacter::from(source_char);

                if graphical_flag {
                    return write!(s, "{:#}", source_char);
                } else {
                    return write!(s, "{}", source_char);
                }
            },
            Escape::LongName => {
                let LongName: &str = codepoint_to_longname(i)
                    .expect("unable to find longname for codepoint");

                format_char(SourceCharacter::from('\\'))?;
                format_char(SourceCharacter::from('['))?;

                for long_name_char in LongName.chars() {
                    format_char(SourceCharacter::from(long_name_char))?
                }
                // for (size_t idx = 0; idx < LongName.len(); idx++) {
                //     s << SourceCharacter::from(LongName[idx]);
                // }

                format_char(SourceCharacter::from(']'))?;

                return Ok(());
            },
            Escape::Octal => {
                i = match i {
                    CodePoint::StringMeta_DoubleQuote => {
                        Char(CODEPOINT_ACTUAL_DOUBLEQUOTE)
                    },
                    CodePoint::StringMeta_Backslash => {
                        Char(CODEPOINT_ACTUAL_BACKSLASH)
                    },
                    _ => i,
                };

                let mut i = u32::from(i.as_char().unwrap());

                let o0: u8 = (i % 8) as u8;
                i /= 8;
                let o1: u8 = (i % 8) as u8;
                i /= 8;
                let o2: u8 = (i % 8) as u8;

                format_char(SourceCharacter::from('\\'))?;
                format_char(SourceCharacter::from(fromDigit(o2)))?;
                format_char(SourceCharacter::from(fromDigit(o1)))?;
                format_char(SourceCharacter::from(fromDigit(o0)))?;

                return Ok(());
            },
            Escape::Hex2 => {
                i = match i {
                    CodePoint::StringMeta_DoubleQuote => {
                        Char(CODEPOINT_ACTUAL_DOUBLEQUOTE)
                    },
                    CodePoint::StringMeta_Backslash => {
                        Char(CODEPOINT_ACTUAL_BACKSLASH)
                    },
                    _ => i,
                };

                let mut i = u32::from(i.as_char().unwrap());

                let x0: u8 = (i % 16) as u8;
                i /= 16;
                let x1: u8 = (i % 16) as u8;

                format_char(SourceCharacter::from('\\'))?;
                format_char(SourceCharacter::from('.'))?;
                format_char(SourceCharacter::from(fromDigit(x1)))?;
                format_char(SourceCharacter::from(fromDigit(x0)))?;

                return Ok(());
            },
            Escape::Hex4 => {
                i = match i {
                    CodePoint::StringMeta_DoubleQuote => {
                        Char(CODEPOINT_ACTUAL_DOUBLEQUOTE)
                    },
                    CodePoint::StringMeta_Backslash => {
                        Char(CODEPOINT_ACTUAL_BACKSLASH)
                    },
                    _ => i,
                };

                let mut i = u32::from(i.as_char().unwrap());

                let x0: u8 = (i % 16) as u8;
                i /= 16;
                let x1: u8 = (i % 16) as u8;
                i /= 16;
                let x2: u8 = (i % 16) as u8;
                i /= 16;
                let x3: u8 = (i % 16) as u8;

                format_char(SourceCharacter::from('\\'))?;
                format_char(SourceCharacter::from(':'))?;
                format_char(SourceCharacter::from(fromDigit(x3)))?;
                format_char(SourceCharacter::from(fromDigit(x2)))?;
                format_char(SourceCharacter::from(fromDigit(x1)))?;
                format_char(SourceCharacter::from(fromDigit(x0)))?;

                return Ok(());
            },
            Escape::Hex6 => {
                i = match i {
                    CodePoint::StringMeta_DoubleQuote => {
                        Char(CODEPOINT_ACTUAL_DOUBLEQUOTE)
                    },
                    CodePoint::StringMeta_Backslash => {
                        Char(CODEPOINT_ACTUAL_BACKSLASH)
                    },
                    _ => i,
                };

                let mut i = u32::from(i.as_char().unwrap());

                let x0: u8 = (i % 16) as u8;
                i /= 16;
                let x1: u8 = (i % 16) as u8;
                i /= 16;
                let x2: u8 = (i % 16) as u8;
                i /= 16;
                let x3: u8 = (i % 16) as u8;
                i /= 16;
                let x4: u8 = (i % 16) as u8;
                i /= 16;
                let x5: u8 = (i % 16) as u8;

                format_char(SourceCharacter::from('\\'))?;
                format_char(SourceCharacter::from('|'))?;
                format_char(SourceCharacter::from(fromDigit(x5)))?;
                format_char(SourceCharacter::from(fromDigit(x4)))?;
                format_char(SourceCharacter::from(fromDigit(x3)))?;
                format_char(SourceCharacter::from(fromDigit(x2)))?;
                format_char(SourceCharacter::from(fromDigit(x1)))?;
                format_char(SourceCharacter::from(fromDigit(x0)))?;

                return Ok(());
            },
        }
    }
}


impl WLCharacter {
    pub(crate) fn graphicalString(&self) -> String {
        // Use '#' to force the "alternate" formatting, which for a
        // SourceCharacter is the graphical representation.
        format!("{:#}", self)

        // std::ostringstream String;
        // String << set_graphical << *this << clear_graphical;
        // return String.str();
    }

    pub(crate) fn safeAndGraphicalString(&self) -> String {
        let WLCharacter { point: _, escape } = *self;

        if escape == Escape::None {
            return format!("\"{}\" ({:#})", self, self);
        } else {
            return format!("{}", self);
        }

        // std::ostringstream String;
        // if (escape() == Escape::None) {
        //     String << "\"" << *this << "\" (" << set_graphical << *this << clear_graphical << ")";
        //     return String.str();
        // }
        // String << *this;
        // return String.str();
    }

    pub(crate) fn isLetterlike(&self) -> bool {
        //
        // Most of ASCII control characters are letterlike.
        // jessef: There may be such a thing as *too* binary-safe...
        //
        // Except for LF, CR: those are newlines
        //
        // Except for TAB, VT, and FF: those are spaces
        //
        // Except for BEL and DEL: those are uninterpretable
        //
        match self.to_point() {
            #[rustfmt::skip]
            Char('A' ..='Z' | 'a'..='z' |
            '$' |
            '\x00' |'\x01' | '\x02' | '\x03' | '\x04' | '\x05' | '\x06' | /*    \x07*/
            '\x08' | /*    \x09*/ /*    \x0a*/ /*    \x0b*/ /*    \x0c*/ /*    \x0d*/ '\x0e' | '\x0f' |
            '\x10' | '\x11' | '\x12' | '\x13' | '\x14' | '\x15' | '\x16' | '\x17' |
            '\x18' | '\x19' | '\x1a' | '\x1b' | '\x1c' | '\x1d' | '\x1e' | '\x1f') => {
            /* \x7f*/
                true
            },
            _ => false,
        }
    }

    pub(crate) fn isStrangeLetterlike(&self) -> bool {
        //
        // Dump out if not a letterlike character
        //
        if !self.isLetterlike() {
            return false;
        }

        //
        // Using control character as letterlike is strange
        //
        // jessef: There may be such a thing as *too* binary-safe...
        //
        if self.isControl() {
            return true;
        }

        return false;
    }

    pub(crate) fn isWhitespace(&self) -> bool {
        match self.to_point() {
            Char(' ' | '\t' | ASCII_VTAB | ASCII_FORM_FEED) => true,
            _ => false,
        }
    }

    pub(crate) fn isStrangeWhitespace(&self) -> bool {
        match self.to_point() {
            Char(ASCII_VTAB | ASCII_FORM_FEED) => true,
            _ => false,
        }
    }

    pub(crate) fn isUpper(&self) -> bool {
        match self.to_point() {
            Char('A'..='Z') => true,
            _ => false,
        }
    }

    pub(crate) fn isAlpha(&self) -> bool {
        match self.to_point() {
            Char('A'..='Z' | 'a'..='z') => true,
            _ => false,
        }
    }

    pub(crate) fn isDigit(&self) -> bool {
        match self.to_point() {
            Char('0'..='9') => true,
            _ => false,
        }
    }

    pub(crate) fn isAlphaOrDigit(&self) -> bool {
        match self.to_point() {
            Char('A'..='Z' | 'a'..='z' | '0'..='9') => true,
            _ => false,
        }
    }

    pub(crate) fn isHex(&self) -> bool {
        match self.to_point() {
            Char('A'..='F' | 'a'..='f' | '0'..='9') => true,
            _ => false,
        }
    }

    pub(crate) fn isOctal(&self) -> bool {
        match self.to_point() {
            Char('0' | '1' | '2' | '3' | '4' | '5' | '6' | '7') => true,
            _ => false,
        }
    }

    pub(crate) fn isControl(&self) -> bool {
        let point = self.to_point();

        if !point.is_ascii() {
            return false;
        }

        match self.to_point() {
            // FIXME: In c++ this was std::iscntrl. Rust has char::is_control() and
            // char::is_ascii_control(), and it's not entirely clear which should
            // be used here.
            // return std::iscntrl(val);
            CodePoint::Char(c) => c.is_control(),
            _ => false,
        }
    }

    pub(crate) fn isSign(&self) -> bool {
        match self.to_point() {
            Char('-' | '+') => true,
            _ => false,
        }
    }

    //
    // Multi-byte character properties
    //

    pub(crate) fn isMBLinearSyntax(&self) -> bool {
        match self.to_point() {
            CodePoint::Char(
                CODEPOINT_LINEARSYNTAX_CLOSEPAREN
                | CODEPOINT_LINEARSYNTAX_BANG
                | CODEPOINT_LINEARSYNTAX_AT
                | CODEPOINT_LINEARSYNTAX_PERCENT
                | CODEPOINT_LINEARSYNTAX_CARET
                | CODEPOINT_LINEARSYNTAX_AMP
                | CODEPOINT_LINEARSYNTAX_STAR
                | CODEPOINT_LINEARSYNTAX_OPENPAREN
                | CODEPOINT_LINEARSYNTAX_UNDER
                | CODEPOINT_LINEARSYNTAX_PLUS
                | CODEPOINT_LINEARSYNTAX_SLASH
                | CODEPOINT_LINEARSYNTAX_BACKTICK,
            )
            | LinearSyntax_Space => true,
            _ => false,
        }
    }

    pub(crate) fn isMBStringMeta(&self) -> bool {
        let val = self.to_point();

        match val {
            StringMeta_Open
            | StringMeta_Close
            | StringMeta_Backslash
            | StringMeta_DoubleQuote
            | StringMeta_Backspace
            | StringMeta_FormFeed
            | StringMeta_LineFeed
            | StringMeta_CarriageReturn
            | StringMeta_Tab => true,
            _ => false,
        }
    }

    /// isLetterlikeCharacter is special because it is defined in terms of other categories
    ///
    /// basically, if it's not anything else, then it's letterlike
    pub(crate) fn isMBLetterlike(&self) -> bool {
        let val = self.to_point();

        //
        // Reject if single byte, should use isLetterlike()
        //
        if val.is_ascii() {
            return false;
        }

        if self.isMBPunctuation() {
            return false;
        }

        //
        // Must handle all of the specially defined CodePoints
        //

        if val == EndOfFile {
            return false;
        }

        if val == CRLF {
            return false;
        }

        if self.isMBLineContinuation() {
            return false;
        }

        if self.isMBLinearSyntax() {
            return false;
        }

        if self.isMBStringMeta() {
            return false;
        }

        if self.isMBWhitespace() {
            return false;
        }

        if self.isMBNewline() {
            return false;
        }

        if self.isMBUninterpretable() {
            return false;
        }

        if self.isMBUnsafeUTF8Sequence() {
            return false;
        }

        return true;
    }

    pub(crate) fn isMBStrangeLetterlike(&self) -> bool {
        //
        // Dump out if not a letterlike character
        //
        if !self.isMBLetterlike() {
            return false;
        }

        return !LongNames::isMBNotStrangeLetterlike(self.to_point());
    }

    pub(crate) fn isMBStrangeWhitespace(&self) -> bool {
        //
        // Dump out if not a space character
        //
        if !self.isMBWhitespace() {
            return false;
        }

        return true;
    }

    pub(crate) fn isMBStrangeNewline(&self) -> bool {
        //
        // Dump out if not a newline character
        //
        if !self.isMBNewline() {
            return false;
        }

        let val = self.to_point();

        //
        // \r\n is not strange
        //
        if val == CRLF {
            return false;
        }

        //
        // FIXME: somehow supply encodingMode to test here
        //
        // \[IndentingNewLine] is not strange if coming from boxes
        //
        //    if (val == CODEPOINT_LONGNAME_INDENTINGNEWLINE) {
        //        if (encodingMode == ENCODINGMODE_BOX) {
        //            return false;
        //        }
        //    }

        return true;
    }

    pub(crate) fn isMBNewline(&self) -> bool {
        let val = self.to_point();

        return LongNames::isMBNewline(val);
    }

    pub(crate) fn isMBWhitespace(&self) -> bool {
        let val = self.to_point();

        //
        // Handle COMPATIBILITY characters here
        //
        // Cannot use LongNames data here, because the long name \[COMPATIBILITYNoBreak] is Unsupported and
        // LongNames::isMBWhitespace(val) returns false
        //
        // But the actual *character* U+F3A2 is whitespace
        //
        // Yes, this is a bit messy
        //
        if val == CODEPOINT_LONGNAME_COMPATIBILITYNOBREAK {
            return true;
        }

        return LongNames::isMBWhitespace(val);
    }

    pub(crate) fn isMBPunctuation(&self) -> bool {
        return LongNames::isMBPunctuation(self.to_point());
    }

    pub(crate) fn isMBUninterpretable(&self) -> bool {
        return LongNames::isMBUninterpretable(self.to_point());
    }

    pub(crate) fn isMBLineContinuation(&self) -> bool {
        match self.to_point() {
            LineContinuation_LineFeed
            | LineContinuation_CarriageReturn
            | LineContinuation_CRLF => true,
            _ => false,
        }
    }

    pub(crate) fn isMBUnsafeUTF8Sequence(&self) -> bool {
        match self.to_point() {
            Unsafe1ByteUtf8Sequence
            | Unsafe2ByteUtf8Sequence
            | Unsafe3ByteUtf8Sequence => true,
            _ => false,
        }
    }
}

const FROM_DIGIT_LOOKUP: [char; 256] = [
    '0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'a', 'b', 'c', 'd', 'e',
    'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm', 'n', 'o', 'p', 'q', 'r', 's', 't',
    'u', 'v', 'w', 'x', 'y', 'z', '!', '!', '!', '!', '!', '!', '!', '!', '!',
    '!', '!', '!', '!', '!', '!', '!', '!', '!', '!', '!', '!', '!', '!', '!',
    '!', '!', '!', '!', '!', '!', '!', '!', '!', '!', '!', '!', '!', '!', '!',
    '!', '!', '!', '!', '!', '!', '!', '!', '!', '!', '!', '!', '!', '!', '!',
    '!', '!', '!', '!', '!', '!', '!', '!', '!', '!', '!', '!', '!', '!', '!',
    '!', '!', '!', '!', '!', '!', '!', '!', '!', '!', '!', '!', '!', '!', '!',
    '!', '!', '!', '!', '!', '!', '!', '!', '!', '!', '!', '!', '!', '!', '!',
    '!', '!', '!', '!', '!', '!', '!', '!', '!', '!', '!', '!', '!', '!', '!',
    '!', '!', '!', '!', '!', '!', '!', '!', '!', '!', '!', '!', '!', '!', '!',
    '!', '!', '!', '!', '!', '!', '!', '!', '!', '!', '!', '!', '!', '!', '!',
    '!', '!', '!', '!', '!', '!', '!', '!', '!', '!', '!', '!', '!', '!', '!',
    '!', '!', '!', '!', '!', '!', '!', '!', '!', '!', '!', '!', '!', '!', '!',
    '!', '!', '!', '!', '!', '!', '!', '!', '!', '!', '!', '!', '!', '!', '!',
    '!', '!', '!', '!', '!', '!', '!', '!', '!', '!', '!', '!', '!', '!', '!',
    '!', '!', '!', '!', '!', '!', '!', '!', '!', '!', '!', '!', '!', '!', '!',
    '!',
];

fn fromDigit(d: u8) -> char {
    return FROM_DIGIT_LOOKUP[usize::from(d)];
}
