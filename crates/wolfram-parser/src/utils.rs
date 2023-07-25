use crate::{
    generated::long_names_registration::*,
    issue::CodeAction,
    read::{
        code_point::{CodePoint::Char, *},
        wl_character::{EscapeStyle, WLCharacter},
    },
    source::Span,
};

pub fn isStrange(point: CodePoint) -> bool {
    match point {
            //
            // C0 control characters
            //
            // Skipping LF, CR, TAB, and ESC
            //
        CodePoint::Char('\x00' | '\x01' | '\x02' | '\x03' | '\x04' | '\x05' | '\x06' | '\x07' |
        '\x08' | /*    \x09*/ /*    \x0a*/ '\x0b' | '\x0c' | /*    \x0d*/ '\x0e' | '\x0f' |
        '\x10' | '\x11' | '\x12' | '\x13' | '\x14' | '\x15' | '\x16' | '\x17' |
        '\x18' | '\x19' | '\x1a' | /*    \x1b*/ '\x1c' | '\x1d' | '\x1e' | '\x1f' |
            //
            // Make sure to include DEL
            //
        '\x7f') => {
            return true;
        },
        _ => false
    }
}

pub fn isMBStrange(point: CodePoint) -> bool {
    //
    // Reject if ASCII, should use isStrange()
    //
    if 0x00 <= point.as_i32() && point.as_i32() <= 0x7f {
        return false;
    }

    //
    // Individual characters
    //
    match point {
        Char(CODEPOINT_ZEROWIDTHSPACE) => {
            return true;
        },
        //
        // ZERO WIDTH NON-JOINER
        //
        Char('\u{200c}') => {
            return true;
        },
        //
        // ZERO WIDTH JOINER
        //
        Char('\u{200d}') => {
            return true;
        },
        //            //
        //            // LINE SEPARATOR
        //            //
        //        case 0x2028:
        //            return true;
        //            //
        //            // WORD JOINER
        //            //
        //            // This is the character that is recommended to use for ZERO WIDTH NON-BREAKING SPACE
        //            // https://unicode.org/faq/utf_bom.html#bom6
        //            //
        //        case 0x2060:
        //            return true;
        //
        // Various curly quotes
        //
        Char(
            CODEPOINT_LONGNAME_OPENCURLYQUOTE
            | CODEPOINT_LONGNAME_CLOSECURLYQUOTE
            | CODEPOINT_LONGNAME_OPENCURLYDOUBLEQUOTE
            | CODEPOINT_LONGNAME_CLOSECURLYDOUBLEQUOTE,
        ) => {
            return true;
        },
        //
        // U+2061
        //
        Char(CODEPOINT_FUNCTIONAPPLICATION) => {
            return true;
        },
        //
        // U+2063
        //
        Char(CODEPOINT_INVISIBLESEPARATOR) => {
            return true;
        },
        //
        // U+2064
        //
        Char(CODEPOINT_INVISIBLEPLUS) => {
            return true;
        },
        //
        // U+2192
        //
        Char(CODEPOINT_LONGNAME_RIGHTARROW) => {
            return true;
        },
        //
        // U+279D
        //
        Char(CODEPOINT_TRIANGLEHEADEDRIGHTWARDSARROW) => {
            return true;
        },
        //
        // U+29F4
        //
        Char(CODEPOINT_RULEDELAYED) => {
            return true;
        },
        Char(CODEPOINT_LONGNAME_COMPATIBILITYNOBREAK) => {
            return true;
        },
        //
        // Yes, we suggest \:2061 -> \[InvisibleApplication], but that is not saying \[InvisibleApplication] is not also strange!
        //
        Char(CODEPOINT_LONGNAME_INVISIBLEAPPLICATION) => {
            return true;
        },
        _ => (),
    }

    //
    // C1
    //
    if 0x0080 <= point.as_i32() && point.as_i32() <= 0x009f {
        return true;
    }

    //
    // TODO: implement isBMPPUAUnassigned
    //
    //    if Utils::isBMPPUAUnassigned(point) {
    //        return true;
    //    }

    //
    // TODO: implement isBMPNoncharacters
    //
    //    if Utils::isBMPNoncharacters(point) {
    //        return true;
    //    }

    if point.as_i32() <= 0xffff {
        return false;
    }

    //
    // Non-BMP
    //

    //
    // TODO: implement isNonBMPNoncharacters
    //
    //    if Utils::isNonBMPNoncharacters(point) {
    //        return true;
    //    }

    //
    // Plane 15 PUA
    //
    if 0x0f0000 <= point.as_i32() && point.as_i32() <= 0x0ffffd {
        return true;
    }

    //
    // Plane 16 PUA
    //
    if 0x100000 <= point.as_i32() && point.as_i32() <= 0x10fffd {
        return true;
    }

    return false;
}

pub fn isStraySurrogate(point: u32) -> bool {
    if 0xd800 <= point && point <= 0xdfff {
        return true;
    }

    return false;
}

// int get_graphical_i() {

//     static int i = std::ios_base::xalloc();

//     return i;
// }

// std::ostream& set_graphical(std::ostream& s) {

//     s.iword(get_graphical_i()) = 1;

//     return s;
// }

// std::ostream& clear_graphical(std::ostream& s) {

//     s.iword(get_graphical_i()) = 0;

//     return s;
// }

const DIGIT_LOOKUP: [u8; 256] = [
    99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99,
    99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99,
    0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 99, 99, 99, 99, 99, 99, 99, 10, 11, 12, 13, 14, 15, 16, 17, 18,
    19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 99, 99, 99, 99, 99, 99, 10,
    11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34,
    35, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99,
    99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99,
    99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99,
    99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99,
    99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99,
    99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99,
];

/// Convert val to the digit that it represents
pub fn toDigit(val: u8) -> u8 {
    return DIGIT_LOOKUP[usize::from(val)];
}

// if c is an ASCII WLCharacter, then compare to test
// TODO: Make these two types the same
pub fn ifASCIIWLCharacter(c_byte: u8, test: u8) -> bool {
    if c_byte > 0x7f {
        return true;
    }

    let c = char::from(c_byte);

    //
    // What is the last possible byte of an escaped WLCharacter?
    //
    if c.is_alphanumeric() || c == ']' {
        return true;
    }

    //
    // there may be a line continuation and so testing against  '^'  may actually involve the bytes  '\' '\n' '^'
    //
    if c == '\\' {
        return true;
    }

    return c_byte == test;
}

//
// Give suggestions for replacing certain characters with other characters:
//
// \[COMPATIBILITYNoBreak] -> \[NoBreak]
// \:2061 -> \[InvisibleApplication]
// \:2063 -> \[InvisibleComma]
// \:2064 -> \[ImplicitPlus]
// \[RightArrow] -> \[Rule]
// \:279D -> \[Rule]
// \:29F4 -> \[RuleDelayed]
// \:200B -> \[InvisibleSpace]
//
pub fn certainCharacterReplacementActions(c: WLCharacter, src: Span) -> Vec<CodeAction> {
    let mut Actions: Vec<CodeAction> = Vec::new();

    match c.to_point() {
        Char(CODEPOINT_LONGNAME_COMPATIBILITYNOBREAK) => {

            let safeAndGraphicalStr1 = c.safeAndGraphicalString();
            let safeAndGraphicalStr2 = WLCharacter::new(CODEPOINT_LONGNAME_NOBREAK).safeAndGraphicalString();

            //
            // UTF-8 bytes for U+2060 (\[NoBreak])
            //
            Actions.push(CodeAction::replace_text(
                format!("Replace ``{safeAndGraphicalStr1}`` with ``{safeAndGraphicalStr2}``"),
                src,
                (if c.escape() == EscapeStyle::None { "\u{2060}" } else {"\\[NoBreak]"}).to_owned()
            ));
        }
        Char(CODEPOINT_LONGNAME_RIGHTARROW) |
            //
            // U+279D Triangle-Headed Rightwards Arrow being used in place of \[Rule] is seen here:
            // http://mail-archive.wolfram.com/archive/t-paclets/2022/Mar00/0004.html
            //
        Char(CODEPOINT_TRIANGLEHEADEDRIGHTWARDSARROW) => {

            let safeAndGraphicalStr1 = c.safeAndGraphicalString();
            let safeAndGraphicalStr2 = WLCharacter::new(CODEPOINT_LONGNAME_RULE).safeAndGraphicalString();

            //
            // UTF-8 bytes for U+F522 (\[Rule])
            //
            Actions.push(CodeAction::replace_text(
                format!("Replace ``{safeAndGraphicalStr1}`` with ``{safeAndGraphicalStr2}``"),
                src,
                (if c.escape() == EscapeStyle::None { "\u{F522}" } else { "\\[Rule]" }).to_owned()
            ));
        }
        Char(CODEPOINT_RULEDELAYED) => {

            let safeAndGraphicalStr1 = c.safeAndGraphicalString();
            let safeAndGraphicalStr2 = WLCharacter::new(CODEPOINT_LONGNAME_RULEDELAYED).safeAndGraphicalString();

            //
            // UTF-8 bytes for U+F51F (\[RuleDelayed])
            //
            Actions.push(CodeAction::replace_text(
                format!("Replace ``{safeAndGraphicalStr1}`` with ``{safeAndGraphicalStr2}``"),
                src,
                (if c.escape() == EscapeStyle::None { "\u{F51F}" } else { "\\[RuleDelayed]" }).to_owned()
            ));
        }
        Char(CODEPOINT_FUNCTIONAPPLICATION) => {

            let safeAndGraphicalStr1 = c.safeAndGraphicalString();
            let safeAndGraphicalStr2 = WLCharacter::new(CODEPOINT_LONGNAME_INVISIBLEAPPLICATION).safeAndGraphicalString();

            //
            // UTF-8 bytes for U+F76D (\[InvisibleApplication])
            //
            Actions.push(CodeAction::replace_text(
                format!("Replace ``{safeAndGraphicalStr1}`` with ``{safeAndGraphicalStr2}``"),
                src,
                (if c.escape() == EscapeStyle::None { "\u{F76D}" } else { "\\[InvisibleApplication]"}).to_owned()
            ));

            Actions.push(CodeAction::delete_text(
                format!("Delete ``{safeAndGraphicalStr1}``"),
                src
            ));
        },
        Char(CODEPOINT_INVISIBLESEPARATOR) => {

            let safeAndGraphicalStr1 = c.safeAndGraphicalString();
            let safeAndGraphicalStr2 = WLCharacter::new(CODEPOINT_LONGNAME_INVISIBLECOMMA).safeAndGraphicalString();

            //
            // UTF-8 bytes for U+F765 (\[InvisibleComma])
            //
            Actions.push(CodeAction::replace_text(
                format!("Replace ``{safeAndGraphicalStr1}`` with ``{safeAndGraphicalStr2}``"),
                src,
                if c.escape() == EscapeStyle::None { "\u{F765}" } else { "\\[InvisibleComma]" }.to_owned()
            ));
        },
        Char(CODEPOINT_INVISIBLEPLUS) => {

            let safeAndGraphicalStr1 = c.safeAndGraphicalString();
            let safeAndGraphicalStr2 = WLCharacter::new(CODEPOINT_LONGNAME_IMPLICITPLUS).safeAndGraphicalString();

            //
            // UTF-8 bytes for U+F39E (\[ImplicitPlus])
            //
            Actions.push(CodeAction::replace_text(
                format!("Replace ``{safeAndGraphicalStr1}`` with ``{safeAndGraphicalStr2}``"),
                src,
                if c.escape() == EscapeStyle::None { "\u{F39E}" } else { "\\[ImplicitPlus]" }.to_owned()
            ));
        },
        Char(CODEPOINT_ZEROWIDTHSPACE) => {

            let safeAndGraphicalStr1 = c.safeAndGraphicalString();
            let safeAndGraphicalStr2 = WLCharacter::new(CODEPOINT_LONGNAME_INVISIBLESPACE).safeAndGraphicalString();

            //
            // UTF-8 bytes for U+F360 (\[InvisibleSpace])
            //
            Actions.push(CodeAction::replace_text(
                format!("Replace ``{safeAndGraphicalStr1}`` with ``{safeAndGraphicalStr2}``"),
                src,
                if c.escape() == EscapeStyle::None { "\u{F360}" } else {"\\[InvisibleSpace]"}.to_owned()
            ));
        },
        _ => ()
    }

    return Actions;
}

/// TODO(cleanup): Replace with call to `is_sorted()` method once that method is
///                stabilized. (See: <https://github.com/rust-lang/rust/issues/53485>)
pub(crate) fn is_sorted<T: Ord>(slice: &[T]) -> bool {
    slice.windows(2).all(|elem| elem[0] <= elem[1])
}
