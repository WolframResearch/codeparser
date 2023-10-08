//! Collection of utility functions for codepoints and long names

use crate::{
    generated::long_names_registration::{
        ASCII_REPLACEMENTS_MAP, CODEPOINT_TO_LONGNAME_MAP,
        LONGNAME_TO_CODEPOINT_MAP, MB_NEWLINE_CODE_POINTS,
        MB_NOT_STRAGE_LETTERLIKE_CODE_POINTS, MB_PUNCTUATION_CODE_POINTS,
        MB_UNINTERPRETABLE_CODE_POINTS, MB_WHITESPACE_CODE_POINTS, RAW_SET,
    },
    read::code_point::CodePoint,
    utils,
};

pub(crate) fn codepoint_has_longname(point: char) -> bool {
    codepoint_to_longname(CodePoint::Char(point)).is_some()
}

pub(crate) fn codepoint_to_longname(point: CodePoint) -> Option<&'static str> {
    // NOTE: This assertion currently spuriously fails because the
    //       StringMeta_DoubleQuote and StringMeta_Backslash codepoints are fake
    //       codepoints with negative values.
    /*
    debug_assert!(utils::is_sorted_by(
        &CODEPOINT_TO_LONGNAME_MAP,
        |(point, _): &(CodePoint, &str)| *point
    ));
    */

    let index: usize = CODEPOINT_TO_LONGNAME_MAP
        .binary_search_by(|(cp, _)| cp.cmp(&point))
        .ok()?;

    let (_, longname) = CODEPOINT_TO_LONGNAME_MAP[index];

    Some(longname)
}

pub(crate) fn longname_to_codepoint(longname: &str) -> Option<CodePoint> {
    debug_assert!(utils::is_sorted_by(
        &LONGNAME_TO_CODEPOINT_MAP,
        |(str, _): &(&str, CodePoint)| *str
    ));

    let index: usize = LONGNAME_TO_CODEPOINT_MAP
        .binary_search_by(|&(str, _)| str.cmp(longname))
        .ok()?;

    let (_, point) = LONGNAME_TO_CODEPOINT_MAP[index];

    Some(point)
}

/// Is this \[Raw] something?
pub fn isRaw(long_name_str: &str) -> bool {
    debug_assert!(utils::is_sorted(&RAW_SET));
    return RAW_SET.binary_search(&long_name_str).is_ok();
}

pub fn isMBNotStrangeLetterlike(point: CodePoint) -> bool {
    // TODO(cleanup): Change param type?
    let Some(char) = point.as_char() else {
        return false;
    };

    debug_assert!(utils::is_sorted(&MB_NOT_STRAGE_LETTERLIKE_CODE_POINTS));
    return MB_NOT_STRAGE_LETTERLIKE_CODE_POINTS
        .binary_search(&char)
        .is_ok();
}

pub fn asciiReplacements(point: CodePoint) -> Vec<String> {
    // TODO(cleanup): Change param type?
    let Some(char) = point.as_char() else {
        return Vec::new();
    };

    debug_assert!(utils::is_sorted(ASCII_REPLACEMENTS_MAP));

    let Some(index): Option<usize> = ASCII_REPLACEMENTS_MAP
        .binary_search_by(|(cp, _)| cp.cmp(&char))
        .ok()
    else {
        return Vec::new();
    };

    let (_, replacements) = ASCII_REPLACEMENTS_MAP[index];

    replacements
        .into_iter()
        .map(|&s: &&str| s.to_owned())
        .collect()
}

pub fn replacementGraphical(replacement: String) -> String {
    if replacement == " " {
        //
        // \[SpaceIndicator]
        //

        // this was:
        // return "\u2423";
        //
        // But MSVC gave:
        // warning C4566: character represented by universal-character-name '\u2423' cannot be represented in the current code page (1252)
        //

        //
        // UTF-8 bytes for U+2423
        //
        return String::from("\u{2423}");
    }

    if replacement == "\n" {
        return String::from("\\n");
    }

    return replacement;
}

pub fn isMBPunctuation(point: CodePoint) -> bool {
    // TODO(cleanup): Change param type?
    let Some(char) = point.as_char() else {
        return false;
    };

    debug_assert!(utils::is_sorted(&MB_PUNCTUATION_CODE_POINTS));
    return MB_PUNCTUATION_CODE_POINTS.binary_search(&char).is_ok();
}

pub fn isMBWhitespace(point: CodePoint) -> bool {
    // TODO(cleanup): Change param type?
    let Some(char) = point.as_char() else {
        return false;
    };

    debug_assert!(utils::is_sorted(&MB_WHITESPACE_CODE_POINTS));
    return MB_WHITESPACE_CODE_POINTS.binary_search(&char).is_ok();
}

pub fn isMBNewline(point: CodePoint) -> bool {
    debug_assert!(utils::is_sorted(&MB_NEWLINE_CODE_POINTS));
    return MB_NEWLINE_CODE_POINTS.binary_search(&point).is_ok();
}

pub fn isMBUninterpretable(point: CodePoint) -> bool {
    // TODO(cleanup): Change param type?
    let Some(char) = point.as_char() else {
        return false;
    };

    debug_assert!(utils::is_sorted(&MB_UNINTERPRETABLE_CODE_POINTS));
    return MB_UNINTERPRETABLE_CODE_POINTS.binary_search(&char).is_ok();
}
