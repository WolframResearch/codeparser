//! Collection of utility functions for codepoints and long names

use crate::{
    code_point::CodePoint,
    long_names_registration::{
        ASCII_REPLACEMENTS_MAP, CODE_POINT_TO_LONGNAME_MAP__NAMES,
        CODE_POINT_TO_LONGNAME_MAP__POINTS, MB_NEWLINE_CODE_POINTS,
        MB_NOT_STRAGE_LETTERLIKE_CODE_POINTS, MB_PUNCTUATION_CODE_POINTS,
        MB_UNINTERPRETABLE_CODE_POINTS, MB_WHITESPACE_CODE_POINTS, RAW_SET,
    },
    utils,
};

pub fn code_point_has_long_name(point: CodePoint) -> bool {
    // debug_assert!(CODE_POINT_TO_LONGNAME_MAP__POINTS.is_sorted());
    // CODE_POINT_TO_LONGNAME_MAP__POINTS.binary_search(&point).is_ok()

    // TODO(optimize): This linear search is likely slower than the commented
    //                 out binary_search(). Fix the sorting of this table so
    //                 that the debug_assert!(..is_sorted()) succeeds, and then
    //                 switch back to the binary search code.
    CODE_POINT_TO_LONGNAME_MAP__POINTS.contains(&point)
}

pub fn code_point_to_long_name(point: CodePoint) -> &'static str {
    // debug_assert!(CODE_POINT_TO_LONGNAME_MAP__POINTS.is_sorted());
    // let idx: usize = CODE_POINT_TO_LONGNAME_MAP__POINTS
    //     .binary_search(&point)
    //     .expect("unable to find long name for code point");

    // TODO(optimize): This linear search is likely slower than the commented
    //                 out binary_search(). Fix the sorting of this table so
    //                 that the debug_assert!(..is_sorted()) succeeds, and then
    //                 switch back to the binary search code.
    let idx: usize = CODE_POINT_TO_LONGNAME_MAP__POINTS
        .iter()
        .position(|p| *p == point)
        .expect("unable to find long name for code point");

    let long_name: &str = CODE_POINT_TO_LONGNAME_MAP__NAMES[idx];

    long_name
}

/// Is this \[Raw] something?
pub fn isRaw(long_name_str: &str) -> bool {
    debug_assert!(utils::is_sorted(&RAW_SET));
    return RAW_SET.binary_search(&long_name_str).is_ok();
}

pub fn isMBNotStrangeLetterlike(point: CodePoint) -> bool {
    debug_assert!(utils::is_sorted(&MB_NOT_STRAGE_LETTERLIKE_CODE_POINTS));
    return MB_NOT_STRAGE_LETTERLIKE_CODE_POINTS
        .binary_search(&point)
        .is_ok();
}

pub fn asciiReplacements(point: CodePoint) -> Vec<String> {
    match ASCII_REPLACEMENTS_MAP.get(&point) {
        Some(replacements) => replacements
            .into_iter()
            .map(|&s: &&str| s.to_owned())
            .collect(),
        None => Vec::new(),
    }
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
    debug_assert!(utils::is_sorted(&MB_PUNCTUATION_CODE_POINTS));
    return MB_PUNCTUATION_CODE_POINTS.binary_search(&point).is_ok();
}

pub fn isMBWhitespace(point: CodePoint) -> bool {
    debug_assert!(utils::is_sorted(&MB_WHITESPACE_CODE_POINTS));
    return MB_WHITESPACE_CODE_POINTS.binary_search(&point).is_ok();
}

pub fn isMBNewline(point: CodePoint) -> bool {
    debug_assert!(utils::is_sorted(&MB_NEWLINE_CODE_POINTS));
    return MB_NEWLINE_CODE_POINTS.binary_search(&point).is_ok();
}

pub fn isMBUninterpretable(point: CodePoint) -> bool {
    debug_assert!(utils::is_sorted(&MB_UNINTERPRETABLE_CODE_POINTS));
    return MB_UNINTERPRETABLE_CODE_POINTS.binary_search(&point).is_ok();
}
