//! Collection of utility functions for codepoints and long names

use crate::{
    code_point::CodePoint,
    long_names_registration::{
        asciiReplacementsMap, mbNewlineCodePoints, mbNotStrangeLetterlikeCodePoints,
        mbPunctuationCodePoints, mbUninterpretableCodePoints, mbWhitespaceCodePoints,
        CodePointToLongNameMap_names, CodePointToLongNameMap_points, RawSet,
    },
    utils,
};

pub fn code_point_has_long_name(point: CodePoint) -> bool {
    // debug_assert!(CodePointToLongNameMap_points.is_sorted());
    // CodePointToLongNameMap_points.binary_search(&point).is_ok()

    // TODO(optimize): This linear search is likely slower than the commented
    //                 out binary_search(). Fix the sorting of this table so
    //                 that the debug_assert!(..is_sorted()) succeeds, and then
    //                 switch back to the binary search code.
    CodePointToLongNameMap_points.contains(&point)
}

pub fn code_point_to_long_name(point: CodePoint) -> &'static str {
    // let it = std::lower_bound(CodePointToLongNameMap_points.begin(), CodePointToLongNameMap_points.end(), i);
    // assert!(it != CodePointToLongNameMap_points.end());
    // assert!(*it == i);
    // let idx = it - CodePointToLongNameMap_points.begin();


    // debug_assert!(CodePointToLongNameMap_points.is_sorted());
    // let idx: usize = CodePointToLongNameMap_points
    //     .binary_search(&point)
    //     .expect("unable to find long name for code point");

    // TODO(optimize): This linear search is likely slower than the commented
    //                 out binary_search(). Fix the sorting of this table so
    //                 that the debug_assert!(..is_sorted()) succeeds, and then
    //                 switch back to the binary search code.
    let idx: usize = CodePointToLongNameMap_points
        .iter()
        .position(|p| *p == point)
        .expect("unable to find long name for code point");

    let long_name: &str = CodePointToLongNameMap_names[idx];

    long_name
}

/// Is this \[Raw] something?
pub fn isRaw(long_name_str: &str) -> bool {
    debug_assert!(utils::is_sorted(&RawSet));
    return RawSet.binary_search(&long_name_str).is_ok();

    // let it = std::lower_bound(RawSet.begin(), RawSet.end(), LongNameStr);
    // return it != RawSet.end() && *it == LongNameStr;
}

pub fn isMBNotStrangeLetterlike(point: CodePoint) -> bool {
    debug_assert!(utils::is_sorted(&mbNotStrangeLetterlikeCodePoints));
    return mbNotStrangeLetterlikeCodePoints
        .binary_search(&point)
        .is_ok();

    // let it = std::lower_bound(
    //     mbNotStrangeLetterlikeCodePoints.begin(),
    //     mbNotStrangeLetterlikeCodePoints.end(),
    //     point,
    // );
    // return it != mbNotStrangeLetterlikeCodePoints.end() && *it == point;
}

pub fn asciiReplacements(point: CodePoint) -> Vec<String> {
    match asciiReplacementsMap.get(&point) {
        Some(replacements) => replacements
            .into_iter()
            .map(|&s: &&str| s.to_owned())
            .collect(),
        None => Vec::new(),
    }

    // let it = asciiReplacementsMap.find(point);
    // return if it != asciiReplacementsMap.end() { it.second } else { Vec::new() };
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
    debug_assert!(utils::is_sorted(&mbPunctuationCodePoints));
    return mbPunctuationCodePoints.binary_search(&point).is_ok();

    // let it = std::lower_bound(mbPunctuationCodePoints.begin(), mbPunctuationCodePoints.end(), point);
    // return it != mbPunctuationCodePoints.end() && *it == point;
}

pub fn isMBWhitespace(point: CodePoint) -> bool {
    debug_assert!(utils::is_sorted(&mbWhitespaceCodePoints));
    return mbWhitespaceCodePoints.binary_search(&point).is_ok();

    // let it = mbWhitespaceCodePoints.partition_point(|p| p <)
    // let it = std::lower_bound(
    //     mbWhitespaceCodePoints.begin(),
    //     mbWhitespaceCodePoints.end(),
    //     point,
    // );
    // return it != mbWhitespaceCodePoints.end() && *it == point;
}

pub fn isMBNewline(point: CodePoint) -> bool {
    debug_assert!(utils::is_sorted(&mbNewlineCodePoints));
    return mbNewlineCodePoints.binary_search(&point).is_ok();

    // let it = std::lower_bound(
    //     mbNewlineCodePoints.begin(),
    //     mbNewlineCodePoints.end(),
    //     point,
    // );
    // return it != mbNewlineCodePoints.end() && *it == point;
}

pub fn isMBUninterpretable(point: CodePoint) -> bool {
    debug_assert!(utils::is_sorted(&mbUninterpretableCodePoints));
    return mbUninterpretableCodePoints.binary_search(&point).is_ok();

    // let it = std::lower_bound(mbUninterpretableCodePoints.begin(), mbUninterpretableCodePoints.end(), point);
    // return it != mbUninterpretableCodePoints.end() && *it == point;
}
