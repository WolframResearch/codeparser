use std::{fmt::Display, num::NonZeroU32};

use crate::{
    generated::long_names_registration::*,
    issue::CodeAction,
    read::{
        code_point::{CodePoint::Char, *},
        Escape, WLCharacter,
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
    if point.is_ascii() {
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
    99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99,
    99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99,
    99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 99,
    99, 99, 99, 99, 99, 99, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22,
    23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 99, 99, 99, 99, 99, 99,
    10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28,
    29, 30, 31, 32, 33, 34, 35, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99,
    99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99,
    99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99,
    99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99,
    99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99,
    99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99,
    99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99,
    99, 99, 99, 99, 99, 99, 99,
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
pub fn certainCharacterReplacementActions(
    c: WLCharacter,
    src: Span,
) -> Vec<CodeAction> {
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
                (if c.escape() == Escape::None { "\u{2060}" } else {"\\[NoBreak]"}).to_owned()
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
                (if c.escape() == Escape::None { "\u{F522}" } else { "\\[Rule]" }).to_owned()
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
                (if c.escape() == Escape::None { "\u{F51F}" } else { "\\[RuleDelayed]" }).to_owned()
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
                (if c.escape() == Escape::None { "\u{F76D}" } else { "\\[InvisibleApplication]"}).to_owned()
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
                if c.escape() == Escape::None { "\u{F765}" } else { "\\[InvisibleComma]" }.to_owned()
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
                if c.escape() == Escape::None { "\u{F39E}" } else { "\\[ImplicitPlus]" }.to_owned()
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
                if c.escape() == Escape::None { "\u{F360}" } else {"\\[InvisibleSpace]"}.to_owned()
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

/// TODO(cleanup): Replace with call to `is_sorted_by()` method once that method is
///                stabilized. (See: <https://github.com/rust-lang/rust/issues/53485>)
pub(crate) fn is_sorted_by<T, B: Ord, F: Fn(&T) -> B>(
    slice: &[T],
    by: F,
) -> bool {
    slice.windows(2).all(|elem| by(&elem[0]) <= by(&elem[1]))
}

/// Alternative to [`std::array::from_fn`] that works in `const` contexts.
///
/// See also: <https://doc.rust-lang.org/stable/std/mem/union.MaybeUninit.html#initializing-an-array-element-by-element>
macro_rules! from_fn {
    ([$T:ty, $N:expr], |$index:ident: usize| $expr:expr) => {{
        use std::mem::MaybeUninit;

        let mut table: [MaybeUninit<$T>; $N] =
            unsafe { MaybeUninit::uninit().assume_init() };

        let mut $index: usize = 0;

        loop {
            if $index >= $N {
                break;
            }

            table[$index] = MaybeUninit::new($expr);

            $index += 1;
        }

        unsafe { std::mem::transmute::<[MaybeUninit<$T>; $N], [$T; $N]>(table) }
    }};
}

pub(crate) use from_fn;

//======================================
// String comparision and slice contains in `const`
//======================================

pub(crate) const fn contains(slice: &[&str], needle: &str) -> bool {
    let mut i = 0;
    loop {
        if i >= slice.len() {
            return false;
        }

        let elem = slice[i];

        if const_str_equal(elem, needle) {
            return true;
        }

        i += 1;
    }
}

const fn const_str_equal(lhs: &str, rhs: &str) -> bool {
    const_slice_equal(lhs.as_bytes(), rhs.as_bytes())
}

const fn const_slice_equal(lhs: &[u8], rhs: &[u8]) -> bool {
    if lhs.len() != rhs.len() {
        return false;
    }

    let mut i = 0;
    while i < lhs.len() {
        if lhs[i] != rhs[i] {
            return false;
        }

        i += 1;
    }

    true
}

//=======================================
// Splitting lists pair-wise
//=======================================

/// Split a slice by comparing pairs of elements.
pub fn split_by_pairs<T: std::fmt::Debug, F>(
    mut slice: &[T],
    mut pred: F,
) -> Vec<&[T]>
where
    F: FnMut(&T, &T) -> bool,
{
    if slice.is_empty() {
        return Vec::new();
    }
    if slice.len() == 1 {
        return vec![slice];
    }

    let mut chunks = Vec::new();
    let mut right_index = 1;

    while right_index < slice.len() {
        let left = &slice[right_index - 1];
        let right = &slice[right_index];

        right_index += 1;

        if pred(&left, &right) {
            continue;
        }

        chunks.push(&slice[..right_index - 1]);

        slice = &slice[right_index - 1..];
        right_index = 1;
    }

    if !slice.is_empty() {
        chunks.push(slice);
    }

    chunks
}

#[test]
fn test_split_by_pairs() {
    assert_eq!(
        split_by_pairs(&Vec::<u32>::new(), u32::eq),
        Vec::<&[u32]>::new()
    );

    assert_eq!(split_by_pairs(&[1], u32::eq), vec![&[1]]);

    assert_eq!(split_by_pairs(&[1, 2], u32::eq), vec![&[1], &[2]]);

    assert_eq!(
        split_by_pairs(&[1, 1, 2, 2], u32::eq),
        vec![&[1, 1], &[2, 2]]
    );

    assert_eq!(
        split_by_pairs(&[1, 2, 3, 4], u32::eq),
        vec![&[1], &[2], &[3], &[4]]
    );

    // Split when the odd/even-ness changes.
    assert_eq!(
        split_by_pairs(&[1, 3, 5, 4, 7, 9, 6, 2, 3], |left, right| left % 2
            == right % 2),
        vec![&[1, 3, 5], [4].as_slice(), &[7, 9], &[6, 2], &[3]]
    );

    // Split when the abs difference is not 1.
    assert_eq!(
        split_by_pairs(
            &[1u32, 2, 4, 5, 4, 6, 7, 8, 11, 13, 15, 16],
            |&left, &right| left.abs_diff(right) == 1
        ),
        vec![
            &[1, 2],
            [4, 5, 4].as_slice(),
            &[6, 7, 8],
            &[11],
            &[13],
            &[15, 16]
        ]
    );
}

//=======================================
// Mutating NonZeruU32 values
//=======================================

#[inline]
pub(crate) fn non_zero_u32_incr(value: NonZeroU32) -> NonZeroU32 {
    debug_assert!(
        value.checked_add(1).is_some(),
        "NonZeroU32 increment overflows u32"
    );

    value.saturating_add(1)
}

#[inline]
pub(crate) fn non_zero_u32_add(value: NonZeroU32, add: u32) -> NonZeroU32 {
    debug_assert!(
        value.checked_add(add).is_some(),
        "NonZeroU32 add overflows u32"
    );

    value.saturating_add(add)
}

//=======================================
// Functional list operations
//=======================================

/// Join an array and a vector.
pub(crate) fn join<T, const N: usize>(
    array: [T; N],
    mut vec: Vec<T>,
) -> Vec<T> {
    vec.splice(0..0, array);
    vec
}

#[allow(dead_code)]
pub(crate) fn append<T>(mut list: Vec<T>, elem: T) -> Vec<T> {
    list.push(elem);
    list
}

#[allow(dead_code)]
pub(crate) fn prepend<T>(mut list: Vec<T>, elem: T) -> Vec<T> {
    list.insert(0, elem);
    list
}

pub(crate) fn most_slice<T>(list: &[T]) -> Option<&[T]> {
    if list.is_empty() {
        return None;
    }

    Some(&list[..list.len() - 1])
}

#[allow(dead_code)]
pub(crate) fn insert_before_last<T>(
    mut list: Vec<T>,
    elems: impl Iterator<Item = T>,
) -> Vec<T> {
    debug_assert!(!list.is_empty());

    list.splice(list.len() - 1..list.len() - 1, elems);

    list
}

#[test]
fn test_join() {
    assert_eq!(join([1, 2, 3], vec![4, 5, 6]), vec![1, 2, 3, 4, 5, 6])
}

#[test]
fn test_most_slice() {
    assert_eq!(most_slice::<i32>(&[]), None);

    assert_eq!(most_slice(&[1]), Some([].as_slice()));

    assert_eq!(most_slice(&[1, 2, 3]), Some([1, 2].as_slice()));
}

#[test]
fn test_insert_before_last() {
    assert_eq!(
        insert_before_last(vec![1, 2, 3], [4, 5, 6].into_iter()),
        vec![1, 2, 4, 5, 6, 3]
    )
}

//======================================
// debug_assert_matches!
//======================================

macro_rules! debug_assert_matches {
    ($left:expr, $right:pat) => {
        debug_assert!(matches!($left, $right))
    };
}

pub(crate) use debug_assert_matches;

//======================================
// CommaSeparated and CommaTerminated display forms
//======================================

pub(crate) struct CommaSeparated<'a, T>(pub &'a Vec<T>);

pub(crate) struct CommaTerminated<'a, T>(pub &'a Vec<T>);

impl<'a, T: Display> Display for CommaSeparated<'a, T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let CommaSeparated(sequence) = self;

        for (index, elem) in sequence.iter().enumerate() {
            write!(f, "{elem}")?;

            let is_last = index == sequence.len() - 1;

            if !is_last {
                write!(f, ", ")?
            }
        }

        Ok(())
    }
}

impl<'a, T: Display> Display for CommaTerminated<'a, T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let CommaTerminated(sequence) = self;

        for elem in sequence.into_iter() {
            write!(f, "{elem}, ")?;
        }

        Ok(())
    }
}

//======================================
// is_interval_member()
//======================================

/// Returns true if `b` is an interval that is completely contained inside `a`.
pub(crate) fn is_interval_member(a: (u32, u32), b: (u32, u32)) -> bool {
    let (a_start, a_end) = a;
    let (b_start, b_end) = b;

    b_start >= a_start && b_end <= a_end
}

// TODO: Do more testing of this intersection function.
pub(crate) fn intersection(a: (u32, u32), b: (u32, u32)) -> Option<(u32, u32)> {
    use std::cmp::{max, min};

    let (a, b) = if a.0 < b.0 { (a, b) } else { (b, a) };

    let (a_start, a_end) = a;
    let (b_start, b_end) = b;

    assert!(a_start <= a_end);
    assert!(b_start <= b_end);

    debug_assert!(a_start <= b_start);

    let highest_start = max(a_start, b_start);
    let lowest_end = min(a_end, b_end);

    if highest_start > lowest_end {
        return None;
    }

    Some((highest_start, lowest_end))
}

#[test]
fn test_intersection() {
    assert_eq!(intersection((0, 0), (0, 0)), Some((0, 0)));
    assert_eq!(intersection((1, 3), (1, 3)), Some((1, 3)));
    assert_eq!(intersection((1, 4), (1, 3)), Some((1, 3)));
    assert_eq!(intersection((1, 3), (1, 4)), Some((1, 3)));
    assert_eq!(intersection((1, 2), (3, 4)), None);
    assert_eq!(intersection((3, 4), (1, 2)), None);
    assert_eq!(intersection((3, 4), (1, 3)), Some((3, 3)));
}

//======================================
// Copy to clipboard
//======================================

#[cfg(test)]
pub(crate) fn copy_to_clipboard(content: &str) {
    if cfg!(target_os = "macos") {
        use std::io::Write;
        use std::process::{Command, Stdio};

        let mut child = Command::new("pbcopy")
            .stdin(Stdio::piped())
            .stdout(Stdio::piped())
            .spawn()
            .unwrap();

        let child_stdin = child.stdin.as_mut().unwrap();
        child_stdin.write_all(content.as_bytes()).unwrap();

        let output = child.wait_with_output().unwrap();

        eprintln!("copied to clipboard with result: {output:?}");
    }
}
