use std::{
    cmp::Ordering,
    fmt::{self, Debug, Display},
    ops::Index,
    slice::SliceIndex,
};

use crate::{
    code_point::{
        CodePoint::{self, Char, *},
        CODEPOINT_DEL, CODEPOINT_ESC, *,
    },
    feature,
    long_names::{self as LongNames, code_point_has_long_name},
    my_string::MyString,
    my_string_registration::STRING_FATAL,
    symbol::Symbol,
    symbol_registration::{
        SYMBOL_CODEPARSER_ENCODINGISSUE, SYMBOL_CODEPARSER_FORMATISSUE,
        SYMBOL_CODEPARSER_SYNTAXISSUE,
    },
    tokenizer::{ASCII_FORM_FEED, ASCII_VTAB},
    wl_character::{EscapeStyle, WLCharacter},
};

pub type IssuePtrSet = Vec<Issue>;

pub type AdditionalDescriptionVector = Vec<String>;


/// This type represents a subslice of the complete input byte sequence,
/// starting at the specified byte [`offset`][Buffer::offset].
///
/// Note that the length of [`slice`][Buffer::slice] is NOT guaranteed to correspond to any
/// semantically meaningful span of the input. (For that, see [`BufferAndLength`]).
#[derive(Debug, Copy, Clone, PartialEq)]
pub(crate) struct Buffer<'i> {
    pub slice: &'i [u8],
    pub offset: usize,
}

#[derive(Debug, Copy, Clone)]
pub(crate) struct BufferAndLength<'i> {
    pub buf: Buffer<'i>,
    // PRE_COMMIT: Does this bitfield size break anything in the port?
    // const uint64_t Len : 48;
    /// The number of bytes in `buf.slice` that are refered to by this reference.
    ///
    /// Note that this length may be smaller than `buf.slice.len()`
    pub len: usize,
}

/* FIXME: Re-enable this assertion, or a variation of it.
const _: () = assert!(
    (SIZEOF_VOID_P == 8 && std::mem::size_of::<BufferAndLength>() == 16) || (SIZEOF_VOID_P == 4),
    "Check your assumptions"
);
*/

#[derive(Debug, Copy, Clone, PartialEq)]
pub struct ByteSpan {
    /// The offset into the [`ParserSession.input`] buffer that this token
    /// starts at.
    pub offset: usize,
    pub len: usize,
}

impl ByteSpan {
    #[cfg(test)]
    pub(crate) fn new(offset: usize, len: usize) -> Self {
        ByteSpan { offset, len }
    }
}

//
//
//
pub mod NextPolicyBits {
    //
    /// Enable character decoding issues
    ///
    /// "\c" gives a CharacterDecoding error (issues are ENABLED)
    ///
    /// (*\c*) does NOT give a CharacterDecoding error (issues are DISABLED)
    ///
    /// This is also used when peeking: no need to report issues while peeking
    ///
    /// Used By ByteDecoder, CharacterDecoder
    //
    pub const ENABLE_CHARACTER_DECODING_ISSUES: u8 = 0x01;

    //
    /// when inside Tokenizer_currentWLCharacter, then do not track line continuations
    /// since Tokenizer_currentWLCharacter is implemented as Tokenizer_nextWLCharacter that is then reset, there should be no side-effects
    //
    pub const TRACK_LC: u8 = 0x02;

    //
    /// Used by Tokenizer
    //
    pub const RETURN_TOPLEVELNEWLINE: u8 = 0x04;

    //
    /// This bit serves 2 purposes:
    /// Complex line continuations
    /// Decrease severity of unexpected characters
    ///
    /// These are exactly what we care about with strings and comments, so use only a single bit for both purposes.
    ///
    /// NOTE: If the set:
    /// {strings, comments}
    /// is ever not exactly the same as the set:
    /// (things that care about complex line continuations and decreased severity of unexpected characters)
    /// then we need to rethink these bits.
    ///
    /// Complex line continuations:
    /// Line continuations inside of strings or comments are "complex":
    /// Formatting matters
    ///
    /// All other line continuations are simple:
    /// inside or outside of other tokens
    /// outside of strings or comments
    ///
    ///
    /// Decrease severity of unexpected characters:
    /// Outside of strings, \[RightArrow] should be a warning
    /// Inside of strings, \[RightArrow] should be a remark
    //
    pub const STRING_OR_COMMENT: u8 = 0x08;

    //
    /// If inside #, then give syntax warnings for #"123" and #a`b syntax (which is undocumented syntax)
    ///
    /// But obviously "123" and a`b are fine outside of #
    ///
    /// Also return symbols as strings, e.g., the  abc  in  #abc  is a string
    ///
    /// Used by Tokenizer
    //
    pub const TAGSLOT_BEHAVIOR_FOR_STRINGS: u8 = 0x10;

    //
    /// When tokenizing numbers, return immediately when an integer has been tokenized
    ///
    /// This is used when parsing Slot, SlotSequence, and Out
    ///
    /// For example, we must consider  `#2.c`  to be `Slot[2] . c`  and NOT  `Slot[1] 2. c`
    //
    pub const INTEGER_SHORT_CIRCUIT: u8 = 0x20;

    //
    // With input  "\\[Alpa]"  , then report \[Alpa] as unrecognized, even though this is valid syntax
    //
    pub const SCAN_FOR_UNRECOGNIZEDLONGNAMES: u8 = 0x40;
}

pub use self::NextPolicyBits::*;

const _: () = assert!(
    RETURN_TOPLEVELNEWLINE == 0x04,
    "Needs to be 0b100, for easy or-ing of TOKEN_INTERNALNEWLINE to TOKEN_TOPLEVELNEWLINE"
);

pub(crate) type NextPolicy = u8;

use ordered_float::NotNan;
use NextPolicyBits::{
    ENABLE_CHARACTER_DECODING_ISSUES, INTEGER_SHORT_CIRCUIT, RETURN_TOPLEVELNEWLINE,
    TAGSLOT_BEHAVIOR_FOR_STRINGS, TRACK_LC,
};

pub const TOPLEVEL: NextPolicy =
    ENABLE_CHARACTER_DECODING_ISSUES | RETURN_TOPLEVELNEWLINE | TRACK_LC;

#[allow(dead_code)] // TODO(cleanup): Is it meaningful that this is unused?
pub const INSIDE_SYMBOL: NextPolicy = ENABLE_CHARACTER_DECODING_ISSUES | TRACK_LC;

pub const INSIDE_STRINGIFY_AS_TAG: NextPolicy =
    ENABLE_CHARACTER_DECODING_ISSUES | TAGSLOT_BEHAVIOR_FOR_STRINGS | TRACK_LC;
pub const INSIDE_STRINGIFY_AS_FILE: NextPolicy = RETURN_TOPLEVELNEWLINE;

pub const INSIDE_SLOT: NextPolicy = TAGSLOT_BEHAVIOR_FOR_STRINGS | INTEGER_SHORT_CIRCUIT | TRACK_LC;

pub const INSIDE_SLOTSEQUENCE: NextPolicy =
    ENABLE_CHARACTER_DECODING_ISSUES | INTEGER_SHORT_CIRCUIT | TRACK_LC;

pub const INSIDE_OUT: NextPolicy =
    ENABLE_CHARACTER_DECODING_ISSUES | INTEGER_SHORT_CIRCUIT | TRACK_LC;

/// A single character of source code
///
/// The text `\[Alpha]` would be 8 separate SourceCharacters
// TODO(cleanup): remove CodePoint, just have SourceCharacter?
pub type SourceCharacter = CodePoint;

const _: () = assert!(
    std::mem::size_of::<SourceCharacter>() == 4,
    "Check your assumptions"
);

#[derive(Debug, Copy, Clone)]
pub enum SourceConvention {
    /// Handle next (non-newline) SourceLocation by incrementing column.
    /// Handle next newline by incrementing line.
    LineColumn = 0,
    // TODO: Clarify, is this the index by unicode character(?), or the byte
    //  offset, or something else?
    /// Handle next (non-newline) SourceLocation by incrementing index.
    /// Handle next newline by incrementing index.
    CharacterIndex = 1,
}

pub const DEFAULT_TAB_WIDTH: u32 = 4;

#[derive(Copy, Clone, PartialEq, Eq, Hash)]
pub struct SourceLocation {
    /// if source convention is LineColumn, this is Line
    /// if source convention is SourceCharacterIndex, this is unused (always 0)
    pub first: u32,

    /// if source convention is LineColumn, this is Column
    /// if source convention is SourceCharacterIndex, this is index
    pub second: u32,
}

const _: () = assert!(
    std::mem::size_of::<SourceLocation>() == 8,
    "Check your assumptions"
);

impl Display for SourceLocation {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let SourceLocation { first, second } = *self;

        write!(f, "{first}:{second}")
    }
}

impl Debug for SourceLocation {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self)
    }
}

// For LineContinuations and EmbeddedNewlines
//
// bool operator<(SourceLocation a, SourceLocation b);
// bool operator<=(SourceLocation a, SourceLocation b);

//
// For googletest
//
#[cfg(feature = "BUILD_TESTS")]
fn PrintTo(Loc: &SourceLocation, s: &mut std::ostream);

#[derive(Copy, Clone, PartialEq, Hash)]
pub struct Source {
    pub start: SourceLocation,
    pub end: SourceLocation,
}

const _: () = assert!(
    std::mem::size_of::<Source>() == 16,
    "Check your assumptions"
);

impl Display for Source {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let Source { start, end } = *self;

        write!(f, "{start:?}-{end:?}")
    }
}

impl Debug for Source {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self)
    }
}

// For googletest
#[cfg(feature = "BUILD_TESTS")]
fn PrintTo(src: &Source, s: &mut std::ostream);

//
// For std::set
//
// PRE_COMMTI: Update uses
// struct IssuePtrCompare {
// public:

//     bool operator() (const IssuePtr& L, const IssuePtr& R) const;
// };

#[derive(Debug, Clone, Hash)]
pub struct Issue {
    pub make_sym: Symbol,
    pub tag: MyString,
    pub msg: String,
    pub sev: MyString,
    pub src: Source,
    pub val: NotNan<f64>,
    pub actions: Vec<CodeAction>,
    pub additional_descriptions: AdditionalDescriptionVector,
}

#[derive(Debug, Clone, PartialEq, Hash)]
pub struct CodeAction {
    pub label: String,
    pub src: Source,
    pub kind: CodeActionKind,
}

#[derive(Debug, Clone, PartialEq, Hash)]
pub enum CodeActionKind {
    ReplaceText { replacement_text: String },
    InsertText { insertion_text: String },
    DeleteText,
}

impl CodeAction {
    pub fn replace_text(label: String, src: Source, replacement_text: String) -> Self {
        CodeAction {
            label,
            src,
            kind: CodeActionKind::ReplaceText { replacement_text },
        }
    }

    pub fn insert_text(label: String, src: Source, insertion_text: String) -> Self {
        CodeAction {
            label,
            src,
            kind: CodeActionKind::InsertText { insertion_text },
        }
    }

    pub fn delete_text(label: String, src: Source) -> Self {
        CodeAction {
            label,
            src,
            kind: CodeActionKind::DeleteText,
        }
    }
}

impl<'i, I: SliceIndex<[u8]>> Index<I> for Buffer<'i> {
    type Output = <I as SliceIndex<[u8]>>::Output;

    fn index(&self, index: I) -> &Self::Output {
        let Buffer { slice, offset: _ } = self;

        &slice[index]
    }
}

impl<'i> BufferAndLength<'i> {
    pub fn from_buffer_with_len(buf: Buffer<'i>, len: usize) -> Self {
        BufferAndLength { buf, len }
    }

    // BufferAndLength::BufferAndLength(Buffer Buf) : Buf(Buf), Len(0) {}

    /*
    pub fn from_buffer(buf: Buffer<'i>) -> Self {
        // BufferAndLength { buf, len: 0 }
        BufferAndLength(buf)
    }
    */

    /// Construct a new [`BufferAndLength`] between `[start, end)` (the first
    /// character in `end` will *not* be in the resulting buffer).
    //
    // TODO: Change the second parameter to be a usize.
    pub fn between<'s, 'e>(start: Buffer<'s>, end: Buffer<'e>) -> BufferAndLength<'s> {
        debug_assert!(start.offset <= end.offset);

        let size = end.offset - start.offset;

        debug_assert!(start.slice.len() >= size);

        BufferAndLength {
            buf: Buffer {
                slice: &start.slice[0..size],
                offset: start.offset,
            },
            len: size,
        }
    }

    // PRE_COMMIT: Make this fallible?
    pub fn as_str(&self) -> &str {
        std::str::from_utf8(self.as_bytes()).expect("unable to convert BufferAndLength to &str")
    }

    // BufferAndLength::BufferAndLength(Buffer Buf, size_t Len) : Buf(Buf), Len(Len) {
    //     assert!(Len < 1ULL << 48);
    // }

    // TODO: Remove this method? Currently only used in token.rs for debug
    //       assertion.
    pub(crate) fn length(&self) -> usize {
        // return self.0.len();
        return self.len;
    }

    // pub fn end(&self) -> Buffer {
    //     return self.buf + self.len;
    // }

    pub fn as_bytes(&self) -> &[u8] {
        let BufferAndLength { buf, len } = *self;

        &buf.slice[..len]
    }

    pub(crate) fn byte_span(&self) -> ByteSpan {
        let BufferAndLength {
            buf: Buffer { slice: _, offset },
            len,
        } = *self;

        ByteSpan { offset, len }
    }

    #[allow(dead_code)]
    pub fn containsOnlyASCII(&self) -> bool {
        for c in self.as_bytes() {
            // TODO: Use is_ascii()
            if *c > 0x7f {
                return false;
            }
        }

        return true;
    }

    #[allow(dead_code)]
    pub fn containsTab(&self) -> bool {
        // TODO: Use .contains() for performance.
        for c in self.as_bytes() {
            if *c == 0x09 {
                return true;
            }
        }

        return false;
    }

    // TODO: Display
    // void BufferAndLength::print(std::ostream& s) const {
    //     s.write(reinterpret_cast<const char *>(Buf), length());
    // }
}

// bool IssuePtrCompare::operator()(const IssuePtr& L, const IssuePtr& R) const {

//     if (L->Src < R->Src) {
//         return true;
//     }

//     if (L->Tag < R->Tag) {
//         return true;
//     }

//     return false;
// }

impl PartialOrd for Issue {
    // TODO: Is this implementation of sorting for Issue's right?
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        if self.src < other.src {
            return Some(Ordering::Less);
        }

        // Sort in reverse order by ID. Why? Not sure, but empirically it
        // matches the C++ version.
        // TODO: What is the reason for doing this? This seems to result in
        //       otherwise equal issues being sorted in reverse alphabetical
        //       order by issue name.
        Some(self.tag.id.cmp(&other.tag.id).reverse())
    }
}

impl Ord for Issue {
    fn cmp(&self, other: &Self) -> Ordering {
        self.partial_cmp(other).unwrap_or(Ordering::Equal)
    }
}

impl PartialEq for Issue {
    fn eq(&self, other: &Self) -> bool {
        self.make_sym == other.make_sym
            && self.tag == other.tag
            && self.msg == other.msg
            && self.sev == other.sev
            && self.src == other.src
            && self.val == other.val
            && self.actions == other.actions
            && self.additional_descriptions == other.additional_descriptions
    }
}

impl Eq for Issue {}

impl Issue {
    pub fn new(
        make_sym: Symbol,
        tag: MyString,
        msg: String,
        sev: MyString,
        src: Source,
        val: std::os::raw::c_double,
        actions: Vec<CodeAction>,
        additional_descriptions: AdditionalDescriptionVector,
    ) -> Issue {
        let val = NotNan::new(val).expect("unable to construct Issue with NaN val");

        Issue {
            make_sym,
            tag,
            msg,
            sev,
            src,
            val,
            actions,
            additional_descriptions,
        }
    }

    // TODO: Display
    // fn print(std::ostream& s) const {

    //     MakeSym.print(s);
    //     s << "[";

    //     Tag.print(s);
    //     s << ", ";

    //     s << Msg;
    //     s << ", ";

    //     Sev.print(s);
    //     s << ", ";

    //     Src.print(s);
    //     s << ", ";

    //     s << Val;
    //     s << ", ";

    //     for (auto& A : Actions) {
    //         A->print(s);
    //         s << ", ";
    //     }

    //     for (auto& D : AdditionalDescriptions) {
    //         s << D.c_str();
    //         s << ", ";
    //     }

    //     s << "]";
    // }

    pub fn check(&self) -> bool {
        return self.sev != STRING_FATAL;
    }
}

pub fn SyntaxIssue(
    tag: MyString,
    msg: String,
    sev: MyString,
    src: Source,
    val: std::os::raw::c_double,
    actions: Vec<CodeAction>,
    additional_descriptions: AdditionalDescriptionVector,
) -> Issue {
    Issue::new(
        SYMBOL_CODEPARSER_SYNTAXISSUE,
        tag,
        msg,
        sev,
        src,
        val,
        actions,
        additional_descriptions,
    )
}

pub(crate) fn FormatIssue(
    tag: MyString,
    msg: String,
    sev: MyString,
    src: Source,
    val: std::os::raw::c_double,
    actions: Vec<CodeAction>,
    additional_descriptions: AdditionalDescriptionVector,
) -> Issue {
    Issue::new(
        SYMBOL_CODEPARSER_FORMATISSUE,
        tag,
        msg,
        sev,
        src,
        val,
        actions,
        additional_descriptions,
    )
}

pub fn EncodingIssue(
    tag: MyString,
    msg: String,
    sev: MyString,
    src: Source,
    val: std::os::raw::c_double,
    actions: Vec<CodeAction>,
    additional_descriptions: AdditionalDescriptionVector,
) -> Issue {
    Issue::new(
        SYMBOL_CODEPARSER_ENCODINGISSUE,
        tag,
        msg,
        sev,
        src,
        val,
        actions,
        additional_descriptions,
    )
}

// TODO: Display
// void ReplaceTextCodeAction::print(std::ostream& s) const {

//     SYMBOL_CODEPARSER_CODEACTION.print(s);
//     s << "[";

//     s << Label;
//     s << ", ";

//     SYMBOL_CODEPARSER_REPLACETEXT.print(s);
//     s << ", ";

//     Src.print(s);
//     s << ", ";

//     s << ReplacementText;

//     s << "]";
// }

// void InsertTextCodeAction::print(std::ostream& s) const {

//     SYMBOL_CODEPARSER_CODEACTION.print(s);
//     s << "[";

//     s << Label;
//     s << ", ";

//     SYMBOL_CODEPARSER_INSERTTEXT.print(s);
//     s << ", ";

//     Src.print(s);
//     s << ", ";

//     s << InsertionText;

//     s << "]";
// }

// void DeleteTextCodeAction::print(std::ostream& s) const {

//     SYMBOL_CODEPARSER_CODEACTION.print(s);
//     s << "[";

//     s << Label;
//     s << ", ";

//     SYMBOL_CODEPARSER_DELETETEXT.print(s);
//     s << ", ";

//     Src.print(s);

//     s << "]";
// }

//
// SourceLocation
//

impl SourceLocation {
    pub fn new(first: u32, second: u32) -> Self {
        Self { first, second }
    }
}

// bool operator==(SourceLocation a, SourceLocation b) {
//     return a.first == b.first && a.second == b.second;
// }

// bool operator<(SourceLocation a, SourceLocation b) {
//     if (a.first < b.first) {
//         return true;
//     }
//     if (a.first != b.first) {
//         return false;
//     }
//     if (a.second < b.second) {
//         return true;
//     }
//     return false;
// }

// TODO: Only used for assertions; make this a method
// TODO: Replace this with derive(PartialOrd)? I think they would have the same
//       semantics.
impl PartialOrd for SourceLocation {
    fn partial_cmp(&self, b: &Self) -> Option<std::cmp::Ordering> {
        let a = self;
        if a.first < b.first {
            return Some(Ordering::Less);
        }
        if a.first == b.first {
            return a.second.partial_cmp(&b.second);
        }
        if a.first != b.first {
            return Some(Ordering::Greater);
        }
        if a.second < b.second {
            return Some(Ordering::Less);
        }
        // return false;
        None
    }
}

// bool operator<=(SourceLocation a, SourceLocation b) {

//     if (a.first < b.first) {
//         return true;
//     }

//     if (a.first != b.first) {
//         return false;
//     }

//     if (a.second <= b.second) {
//         return true;
//     }

//     return false;
// }

impl SourceLocation {
    pub(crate) fn next(self) -> Self {
        if feature::COMPUTE_SOURCE {
            let SourceLocation { first, second } = self;

            SourceLocation::new(first, second + 1)
        } else {
            SourceLocation::new(0, 0)
        }
    }

    pub(crate) fn previous(self) -> Self {
        if feature::COMPUTE_SOURCE {
            let SourceLocation { first, second } = self;

            // TODO: What should this do if `second` is equal to 0? Can `second`
            //       even validly be equal to zero?
            debug_assert!(second >= 1);

            SourceLocation::new(first, second - 1)
        } else {
            SourceLocation::new(0, 0)
        }
    }
}
// SourceLocation SourceLocation::next() const {
// #if COMPUTE_SOURCE
//     return SourceLocation(first, second + 1);
// #else
//     return SourceLocation();
// #endif // COMPUTE_SOURCE
// }

// SourceLocation SourceLocation::previous() const {
// #if COMPUTE_SOURCE
//     return SourceLocation(first, second - 1);
// #else
//     return SourceLocation();
// #endif // COMPUTE_SOURCE
// }

// TODO: Display
// void SourceLocation::print(std::ostream& s) const {
//     s << first;
//     s << second;
// }

//
// For googletest
//
#[cfg(feature = "BUILD_TESTS")]
fn PrintTo(loc: &SourceLocation, s: &mut std::ostream) {
    loc.print(*s);
}

//
// Source
//

impl Source {
    pub fn new(start: SourceLocation, end: SourceLocation) -> Self {
        assert!(start <= end);

        Source { start, end }
    }

    pub fn from_location(only: SourceLocation) -> Self {
        Source {
            start: only,
            end: only,
        }
    }

    pub fn new_from_source(start: Source, end: Source) -> Self {
        assert!(start <= end);

        Source {
            start: start.start,
            end: end.end,
        }
    }

    // TODO: Display
    // void Source::print(std::ostream& s) const {
    //     Start.print(s);
    //     End.print(s);
    // }

    // PRE_COMMIT: Rename; this function will panic depending on the source
    //             convention used.
    pub(crate) fn len(&self) -> usize {
        let Source { start, end } = self;
        assert!(start.first == end.first);

        return end.second as usize - start.second as usize;
    }
}

// bool operator<(Source a, Source b) {
//     return a.Start < b.Start;
// }
// PRE_COMMIT: Only used for assertions; make this a method
impl PartialOrd for Source {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        self.start.partial_cmp(&other.start)
    }
}

//
// SourceCharacter
//

impl SourceCharacter {
    pub fn isAlphaOrDigit(&self) -> bool {
        match self {
            CodePoint::Char(c) => c.is_alphanumeric(),
            _ => false,
        }
    }

    pub fn isHex(&self) -> bool {
        match self {
            CodePoint::Char(c) => c.is_digit(16),
            _ => false,
        }
    }

    pub fn isOctal(self) -> bool {
        match self {
            CodePoint::Char(val) => '0' <= val && val <= '7',
            _ => false,
        }
    }

    pub fn isUpper(&self) -> bool {
        // return std::isupper(val);
        match self {
            CodePoint::Char(c) => c.is_ascii_uppercase(),
            _ => false,
        }
    }

    pub fn isEndOfFile(&self) -> bool {
        // let Self { val } = self;
        // return val == EOF;

        match self {
            CodePoint::EndOfFile => true,
            _ => false,
        }
    }

    #[allow(dead_code)]
    pub fn isNewline(&self) -> bool {
        // FIXME: What about SpecialCodePoint::CRLF, which is a kind of newline / whitespace?
        match self {
            CodePoint::Char('\n' | '\r') => true,
            _ => false,
        }
    }

    #[allow(dead_code)]
    pub fn isWhitespace(self) -> bool {
        // FIXME: What about SpecialCodePoint::CRLF, which is a kind of newline / whitespace?
        match self {
            CodePoint::Char(' ' | '\t' | ASCII_VTAB | ASCII_FORM_FEED) => true,
            _ => false,
        }
    }

    pub fn isMBNewline(self) -> bool {
        return LongNames::isMBNewline(self);
    }

    pub fn isMBWhitespace(self) -> bool {
        return LongNames::isMBWhitespace(self);
    }

    pub fn isMBUnsafeUTF8Sequence(&self) -> bool {
        match self {
            Unsafe1ByteUtf8Sequence | Unsafe2ByteUtf8Sequence | Unsafe3ByteUtf8Sequence => true,
            _ => false,
        }
    }

    pub fn graphicalString(&self) -> String {
        // Use '#' to force the "alternate" formatting, which for a
        // SourceCharacter is the graphical representation.
        format!("{:#}", self)

        // std::ostringstream String;
        // String << set_graphical << *this << clear_graphical;
        // return String.str();
    }

    pub fn safeAndGraphicalString(&self) -> String {
        format!("\"{}\" ({:#})", self, self)

        // std::ostringstream String;
        // String << "\"" << *this << "\" (" << set_graphical << *this << clear_graphical << ")";
        // return String.str();
    }
}

impl Display for SourceCharacter {
    // std::ostream& operator<<(std::ostream& stream, const SourceCharacter c) {
    fn fmt(&self, stream: &mut fmt::Formatter) -> fmt::Result {
        let c = *self;

        // let graphicalFlag = stream.iword(get_graphical_i()) == 1;
        let graphicalFlag = stream.alternate();

        if !graphicalFlag {
            let val = c;

            assert!(val != CodePoint::AssertFalse);
            assert!(val != CodePoint::EndOfFile);

            crate::byte_encoder::encodeBytes(stream, val)?;

            return Ok(());
        }

        //
        // Graphical
        //

        // From here on down, every character should be formatted 'graphically'
        // That property is propagated to the Display impl for WLCharacter's by
        // setting the '{:#}' formatting flag ("alternate"), which we rely on
        // WLCharacter's formatting implementation to check.
        debug_assert!(graphicalFlag);

        let val = c;

        match val {
            CodePoint::EndOfFile => {
                panic!()
            }
            Char('\x08') /* ASCII backspace */ => {

                return write!(stream, "{:#}", WLCharacter::new_with_escape(StringMeta_Backspace, EscapeStyle::Single))
            }
            Char(ASCII_FORM_FEED) => {

                return write!(stream, "{:#}", WLCharacter::new_with_escape(StringMeta_FormFeed, EscapeStyle::Single))
            }
                //
                // whitespace and newline characters
                //
            Char('\t') => {

                return write!(stream, "{:#}", WLCharacter::new_with_escape(StringMeta_Tab, EscapeStyle::Single))
            }
            Char('\n') => {

                return write!(stream, "{:#}", WLCharacter::new_with_escape(StringMeta_LineFeed, EscapeStyle::Single))
            }
            Char('\r') => {

                return write!(stream, "{:#}", WLCharacter::new_with_escape(StringMeta_CarriageReturn, EscapeStyle::Single))
            }
            StringMeta_LineFeed => {

                //
                // no single SourceCharacter for \<LF>
                //
                panic!()
            }
            StringMeta_CarriageReturn => {

                //
                // no single SourceCharacter for \<CR>
                //
                panic!()
            }
            StringMeta_Tab => {

                //
                // no single SourceCharacter for \t
                //
                panic!()
            }
            StringMeta_DoubleQuote => {

                //
                // Coming from \[RawDoubleQuote]
                //

                return write!(stream, "{:#}", WLCharacter::new_with_escape(StringMeta_DoubleQuote, EscapeStyle::Single))
            }
            StringMeta_Backslash => {

                //
                // Coming from \[RawBackslash]
                //

                return write!(stream, "{:#}", WLCharacter::new_with_escape(StringMeta_Backslash, EscapeStyle::Single))
            }
            CRLF => {

                write!(stream, "{:#}", WLCharacter::new_with_escape(StringMeta_CarriageReturn, EscapeStyle::Single))?;
                write!(stream, "{:#}", WLCharacter::new_with_escape(StringMeta_LineFeed, EscapeStyle::Single))?;

                return Ok(())
            }
            Char(CODEPOINT_LINEARSYNTAX_BANG) => {

                return write!(stream, "{:#}", WLCharacter::new_with_escape(CODEPOINT_LINEARSYNTAX_BANG, EscapeStyle::Single))
            }
            Char(CODEPOINT_LINEARSYNTAX_PERCENT) => {

                return write!(stream, "{:#}", WLCharacter::new_with_escape(CODEPOINT_LINEARSYNTAX_PERCENT, EscapeStyle::Single))
            }
            Char(CODEPOINT_LINEARSYNTAX_AMP) => {

                return write!(stream, "{:#}", WLCharacter::new_with_escape(CODEPOINT_LINEARSYNTAX_AMP, EscapeStyle::Single))
            }
            Char(CODEPOINT_LINEARSYNTAX_OPENPAREN) => {

                return write!(stream, "{:#}", WLCharacter::new_with_escape(CODEPOINT_LINEARSYNTAX_OPENPAREN, EscapeStyle::Single))
            }
            Char(CODEPOINT_LINEARSYNTAX_CLOSEPAREN) => {

                return write!(stream, "{:#}", WLCharacter::new_with_escape(CODEPOINT_LINEARSYNTAX_CLOSEPAREN, EscapeStyle::Single))
            }
            Char(CODEPOINT_LINEARSYNTAX_STAR) => {

                return write!(stream, "{:#}", WLCharacter::new_with_escape(CODEPOINT_LINEARSYNTAX_STAR, EscapeStyle::Single))
            }
            Char(CODEPOINT_LINEARSYNTAX_PLUS) => {

                return write!(stream, "{:#}", WLCharacter::new_with_escape(CODEPOINT_LINEARSYNTAX_PLUS, EscapeStyle::Single))
            }
            Char(CODEPOINT_LINEARSYNTAX_SLASH) => {

                return write!(stream, "{:#}", WLCharacter::new_with_escape(CODEPOINT_LINEARSYNTAX_SLASH, EscapeStyle::Single))
            }
            Char(CODEPOINT_LINEARSYNTAX_AT) => {

                return write!(stream, "{:#}", WLCharacter::new_with_escape(CODEPOINT_LINEARSYNTAX_AT, EscapeStyle::Single))
            }
            Char(CODEPOINT_LINEARSYNTAX_CARET) => {

                return write!(stream, "{:#}", WLCharacter::new_with_escape(CODEPOINT_LINEARSYNTAX_CARET, EscapeStyle::Single))
            }
            Char(CODEPOINT_LINEARSYNTAX_UNDER) => {

                return write!(stream, "{:#}", WLCharacter::new_with_escape(CODEPOINT_LINEARSYNTAX_UNDER, EscapeStyle::Single))
            }
            Char(CODEPOINT_LINEARSYNTAX_BACKTICK) => {

                return write!(stream, "{:#}", WLCharacter::new_with_escape(CODEPOINT_LINEARSYNTAX_BACKTICK, EscapeStyle::Single))
            }
            LinearSyntax_Space => {

                //
                // no single SourceCharacter for \<space>
                //
                panic!()
            }
                //
                // escape
                //
            Char('\x1b') => {

                return write!(stream, "{:#}", WLCharacter::new_with_escape(CODEPOINT_ESC, EscapeStyle::LongName))
            }
                //
                // C0 control characters
                //
                //
                // Skip BS, TAB, LF, FF, CR, and ESC. They are handled above
                //
            Char('\x00' | '\x01' | '\x02' | '\x03' | '\x04' | '\x05' | '\x06' | '\x07' |
            /*    \x08*/ /*    \x09*/ /*    \x0a*/ '\x0b' | /*    \x0c*/ /*    \x0d*/ '\x0e' | '\x0f' |
            '\x10' | '\x11' | '\x12' | '\x13' | '\x14' | '\x15' | '\x16' | '\x17' |
            '\x18' | '\x19' | '\x1a' | /*    \x1b*/ '\x1c' | '\x1d' | '\x1e' | '\x1f' |
                //
                // Make sure to include DEL
                //
            CODEPOINT_DEL )|
                //
                // C1 control characters
                //
            Char(
                '\u{80}' | '\u{81}' | '\u{82}' | '\u{83}' | '\u{84}' | '\u{85}' | '\u{86}' | '\u{87}' |
                '\u{88}' | '\u{89}' | '\u{8a}' | '\u{8b}' | '\u{8c}' | '\u{8d}' | '\u{8e}' | '\u{8f}' |
                '\u{90}' | '\u{91}' | '\u{92}' | '\u{93}' | '\u{94}' | '\u{95}' | '\u{96}' | '\u{97}' |
                '\u{98}' | '\u{99}' | '\u{9a}' | '\u{9b}' | '\u{9c}' | '\u{9d}' | '\u{9e}' | '\u{9f}'
            ) => {
                return write!(stream, "{:#}", WLCharacter::new_with_escape(val, EscapeStyle::Hex2))
            }
            Char(_) => (),
            _ => panic!("unable to format special char: {val:?}"),
        }

        let val_i32 = val.as_i32();

        assert!(val_i32 >= 0);

        if val_i32 > 0xffff {
            if code_point_has_long_name(val) {
                //
                // Use LongName if available
                //

                return write!(
                    stream,
                    "{:#}",
                    WLCharacter::new_with_escape(val, EscapeStyle::LongName)
                );
            }

            return write!(
                stream,
                "{:#}",
                WLCharacter::new_with_escape(val, EscapeStyle::Hex6)
            );
        }

        if val_i32 > 0xff {
            if code_point_has_long_name(val) {
                //
                // Use LongName if available
                //

                return write!(
                    stream,
                    "{:#}",
                    WLCharacter::new_with_escape(val, EscapeStyle::LongName)
                );
            }

            return write!(
                stream,
                "{:#}",
                WLCharacter::new_with_escape(val, EscapeStyle::Hex4)
            );
        }

        if val_i32 > 0x7f {
            if code_point_has_long_name(val) {
                //
                // Use LongName if available
                //

                return write!(
                    stream,
                    "{:#}",
                    WLCharacter::new_with_escape(val, EscapeStyle::LongName)
                );
            }

            return write!(
                stream,
                "{:#}",
                WLCharacter::new_with_escape(val, EscapeStyle::Hex2)
            );
        }

        //
        // ASCII is untouched
        // Do not use CodePointToLongNameMap to find Raw names
        //

        crate::byte_encoder::encodeBytes(stream, val)?;

        return Ok(());
    }
}
