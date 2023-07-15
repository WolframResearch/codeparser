use std::{
    cmp::Ordering,
    fmt::{self, Debug, Display},
    num::NonZeroU32,
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
    tokenizer::{ASCII_FORM_FEED, ASCII_VTAB},
    wl_character::{EscapeStyle, WLCharacter},
};

use wolfram_expr::Expr;

//==========================================================
// Slices of source code: Buffer and BufferAndLength
//==========================================================

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

#[derive(Debug, Copy, Clone, PartialEq)]
pub(crate) struct BufferAndLength<'i> {
    pub buf: Buffer<'i>,
}

#[cfg(target_pointer_width = "64")]
const _: () = assert!(std::mem::size_of::<BufferAndLength>() == 24);

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
    pub(crate) fn end(&self) -> usize {
        let ByteSpan { offset, len } = *self;

        return offset + len;
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
        let Buffer { offset, slice } = buf;

        let slice = &slice[..len];

        BufferAndLength {
            buf: Buffer { offset, slice },
        }
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
    #[allow(dead_code)]
    pub(crate) fn length(&self) -> usize {
        let BufferAndLength { buf } = self;

        return buf.slice.len();
    }

    // pub fn end(&self) -> Buffer {
    //     return self.buf + self.len;
    // }

    pub fn as_bytes(&self) -> &[u8] {
        let BufferAndLength { buf } = *self;

        buf.slice
    }

    pub(crate) fn byte_span(&self) -> ByteSpan {
        let BufferAndLength {
            buf: Buffer { slice, offset },
        } = *self;

        ByteSpan {
            offset,
            len: slice.len(),
        }
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


//==========================================================
// Source character reading behavior
//==========================================================

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


//==========================================================
// Locating source code
//==========================================================

/// A single character of source code
///
/// The text `\[Alpha]` would be 8 separate SourceCharacters
// TODO(cleanup): remove CodePoint, just have SourceCharacter?
pub type SourceCharacter = CodePoint;

const _: () = assert!(std::mem::size_of::<SourceCharacter>() == 4);


#[derive(Debug, Copy, Clone, PartialEq)]
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
pub enum SourceLocation {
    LineColumn(LineColumn),
    CharacterIndex(u32),
}

// NOTE:
//     Keeping the size of the SourceLocation enum limited to 8 bytes depends
//     on using NonZeroU32, so that the 0 value can be used as the enum
//     discriminant.
//
//     See also: https://github.com/rust-lang/rust/pull/94075
const _: () = assert!(std::mem::size_of::<SourceLocation>() == 8);

// For LineContinuations and EmbeddedNewlines
//
// bool operator<(SourceLocation a, SourceLocation b);
// bool operator<=(SourceLocation a, SourceLocation b);

#[derive(Debug, Clone, PartialEq, Hash)]
pub enum GeneralSource {
    String(Source),
    /// Box structure position.
    BoxPosition(Vec<usize>),

    /// `After[{..}]`
    ///
    /// Used to indicate the position of fake implicit Null or Times tokens, or
    /// expected operand error tokens, that come after a specific position in
    /// the source.
    // TODO: Parse this into a strongly typed value
    After(Expr),
}

#[derive(Copy, Clone, PartialEq, Hash)]
pub struct Source {
    pub(crate) start: SourceLocation,
    pub(crate) end: SourceLocation,
}

const _: () = assert!(std::mem::size_of::<Source>() == 16);

#[derive(Debug, Copy, Clone, PartialEq)]
pub enum StringSourceKind {
    LineColumnSpan(LineColumnSpan),
    CharacterSpan(CharacterSpan),
    /// `<||>`
    Unknown,
}

/// A span of Wolfram Language input by character index.
///
/// This range starts indexing at 1, and is exclusive.
///
/// `CharacterSpan(start, end)`
#[derive(Debug, Copy, Clone, PartialEq)]
pub struct CharacterSpan(pub u32, pub u32);

/// `LineColumn(line, column)`
#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub struct LineColumn(pub NonZeroU32, pub u32);

#[derive(Debug, Copy, Clone, PartialEq)]
pub struct LineColumnSpan {
    pub start: LineColumn,
    pub end: LineColumn,
}

const _: () = assert!(std::mem::size_of::<LineColumnSpan>() == 16);

//======================================
// Source types formatting impls
//======================================

impl Display for SourceLocation {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            SourceLocation::LineColumn(LineColumn(line, column)) => write!(f, "{line}:{column}"),
            SourceLocation::CharacterIndex(index) => write!(f, "{index}"),
        }
    }
}

impl Debug for SourceLocation {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self)
    }
}

impl Display for Source {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self.kind() {
            StringSourceKind::CharacterSpan(CharacterSpan(start, end)) => {
                write!(f, "{}..{}", start, end)
            },
            StringSourceKind::LineColumnSpan(LineColumnSpan { start, end }) => {
                write!(f, "{start:?}-{end:?}")
            },
            StringSourceKind::Unknown => {
                // TODO: Better formatting for this? "?"
                write!(f, "Source unknown")
            },
        }
    }
}

impl Debug for Source {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self)
    }
}

//======================================
// Source types inherent impls
//======================================

impl SourceLocation {
    #[doc(hidden)]
    pub fn new(first: u32, second: u32) -> Self {
        if let Some(line) = NonZeroU32::new(first) {
            SourceLocation::LineColumn(LineColumn(line, second))
        } else {
            debug_assert!(first == 0);

            SourceLocation::CharacterIndex(second)
        }
    }

    pub(crate) fn next(self) -> Self {
        if feature::COMPUTE_SOURCE {
            match self {
                SourceLocation::LineColumn(LineColumn(line, column)) => {
                    SourceLocation::LineColumn(LineColumn(line, column + 1))
                },
                SourceLocation::CharacterIndex(index) => SourceLocation::CharacterIndex(index + 1),
            }
        } else {
            SourceLocation::new(0, 0)
        }
    }

    pub(crate) fn previous(self) -> Self {
        if feature::COMPUTE_SOURCE {
            // TODO: What should this do if `second` is equal to 0? Can `second`
            //       even validly be equal to zero?
            match self {
                SourceLocation::LineColumn(LineColumn(line, column)) => {
                    debug_assert!(column >= 1);

                    SourceLocation::LineColumn(LineColumn(line, column - 1))
                },
                SourceLocation::CharacterIndex(index) => {
                    debug_assert!(index >= 1);

                    SourceLocation::CharacterIndex(index - 1)
                },
            }
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


impl CharacterSpan {
    pub(crate) fn tuple(self) -> (u32, u32) {
        let CharacterSpan(start, end) = self;

        (start, end)
    }

    pub(crate) fn to_rust_range(&self) -> std::ops::Range<usize> {
        let CharacterSpan(start, end) = *self;

        debug_assert!(start > 0);

        let start = usize::try_from(start).unwrap();
        let end = usize::try_from(end).unwrap();

        // -1 because CharacterSpan is 1-indexed (like WL)
        let start = start - 1;
        let end = end - 1;

        start..end
    }
}

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

    #[doc(hidden)]
    pub fn from_character_span(start: u32, end: u32) -> Self {
        Source {
            start: SourceLocation::CharacterIndex(start),
            end: SourceLocation::CharacterIndex(end),
        }
    }

    pub fn new_from_source(start: Source, end: Source) -> Self {
        assert!(start <= end);

        Source {
            start: start.start,
            end: end.end,
        }
    }

    pub fn unknown() -> Self {
        // Use incompatible values for `first`.
        Source {
            start: SourceLocation::CharacterIndex(0),
            end: SourceLocation::LineColumn(LineColumn(NonZeroU32::MIN, 0)),
        }
    }

    /// Get the start and end `SourceLocation`s of this source span.
    pub fn start_end(&self) -> (SourceLocation, SourceLocation) {
        let Source { start, end } = *self;

        (start, end)
    }

    pub(crate) fn character_span(&self) -> CharacterSpan {
        match self.kind() {
            StringSourceKind::CharacterSpan(range) => range,
            other => {
                panic!("Source::character_span(): Source is not a character span: {other:?}")
            },
        }
    }

    pub(crate) fn line_column_span(&self) -> LineColumnSpan {
        match self.kind() {
            StringSourceKind::LineColumnSpan(span) => span,
            other => {
                panic!("Source::line_column_span(): Source is not a line/column span: {other:?}")
            },
        }
    }

    pub fn kind(self) -> StringSourceKind {
        let Source { start, end } = self;

        match (start, end) {
            (
                SourceLocation::CharacterIndex(start_char),
                SourceLocation::CharacterIndex(end_char),
            ) => StringSourceKind::CharacterSpan(CharacterSpan(start_char, end_char)),
            (
                SourceLocation::LineColumn(LineColumn(start_line, start_column)),
                SourceLocation::LineColumn(LineColumn(end_line, end_column)),
            ) => StringSourceKind::LineColumnSpan(LineColumnSpan {
                start: LineColumn(start_line, start_column),
                end: LineColumn(end_line, end_column),
            }),
            _ => StringSourceKind::Unknown,
        }
    }

    #[allow(dead_code)]
    pub(crate) fn column_width(&self) -> usize {
        let (start_column, end_column) = match self.kind() {
            StringSourceKind::LineColumnSpan(LineColumnSpan { start, end }) => {
                debug_assert!(
                    start.line() == end.line(),
                    "StringSourceKind::column_width(): source locations are on different lines"
                );

                (start.column(), end.column())
            },
            other => panic!(
                "StringSourceKind::column_width(): Source is not a line column span: {other:?}"
            ),
        };

        return end_column as usize - start_column as usize;
    }
}

impl LineColumn {
    pub fn line(self) -> NonZeroU32 {
        let LineColumn(line, _) = self;

        line
    }

    pub fn column(self) -> u32 {
        let LineColumn(_, column) = self;

        column
    }
}

impl LineColumnSpan {
    /// Check if a [`LineColumn`] location is inside of this [`LineColumnSpan`]
    /// span.
    ///
    /// ```
    /// use wolfram_parser::{source::LineColumn, test_utils::src};
    ///
    /// assert!(src!(1:3-2:0).contains(src!(1:4)));
    ///
    /// assert!(!src!(1:3-2:0).contains(src!(2:4)));
    ///
    /// assert!(src!(1:3-2:0).contains(src!(1:4)));
    ///
    /// assert!(!src!(1:3-2:0).contains(src!(2:4)));
    /// ```
    pub fn contains(self, cursor: LineColumn) -> bool {
        let LineColumnSpan {
            start: LineColumn(srcLine1, srcCol1),
            end: LineColumn(srcLine2, srcCol2),
        } = self;

        let LineColumn(cursorLine, cursorCol) = cursor;

        let srcLine1: u32 = srcLine1.get();
        let srcLine2: u32 = srcLine2.get();
        let cursorLine: u32 = cursorLine.get();

        // not in-between the lines of the spec, so no
        if !(srcLine1 <= cursorLine && cursorLine <= srcLine2) {
            return false;
        }

        // everything is on 1 line, so now test cols
        if cursorLine == srcLine1 && srcLine1 == srcLine2 {
            return srcCol1 <= cursorCol && cursorCol <= srcCol2;
        }

        // on srcLine1, so test that cursor comes after srcCol1
        if cursorLine == srcLine1 {
            return srcCol1 <= cursorCol;
        }

        // on srcLine2, so test that cursor comes before srcCol2
        if cursorLine == srcLine2 {
            return cursorCol <= srcCol2;
        }

        // exclusively in-between start and end, so yes
        true
    }

    /// Check if this [`LineColumnSpan`] partially or completely overlaps with
    /// another [`LineColumnSpan`].
    ///
    /// ```
    /// use wolfram_parser::{Source, SourceLocation, test_utils::src};
    ///
    /// // Complete overlap.
    /// assert!(src!(1:1-1:5).overlaps(src!(1:2-1:4)));
    ///
    /// // Partial overlap.
    /// assert!(src!(1:1-1:5).overlaps(src!(1:4-1:8))); // Single line
    /// assert!(src!(1:1-2:5).overlaps(src!(2:4-3:8))); // Multiline
    ///
    /// assert!(!src!(1:1-1:5).overlaps(src!(2:1-2:5)));
    /// ```
    pub fn overlaps(&self, cursor: LineColumnSpan) -> bool {
        let LineColumnSpan { start, end } = cursor;

        self.contains(start) || self.contains(end)
    }
}

impl GeneralSource {
    pub fn unknown() -> Self {
        GeneralSource::String(Source::unknown())
    }

    pub fn is_unknown(&self) -> bool {
        match self {
            GeneralSource::String(source) => match source.kind() {
                StringSourceKind::Unknown => true,
                _ => false,
            },
            GeneralSource::BoxPosition(_) => false,
            GeneralSource::After(_) => false,
        }
    }
}

//======================================
// Source type conversion impls
//======================================

impl From<LineColumn> for SourceLocation {
    fn from(lc: LineColumn) -> Self {
        SourceLocation::LineColumn(lc)
    }
}

impl From<LineColumnSpan> for Source {
    fn from(value: LineColumnSpan) -> Self {
        let LineColumnSpan { start, end } = value;

        Source {
            start: SourceLocation::LineColumn(start),
            end: SourceLocation::LineColumn(end),
        }
    }
}

impl From<CharacterSpan> for Source {
    fn from(value: CharacterSpan) -> Source {
        let CharacterSpan(start, end) = value;

        Source {
            start: SourceLocation::CharacterIndex(start),
            end: SourceLocation::CharacterIndex(end),
        }
    }
}


//======================================
// Source types comparision impls
//======================================

// TODO: Only used for assertions; make this a method
// TODO: Replace this with derive(PartialOrd)? I think they would have the same
//       semantics.
impl PartialOrd for SourceLocation {
    fn partial_cmp(&self, b: &Self) -> Option<std::cmp::Ordering> {
        let a = self;

        match (a, b) {
            (
                SourceLocation::LineColumn(LineColumn(a_line, a_column)),
                SourceLocation::LineColumn(LineColumn(b_line, b_col)),
            ) => (a_line, a_column).partial_cmp(&(b_line, b_col)),
            (SourceLocation::CharacterIndex(a_index), SourceLocation::CharacterIndex(b_index)) => {
                a_index.partial_cmp(b_index)
            },
            (SourceLocation::LineColumn { .. }, SourceLocation::CharacterIndex(_)) => None,
            (SourceLocation::CharacterIndex(_), SourceLocation::LineColumn { .. }) => None,
        }
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

impl PartialOrd for GeneralSource {
    fn partial_cmp(&self, other: &GeneralSource) -> Option<Ordering> {
        match (self, other) {
            (GeneralSource::String(src1), GeneralSource::String(src2)) => src1.partial_cmp(src2),
            _ => None,
        }
    }
}


//==========================================================
// Source characters
//==========================================================

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
