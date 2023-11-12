//! Types representing locations in the input being processed.

use std::{
    cmp::Ordering,
    fmt::{self, Debug, Display},
    num::NonZeroU32,
};

use crate::{
    feature,
    long_names::{self as LongNames, codepoint_has_longname},
    read::{
        code_point::{
            CodePoint::{self, Char, *},
            CODEPOINT_DEL, CODEPOINT_ESC, *,
        },
        Escape, WLCharacter,
    },
    tokenize::tokenizer::{ASCII_FORM_FEED, ASCII_VTAB},
    utils::{most_slice, non_zero_u32_incr, CommaSeparated, CommaTerminated},
};

//==========================================================
// Slices of source code: Buffer and BufferAndLength
//==========================================================

/// This type represents a subslice of the complete input byte sequence.
///
/// Note that the length of this slice is NOT guaranteed to correspond to any
/// semantically meaningful span of the input. (For that, see [`BufferAndLength`]).
pub(crate) type Buffer<'i> = &'i [u8];

#[derive(Debug, Copy, Clone, PartialEq)]
pub(crate) struct BufferAndLength<'i> {
    pub buf: Buffer<'i>,
}

#[cfg(target_pointer_width = "64")]
const _: () = assert!(std::mem::size_of::<BufferAndLength>() == 16);

/* FIXME: Re-enable this assertion, or a variation of it.
const _: () = assert!(
    (SIZEOF_VOID_P == 8 && std::mem::size_of::<BufferAndLength>() == 16) || (SIZEOF_VOID_P == 4),
    "Check your assumptions"
);
*/

impl<'i> BufferAndLength<'i> {
    pub fn from_buffer_with_len(slice: Buffer<'i>, len: usize) -> Self {
        let slice = &slice[..len];

        BufferAndLength { buf: slice }
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
    pub fn between<'s, 'e>(
        start: Buffer<'s>,
        end: Buffer<'e>,
    ) -> BufferAndLength<'s> {
        let start_addr = start.as_ptr() as usize;
        let end_addr = end.as_ptr() as usize;
        debug_assert!(
            start_addr <= end_addr,
            "start: {start:?} @ {start_addr}     end: {end:?} @ {end_addr}"
        );

        let size = end_addr - start_addr;

        debug_assert!(start.len() >= size);

        BufferAndLength {
            buf: &start[0..size],
        }
    }

    // PRE_COMMIT: Make this fallible?
    pub fn as_str(&self) -> &str {
        std::str::from_utf8(self.as_bytes())
            .expect("unable to convert BufferAndLength to &str")
    }

    // BufferAndLength::BufferAndLength(Buffer Buf, size_t Len) : Buf(Buf), Len(Len) {
    //     assert!(Len < 1ULL << 48);
    // }

    // TODO: Remove this method? Currently only used in token.rs for debug
    //       assertion.
    #[allow(dead_code)]
    pub(crate) fn length(&self) -> usize {
        let BufferAndLength { buf } = self;

        return buf.len();
    }

    // pub fn end(&self) -> Buffer {
    //     return self.buf + self.len;
    // }

    pub fn as_bytes(&self) -> &[u8] {
        let BufferAndLength { buf } = *self;

        buf
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

#[repr(transparent)]
#[derive(Copy, Clone)]
pub(crate) struct NextPolicy(u8);

impl NextPolicy {
    #[allow(dead_code)]
    const fn bits(&self) -> u8 {
        let NextPolicy(bits) = *self;

        bits
    }

    pub(crate) fn from_bits(bits: u8) -> Self {
        NextPolicy(bits)
    }

    /// Returns true if all the bits in `other` are set in `self`.
    #[inline(always)]
    pub(crate) fn contains(&self, other: NextPolicy) -> bool {
        let NextPolicy(self_bits) = *self;
        let NextPolicy(other_bits) = other;

        self_bits & other_bits == other_bits
    }

    #[inline(always)]
    pub(crate) fn without(self, other: NextPolicy) -> NextPolicy {
        let NextPolicy(self_bits) = self;
        let NextPolicy(other_bits) = other;

        NextPolicy(self_bits & !other_bits)
    }

    #[inline(always)]
    pub(crate) fn remove(&mut self, other: NextPolicy) {
        *self = self.without(other);
    }
}

impl std::ops::BitOr for NextPolicy {
    type Output = Self;

    fn bitor(self, rhs: Self) -> Self::Output {
        let NextPolicy(self_bits) = self;
        let NextPolicy(rhs_bits) = rhs;

        NextPolicy(self_bits | rhs_bits)
    }
}

impl std::ops::BitOrAssign for NextPolicy {
    fn bitor_assign(&mut self, rhs: Self) {
        let NextPolicy(self_bits) = self;
        let NextPolicy(rhs_bits) = rhs;

        *self_bits |= rhs_bits;
    }
}

pub(crate) mod NextPolicyBits {
    use super::NextPolicy;

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
    pub(crate) const ENABLE_CHARACTER_DECODING_ISSUES: NextPolicy =
        NextPolicy(0x01);

    //
    /// when inside Tokenizer_currentWLCharacter, then do not track line continuations
    /// since Tokenizer_currentWLCharacter is implemented as Tokenizer_nextWLCharacter that is then reset, there should be no side-effects
    //
    pub(crate) const TRACK_LC: NextPolicy = NextPolicy(0x02);

    //
    /// Used by Tokenizer
    //
    pub(crate) const RETURN_TOPLEVELNEWLINE: NextPolicy = NextPolicy(0x04);

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
    pub(crate) const STRING_OR_COMMENT: NextPolicy = NextPolicy(0x08);

    //
    /// If inside #, then give syntax warnings for #"123" and #a`b syntax (which is undocumented syntax)
    ///
    /// But obviously "123" and a`b are fine outside of #
    ///
    /// Also return symbols as strings, e.g., the  abc  in  #abc  is a string
    ///
    /// Used by Tokenizer
    //
    pub(crate) const TAGSLOT_BEHAVIOR_FOR_STRINGS: NextPolicy =
        NextPolicy(0x10);

    //
    /// When tokenizing numbers, return immediately when an integer has been tokenized
    ///
    /// This is used when parsing Slot, SlotSequence, and Out
    ///
    /// For example, we must consider  `#2.c`  to be `Slot[2] . c`  and NOT  `Slot[1] 2. c`
    //
    pub(crate) const INTEGER_SHORT_CIRCUIT: NextPolicy = NextPolicy(0x20);

    //
    // With input  "\\[Alpa]"  , then report \[Alpa] as unrecognized, even though this is valid syntax
    //
    pub(crate) const SCAN_FOR_UNRECOGNIZEDLONGNAMES: NextPolicy =
        NextPolicy(0x40);
}

pub(crate) use self::NextPolicyBits::*;

const _: () = assert!(
    RETURN_TOPLEVELNEWLINE.bits() == 0x04,
    "Needs to be 0b100, for easy or-ing of TOKEN_INTERNALNEWLINE to TOKEN_TOPLEVELNEWLINE"
);

use NextPolicyBits::{
    ENABLE_CHARACTER_DECODING_ISSUES, INTEGER_SHORT_CIRCUIT,
    RETURN_TOPLEVELNEWLINE, TAGSLOT_BEHAVIOR_FOR_STRINGS, TRACK_LC,
};

pub(crate) const TOPLEVEL: NextPolicy = NextPolicy(
    ENABLE_CHARACTER_DECODING_ISSUES.0 | RETURN_TOPLEVELNEWLINE.0 | TRACK_LC.0,
);

#[allow(dead_code)] // TODO(cleanup): Is it meaningful that this is unused?
pub(crate) const INSIDE_SYMBOL: NextPolicy =
    NextPolicy(ENABLE_CHARACTER_DECODING_ISSUES.0 | TRACK_LC.0);

pub(crate) const INSIDE_STRINGIFY_AS_TAG: NextPolicy = NextPolicy(
    ENABLE_CHARACTER_DECODING_ISSUES.0
        | TAGSLOT_BEHAVIOR_FOR_STRINGS.0
        | TRACK_LC.0,
);
pub(crate) const INSIDE_STRINGIFY_AS_FILE: NextPolicy = RETURN_TOPLEVELNEWLINE;

pub(crate) const INSIDE_SLOT: NextPolicy = NextPolicy(
    TAGSLOT_BEHAVIOR_FOR_STRINGS.0 | INTEGER_SHORT_CIRCUIT.0 | TRACK_LC.0,
);

pub(crate) const INSIDE_SLOTSEQUENCE: NextPolicy = NextPolicy(
    ENABLE_CHARACTER_DECODING_ISSUES.0 | INTEGER_SHORT_CIRCUIT.0 | TRACK_LC.0,
);

pub(crate) const INSIDE_OUT: NextPolicy = NextPolicy(
    ENABLE_CHARACTER_DECODING_ISSUES.0 | INTEGER_SHORT_CIRCUIT.0 | TRACK_LC.0,
);


//==========================================================
// Locating source code
//==========================================================

/// A single character of source code
///
/// The text `\[Alpha]` would be 8 separate SourceCharacters
// TODO(cleanup): remove CodePoint, just have SourceCharacter?
pub type SourceCharacter = CodePoint;

const _: () = assert!(std::mem::size_of::<SourceCharacter>() == 4);


/// Whether source locations are tracked by line:column number or by character
/// index.
#[derive(Debug, Copy, Clone, PartialEq)]
pub enum SourceConvention {
    /// Handle next (non-newline) [`Location`] by incrementing column.
    ///
    /// Handle next newline by incrementing line.
    LineColumn = 0,
    // TODO: Clarify, is this the index by unicode character(?), or the byte
    //  offset, or something else?
    /// Handle next (non-newline) [`Location`] by incrementing index.
    ///
    /// Handle next newline by incrementing index.
    CharacterIndex = 1,
}

// TODO: Should this be a part of the public API as a constant value, or
//       something else 'symbolic'? E.g. prehaps this shouldn't be a
//       required parameter of ParserSession::new().
pub const DEFAULT_TAB_WIDTH: u32 = 4;

// For LineContinuations and EmbeddedNewlines
//
// bool operator<(SourceLocation a, SourceLocation b);
// bool operator<=(SourceLocation a, SourceLocation b);

/// Specifies a region of source code in an input string or box structure.
#[derive(Debug, Clone, PartialEq, Hash)]
pub enum Source {
    /// Text span.
    ///
    /// Source was a string or file containing `InputForm` source code.
    Span(Span),

    /// Box structure position.
    ///
    /// Source was `StandardForm` boxes.
    Box(BoxPosition),

    /// `<||>`
    Unknown,
}

/// Specifies a region ("span") of source code between a start and end location.
///
/// There are two different conventions ([`SourceConvention`]) for specifying a
/// location in source code:
///
/// 1. A line-column position (represented by [`LineColumn`])
/// 2. A character position (represented by a [`u32`] count)
///
/// A [`Span`] can specify a source span using either of these conventions.
///
/// Match over [`Span::kind()`] to access the source span position information
/// stored in a [`Span`] instance.
#[derive(Copy, Clone, PartialEq, Hash)]
pub struct Span {
    start: Location,
    end: Location,
}

const _: () = assert!(std::mem::size_of::<Span>() == 16);

/// A location in the source that can be the start or end of a [`Span`].
#[derive(Copy, Clone, PartialEq, Eq, Hash)]
pub enum Location {
    LineColumn(LineColumn),
    CharacterIndex(u32),
}

// NOTE:
//     Keeping the size of the Location enum limited to 8 bytes depends
//     on using NonZeroU32, so that the 0 value can be used as the enum
//     discriminant.
//
//     See also: https://github.com/rust-lang/rust/pull/94075
const _: () = assert!(std::mem::size_of::<Location>() == 8);


/// Location data from a [`Span`].
#[derive(Debug, Copy, Clone, PartialEq)]
pub enum SpanKind {
    LineColumnSpan(LineColumnSpan),
    CharacterSpan(CharacterSpan),
}

/// A span of input by character start and end point.
///
/// This range starts indexing at 1, and is exclusive.
///
/// `CharacterSpan(start, end)`
#[derive(Debug, Copy, Clone, PartialEq)]
pub struct CharacterSpan(pub u32, pub u32);

/// A position in the input specified by it's line and column.
///
/// `LineColumn(line, column)`
///
/// # Ordering
///
/// Ordering of [`LineColumn`] positions is based on their logical position in
/// the input:
///
/// ```
/// use wolfram_parser::macros::src;
///
/// assert!(src!(1:1) < src!(1:2));
/// assert!(src!(1:1) < src!(2:1));
///
/// assert!(src!(2:1) > src!(1:7));
/// ```
#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub struct LineColumn(
    /// The line.
    pub NonZeroU32,
    /// The column.
    pub NonZeroU32,
);

/// A span of input by line-column start and end point.
#[derive(Debug, Copy, Clone, PartialEq)]
pub struct LineColumnSpan {
    pub start: LineColumn,
    pub end: LineColumn,
}

const _: () = assert!(std::mem::size_of::<LineColumnSpan>() == 16);

/// A position in a a StandardForm box expression.
///
/// The [`src!`][crate::macros::src] can be used to conveniently construct
/// [`BoxPosition`] values.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum BoxPosition {
    /// `{1, 2, 3}`
    At(Vec<usize>),
    /// `{1, 2, 3 ;; 5}`
    Spanning {
        index: Vec<usize>,
        span: (usize, usize),
    },
    /// `Before[{..}]`
    Before(Vec<usize>),
    /// `After[{..}]`
    ///
    /// Used to indicate the position of fake implicit Null or Times tokens, or
    /// expected operand error tokens, that come after a specific position in
    /// the source.
    After(Vec<usize>),
}

//======================================
// Source types formatting impls
//======================================

impl Display for Location {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Location::LineColumn(LineColumn(line, column)) => {
                write!(f, "{line}:{column}")
            },
            Location::CharacterIndex(index) => write!(f, "{index}"),
        }
    }
}

impl Debug for Location {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self)
    }
}

impl Display for Span {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self.kind() {
            SpanKind::CharacterSpan(CharacterSpan(start, end)) => {
                write!(f, "{}..{}", start, end)
            },
            SpanKind::LineColumnSpan(span) => {
                write!(f, "{span}")
            },
        }
    }
}

impl Display for LineColumnSpan {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let LineColumnSpan { start, end } = self;

        if start.line() == end.line() {
            // a:b-c
            write!(f, "{start}-{}", end.column())
        } else {
            // a:b-c:d
            write!(f, "{start}-{end}")
        }
    }
}

impl Display for LineColumn {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let LineColumn(line, column) = self;

        write!(f, "{line}:{column}")
    }
}

impl Display for Source {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Source::Unknown => write!(f, "<unknown>"),
            Source::Span(span) => write!(f, "{span}"),
            Source::Box(box_pos) => write!(f, "{box_pos}"),
        }
    }
}

impl Debug for Span {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        if f.alternate() {
            write!(f, "src!({})", self)
        } else {
            write!(f, "{}", self)
        }
    }
}

/// Format [`BoxPosition`] as `{...}` lists.
///
/// # Examples
///
/// ```
/// use wolfram_parser::source::BoxPosition;
///
/// # assert_eq!(BoxPosition::At(vec![]).to_string(), "{}");
/// assert_eq!(BoxPosition::At(vec![1, 2, 3]).to_string(), "{1, 2, 3}");
///
/// assert_eq!(
///     BoxPosition::Spanning { index: vec![1, 1] , span: (1, 3) }.to_string(),
///     "{1, 1, 1 ;; 3}"
/// );
/// ```
impl Display for BoxPosition {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            BoxPosition::At(position) => {
                write!(f, "{{")?;
                write!(f, "{}", CommaSeparated(position))?;
                write!(f, "}}")
            },
            BoxPosition::Spanning {
                index,
                span: (span_start, span_end),
            } => {
                write!(f, "{{")?;
                write!(f, "{}", CommaTerminated(index))?;
                write!(f, "{span_start} ;; {span_end}")?;
                write!(f, "}}")
            },
            BoxPosition::Before(position) => {
                write!(f, "Before[{{{}}}]", CommaSeparated(position))
            },
            BoxPosition::After(position) => {
                write!(f, "After[{{{}}}]", CommaSeparated(position))
            },
        }
    }
}

//======================================
// Source types inherent impls
//======================================

impl Location {
    pub(crate) fn next(self) -> Self {
        if feature::COMPUTE_SOURCE {
            match self {
                Location::LineColumn(LineColumn(line, column)) => {
                    Location::LineColumn(LineColumn(
                        line,
                        // column + 1
                        non_zero_u32_incr(column),
                    ))
                },
                Location::CharacterIndex(index) => {
                    Location::CharacterIndex(index + 1)
                },
            }
        } else {
            Location::CharacterIndex(0)
        }
    }

    pub(crate) fn previous(self) -> Self {
        if feature::COMPUTE_SOURCE {
            // TODO: What should this do if `second` is equal to 0? Can `second`
            //       even validly be equal to zero?
            match self {
                Location::LineColumn(LineColumn(line, column)) => {
                    let previous = column.get() - 1;
                    let previous = NonZeroU32::new(previous)
                        .expect("previous column is 0");
                    Location::LineColumn(LineColumn(line, previous))
                },
                Location::CharacterIndex(index) => {
                    debug_assert!(index >= 1);

                    Location::CharacterIndex(index - 1)
                },
            }
        } else {
            Location::CharacterIndex(0)
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
    /// Returns `true` if `other` is completely contained inside `self`.
    #[allow(dead_code)]
    pub(crate) fn contains_span(&self, other: CharacterSpan) -> bool {
        crate::utils::is_interval_member(self.tuple(), other.tuple())
    }

    pub(crate) fn intersects(&self, other: CharacterSpan) -> bool {
        crate::utils::intersection(self.tuple(), other.tuple()).is_some()
    }

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

impl Span {
    /// Construct a new span between locations given by their line and column
    /// number.
    pub fn line_column(start: LineColumn, end: LineColumn) -> Self {
        assert!(
            start <= end,
            "Span::line_column: start ({start}) must come before end ({end})"
        );

        Span {
            start: Location::LineColumn(start),
            end: Location::LineColumn(end),
        }
    }

    pub(crate) fn new(start: Location, end: Location) -> Self {
        assert!(start <= end);

        Span { start, end }
    }

    // TODO(cleanup): What does this represent? Before or after this point?
    pub fn at(only: Location) -> Self {
        Span {
            start: only,
            end: only,
        }
    }

    #[doc(hidden)]
    pub fn from_character_span(start: u32, end: u32) -> Self {
        Span {
            start: Location::CharacterIndex(start),
            end: Location::CharacterIndex(end),
        }
    }

    /// Get the start [`Location`] of this source span.
    pub fn start(&self) -> Location {
        let Span { start, end: _ } = *self;

        start
    }

    /// Get the end [`Location`] of this source span.
    pub fn end(&self) -> Location {
        let Span { start: _, end } = *self;

        end
    }

    /// Get the start and end [`Location`]s of this source span.
    pub fn start_end(&self) -> (Location, Location) {
        let Span { start, end } = *self;

        (start, end)
    }

    /// Returns true if `other` partially or completely overlaps with this
    /// [`Span`].
    ///
    /// # Panics
    ///
    /// This function will panic if `self` and `other` are different
    /// [`SpanKind`] values.
    pub fn overlaps(&self, other: Span) -> bool {
        match (self.kind(), other.kind()) {
            (
                SpanKind::LineColumnSpan(self_),
                SpanKind::LineColumnSpan(other),
            ) => {
                self_.overlaps(other)
            },
            (
                SpanKind::CharacterSpan(self_),
                SpanKind::CharacterSpan(other),
            ) => {
                self_.intersects(other)
            },
            (SpanKind::LineColumnSpan(_), SpanKind::CharacterSpan(_))
            | (SpanKind::CharacterSpan(_), SpanKind::LineColumnSpan(_)) => panic!(
                "Invalid combination of Span types: Span::overlaps({self}, {other})"
            )
        }
    }

    pub fn kind(self) -> SpanKind {
        let Span { start, end } = self;

        match (start, end) {
            (
                Location::CharacterIndex(start_char),
                Location::CharacterIndex(end_char),
            ) => SpanKind::CharacterSpan(CharacterSpan(start_char, end_char)),
            (
                Location::LineColumn(LineColumn(start_line, start_column)),
                Location::LineColumn(LineColumn(end_line, end_column)),
            ) => SpanKind::LineColumnSpan(LineColumnSpan {
                start: LineColumn(start_line, start_column),
                end: LineColumn(end_line, end_column),
            }),
            (start, end) => panic!("invalid Location combination in Span: start = {start}, end = {end}"),
        }
    }
}

impl LineColumn {
    pub fn line(self) -> NonZeroU32 {
        let LineColumn(line, _) = self;

        line
    }

    pub fn column(self) -> NonZeroU32 {
        let LineColumn(_, column) = self;

        column
    }
}

impl LineColumnSpan {
    /// Check if a [`LineColumn`] location is inside of this [`LineColumnSpan`]
    /// span.
    ///
    /// ```
    /// use wolfram_parser::{source::LineColumn, macros::src};
    ///
    /// assert!(src!(1:3-2:1).contains(src!(1:4)));
    ///
    /// assert!(!src!(1:3-2:1).contains(src!(2:4)));
    ///
    /// assert!(src!(1:3-2:1).contains(src!(1:4)));
    ///
    /// assert!(!src!(1:3-2:1).contains(src!(2:4)));
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
    /// use wolfram_parser::{source::{Span, Location}, macros::src};
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

    #[cfg_attr(not(debug_assertions), allow(dead_code))]
    pub(crate) fn column_width(&self) -> usize {
        let LineColumnSpan { start, end } = *self;

        debug_assert!(
            start.line() == end.line(),
            "SpanKind::column_width(): source locations are on different lines"
        );

        return end.column().get() as usize - start.column().get() as usize;
    }
}

impl Source {
    pub fn unknown() -> Self {
        Source::Unknown
    }

    pub fn is_unknown(&self) -> bool {
        match self {
            Source::Unknown => true,
            Source::Span(_) | Source::Box(_) => false,
        }
    }
}

impl BoxPosition {
    /// Compute a [`BoxPosition`] value that covers the source region between
    /// `start` and `end`, inclusive.
    ///
    /// # Examples
    ///
    /// ```
    /// use wolfram_parser::{source::BoxPosition, macros::src};
    ///
    /// assert_eq!(
    ///     BoxPosition::between(
    ///         &src!({1, 1, 2}),
    ///         &src!({1, 1, 4})
    ///     ),
    ///     Some(src!({1, 1, (2 ;; 4)}))
    /// );
    /// ```
    pub fn between(
        start: &BoxPosition,
        end: &BoxPosition,
    ) -> Option<BoxPosition> {
        #[derive(Copy, Clone, Debug)]
        enum Pos {
            Index(usize),
            /// `a ;; b`
            Span((usize, usize)),
        }

        let todo = || -> ! {
            todo!("FIXME: Invalid or unhandled box position range: BoxPosition::between({start}, {end})")
        };

        if start == end {
            // TID:231111/1: CompoundNode computed box source
            return Some(start.clone());
        }

        //------------------------------
        // Normalize lengths
        //------------------------------

        let (start, end) = match (start, end) {
            // TID:231110/1 - BoxPosition::between for start/end tokens at different depths
            //     Test case: BoxPosition::between(&src!({1, 1}), &src!({1, 2, 1, 2}))
            (BoxPosition::At(start), BoxPosition::At(end))
                if end.len() == start.len() + 2 =>
            {
                let normalized_end = &end[..end.len() - 2];

                debug_assert_eq!(start.len(), normalized_end.len());

                let start_most = most_slice(start);
                let end_most = most_slice(normalized_end);

                if start_most.is_some() && start_most == end_most {
                    // If the original start and end were {1, 1} and {1, 2, 1, 2},
                    // the new start and end are {1, 1} and {1, 2} -- we've
                    // normalized the end location to make them the same
                    // length.
                    //
                    // The correctness of this assumes that the position
                    // represented by the dropped indexs in `end` were the last
                    // expressions at their respective depth -- which should
                    // always be the case when these positions are the start/end
                    // source location of a well-formed Cst node.
                    (
                        BoxPosition::At(start.to_vec()),
                        BoxPosition::At(normalized_end.to_vec()),
                    )
                } else {
                    (
                        BoxPosition::At(start.to_vec()),
                        BoxPosition::At(end.to_vec()),
                    )
                }
            },
            (start, end) => (start.clone(), end.clone()),
        };

        //------------------------------
        // Compare and extract common prefix
        //------------------------------

        let (common, (start_pos, end_pos)) = match (start, end) {
            (
                BoxPosition::At(start) | BoxPosition::Before(start),
                BoxPosition::At(end) | BoxPosition::After(end),
            ) => match (start.split_last(), end.split_last()) {
                (
                    Some((start_last, start_most)),
                    Some((end_last, end_most)),
                ) => 'ret: {
                    if start_most == end_most {
                        break 'ret (
                            start_most.to_vec(),
                            (Pos::Index(*start_last), Pos::Index(*end_last)),
                        );
                    }

                    todo();
                },

                (None, None) => return Some(BoxPosition::At(vec![])),

                (None, Some(_)) | (Some(_), None) => return None,
            },

            // Occurs when the start position is the head of a CallNode and the
            // end Spanning position is the source of the CallBody::Group.
            //
            // Test case: start = {1, 1, 1}, end = {1, 1, 2 ;; 4}
            //
            // TID:231031/1: Synthetic box source for process plus pair
            // TID:231110/2: Plus pair synthetic "between" BoxPosition
            //
            (
                BoxPosition::At(start),
                BoxPosition::Spanning {
                    index: end,
                    span: end_span,
                },
            ) => {
                let Some((start_last, start_most)) = start.split_last() else {
                    // TODO: Test
                    return None;
                };

                if start_most != end {
                    return None;
                }

                (
                    start_most.to_vec(),
                    (Pos::Index(*start_last), Pos::Span(end_span)),
                )
            },

            (_, _) => {
                todo();
            },
        };

        //------------------------------
        // Compare last elements in each box position
        //------------------------------

        let combined = match (start_pos, end_pos) {
            // TID:231108/1: Computed box source position for PostfixNode with GroupNode
            (Pos::Index(1), Pos::Index(_) | Pos::Span((2, _))) => {
                match most_slice(&common) {
                    Some(parent_box_pos) => {
                        BoxPosition::At(parent_box_pos.to_vec())
                    },
                    None => todo(),
                }
            },

            (Pos::Index(2), Pos::Index(end_pos)) => BoxPosition::Spanning {
                index: common,
                span: (2, end_pos),
            },

            (_, _) => todo(),
        };

        Some(combined)
    }
}

//======================================
// Source type conversion impls
//======================================

impl From<LineColumn> for Location {
    fn from(lc: LineColumn) -> Self {
        Location::LineColumn(lc)
    }
}

impl From<LineColumnSpan> for Span {
    fn from(value: LineColumnSpan) -> Self {
        let LineColumnSpan { start, end } = value;

        Span {
            start: Location::LineColumn(start),
            end: Location::LineColumn(end),
        }
    }
}

impl From<LineColumnSpan> for Source {
    fn from(value: LineColumnSpan) -> Self {
        Source::Span(Span::from(value))
    }
}

impl From<CharacterSpan> for Span {
    fn from(value: CharacterSpan) -> Span {
        let CharacterSpan(start, end) = value;

        Span {
            start: Location::CharacterIndex(start),
            end: Location::CharacterIndex(end),
        }
    }
}

impl From<BoxPosition> for Source {
    fn from(value: BoxPosition) -> Source {
        Source::Box(value)
    }
}

//======================================
// Source types comparision impls
//======================================

// TODO: Only used for assertions; make this a method
// TODO: Replace this with derive(PartialOrd)? I think they would have the same
//       semantics.
impl PartialOrd for Location {
    fn partial_cmp(&self, b: &Self) -> Option<std::cmp::Ordering> {
        let a = self;

        match (a, b) {
            (
                Location::LineColumn(LineColumn(a_line, a_column)),
                Location::LineColumn(LineColumn(b_line, b_col)),
            ) => (a_line, a_column).partial_cmp(&(b_line, b_col)),
            (
                Location::CharacterIndex(a_index),
                Location::CharacterIndex(b_index),
            ) => a_index.partial_cmp(b_index),
            (Location::LineColumn { .. }, Location::CharacterIndex(_)) => None,
            (Location::CharacterIndex(_), Location::LineColumn { .. }) => None,
        }
    }
}

impl PartialOrd for LineColumn {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        let LineColumn(a_line, a_column) = self;
        let LineColumn(b_line, b_column) = other;

        (a_line, a_column).partial_cmp(&(b_line, b_column))
    }
}

// bool operator<(Source a, Source b) {
//     return a.Start < b.Start;
// }
// PRE_COMMIT: Only used for assertions; make this a method
impl PartialOrd for Span {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        self.start.partial_cmp(&other.start)
    }
}

impl PartialOrd for Source {
    fn partial_cmp(&self, other: &Source) -> Option<Ordering> {
        match (self, other) {
            (Source::Span(src1), Source::Span(src2)) => src1.partial_cmp(src2),
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
            Unsafe1ByteUtf8Sequence
            | Unsafe2ByteUtf8Sequence
            | Unsafe3ByteUtf8Sequence => true,
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

        let val: CodePoint = c;

        let val: char = match c {
            CodePoint::EndOfFile => {
                panic!()
            }
            Char('\x08') /* ASCII backspace */ => {

                return write!(stream, "{:#}", WLCharacter::new_with_escape(StringMeta_Backspace, Escape::Single))
            }
            Char(ASCII_FORM_FEED) => {

                return write!(stream, "{:#}", WLCharacter::new_with_escape(StringMeta_FormFeed, Escape::Single))
            }
                //
                // whitespace and newline characters
                //
            Char('\t') => {

                return write!(stream, "{:#}", WLCharacter::new_with_escape(StringMeta_Tab, Escape::Single))
            }
            Char('\n') => {

                return write!(stream, "{:#}", WLCharacter::new_with_escape(StringMeta_LineFeed, Escape::Single))
            }
            Char('\r') => {

                return write!(stream, "{:#}", WLCharacter::new_with_escape(StringMeta_CarriageReturn, Escape::Single))
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

                return write!(stream, "{:#}", WLCharacter::new_with_escape(StringMeta_DoubleQuote, Escape::Single))
            }
            StringMeta_Backslash => {

                //
                // Coming from \[RawBackslash]
                //

                return write!(stream, "{:#}", WLCharacter::new_with_escape(StringMeta_Backslash, Escape::Single))
            }
            CRLF => {

                write!(stream, "{:#}", WLCharacter::new_with_escape(StringMeta_CarriageReturn, Escape::Single))?;
                write!(stream, "{:#}", WLCharacter::new_with_escape(StringMeta_LineFeed, Escape::Single))?;

                return Ok(())
            }
            Char(
                ls @ CODEPOINT_LINEARSYNTAX_BANG
                | ls @ CODEPOINT_LINEARSYNTAX_PERCENT
                | ls @ CODEPOINT_LINEARSYNTAX_AMP
                | ls @ CODEPOINT_LINEARSYNTAX_OPENPAREN
                | ls @ CODEPOINT_LINEARSYNTAX_CLOSEPAREN
                | ls @ CODEPOINT_LINEARSYNTAX_STAR
                | ls @ CODEPOINT_LINEARSYNTAX_PLUS
                | ls @ CODEPOINT_LINEARSYNTAX_SLASH
                | ls @ CODEPOINT_LINEARSYNTAX_AT
                | ls @ CODEPOINT_LINEARSYNTAX_CARET
                | ls @ CODEPOINT_LINEARSYNTAX_UNDER
                | ls @ CODEPOINT_LINEARSYNTAX_BACKTICK
            ) => {

                return write!(stream, "{:#}", WLCharacter::new_with_escape(ls, Escape::Single))
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

                return write!(stream, "{:#}", WLCharacter::new_with_escape(CODEPOINT_ESC, Escape::LongName))
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
            Char('\u{80}' ..= '\u{9f}') => {
                return write!(stream, "{:#}", WLCharacter::new_with_escape(val, Escape::Hex2))
            }
            Char(char) => char,
            _ => panic!("unable to format special char: {val:?}"),
        };

        let val: char = val;

        let escape: Option<Escape> =
            if val > '\u{7f}' && codepoint_has_longname(val) {
                //
                // Use LongName if available and not ASCII
                //
                Some(Escape::LongName)
            } else if val > '\u{ffff}' {
                Some(Escape::Hex6)
            } else if val > '\u{ff}' {
                Some(Escape::Hex4)
            } else if val > '\u{7f}' {
                Some(Escape::Hex2)
            } else {
                None
            };

        if let Some(escape) = escape {
            return write!(
                stream,
                "{:#}",
                WLCharacter::new_with_escape(val, escape)
            );
        }

        debug_assert!(val.is_ascii());

        //
        // ASCII is untouched
        // Do not use CodePointToLongNameMap to find Raw names
        //

        crate::byte_encoder::encodeBytes(stream, CodePoint::Char(val))?;

        return Ok(());
    }
}

//==========================================================
// Tests
//==========================================================

#[test]
fn test_character_span_intersects() {
    let CS = CharacterSpan;

    assert!(CS(1, 2).intersects(CS(1, 2)));
    assert!(CS(1, 3).intersects(CS(2, 4)));
    assert!(CS(2, 4).intersects(CS(1, 3)));

    assert!(!CS(1, 2).intersects(CS(3, 4)));
}

#[test]
fn test_box_position_between() {
    use crate::macros::src;

    assert_eq!(BoxPosition::between(&src!({}), &src!({})), Some(src!({})));

    // Occurs when the start and end positions are the source locations,
    // respectively, of the `[` and `]` tokens in a CallBody::Group.
    assert_eq!(
        BoxPosition::between(&src!({1, 1, 2}), &src!({1, 1, 4})),
        Some(src!({1, 1, (2 ;; 4)}))
    );

    // Occurs when the start position is the head of a CallNode and the
    // end Spanning position is the source of the CallBody::Group.
    assert_eq!(
        BoxPosition::between(&src!({1, 1, 1}), &src!({1, 1, (2 ;; 4)})),
        Some(src!({ 1 }))
    );

    assert_eq!(
        BoxPosition::between(&src!({1, 1, 1}), &src!({1, 1, 3})),
        Some(src!({ 1 }))
    );

    // TID:231110/1 - BoxPosition::between for start/end tokens at different depths
    // Occurs when parsing:
    //     RowBox[{
    //         "\[Integral]",
    //         RowBox[{
    //             RowBox[{"Sin", "[", "x", "]"}],
    //             RowBox[{"\[DifferentialD]", "x"}]
    //         }]
    //     }]
    // where when calculating the position of the outer PrefixBinaryNode,
    // the start position comes from the leaf node for "\[Integral]", and the
    // end position comes from the second "x", which has a deeper source
    // position.
    assert_eq!(
        BoxPosition::between(&src!({1, 1}), &src!({1, 2, 1, 2})),
        Some(src!({}))
    );

    assert_eq!(
        BoxPosition::between(&src!({1, 1, 1, 1}), &src!({1, 1, 1, 2, 1, 2})),
        Some(src!({1, 1}))
    );

    // Occurs when parsing:
    //     RowBox[{"~", "f"}]
    assert_eq!(
        BoxPosition::between(&src!(Before[{1, 1}]), &src!({1, 2})),
        Some(src!({}))
    );
}
