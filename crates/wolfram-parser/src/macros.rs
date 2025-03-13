/// Convenience constructor for one of the crate source location types.
///
/// # Examples
///
/// Construct a [`LineColumn`][crate::source::LineColumn] location:
///
/// ```
/// # use wolfram_parser::{macros::src, source::LineColumn};
/// // Line 5, column 4
/// let pos: LineColumn = src!(5:4);
/// ```
///
/// Construct a [`LineColumnSpan`][crate::source::LineColumnSpan] span:
///
/// ```
/// # use wolfram_parser::{macros::src, source::LineColumnSpan};
/// // Line 1, column 3 through line 1, column 8
/// let span: LineColumnSpan = src!(1:3-1:8);
/// ```
///
/// Construct a [`CharacterSpan`][crate::source::CharacterSpan] span:
///
/// ```
/// # use wolfram_parser::{macros::src, source::CharacterSpan};
/// // Characters 1 through 4
/// let span: CharacterSpan = src!(1-4);
/// ```
#[doc(hidden)]
#[macro_export]
macro_rules! __src {
    // a:b
    ($line:literal : $column:literal) => {
        $crate::source::LineColumn(
            std::num::NonZeroU32::new($line).expect("line must not be zero"),
            $column,
        )
    };

    // a:b-c:d
    ($line1:literal : $column1:literal  -  $line2:literal : $column2:literal) => {
        $crate::source::LineColumnSpan {
            start: $crate::source::LineColumn(
                std::num::NonZeroU32::new($line1).expect("start line must not be zero"),
                $column1,
            ),
            end: $crate::source::LineColumn(
                std::num::NonZeroU32::new($line2).expect("end line must not be zero"),
                $column2,
            ),
        }
    };

    // TODO: Pick only one of these syntaxes to use
    // a-b  OR  a..b
    ($start:literal - $end:literal) => {
        $crate::source::CharacterSpan($start, $end)
    };
    ($start:literal .. $end:literal) => {
        $crate::source::CharacterSpan($start, $end)
    };
}

/// Convenience constructor for [`Token`][crate::tokenize::Token]s.
///
/// **Usage:**
///
/// ```
/// # use wolfram_parser::macros::{src, token};
/// token!(Integer, "5" @ 0, src!(1:1-1:2));
/// //     ^^^^^^^  ... ###  *************
/// ```
///
/// * `^^^` — [`TokenKind`][crate::tokenize::TokenKind] variant
/// * `...` — input content
/// * `###` — byte offset of this token
/// * `***` — [`Source`][crate::source::Source] of the token
///
/// # Example
///
/// ```
/// # use pretty_assertions::assert_eq;
/// use wolfram_parser::{
///     tokenize_bytes,
///     ParseOptions,
///     Tokens,
///     macros::{src, token}
/// };
///
/// let Tokens(tokens) = tokenize_bytes(b"foo+1", &ParseOptions::default()).unwrap();
///
/// assert_eq!(tokens, &[
///     token!(Symbol, b"foo" @ 0, src!(1:1-1:4)),
///     token!(Plus, b"+" @ 3, src!(1:4-1:5)),
///     token!(Integer, b"1" @ 4, src!(1:5-1:6)),
/// ]);
/// ```
#[doc(hidden)]
#[macro_export]
macro_rules! __token {
    ($kind:ident, $input:tt @ $offset:literal, $src:expr) => {
        $crate::tokenize::Token {
            tok: $crate::tokenize::TokenKind::$kind,
            src: $crate::Source::from($src),
            input: $crate::tokenize::BorrowedTokenInput::new($input.as_ref(), $offset),
        }
    };
}

// Publicly export these macros from `wolfram_parser::macros` *without* also
// publicly exporting them from the root `wolfram_parser` module.
//
// This uses the technique described here:
//     <https://users.rust-lang.org/t/how-to-namespace-a-macro-rules-macro-within-a-module-or-macro-export-it-without-polluting-the-top-level-namespace/63779/5#answering-the-original-threads-title-xy-problem-_quid_-of-macro_exported-macros-1>
#[doc(inline)]
pub use {__src as src, __token as token};
