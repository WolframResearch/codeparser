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
/// // Line 1, column 3 through line 4, column 7
/// let span: LineColumnSpan = src!(1:3-4:7);
///
/// // Line 1, column 3 through line 1, column 8
/// let span: LineColumnSpan = src!(1:3-8);
/// ```
///
/// Construct a [`CharacterSpan`][crate::source::CharacterSpan] span:
///
/// ```
/// # use wolfram_parser::{macros::src, source::CharacterSpan};
/// // Characters 1 through 4
/// let span: CharacterSpan = src!(1-4);
/// ```
///
/// ## Boxes
///
/// Construct a [`BoxPosition::At`][crate::source::BoxPosition::At]:
///
/// ```
/// # use wolfram_parser::{macros::src, source::BoxPosition};
/// let pos: BoxPosition = src!({1, 2, 3});
/// ```
///
/// Construct a [`BoxPosition::Spanning { .. }`][crate::source::BoxPosition::Spanning]:
///
/// ```
/// # use wolfram_parser::{macros::src, source::BoxPosition};
/// let pos: BoxPosition = src!({1, 1, (3 ;; 5)});
/// ```
///
/// Construct a [`BoxPosition::Before`][crate::source::BoxPosition::Before]:
///
/// ```
/// # use wolfram_parser::{macros::src, source::BoxPosition};
/// let pos: BoxPosition = src!(Before[{1, 1}]);
/// ```
#[doc(hidden)]
#[macro_export]
macro_rules! __src {
    // a:b
    ($line:literal : $column:literal) => {
        $crate::source::LineColumn(
            std::num::NonZeroU32::new($line).expect("line must not be zero"),
            std::num::NonZeroU32::new($column).expect("column must not be zero"),
        )
    };

    // a:b-c:d
    ($line1:literal : $column1:literal  -  $line2:literal : $column2:literal) => {
        $crate::source::LineColumnSpan {
            start: $crate::source::LineColumn(
                std::num::NonZeroU32::new($line1)
                    .expect("start line must not be zero"),
                std::num::NonZeroU32::new($column1)
                    .expect("start column must not be zero"),
            ),
            end: $crate::source::LineColumn(
                std::num::NonZeroU32::new($line2)
                    .expect("end line must not be zero"),
                std::num::NonZeroU32::new($column2)
                    .expect("end column must not be zero"),
            ),
        }
    };

    // a:b-c
    ($line1:literal : $column1:literal  -  $column2:literal) => {
        $crate::macros::src!($line1 : $column1 - $line1 : $column2)
    };

    // TODO: Pick only one of these syntaxes to use
    // a-b  OR  a..b
    ($start:literal - $end:literal) => {
        $crate::source::CharacterSpan($start, $end)
    };
    ($start:literal .. $end:literal) => {
        $crate::source::CharacterSpan($start, $end)
    };

    //==================================
    // Boxes
    //==================================

    // {a, b, c, ...}
    ({$($value:literal),*}) => {
        $crate::source::BoxPosition::At(vec![$($value),*])
    };

    // Note: (..) are needed to avoid macro ambiguity.
    // {a, b, (c ;; d)}
    ({$($value:literal),*, ($span_start:literal ;; $span_end:literal) }) => {
        $crate::source::BoxPosition::Spanning {
            index: vec![$($value),*],
            span: ($span_start, $span_end)
        }
    };

    // Before[{a, b, c, ...}]
    (Before[{$($value:literal),* }]) => {
        $crate::source::BoxPosition::Before(vec![$($value),*])
    };
}

/// Convenience constructor for [`Token`][crate::tokenize::Token]s.
///
/// **Usage:**
///
/// ```
/// # use wolfram_parser::macros::{src, token};
/// token!(Integer, "5", 1:1-1:2);
/// //     ^^^^^^^  ...  *******
/// ```
///
/// * `^^^` — [`TokenKind`][crate::tokenize::TokenKind] variant
/// * `...` — input content
/// * `***` — [`Source`][crate::source::Source] of the token; supports [`src!`] syntax
///
/// # Example
///
/// ```
/// # use pretty_assertions::assert_eq;
/// use wolfram_parser::{
///     tokenize_bytes,
///     ParseOptions,
///     NodeSeq,
///     macros::{src, token}
/// };
///
/// let NodeSeq(tokens) = tokenize_bytes(b"foo+1", &ParseOptions::default()).unwrap();
///
/// assert_eq!(tokens, &[
///     token!(Symbol, b"foo", 1:1-1:4),
///     token!(Plus, b"+", 1:4-1:5),
///     token!(Integer, b"1", 1:5-1:6),
/// ]);
/// ```
#[doc(hidden)]
#[macro_export]
macro_rules! __token {
    // token!(Kind, "...", 1:1-3:2)
    ($kind:ident, $input:tt, $l1:literal : $c1:literal  -  $l2:literal : $c2:literal) => {
        $crate::macros::token!($kind, $input, $crate::macros::src!($l1:$c1-$l2:$c2))
    };

    // token!(Kind, "...", 1:1-2)
    ($kind:ident, $input:tt, $l1:literal : $c1:literal  -  $c2:literal) => {
        $crate::macros::token!($kind, $input, $crate::macros::src!($l1:$c1-$c2))
    };

    // token!(Kind, "...", 1..3)
    ($kind:ident, $input:tt, $start:literal .. $end:literal) => {
        $crate::macros::token!($kind, $input, $crate::macros::src!($start .. $end))
    };

    // token!(Kind, "...", {1, 2, 3})
    ($kind:ident, $input:tt, {$($value:literal),*}) => {
        $crate::tokenize::Token {
            tok: $crate::tokenize::TokenKind::$kind,
            src: $crate::source::Source::from($crate::macros::src!({$($value),*})),
            input: $crate::tokenize::TokenStr::new($input.as_ref()),
        }
    };

    ($kind:ident, $input:tt, $src:expr) => {
        $crate::tokenize::Token {
            tok: $crate::tokenize::TokenKind::$kind,
            src: $crate::source::Span::from($src),
            input: $crate::tokenize::TokenStr::new($input.as_ref()),
        }
    };
}


/// Convenience constructor for [`Ast::Leaf`][crate::ast::Ast::Leaf]s.
///
/// **Usage:**
///
/// ```
/// # use wolfram_parser::{macros::{src, leaf}, ast::Ast};
/// let ast: Ast = leaf!(Integer, "5", 1:1-1:2);
/// //                   ^^^^^^^  ...  *******
/// ```
///
/// * `^^^` — [`TokenKind`][crate::tokenize::TokenKind] variant
/// * `...` — input content
/// * `***` — [`Source`][crate::source::Source] of the token; supports [`src!`] syntax
///
/// # Example
///
/// Construct an `Ast::Leaf` with unknown or unspecified source location:
///
/// ```
/// use wolfram_parser::{ast::Ast, macros::leaf};
///
/// let ast = leaf!(Integer, "5", <||>);
/// ```
#[doc(hidden)]
#[macro_export]
macro_rules! __leaf {
    // leaf!(Kind, "...", 1:1-3:2)
    ($kind:ident, $input:tt, $l1:literal : $c1:literal  -  $l2:literal : $c2:literal) => {
        $crate::macros::leaf!($kind, $input, $crate::macros::src!($l1:$c1-$l2:$c2))
    };

    // leaf!(Kind, "...", 1:1-2)
    ($kind:ident, $input:tt, $l1:literal : $c1:literal  -  $c2:literal) => {
        $crate::macros::leaf!($kind, $input, $crate::macros::src!($l1:$c1-$c2))
    };

    // leaf!(Kind, "...", {1, 2, 3})
    ($kind:ident, $input:tt, {$($value:literal),*}) => {
        $crate::ast::Ast::Leaf {
            kind: $crate::tokenize::TokenKind::$kind,
            input: $crate::tokenize::TokenString::new($input.as_ref()),
            data: $crate::ast::AstMetadata::from($crate::macros::src!({$($value),*})),
        }
    };

    // leaf!(Kind, "...", <||>)
    ($kind:ident, $input:tt, <||>) => {
        $crate::ast::Ast::Leaf {
            kind: $crate::tokenize::TokenKind::$kind,
            input: $crate::tokenize::TokenString::new($input.as_ref()),
            data: $crate::ast::AstMetadata::empty(),
        }
    };

    ($kind:ident, $input:tt, $src:expr) => {
        $crate::ast::Ast::Leaf {
            kind: $crate::tokenize::TokenKind::$kind,
            input: $crate::tokenize::TokenString::new($input.as_ref()),
            data: $crate::ast::AstMetadata::from($src),
        }
    };
}



// Publicly export these macros from `wolfram_parser::macros` *without* also
// publicly exporting them from the root `wolfram_parser` module.
//
// This uses the technique described here:
//     <https://users.rust-lang.org/t/how-to-namespace-a-macro-rules-macro-within-a-module-or-macro-export-it-without-polluting-the-top-level-namespace/63779/5#answering-the-original-threads-title-xy-problem-_quid_-of-macro_exported-macros-1>
#[doc(inline)]
pub use {__leaf as leaf, __src as src, __token as token};
