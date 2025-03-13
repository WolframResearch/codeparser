//! Wolfram Language input form parser.
//!
//! This library implements a fully-featured parser for [Wolfram Language][WL]
//! input form syntax. Given a string containing Wolfram Language code, either
//! an Abstract Syntax Tree (AST) or Concrete Syntax Tree (CST) can be parsed.
//!
//! [WL]: https://wolfram.com/language
//!
//!
//! # API
//!
//! Input   | Tokenize             | Parse concrete syntax    | Parse abstract syntax
//! --------|----------------------|--------------------------|--------------------
//! `&str`  | [`tokenize()`]       | [`parse_to_cst()`]       | [`parse_to_ast()`]
//! `&[u8]` | [`tokenize_bytes()`] | [`parse_bytes_to_cst()`] | [`parse_bytes_to_ast()`]
//!

//
// Lints
//
#![allow(unused_assignments, non_snake_case)]

macro_rules! incr_diagnostic {
    ($name:ident) => {
        #[cfg(feature = "DIAGNOSTICS")]
        {
            $name += 1;
        }
    };
}

/// Send format string arguments to be displayed using [`Print`][Print].
///
/// This function is intended to be used to print debugging output when this
/// library is used from the Wolfram Language via LibraryLink.
///
/// This function accepts the same format arguments structure as [`println!()`].
///
/// # Examples
///
/// ```ignore
/// let data = [1, 2, 3];
///
/// Print!("The Data: {:?}", data);
/// ```
///
/// [Print]: https://reference.wolfram.com/language/ref/Print
#[allow(unused_macros)]
macro_rules! Print {
    ($fmt:literal $(, $args:expr)*) => {{
        use wolfram_library_link::expr::{Expr, Symbol};
        let string: String = format!($fmt $(, $args)*);

        wolfram_library_link::evaluate(
            &Expr::normal(Symbol::new("System`Print"), vec![Expr::from(string)])
        );
    }}
}

mod utils;


mod byte_encoder;
mod code_point;
#[doc(hidden)]
pub mod issue;
mod long_names;
mod parselet;
#[doc(hidden)]
pub mod quirks;
#[doc(hidden)]
pub mod source;
#[doc(hidden)]
pub mod symbol;
mod token_enum;
mod tokenizer;
mod wl_character;

mod read;

mod error;
mod parser;
mod parser_session;

mod agg;
pub mod ast;
pub mod cst;

#[doc(hidden)]
pub mod abstract_;

#[doc(hidden)]
pub mod fmt_as_expr;

mod feature;

pub mod token;

//===================
// Generated sources
//===================

#[doc(hidden)]
#[path = "generated/symbol_registration.rs"]
pub mod symbol_registration;

#[doc(hidden)]
#[path = "generated/token_enum_registration.rs"]
pub mod token_enum_registration;

#[path = "generated/long_names_registration.rs"]
mod long_names_registration;

#[path = "generated/parselet_registration.rs"]
mod parselet_registration;

#[path = "generated/precedence.rs"]
mod precedence;


#[cfg(test)]
mod tests;


/// Utilties used for unit and integration tests.
///
/// This module is semver exempt and no item inside of it should be depended
/// on by external crates.
#[doc(hidden)]
pub mod test_utils {
    #[macro_export]
    #[doc(hidden)]
    /// Construct one of the crate source location types.
    ///
    /// # Examples
    ///
    /// Construct a [`LineColumn`][crate::source::LineColumn] location:
    ///
    /// ```
    /// # use wolfram_parser::{test_utils::src, source::LineColumn};
    /// // Line 5, column 4
    /// let pos: LineColumn = src!(5:4);
    /// ```
    ///
    /// Construct a [`LineColumnSpan`][crate::source::LineColumnSpan] span:
    ///
    /// ```
    /// # use wolfram_parser::{test_utils::src, source::LineColumnSpan};
    /// // Line 1, column 3 through line 1, column 8
    /// let span: LineColumnSpan = src!(1:3-1:8);
    /// ```
    ///
    /// Construct a [`CharacterSpan`][crate::source::CharacterSpan] span:
    ///
    /// ```
    /// # use wolfram_parser::{test_utils::src, source::CharacterSpan};
    /// // Characters 1 through 4
    /// let span: CharacterSpan = src!(1-4);
    /// ```
    macro_rules! src {
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

    #[macro_export]
    #[doc(hidden)]
    /// Convenience constructor for [`Token`][crate::token::Token]s.
    ///
    /// **Usage:**
    ///
    /// ```
    /// # use wolfram_parser::test_utils::{src, token};
    /// token!(Integer, "5" @ 0, src!(1:1-1:2));
    /// //     ^^^^^^^  ... ###  *************
    /// ```
    ///
    /// * `^^^` — [`TokenKind`][crate::token::TokenKind] variant
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
    ///     test_utils::{src, token}
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
    macro_rules! token {
        ($kind:ident, $input:tt @ $offset:literal, $src:expr) => {
            $crate::token::Token {
                tok: $crate::token::TokenKind::$kind,
                src: $crate::Source::from($src),
                input: $crate::token::BorrowedTokenInput::new($input.as_ref(), $offset),
            }
        };
    }

    pub use {src, token};
}

//==========================================================
// API
//==========================================================

//-----------
// Re-exports
//-----------

pub use crate::{
    parser_session::ParseResult,
    quirks::QuirkSettings,
    source::{
        ByteSpan,
        Source,
        SourceConvention,
        SourceLocation,
        StringSourceKind,
        // TODO: Should this be a part of the public API as a constant value, or
        //       something else 'symbolic'? E.g. prehaps this shouldn't be a
        //       required parameter of ParserSession::new().
        DEFAULT_TAB_WIDTH,
    },
};

//======================================
// Types
//======================================

pub struct Container<N> {
    pub kind: ContainerKind,
    pub body: ContainerBody<N>,
    pub metadata: Metadata,
}

pub enum ContainerKind {
    String,
    File,
    Box,
    Byte,
    // FIXME Is this really a valid container kind?
    Hold,
}

pub enum ContainerBody<N> {
    Nodes(NodeSeq<N>),
    Missing(UnsafeCharacterEncoding),
}

/// A sequence of Nodes
#[derive(Debug, Clone, PartialEq)]
pub struct NodeSeq<N>(pub Vec<N>);

#[derive(Debug)]
pub struct Metadata {
    pub source: GeneralSource,
    pub syntax_issues: Option<Vec<Issue>>,
    pub confidence_level: Option<Number>,
    pub code_actions: Option<Vec<CodeAction>>,
    pub additional_descriptions: Option<Vec<String>>,
    // TODO: Change this to Option<String>?
    pub file_name: Option<Expr>,
    pub embedded_tabs: Option<Expr>,
    pub embedded_newlines: Option<Expr>,
    pub simple_line_continuations: Option<Expr>,
    pub complex_line_continuations: Option<Expr>,
}

/// How `#!` [shebangs](https://en.wikipedia.org/wiki/Shebang_(Unix))
/// should be treated if they appear in the first line of input.
#[derive(Copy, Clone, Debug)]
pub enum FirstLineBehavior {
    /// Source is a string or something, so if `#!` is on first line, then do
    /// not treat special
    NotScript = 0,

    /// Source is something like .wl file that is being treated as a script
    ///
    /// Or source is .wl file that is NOT being treated as a script
    ///
    /// `#!` may be present, or it might not
    Check = 1,

    /// Source is a .wls file and there is definitely a `#!` on first line
    Script = 2,
}

/// Different encoding modes
#[derive(Copy, Clone, Debug, PartialEq)]
pub enum EncodingMode {
    /// Generates issues that you would expect if coming from a file or a string
    Normal = 0,

    /// Coming from a box, so some issues will be disabled
    ///
    /// These issues will be disabled:
    ///
    /// * NonASCIICharacters
    /// * Unexpected newline character: `\[IndentingNewLine]`
    Box = 1,
}

/// The modes that stringifying could happen in
#[doc(hidden)]
pub enum StringifyMode {
    /// Tokens are treated normally
    Normal = 0,

    /// Stringify the next token as a tag:
    ///
    /// ```wolfram
    /// a::bcd
    /// a::"bcd"
    /// #abc
    /// #"abc"
    /// ```
    Tag = 1,

    /// Stringify the next token as a file:
    ///
    /// ```wolfram
    /// << foo
    /// foo >> bar
    /// foo >>> bar
    /// ```
    File = 2,
}

pub struct Tokens<I = OwnedTokenInput, S = Source>(pub Vec<Token<I, S>>);

//-------------
// ParseOptions
//-------------

pub struct ParseOptions {
    first_line_behavior: FirstLineBehavior,
    src_convention: SourceConvention,
    encoding_mode: EncodingMode,
    tab_width: u32,
    quirk_settings: QuirkSettings,
}

impl Default for ParseOptions {
    fn default() -> ParseOptions {
        ParseOptions {
            first_line_behavior: FirstLineBehavior::NotScript,
            src_convention: SourceConvention::LineColumn,
            encoding_mode: EncodingMode::Normal,
            tab_width: DEFAULT_TAB_WIDTH,
            quirk_settings: QuirkSettings::default(),
        }
    }
}

impl ParseOptions {
    /// Helper constructor that requires every field be given a value.
    #[doc(hidden)]
    pub fn make(
        first_line_behavior: FirstLineBehavior,
        src_convention: SourceConvention,
        encoding_mode: EncodingMode,
        tab_width: u32,
        quirk_settings: QuirkSettings,
    ) -> Self {
        ParseOptions {
            first_line_behavior,
            src_convention,
            encoding_mode,
            tab_width,
            quirk_settings,
        }
    }

    pub fn tab_width(self, tab_width: u32) -> Self {
        ParseOptions { tab_width, ..self }
    }

    pub fn source_convention(self, src_convention: SourceConvention) -> Self {
        ParseOptions {
            src_convention,
            ..self
        }
    }
}

//======================================
// Functions
//======================================

use crate::parser_session::ParserSession;

/// Parse a string containing Wolfram Language input into a sequence of tokens.
///
/// # Examples
///
/// Tokenize `2 + 2`:
///
/// ```
/// use wolfram_parser::{
///     tokenize, ParseOptions, Tokens,
///     test_utils::{token, src}
/// };
///
/// let Tokens(tokens) = tokenize("2 + 2", &ParseOptions::default());
///
/// assert_eq!(tokens, &[
///     token![Integer, "2" @ 0, src!(1:1-1:2)],
///     token![Whitespace, " " @ 1, src!(1:2-1:3)],
///     token![Plus, "+" @ 2, src!(1:3-1:4)],
///     token![Whitespace, " " @ 3, src!(1:4-1:5)],
///     token![Integer, "2" @ 4, src!(1:5-1:6)],
/// ]);
/// ```
pub fn tokenize<'i>(input: &'i str, opts: &ParseOptions) -> Tokens<BorrowedTokenInput<'i>> {
    tokenize_bytes(input.as_bytes(), opts)
        .expect("unexpected character encoding error tokenizing &str")
}

/// Parse bytes containing Wolfram Language input into a sequence of tokens.
pub fn tokenize_bytes<'i>(
    input: &'i [u8],
    opts: &ParseOptions,
) -> Result<Tokens<BorrowedTokenInput<'i>>, UnsafeCharacterEncoding> {
    let mut session = ParserSession::new(input, opts);

    session.tokenize()
}

//======================================
// Parse CST
//======================================

/// Parse a string containing Wolfram Language input into a concrete syntax tree.
///
/// # Examples
///
/// Parse `2 + 2`:
///
/// ```
/// use wolfram_parser::{parse_to_cst, ParseOptions};
///
/// let result = parse_to_cst("2 + 2", &ParseOptions::default());
///
/// // TODO: assert_eq!(result.nodes(), &[]);
/// ```
pub fn parse_to_cst<'i>(
    input: &'i str,
    opts: &ParseOptions,
) -> ParseResult<CstNode<BorrowedTokenInput<'i>>> {
    parse_bytes_to_cst(input.as_bytes(), opts)
}

/// Parse bytes containing Wolfram Language input into a concrete syntax tree.
pub fn parse_bytes_to_cst<'i>(
    bytes: &'i [u8],
    opts: &ParseOptions,
) -> ParseResult<CstNode<BorrowedTokenInput<'i>>> {
    let mut session = ParserSession::new(bytes, opts);

    session.concrete_parse_expressions()
}

//======================================
// Parse AST
//======================================

/// Parse a string containing Wolfram Language input into an abstract syntax tree.
pub fn parse_to_ast<'i>(input: &'i str, opts: &ParseOptions) -> ParseResult<AstNode> {
    parse_bytes_to_ast(input.as_bytes(), opts)
}

/// Parse bytes containing Wolfram Language input into an abstract syntax tree.
pub fn parse_bytes_to_ast<'i>(bytes: &'i [u8], opts: &ParseOptions) -> ParseResult<AstNode> {
    let mut session = ParserSession::new(bytes, opts);

    session.abstract_parse_expressions()
}

//==========================================================
// LibraryLink
//==========================================================

#[doc(hidden)]
pub fn parse_to_token<'i>(
    bytes: &'i [u8],
    opts: &ParseOptions,
    stringify_mode: StringifyMode,
) -> ParseResult<Token<BorrowedTokenInput<'i>>> {
    let mut session = ParserSession::new(bytes, opts);

    session.concreteParseLeaf(stringify_mode)
}

#[doc(hidden)]
pub fn safe_string<'i>(
    bytes: &'i [u8],
    opts: &ParseOptions,
) -> Result<&'i str, UnsafeCharacterEncoding> {
    let mut session = ParserSession::new(bytes, opts);

    session.safe_string()
}

// TODO(cleanup): This doesn't need to be a method on ParserSession.
pub(crate) fn abortQ() -> bool {
    // if self.libData.is_null() {
    //     return false;
    // }

    //
    // AbortQ() returns a mint
    //
    // return self.libData.AbortQ();

    #[cfg(feature = "USE_MATHLINK")]
    return unsafe { wolfram_library_link::rtl::AbortQ() } != 0;

    #[cfg(not(feature = "USE_MATHLINK"))]
    return false;
}

//======================================
// Magic number conversions
//======================================

impl TryFrom<i32> for FirstLineBehavior {
    type Error = ();

    fn try_from(value: i32) -> Result<Self, Self::Error> {
        let variant = match value {
            0 => FirstLineBehavior::NotScript,
            1 => FirstLineBehavior::Check,
            2 => FirstLineBehavior::Script,
            _ => return Err(()),
        };
        Ok(variant)
    }
}

impl TryFrom<i32> for EncodingMode {
    type Error = ();

    fn try_from(value: i32) -> Result<Self, Self::Error> {
        let variant = match value {
            0 => EncodingMode::Normal,
            1 => EncodingMode::Box,
            _ => return Err(()),
        };
        Ok(variant)
    }
}

impl TryFrom<i32> for StringifyMode {
    type Error = ();

    fn try_from(value: i32) -> Result<Self, Self::Error> {
        let variant = match value {
            0 => StringifyMode::Normal,
            1 => StringifyMode::Tag,
            2 => StringifyMode::File,
            _ => return Err(()),
        };
        Ok(variant)
    }
}

impl TryFrom<i32> for SourceConvention {
    type Error = ();

    fn try_from(value: i32) -> Result<Self, Self::Error> {
        let variant = match value {
            0 => SourceConvention::LineColumn,
            1 => SourceConvention::CharacterIndex,
            _ => return Err(()),
        };
        Ok(variant)
    }
}

//--------------------------------------
// Macros
//--------------------------------------

macro_rules! panic_if_aborted {
    () => {
        if crate::feature::CHECK_ABORT && crate::abortQ() {
            panic!("aborting parsing by panicking")
        }
    };
}

use ast::AstNode;
use cst::CstNode;
use issue::{CodeAction, Issue};
pub(crate) use panic_if_aborted;
use source::GeneralSource;
use token::{BorrowedTokenInput, OwnedTokenInput, Token};
#[doc(hidden)]
pub use tokenizer::UnsafeCharacterEncoding;
use wolfram_expr::{Expr, Number};
