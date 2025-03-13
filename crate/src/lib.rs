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

mod character_decoder;
mod utils;

mod integral_parselet;
mod semi_semi_parselet;
mod times_parselet;
mod under_parselet;

mod byte_buffer;
mod byte_decoder;
mod byte_encoder;
mod code_point;
mod long_names;
mod my_string;
mod parselet;
mod quirks;
mod source;
mod symbol;
mod token_enum;
mod tokenizer;
mod wl_character;

mod error;
mod parser;
mod parser_session;

mod agg;
mod ast;

mod abstract_;

#[cfg(feature = "USE_MATHLINK")]
mod convert_wstp;

#[cfg(feature = "USE_EXPR_LIB")]
mod convert_expr;

#[cfg(feature = "USE_MATHLINK")]
mod from_expr;

#[cfg(feature = "USE_MATHLINK")]
mod wll_api;

mod feature;

pub mod node;
pub mod token;

//===================
// Generated sources
//===================

#[path = "generated/symbol_registration.rs"]
mod symbol_registration;

#[path = "generated/token_enum_registration.rs"]
mod token_enum_registration;

#[path = "generated/long_names_registration.rs"]
mod long_names_registration;

#[path = "generated/my_string_registration.rs"]
mod my_string_registration;

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
    macro_rules! src {
        // a:b
        ($line:literal : $column:literal) => {
            $crate::Source::from_location($crate::SourceLocation::new($line, $column))
        };
        // a:b-c:d
        ($line1:literal : $column1:literal  -  $line2:literal : $column2:literal) => {
            $crate::Source::new(
                $crate::SourceLocation::new($line1, $column1),
                $crate::SourceLocation::new($line2, $column2),
            )
        };
        // a..b
        ($start:literal .. $end:literal) => {
            $crate::Source::from_character_range($start, $end)
        };
    }

    #[macro_export]
    #[doc(hidden)]
    /// Convenience constructor for [`Token`][crate::token::Token]s.
    ///
    /// **Usage:**
    ///
    /// ```
    /// # use wolfram_code_parse::test_utils::{src, token};
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
    /// use wolfram_code_parse::{
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
                src: $src,
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

pub struct Container<S> {
    pub kind: ContainerKind,
    pub body: ContainerBody<S>,
    pub metadata: Metadata,
}

pub enum ContainerBody<S> {
    Nodes(NodeSeq<OwnedTokenInput, S>),
    Missing(UnsafeCharacterEncoding),
}

pub enum ContainerKind {
    String,
    File,
    Box,
    Byte,
    // FIXME Is this really a valid container kind?
    Hold,
}

#[derive(Debug)]
pub struct Metadata {
    pub source: GeneralSource,
    pub syntax_issues: Option<Vec<Issue>>,
    pub confidence_level: Option<Number>,
    pub code_actions: Option<Vec<CodeAction>>,
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
pub(crate) enum StringifyMode {
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

pub struct Tokens<I = OwnedTokenInput>(pub Vec<Token<I>>);

//-------------
// ParseOptions
//-------------

pub struct ParseOptions {
    first_line_behavior: FirstLineBehavior,
    src_convention: SourceConvention,
    encoding_mode: EncodingMode,
    tab_width: u32,
}

impl Default for ParseOptions {
    fn default() -> ParseOptions {
        ParseOptions {
            first_line_behavior: FirstLineBehavior::NotScript,
            src_convention: SourceConvention::LineColumn,
            encoding_mode: EncodingMode::Normal,
            tab_width: DEFAULT_TAB_WIDTH,
        }
    }
}

impl ParseOptions {
    pub fn tab_width(self, tab_width: u32) -> Self {
        ParseOptions { tab_width, ..self }
    }
}

//======================================
// Functions
//======================================

use crate::parser_session::ParserSession;

/// Parse bytes containing Wolfram Language input into a sequence of tokens.
///
/// # Examples
///
/// Tokenize `2 + 2`:
///
/// ```
/// use wolfram_code_parse::{
///     tokenize_bytes, ParseOptions, Tokens,
///     test_utils::{token, src}
/// };
///
/// let Tokens(tokens) = tokenize_bytes(b"2 + 2", &ParseOptions::default())
///     .unwrap();
///
/// assert_eq!(tokens, &[
///     token![Integer, "2" @ 0, src!(1:1-1:2)],
///     token![Whitespace, " " @ 1, src!(1:2-1:3)],
///     token![Plus, "+" @ 2, src!(1:3-1:4)],
///     token![Whitespace, " " @ 3, src!(1:4-1:5)],
///     token![Integer, "2" @ 4, src!(1:5-1:6)],
/// ]);
/// ```
pub fn tokenize_bytes<'i>(
    input: &'i [u8],
    opts: &ParseOptions,
) -> Result<Tokens<BorrowedTokenInput<'i>>, UnsafeCharacterEncoding> {
    let ParseOptions {
        first_line_behavior,
        src_convention,
        encoding_mode,
        tab_width,
    } = *opts;

    let mut session = ParserSession::new(
        input,
        src_convention,
        tab_width,
        first_line_behavior,
        encoding_mode,
    );

    session.tokenize()
}

/// Parse a string containing Wolfram Language input into concrete syntax tree.
///
/// # Examples
///
/// Parse `2 + 2`:
///
/// ```
/// use wolfram_code_parse::{parse_concrete, ParseOptions};
///
/// let result = parse_concrete("2 + 2", &ParseOptions::default());
///
/// // TODO: assert_eq!(result.nodes(), &[]);
/// ```
pub fn parse_concrete<'i>(
    input: &'i str,
    opts: &ParseOptions,
) -> ParseResult<BorrowedTokenInput<'i>> {
    let ParseOptions {
        first_line_behavior,
        src_convention,
        encoding_mode,
        tab_width,
    } = *opts;

    let mut session = ParserSession::new(
        input.as_bytes(),
        src_convention,
        tab_width,
        first_line_behavior,
        encoding_mode,
    );

    session.concrete_parse_expressions()
}

pub fn abstract_parse_expressions<'i>(
    _input: &'i str,
    _opts: &ParseOptions,
) -> ParseResult<BorrowedTokenInput<'i>> {
    // let ParseOptions {
    //     first_line_behavior,
    //     src_convention,
    //     encoding_mode,
    //     tab_width,
    // } = *opts;

    // let mut session = ParserSession::new(
    //     input.as_bytes(),
    //     src_convention,
    //     tab_width,
    //     first_line_behavior,
    //     encoding_mode,
    // );

    // session.abstract_parse_expressions()

    todo!()
}

//======================================
// LibraryLink
//======================================

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

use crate::node::NodeSeq;
pub(crate) use panic_if_aborted;
use source::{CodeAction, GeneralSource, Issue};
use token::{BorrowedTokenInput, OwnedTokenInput, Token};
use tokenizer::UnsafeCharacterEncoding;
use wolfram_expr::{Expr, Number};
