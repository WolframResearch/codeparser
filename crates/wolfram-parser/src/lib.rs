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
#[doc(hidden)]
pub mod issue;
mod long_names;
#[doc(hidden)]
pub mod quirks;
pub mod source;
#[doc(hidden)]
pub mod symbol;

mod read;
pub mod tokenize;
mod parse;

mod error;

mod agg;
pub mod ast;
pub mod cst;

pub mod abstract_;

#[doc(hidden)]
pub mod fmt_as_expr;

mod feature;

/// Contains modules whose source code is generated dynamically at project build
/// time.
#[doc(hidden)]
pub mod generated {
    pub(crate) mod long_names_registration;
    pub(crate) mod precedence_values;
}

mod precedence;
#[doc(hidden)]
pub mod symbols;


#[cfg(test)]
mod tests;

pub mod macros;

//==========================================================
// API
//==========================================================

use wolfram_expr::{Expr, Number};

use crate::{
    ast::Ast,
    cst::Cst,
    issue::{CodeAction, Issue},
    parse::ParserSession,
    source::{Source, SourceConvention, Span, DEFAULT_TAB_WIDTH},
    tokenize::{Token, TokenStr, TokenString},
};



//-----------
// Re-exports
//-----------

pub use crate::{parse::ParseResult, quirks::QuirkSettings};

#[doc(hidden)]
pub use crate::tokenize::tokenizer::UnsafeCharacterEncoding;

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
    Byte,
    Box,
    Cell,
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
    pub source: Source,
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

#[derive(Debug, Clone, PartialEq)]
pub struct Tokens<I = TokenString, S = Span>(pub Vec<Token<I, S>>);

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

    #[doc(hidden)]
    pub fn first_line_behavior(
        self,
        first_line_behavior: FirstLineBehavior,
    ) -> Self {
        ParseOptions {
            first_line_behavior,
            ..self
        }
    }
}

//======================================
// Functions
//======================================

/// Parse a string containing Wolfram Language input into a sequence of tokens.
///
/// # Examples
///
/// Tokenize `2 + 2`:
///
/// ```
/// use wolfram_parser::{
///     tokenize, ParseOptions, Tokens,
///     macros::{token, src}
/// };
///
/// let Tokens(tokens) = tokenize("2 + 2", &ParseOptions::default());
///
/// assert_eq!(tokens, &[
///     token![Integer, "2", src!(1:1-1:2)],
///     token![Whitespace, " ", src!(1:2-1:3)],
///     token![Plus, "+", src!(1:3-1:4)],
///     token![Whitespace, " ", src!(1:4-1:5)],
///     token![Integer, "2", src!(1:5-1:6)],
/// ]);
/// ```
pub fn tokenize<'i>(
    input: &'i str,
    opts: &ParseOptions,
) -> Tokens<TokenStr<'i>> {
    tokenize_bytes(input.as_bytes(), opts)
        .expect("unexpected character encoding error tokenizing &str")
}

/// Parse bytes containing Wolfram Language input into a sequence of tokens.
pub fn tokenize_bytes<'i>(
    input: &'i [u8],
    opts: &ParseOptions,
) -> Result<Tokens<TokenStr<'i>>, UnsafeCharacterEncoding> {
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
) -> ParseResult<Cst<TokenStr<'i>>> {
    parse_bytes_to_cst(input.as_bytes(), opts)
}

/// Parse bytes containing Wolfram Language input into a concrete syntax tree.
pub fn parse_bytes_to_cst<'i>(
    bytes: &'i [u8],
    opts: &ParseOptions,
) -> ParseResult<Cst<TokenStr<'i>>> {
    let mut session = ParserSession::new(bytes, opts);

    session.concrete_parse_expressions()
}

//======================================
// Parse AST
//======================================

/// Parse a string containing Wolfram Language input into an abstract syntax tree.
pub fn parse_to_ast<'i>(
    input: &'i str,
    opts: &ParseOptions,
) -> ParseResult<Ast> {
    parse_bytes_to_ast(input.as_bytes(), opts)
}

/// Parse bytes containing Wolfram Language input into an abstract syntax tree.
pub fn parse_bytes_to_ast<'i>(
    bytes: &'i [u8],
    opts: &ParseOptions,
) -> ParseResult<Ast> {
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
) -> ParseResult<Token<TokenStr<'i>>> {
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

pub(crate) use panic_if_aborted;
