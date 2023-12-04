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
//! Operation                   | Result             | Input: `&str`       | Input: `&[u8]`
//! ----------------------------|--------------------|---------------------|----------------------
//! Tokenization                | [`NodeSeq<Token>`] | [`tokenize()`]      | [`tokenize_bytes()`]
//! Parse concrete syntax       | [`Cst`]            | [`parse_cst()`]     | [`parse_bytes_cst()`]
//! Parse abstract syntax       | [`Ast`]            | [`parse_ast()`]     | [`parse_bytes_ast()`]
//! Sequence of concrete syntax | [`NodeSeq<Cst>`]   | [`parse_cst_seq()`] | [`parse_bytes_cst_seq()`]
//! Sequence of abstract syntax | [`NodeSeq<Ast>`]   | [`parse_ast_seq()`] | [`parse_bytes_ast_seq()`]
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
pub mod issue;
mod long_names;
pub mod quirks;
pub mod source;
#[doc(hidden)]
pub mod symbol;

pub mod read;
pub mod tokenize;
pub mod parse;

pub mod iter;

mod error;

mod agg;
pub mod ast;
pub mod cst;

pub mod abstract_cst;

#[doc(hidden)]
pub mod fmt_as_expr;

mod feature;

/// Contains modules whose source code is generated dynamically at project build
/// time.
pub(crate) mod generated {
    pub(crate) mod long_names_registration;
    pub(crate) mod precedence_values;
}

mod precedence;
#[doc(hidden)]
pub mod symbols;


#[cfg(test)]
mod tests;

pub mod macros;

mod parse_cst;

//==========================================================
// API
//==========================================================

use std::fmt::Debug;

use wolfram_expr::{Expr, Number};

use crate::{
    abstract_cst::{abstract_cst, aggregate_cst_seq},
    ast::Ast,
    cst::Cst,
    cst::CstSeq,
    issue::{CodeAction, Issue},
    parse_cst::ParseCst,
    source::TOPLEVEL,
    source::{Source, SourceConvention, DEFAULT_TAB_WIDTH},
    tokenize::{
        tokenizer::{
            Tokenizer_nextToken_stringifyAsFile,
            Tokenizer_nextToken_stringifyAsTag, TrackedSourceLocations,
        },
        Token, TokenKind, TokenStr, Tokenizer,
    },
};



//-----------
// Re-exports
//-----------

pub use crate::quirks::QuirkSettings;

pub use crate::tokenize::tokenizer::UnsafeCharacterEncoding;

//======================================
// Types
//======================================

pub struct Container<N> {
    pub kind: ContainerKind,
    pub body: ContainerBody<N>,
    pub metadata: Metadata,
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
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
    Missing(ContainerMissingReason),
}

pub enum ContainerMissingReason {
    EmptyInput,
    UnsafeCharacterEncoding(UnsafeCharacterEncoding),
}

/// A sequence of Nodes
#[derive(Clone, PartialEq)]
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

//--------------------------------------
// ParseResult
//--------------------------------------

pub struct ParseResult<T> {
    /// Tokens, concrete syntax, or abstract syntax.
    pub syntax: T,

    #[doc(hidden)]
    pub unsafe_character_encoding: Option<UnsafeCharacterEncoding>,

    #[doc(hidden)]
    pub fatal_issues: Vec<Issue>,
    #[doc(hidden)]
    pub non_fatal_issues: Vec<Issue>,

    #[doc(hidden)]
    pub tracked: TrackedSourceLocations,
}

//-------------
// ParseOptions
//-------------

pub struct ParseOptions {
    first_line_behavior: FirstLineBehavior,
    src_convention: SourceConvention,
    encoding_mode: EncodingMode,
    tab_width: u32,
    check_issues: bool,
    compute_oob: bool,
    pub quirk_settings: QuirkSettings,
}

impl Default for ParseOptions {
    fn default() -> ParseOptions {
        ParseOptions {
            first_line_behavior: FirstLineBehavior::NotScript,
            src_convention: SourceConvention::LineColumn,
            encoding_mode: EncodingMode::Normal,
            tab_width: DEFAULT_TAB_WIDTH,
            check_issues: true,
            compute_oob: true,
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
            check_issues: true,
            compute_oob: true,
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
///     tokenize, ParseOptions, NodeSeq,
///     macros::token
/// };
///
/// let NodeSeq(tokens) = tokenize("2 + 2", &ParseOptions::default());
///
/// assert_eq!(tokens, &[
///     token![Integer, "2", 1:1-1:2],
///     token![Whitespace, " ", 1:2-1:3],
///     token![Plus, "+", 1:3-1:4],
///     token![Whitespace, " ", 1:4-1:5],
///     token![Integer, "2", 1:5-1:6],
/// ]);
/// ```
pub fn tokenize<'i>(
    input: &'i str,
    opts: &ParseOptions,
) -> NodeSeq<Token<TokenStr<'i>>> {
    tokenize_bytes(input.as_bytes(), opts)
        .expect("unexpected character encoding error tokenizing &str")
}

/// Parse bytes containing Wolfram Language input into a sequence of tokens.
pub fn tokenize_bytes<'i>(
    input: &'i [u8],
    opts: &ParseOptions,
) -> Result<NodeSeq<Token<TokenStr<'i>>>, UnsafeCharacterEncoding> {
    let mut tokenizer = Tokenizer::new(input, opts);

    let mut tokens = Vec::new();

    loop {
        if feature::CHECK_ABORT && crate::abortQ() {
            break;
        }

        let tok = tokenizer.peek_token();

        if tok.tok == TokenKind::EndOfFile {
            break;
        }

        tokens.push(tok);

        tok.skip(&mut tokenizer);
    } // while (true)

    if let Some(flag) = tokenizer.unsafe_character_encoding_flag {
        return Err(flag);
    }

    if let Ok(input) = std::str::from_utf8(tokenizer.input) {
        NodeSeq(tokens) = crate::error::reparse_unterminated_tokens(
            NodeSeq(tokens),
            input,
            usize::try_from(tokenizer.tab_width).unwrap(),
        );
    }

    return Ok(NodeSeq(tokens));
}

//======================================
// Parse CST
//======================================

//--------------------------------------
// Scalar Cst
//--------------------------------------

/// Parse a string containing Wolfram Language input into a concrete syntax tree.
///
/// # Examples
///
/// Parse `2 + 2`:
///
/// ```
/// # use pretty_assertions::assert_eq;
/// use wolfram_parser::{
///     parse_cst, ParseOptions, NodeSeq,
///     cst::{Cst, InfixNode, OperatorNode},
///     parse::operators::InfixOperator,
///     macros::{token, src},
/// };
///
/// let result = parse_cst("2 + 2", &ParseOptions::default());
///
/// assert_eq!(
///     result.syntax,
///     Cst::Infix(InfixNode(OperatorNode {
///         op: InfixOperator::Plus,
///         children: NodeSeq(vec![
///             Cst::Token(token!(Integer, "2", 1:1-2)),
///             Cst::Token(token!(Whitespace, " ", 1:2-3)),
///             Cst::Token(token!(Plus, "+", 1:3-4)),
///             Cst::Token(token!(Whitespace, " ", 1:4-5)),
///             Cst::Token(token!(Integer, "2", 1:5-6))
///         ]),
///     }))
/// );
/// ```
pub fn parse_cst<'i>(
    input: &'i str,
    opts: &ParseOptions,
) -> ParseResult<Cst<TokenStr<'i>>> {
    let result = parse_cst_seq(input, opts);

    let result = expect_single_item(result, "parse_cst", "Cst");

    #[cfg(test)]
    {
        crate::utils::copy_to_clipboard(&format!("{:#?}", result.syntax));
    }

    result
}

/// Parse bytes containing Wolfram Language input into a concrete syntax tree.
pub fn parse_bytes_cst<'i>(
    input: &'i [u8],
    opts: &ParseOptions,
) -> ParseResult<Cst<TokenStr<'i>>> {
    let result = parse_bytes_cst_seq(input, opts);

    expect_single_item(result, "parse_bytes_cst", "Cst")
}

//--------------------------------------
// Sequence of Cst
//--------------------------------------

pub fn parse_cst_seq<'i>(
    input: &'i str,
    opts: &ParseOptions,
) -> ParseResult<CstSeq<TokenStr<'i>>> {
    parse_bytes_cst_seq(input.as_bytes(), opts)
}

pub fn parse_bytes_cst_seq<'i>(
    bytes: &'i [u8],
    opts: &ParseOptions,
) -> ParseResult<CstSeq<TokenStr<'i>>> {
    parse::parse::<ParseCst>(bytes, opts)
}

//======================================
// Parse AST
//======================================

//--------------------------------------
// Scalar Ast
//--------------------------------------

/// Parse a string containing Wolfram Language input into an abstract syntax tree.
///
/// # Examples
///
/// Parse `2 + 2`:
///
/// ```
/// # use pretty_assertions::assert_eq;
/// use wolfram_parser::{
///     parse_ast, ParseOptions,
///     tokenize::{TokenKind, TokenString},
///     source::Span,
///     ast::{Ast, AstMetadata},
///     macros::src,
/// };
///
/// let result = parse_ast("2 + 2", &ParseOptions::default());
///
/// assert_eq!(
///     result.syntax,
///     Ast::Call {
///         head: Box::new(Ast::Leaf {
///             kind: TokenKind::Symbol,
///             input: TokenString::new("Plus"),
///             data: AstMetadata::empty(),
///         }),
///         args: vec![
///             Ast::Leaf {
///                 kind: TokenKind::Integer,
///                 input: TokenString::new("2"),
///                 data: AstMetadata::from_src(Span::from(src!(1:1-2))),
///             },
///             Ast::Leaf {
///                 kind: TokenKind::Integer,
///                 input: TokenString::new("2"),
///                 data: AstMetadata::from_src(Span::from(src!(1:5-6))),
///             },
///         ],
///         data: AstMetadata::from_src(Span::from(src!(1:1-6))),
///     },
/// );
/// ```
pub fn parse_ast<'i>(input: &'i str, opts: &ParseOptions) -> ParseResult<Ast> {
    expect_single_item(
        parse_bytes_ast_seq(input.as_bytes(), opts),
        "parse_ast",
        "Ast",
    )
}

/// Parse bytes containing Wolfram Language input into an abstract syntax tree.
pub fn parse_bytes_ast<'i>(
    bytes: &'i [u8],
    opts: &ParseOptions,
) -> ParseResult<Ast> {
    expect_single_item(
        parse_bytes_ast_seq(bytes, opts),
        "parse_bytes_ast",
        "Ast",
    )
}

//--------------------------------------
// Sequence of Ast
//--------------------------------------

pub fn parse_ast_seq<'i>(
    input: &'i str,
    opts: &ParseOptions,
) -> ParseResult<NodeSeq<Ast>> {
    parse_bytes_ast_seq(input.as_bytes(), opts)
}

pub fn parse_bytes_ast_seq<'i>(
    bytes: &'i [u8],
    opts: &ParseOptions,
) -> ParseResult<NodeSeq<Ast>> {
    let result = parse::parse::<ParseCst>(bytes, opts);

    let ParseResult {
        syntax: nodes,
        unsafe_character_encoding,
        fatal_issues,
        non_fatal_issues,
        tracked,
    } = result;

    let NodeSeq(nodes) = aggregate_cst_seq(nodes);

    let nodes = nodes
        .into_iter()
        .map(|cst| abstract_cst(cst, opts.quirk_settings))
        .collect();

    ParseResult {
        syntax: NodeSeq(nodes),
        unsafe_character_encoding,
        fatal_issues,
        non_fatal_issues,
        tracked,
    }
}

//==========================================================
// LibraryLink
//==========================================================

#[doc(hidden)]
pub fn parse_to_token<'i>(
    bytes: &'i [u8],
    opts: &ParseOptions,
    mode: StringifyMode,
) -> ParseResult<NodeSeq<Token<TokenStr<'i>>>> {
    let mut tokenizer = Tokenizer::new(bytes, opts);

    //
    // Collect all expressions
    //

    let mut exprs: NodeSeq<Token<_>> = NodeSeq::new();

    let token = match mode {
        StringifyMode::Normal => tokenizer.next_token(),
        StringifyMode::Tag => {
            Tokenizer_nextToken_stringifyAsTag(&mut tokenizer)
        },
        StringifyMode::File => {
            Tokenizer_nextToken_stringifyAsFile(&mut tokenizer)
        },
    };

    exprs.push(token);

    return create_parse_result(&tokenizer, exprs);
}

// TODO(cleanup): What is this used for? Perhaps ultimately this is just
//                std::str::from_utf8()?
#[doc(hidden)]
pub fn safe_string<'i>(
    bytes: &'i [u8],
    opts: &ParseOptions,
) -> Result<&'i str, UnsafeCharacterEncoding> {
    let mut tokenizer = Tokenizer::new(bytes, opts);

    //
    // read all characters, just to set unsafeCharacterEncoding flag if necessary
    //
    loop {
        let char = tokenizer.next_source_char(TOPLEVEL);

        if char.isEndOfFile() {
            break;
        }
    } // while (true)

    match tokenizer.unsafe_character_encoding_flag {
        None => {
            // let N = SafeStringNode::new(BufferAndLength::new(self.start, self.end - self.start));
            let str = std::str::from_utf8(tokenizer.input).expect(
                "safeString: unable to convert source input into safe string",
            );

            Ok(str)
        },
        Some(flag) => {
            debug_assert!(
                std::str::from_utf8(tokenizer.input).is_err()
                    || flag == UnsafeCharacterEncoding::BOM
            );

            Err(flag)
        },
    }
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

//======================================
// Macros and helpers
//======================================

macro_rules! panic_if_aborted {
    () => {
        if crate::feature::CHECK_ABORT && crate::abortQ() {
            panic!("aborting parsing by panicking")
        }
    };
}

pub(crate) use panic_if_aborted;

fn expect_single_item<N: Debug>(
    result: ParseResult<NodeSeq<N>>,
    func: &'static str,
    ty: &'static str,
) -> ParseResult<N> {
    let ParseResult {
        syntax: NodeSeq(syntax),
        unsafe_character_encoding,
        fatal_issues,
        non_fatal_issues,
        tracked,
    } = result;

    // FIXME: Make the "error" case hold a type for resuming parsing where this
    //        one left off. ParseResult is a bad name anyway because it sounds
    //        like a type alias for Result<T, ParseError> or something similar.
    //        Maybe ParseData and ResumableParseData? Or ParseData<I, Resume = ()>?
    let [item]: [_; 1] = syntax.try_into().unwrap_or_else(|syntax| {
        panic!("{func}: more than one {ty} in input: {syntax:?}")
    });

    ParseResult {
        syntax: item,
        unsafe_character_encoding,
        fatal_issues,
        non_fatal_issues,
        tracked,
    }
}

fn create_parse_result<N>(tokenizer: &Tokenizer, nodes: N) -> ParseResult<N> {
    let result = ParseResult {
        syntax: nodes,
        unsafe_character_encoding: tokenizer.unsafe_character_encoding_flag,
        fatal_issues: tokenizer.fatal_issues.clone(),
        non_fatal_issues: tokenizer.non_fatal_issues.clone(),
        tracked: tokenizer.tracked.clone(),
    };

    result
}

//======================================
// Formatting Impls
//======================================

impl<N: Debug> Debug for NodeSeq<N> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let NodeSeq(list) = self;

        if cfg!(test) {
            write!(f, "NodeSeq(vec!{:#?})", list)
        } else {
            f.debug_tuple("NodeSeq").field(&self.0).finish()
        }
    }
}
