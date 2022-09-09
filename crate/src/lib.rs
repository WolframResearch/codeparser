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
mod source;
mod symbol;
mod token_enum;
mod tokenizer;
mod wl_character;

mod parser;
mod parser_session;

#[cfg(feature = "USE_MATHLINK")]
mod convert_wstp;

#[cfg(feature = "USE_EXPR_LIB")]
mod convert_expr;

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
    }

    pub use src;
}

//======================================
// API
//======================================

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
        // TODO: Should this be a part of the public API as a constant value, or
        //       something else 'symbolic'? E.g. prehaps this shouldn't be a
        //       required parameter of ParserSession::new().
        DEFAULT_TAB_WIDTH,
    },
};

//-----------
// Types
//-----------

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

use crate::parser_session::ParserSession;

/// Parse bytes containing Wolfram Language input into a sequence of tokens.
///
/// # Examples
///
/// Tokenize `2 + 2`:
///
/// ```
/// use wolfram_code_parse::{
///     tokenize_bytes,
///     ParseOptions,
///     node::{NodeContainer, NodeSeq}
/// };
///
/// let nodes = tokenize_bytes(b"2 + 2", &ParseOptions::default());
///
/// /* TODO: assert_eq!(nodes, NodeContainer {
///     nodes: NodeSeq(vec![
///
///     ])
/// }); */
/// ```
pub fn tokenize_bytes(input: &[u8], opts: &ParseOptions) -> NodeContainer {
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
pub fn parse_concrete(input: &str, opts: &ParseOptions) -> ParseResult {
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

use node::NodeContainer;
pub(crate) use panic_if_aborted;
