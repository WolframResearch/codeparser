//! Iterators over source characters, Wolfram characters, and tokens.
//!
//! ## Source Characters
//!
//! Iterate over [`SourceCharacter`]s using [`source_chars()`]:
//!
//! ```
//! use wolfram_parser::{iter::source_chars, source::SourceCharacter};
//!
//! let mut chars = source_chars(r#"2*\[Pi]"#, &Default::default());
//!
//! assert_eq!(chars.next(), Some(SourceCharacter::Char('2')));
//! assert_eq!(chars.next(), Some(SourceCharacter::Char('*')));
//! assert_eq!(chars.next(), Some(SourceCharacter::Char('\\')));
//! assert_eq!(chars.next(), Some(SourceCharacter::Char('[')));
//! assert_eq!(chars.next(), Some(SourceCharacter::Char('P')));
//! assert_eq!(chars.next(), Some(SourceCharacter::Char('i')));
//! assert_eq!(chars.next(), Some(SourceCharacter::Char(']')));
//! assert_eq!(chars.next(), None);
//! ```
//!
//! ## Wolfram Characters
//!
//! Iterate over [`WLCharacter`]s using [`wolfram_chars()`]:
//!
//! ```
//! use wolfram_parser::{iter::wolfram_chars, read::{WLCharacter, Escape}};
//!
//! let mut chars = wolfram_chars(r#"2*\[Pi]"#, &Default::default());
//!
//! assert_eq!(chars.next(), Some(WLCharacter::new('2')));
//! assert_eq!(chars.next(), Some(WLCharacter::new('*')));
//! assert_eq!(chars.next(), Some(WLCharacter::escaped('π', Escape::LongName)));
//! assert_eq!(chars.next(), None);
//! ```
//!
//! ## Tokens
//!
//! Iterate over [`Token`]s using [`tokens()`]:
//!
//! ```
//! use wolfram_parser::{
//!     iter::tokens,
//!     tokenize::{Token, TokenKind},
//!     macros::src,
//! };
//!
//! let mut chars = tokens(r#"2*\[Pi]"#, &Default::default());
//!
//! assert_eq!(chars.next(), Some(Token::new(TokenKind::Integer, "2", src!(1:1-2))));
//! assert_eq!(chars.next(), Some(Token::new(TokenKind::Star, "*", src!(1:2-3))));
//! assert_eq!(chars.next(), Some(Token::new(TokenKind::Symbol, "\\[Pi]", src!(1:3-8))));
//! assert_eq!(chars.next(), None);
//! ```

use crate::{
    read::{code_point::CodePoint, Reader, WLCharacter},
    source::{SourceCharacter, TOPLEVEL},
    tokenize::{Token, TokenKind, TokenStr, Tokenizer},
    ParseOptions,
};

//======================================
// API Functions
//======================================

/// Get an iterator over the [`SourceCharacter`]s in a Wolfram Language input.
pub fn source_chars<'i>(
    input: &'i str,
    opts: &ParseOptions,
) -> SourceChars<'i> {
    SourceChars {
        reader: Reader::new(input.as_bytes(), opts),
    }
}

/// Get an iterator over the [`WLCharacter`]s in a Wolfram Language input.
pub fn wolfram_chars<'i>(
    input: &'i str,
    opts: &ParseOptions,
) -> WolframChars<'i> {
    WolframChars {
        reader: Reader::new(input.as_bytes(), opts),
    }
}

/// Get an iterator over the [`Token`]s in a Wolfram Language input.
pub fn tokens<'i>(input: &'i str, opts: &ParseOptions) -> Tokens<'i> {
    Tokens {
        tokenizer: Tokenizer::new(input.as_bytes(), opts),
    }
}

//======================================
// Types
//======================================

/// Iterator over [`SourceCharacter`]s in a Wolfram Language input.
///
/// Returned by [`source_chars()`].
pub struct SourceChars<'i> {
    reader: Reader<'i>,
}


/// Iterator over [`WLCharacter`]s in a Wolfram Language input.
///
/// Returned by [`wolfram_chars()`].
pub struct WolframChars<'i> {
    reader: Reader<'i>,
}

/// Iterator over [`Token`]s in a Wolfram Language input.
///
/// Returned by [`tokens()`].
pub struct Tokens<'i> {
    tokenizer: Tokenizer<'i>,
}

//=======================================
// Iterator Impls
//=======================================

impl<'i> Iterator for SourceChars<'i> {
    type Item = SourceCharacter;

    fn next(&mut self) -> Option<Self::Item> {
        let SourceChars { reader } = self;

        let char = reader.next_source_char(TOPLEVEL);

        if char != CodePoint::EndOfFile {
            Some(char)
        } else {
            None
        }
    }
}

impl<'i> Iterator for WolframChars<'i> {
    type Item = WLCharacter;

    fn next(&mut self) -> Option<Self::Item> {
        let WolframChars { reader } = self;

        let char = reader.next_wolfram_char(TOPLEVEL);

        if char.point != CodePoint::EndOfFile {
            Some(char)
        } else {
            None
        }
    }
}

impl<'i> Iterator for Tokens<'i> {
    type Item = Token<TokenStr<'i>>;

    fn next(&mut self) -> Option<Self::Item> {
        let Tokens { tokenizer } = self;

        let token = tokenizer.next_token();

        if token.tok != TokenKind::EndOfFile {
            Some(token)
        } else {
            None
        }
    }
}
