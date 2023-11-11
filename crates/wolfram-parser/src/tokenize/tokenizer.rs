//! Tokenizer takes a stream of WL characters and tokenizes them

use std::{collections::HashSet, os::raw::c_int};

use crate::{
    feature,
    issue::{CodeAction, FormatIssue, IssueTag, Severity, SyntaxIssue},
    read::{
        code_point::{
            CodePoint::{Char, *},
            *,
        },
        Escape, InputMark, Reader, WLCharacter,
    },
    source::{
        Buffer, BufferAndLength, Location, NextPolicy, SourceCharacter, Span,
        INSIDE_SLOT, INSIDE_STRINGIFY_AS_FILE, INSIDE_STRINGIFY_AS_TAG,
        TOPLEVEL,
    },
    tokenize::{token_kind::Closer, Token, TokenKind, TokenRef},
    utils::{self, from_fn},
    FirstLineBehavior, ParseOptions,
};

use crate::source::NextPolicyBits::*;

#[derive(Debug)]
pub(crate) struct Tokenizer<'i> {
    reader: Reader<'i>,

    first_line_behavior: FirstLineBehavior,

    pub(crate) GroupStack: Vec<Closer>,

    pub(crate) tracked: TrackedSourceLocations,
}

#[doc(hidden)]
#[derive(Debug, Clone)]
pub struct TrackedSourceLocations {
    pub simple_line_continuations: HashSet<Location>,
    pub complex_line_continuations: HashSet<Location>,
    pub embedded_newlines: HashSet<Location>,
    pub embedded_tabs: HashSet<Location>,
}

#[derive(Debug, Copy, Clone, PartialEq)]
pub enum UnsafeCharacterEncoding {
    // Ok = 0,
    IncompleteUTF8Sequence = 1,
    StraySurrogate = 2,
    ///
    BOM = 3,
}

/// Marker that records where a token started.
///
/// An instance of this type is created when the first character in a new
/// token is read by [`Tokenizer_nextToken()`]. When the last character in a
/// token is read, a call to
/// [`Tokenizer::token(kind, token_start)`][Tokenizer::token] is made to
/// construct a new [`Token`] whose source buffer and [`Span`] start at the specified
/// `TokenStart` instance and end at the current character in the input.
///
/// See also: [`InputMark`], which is a similar type that is used
/// for resetting the input cursor to an earlier state.
#[derive(Debug, Copy, Clone)]
struct TokenStart<'i> {
    buf: Buffer<'i>,
    loc: Location,
}

const _: () = assert!(std::mem::size_of::<TokenStart>() == 24);
const _: () = assert!(std::mem::size_of::<&TokenStart>() == 8);

impl UnsafeCharacterEncoding {
    pub fn as_str(&self) -> &'static str {
        match self {
            UnsafeCharacterEncoding::IncompleteUTF8Sequence => {
                "IncompleteUTF8Sequence"
            },
            UnsafeCharacterEncoding::StraySurrogate => "StraySurrogate",
            UnsafeCharacterEncoding::BOM => "BOM",
            // NOTE: When adding a case here, also update from_str().
        }
    }

    #[doc(hidden)]
    pub fn from_str(string: &str) -> Option<Self> {
        let value = match string {
            "IncompleteUTF8Sequence" => {
                UnsafeCharacterEncoding::IncompleteUTF8Sequence
            },
            "StraySurrogate" => UnsafeCharacterEncoding::StraySurrogate,
            "BOM" => UnsafeCharacterEncoding::BOM,
            _ => return None,
        };

        Some(value)
    }

    #[doc(hidden)]
    pub fn from_missing_reason(missing_reason: &str) -> Option<Self> {
        if !missing_reason.starts_with("UnsafeCharacterEncoding_") {
            return None;
        }

        let variant_name: &str =
            missing_reason.trim_start_matches("UnsafeCharacterEncoding_");

        UnsafeCharacterEncoding::from_str(variant_name)
    }

    #[doc(hidden)]
    pub fn missing_reason(&self) -> String {
        let variant_name: &'static str = self.as_str();

        format!("UnsafeCharacterEncoding_{variant_name}")
    }
}

impl<'i> std::ops::Deref for Tokenizer<'i> {
    type Target = Reader<'i>;

    fn deref(&self) -> &Self::Target {
        &self.reader
    }
}

impl<'i> std::ops::DerefMut for Tokenizer<'i> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.reader
    }
}

impl<'i> Tokenizer<'i> {
    pub(crate) fn new(input: &'i [u8], opts: &ParseOptions) -> Self {
        let ParseOptions {
            first_line_behavior,
            src_convention: _,
            encoding_mode: _,
            tab_width: _,
            check_issues: _,
            compute_oob: _,
            quirk_settings: _,
        } = *opts;

        let mut tokenizer = Tokenizer {
            reader: Reader::new(input, opts),

            first_line_behavior,

            GroupStack: Vec::new(),

            tracked: TrackedSourceLocations {
                simple_line_continuations: HashSet::new(),
                complex_line_continuations: HashSet::new(),
                embedded_newlines: HashSet::new(),
                embedded_tabs: HashSet::new(),
            },
        };

        tokenizer.handle_first_line();

        tokenizer
    }

    fn handle_first_line(&mut self) {
        match self.first_line_behavior {
            FirstLineBehavior::NotScript => {
                return;
            },
            FirstLineBehavior::Check => {
                //
                // Handle the optional #! shebang
                //

                let mut peek = self.peek_token();

                if peek.tok != TokenKind::Hash {
                    // not #!

                    return;
                }

                peek.skip(self);

                peek = self.peek_token();

                if peek.tok != TokenKind::Bang {
                    // not #!

                    return;
                }

                //
                // Definitely a shebang
                //

                peek.skip(self);

                loop {
                    if feature::CHECK_ABORT && crate::abortQ() {
                        break;
                    }

                    let peek = self.peek_token();

                    if peek.tok == TokenKind::EndOfFile {
                        break;
                    }

                    if peek.tok == TokenKind::ToplevelNewline {
                        peek.skip(self);

                        break;
                    }

                    peek.skip(self);
                } // while (true)

                //
                // TODO: if anyone ever asks, then consider providing the shebang as a token
                // but only after BIGCODEMERGE!!
                //
            },
            FirstLineBehavior::Script => {
                //
                // Handle the #! shebang
                //

                let mut peek = self.peek_token();

                if peek.tok != TokenKind::Hash {
                    //
                    // TODO: add to Issues
                    //

                    return;
                }

                peek.skip(self);

                peek = self.peek_token();

                if peek.tok != TokenKind::Bang {
                    //
                    // TODO: add to Issues
                    //

                    return;
                }

                peek.skip(self);

                loop {
                    if feature::CHECK_ABORT && crate::abortQ() {
                        break;
                    }

                    let peek = self.peek_token();

                    if peek.tok == TokenKind::EndOfFile {
                        break;
                    }

                    if peek.tok == TokenKind::ToplevelNewline {
                        peek.skip(self);

                        break;
                    }

                    peek.skip(self);
                } // while (true)
            },
        }
    }

    //==================================
    // Read tokens
    //==================================

    /// Returns the next token in the input without advancing.
    ///
    /// Consecutive calls to `peek_token()` will always return the same token.
    ///
    /// Use [`Tokenizer::next_token()`] to return the current token and advance
    /// to the subsequent token.
    #[must_use]
    pub(crate) fn peek_token(&mut self) -> TokenRef<'i> {
        self.peek_token_with(TOPLEVEL)
    }

    /// Returns the next token in the input without advancing, using the specified
    /// policy settings.
    #[must_use]
    pub(crate) fn peek_token_with(
        &mut self,
        mut policy: NextPolicy,
    ) -> TokenRef<'i> {
        let insideGroup: bool = !self.GroupStack.is_empty();

        //
        // if insideGroup:
        //   returnInternalNewlineMask is 0b100
        // else:
        //   returnInternalNewlineMask is 0b000
        //
        let returnInternalNewlineMask = (insideGroup as u8) << 2;

        policy.remove(NextPolicy::from_bits(returnInternalNewlineMask));

        let mark = self.mark();

        let tok = Tokenizer_nextToken(self, policy);

        // Reset so it is as if we didn't advance.
        self.seek(mark);

        tok
    }

    /// Returns the next token in the input and advances.
    ///
    /// Precondition: buffer is pointing to current token
    ///
    /// Postcondition: buffer is pointing to next token
    ///
    /// Example:
    ///
    /// ```text
    /// memory: 1+\[Alpha]-2
    ///           ^
    ///           buffer
    /// ```
    ///
    /// after calling `next_token()`:
    ///
    /// ```text
    /// memory: 1+\[Alpha]-2
    ///                   ^
    ///                   buffer
    /// ```
    ///
    /// and `\[Alpha]` is returned.
    #[must_use]
    pub(crate) fn next_token(&mut self) -> TokenRef<'i> {
        Tokenizer_nextToken(self, crate::source::TOPLEVEL)
    }

    //==================================
    // Create tokens
    //==================================

    /// Construct a new token whose source buffer and location begin at `start`
    /// and end at the current buffer and source location.
    fn token<T: Into<TokenKind>>(
        &self,
        tok: T,
        start: &TokenStart<'i>,
    ) -> TokenRef<'i> {
        let tok = tok.into();

        let buf = self.get_token_buffer_and_length(start.buf);

        let span = self.get_token_span(start.loc);

        Token::new2(tok, buf, span)
    }

    /// Construct a new token whose source buffer and location are exactly the
    /// character located at `at`.
    fn error_token_at<T: Into<TokenKind>>(
        &self,
        tok: T,
        at: &TokenStart<'i>,
    ) -> TokenRef<'i> {
        let tok = tok.into();

        debug_assert!(tok.isError());

        let buf = BufferAndLength::between(at.buf, at.buf);

        debug_assert_eq!(buf.buf.len(), 0);

        let span = Span::at(at.loc);

        Token::new2(tok, buf, span)
    }

    fn get_token_span(&self, tok_start_loc: Location) -> Span {
        debug_assert!(tok_start_loc <= self.SrcLoc);

        return Span::new(tok_start_loc, self.SrcLoc);
    }

    fn get_token_buffer_and_length(
        &self,
        tok_start_buf: Buffer<'i>,
    ) -> BufferAndLength<'i> {
        // return BufferAndLength::new(tokStartBuf, session.buffer - tokStartBuf);

        BufferAndLength::between(tok_start_buf, self.buffer())
    }

    //==================================
    // Tracked locations
    //==================================

    fn addSimpleLineContinuation(&mut self, loc: Location) {
        self.tracked.simple_line_continuations.insert(loc);
    }

    fn addComplexLineContinuation(&mut self, loc: Location) {
        self.tracked.complex_line_continuations.insert(loc);
    }

    fn addEmbeddedNewline(&mut self, loc: Location) {
        self.tracked.embedded_newlines.insert(loc);
    }

    fn addEmbeddedTab(&mut self, loc: Location) {
        self.tracked.embedded_tabs.insert(loc);
    }
}

impl TrackedSourceLocations {
    #[allow(dead_code)]
    #[doc(hidden)]
    pub fn to_nodes(&self) -> [HashSet<Location>; 4] {
        let TrackedSourceLocations {
            simple_line_continuations,
            complex_line_continuations,
            embedded_newlines,
            embedded_tabs,
        } = self;

        [
            simple_line_continuations.clone(),
            complex_line_continuations.clone(),
            embedded_newlines.clone(),
            embedded_tabs.clone(),
        ]
    }
}

struct NumberTokenizationContext {
    InvalidBase: bool,

    UnrecognizedDigit: bool,

    NegativeExponent: bool,

    Real: bool,

    NonZeroExponentDigitCount: u32,
    //
    // Use the convention that base of 0 means the default, unspecified base
    //
    Base: c_int,
}

// TODO: Why does putting this type in the root module cause Rust Analyzer
//             to not work on this file?
// pub mod handler {
type HandlerFunction = for<'p, 'i> fn(
    session: &'p mut Tokenizer<'i>,
    // NOTE: Passing `token_start` by reference instead of by value is an
    //       optimization. At time of writing TokenStart is 32 bytes in size,
    //       but a reference is only 8.
    token_start: &TokenStart<'i>,
    c: WLCharacter,
    policy: NextPolicy,
) -> TokenRef<'i>;
// }

/// Lookup table for handling ASCII characters that start a token.
///
/// This lookup table is equivalent to the match statement used in this `const`
/// definition to populate the table. However, using a lookup table instead of
/// a match is an optimization—doing an offset jump is more efficient than a
/// series of comparisions to find the right match arm.
///
/// See also `CHARACTER_DECODER_HANDLER_TABLE` in character_decoder.rs.
#[rustfmt::skip]
const TOKENIZER_HANDLER_TABLE: [HandlerFunction; 128] = from_fn!(
    [HandlerFunction, 128],
    |index: usize| {
        let index = index as u8;

        match index {
            0..=9 => Tokenizer_nextToken_uncommon,
            10 => Tokenizer_handleLineFeed,
            11..=31 => Tokenizer_nextToken_uncommon,
            32 => Tokenizer_handleSpace,
            33 => Tokenizer_nextToken_uncommon,      // !
            34 => Tokenizer_handleString,            // "
            35 => Tokenizer_nextToken_uncommon,      // #
            36 => Tokenizer_handleSymbol,            // $
            37..=43 => Tokenizer_nextToken_uncommon,
            44 => Tokenizer_handleComma,             // ,
            45 => Tokenizer_handleMinus,             // -
            46..=47 => Tokenizer_nextToken_uncommon,
            48..=57 => Tokenizer_handleNumber,       // 0-9
            58..=64 => Tokenizer_nextToken_uncommon,
            65..=90 => Tokenizer_handleSymbol,       // A-Z
            91 => Tokenizer_handleOpenSquare,        // [
            92 => Tokenizer_nextToken_uncommon,      // \
            93 => Tokenizer_handleCloseSquare,       // ]
            94..=96 => Tokenizer_nextToken_uncommon,
            97..=122 => Tokenizer_handleSymbol,      // a-z
            123 => Tokenizer_handleOpenCurly,        // {
            124 => Tokenizer_nextToken_uncommon,     // |
            125 => Tokenizer_handleCloseCurly,       // }
            126..=127 => Tokenizer_nextToken_uncommon,
            // Not a valid ASCII character
            128..=255 => panic!(),
        }
    }
);

pub(crate) const ASCII_VTAB: char = '\x0B';
pub(crate) const ASCII_FORM_FEED: char = '\x0C';

fn Tokenizer_nextToken<'i>(
    session: &mut Tokenizer<'i>,
    policy: NextPolicy,
) -> TokenRef<'i> {
    let token_start = &TokenStart {
        buf: session.buffer(),
        loc: session.SrcLoc,
    };

    let c = Tokenizer_nextWLCharacter(session, token_start, policy);

    let point: CodePoint = c.to_point();

    if !point.is_ascii() {
        return Tokenizer_nextToken_uncommon(session, token_start, c, policy);
    }

    let index = usize::try_from(point.as_i32()).unwrap();

    let func = TOKENIZER_HANDLER_TABLE[index];
    return func(session, token_start, c, policy);
}

fn Tokenizer_nextToken_uncommon<'i>(
    session: &mut Tokenizer<'i>,
    token_start: &TokenStart<'i>,
    c: WLCharacter,
    policy: NextPolicy,
) -> TokenRef<'i> {
    match c.to_point() {
        Char(_) => (),
        EndOfFile => {
            return session.token(TokenKind::EndOfFile, token_start);
        },
        Unsafe1ByteUtf8Sequence
        | Unsafe2ByteUtf8Sequence
        | Unsafe3ByteUtf8Sequence => {
            //
            // This will be disposed before the user sees it
            //

            return session
                .token(TokenKind::Error_UnsafeCharacterEncoding, token_start);
        },
        _ => (),
    }

    match c.to_point() {
        //
        // all single-byte characters
        //
        // most control characters are letterlike
        // jessef: There may be such a thing as *too* binary-safe...
        //
        Char('`' |
        'a' | 'b' | 'c' | 'd' | 'e' | 'f' | 'g' | 'h' | 'i' | 'j' | 'k' | 'l' | 'm' |
        'n' | 'o' | 'p' | 'q' | 'r' | 's' | 't' | 'u' | 'v' | 'w' | 'x' | 'y' | 'z' |
        '\x00' | '\x01' | '\x02' | '\x03' | '\x04' | '\x05' | '\x06' | /*    \x07*/
        '\x08' | /*    \x09*/ /*    \x0a*/ /*    \x0b*/ /*    \x0c*/ /*    \x0d*/ '\x0e' | '\x0f' |
        '\x10' | '\x11' | '\x12' | '\x13' | '\x14' | '\x15' | '\x16' | '\x17' |
        '\x18' | '\x19' | '\x1a' | '\x1b' | '\x1c' | '\x1d' | '\x1e' | '\x1f') => {

            return Tokenizer_handleSymbol(session, token_start, c, policy);
        }
        Char(CODEPOINT_BEL | CODEPOINT_DEL) => {
            return session.token(TokenKind::Error_UnhandledCharacter, token_start);
        }
        Char('\t') => {
            // MUSTTAIL
            return session.token(TokenKind::Whitespace, token_start);
        }
        Char(ASCII_VTAB | ASCII_FORM_FEED) => {

//            MUSTTAIL
            return Tokenizer_handleStrangeWhitespace(session, token_start, c, policy);
        }
        Char('\r') => {

            incr_diagnostic!(Tokenizer_NewlineCount);

            //
            // Return INTERNALNEWLINE or TOPLEVELNEWLINE, depending on policy
            //
            return session.token(
                TokenKind::newline_with_policy(policy), token_start);
        }
        Char('(') => {

//            MUSTTAIL
            return Tokenizer_handleOpenParen(session, token_start, c, policy);
        }
        Char(')') => {

            incr_diagnostic!(Tokenizer_CloseParenCount);

            return session.token(TokenKind::CloseParen, token_start);
        }
        Char('+') => {

//            MUSTTAIL
            return Tokenizer_handlePlus(session, token_start, c, policy);
        }
        Char('^') => {

//            MUSTTAIL
            return Tokenizer_handleCaret(session, token_start, c, policy);
        }
        Char('=') => {

//            MUSTTAIL
            return Tokenizer_handleEqual(session, token_start, c, policy);
        }
        Char(';') => {

//            MUSTTAIL
            return Tokenizer_handleSemi(session, token_start, c, policy);
        }
        Char(':') => {

//            MUSTTAIL
            return Tokenizer_handleColon(session, token_start, c, policy);
        }
        Char('#') => {

//            MUSTTAIL
            return Tokenizer_handleHash(session, token_start, c, policy);
        }
        Char('&') => {

//            MUSTTAIL
            return Tokenizer_handleAmp(session, token_start, c, policy);
        }
        Char('!') => {

//            MUSTTAIL
            return Tokenizer_handleBang(session, token_start, c, policy);
        }
        Char('%') => {

//            MUSTTAIL
            return Tokenizer_handlePercent(session, token_start, c, policy);
        }
        Char('\'') => {

            return session.token(TokenKind::SingleQuote, token_start);
        }
        Char('*') => {

//            MUSTTAIL
            return Tokenizer_handleStar(session, token_start, c, policy);
        }
        Char('.') => {

//            MUSTTAIL
            return Tokenizer_handleDot(session, token_start, c, policy);
        }
        Char('/') => {

//            MUSTTAIL
            return Tokenizer_handleSlash(session, token_start, c, policy);
        }
        Char('<') => {

//            MUSTTAIL
            return Tokenizer_handleLess(session, token_start, c, policy);
        }
        Char('>') => {

//            MUSTTAIL
            return Tokenizer_handleGreater(session, token_start, c, policy);
        }
        Char('?') => {

//            MUSTTAIL
            return Tokenizer_handleQuestion(session, token_start, c, policy);
        }
        Char('@') => {

//            MUSTTAIL
            return Tokenizer_handleAt(session, token_start, c, policy);
        }
        Char('\\') => {

//            MUSTTAIL
            return Tokenizer_handleUnhandledBackslash(session, token_start, c, policy);
        }
        Char('_') => {

//            MUSTTAIL
            return Tokenizer_handleUnder(session, token_start, c, policy);
        }
        Char('|') => {

//            MUSTTAIL
            return Tokenizer_handleBar(session, token_start, c, policy);
        }
        Char('~') => {
            // MUSTTAIL
            return Tokenizer_handleTilde(session, token_start, c, policy);
        }
        Char(CODEPOINT_LINEARSYNTAX_BANG) => {
            return session.token(TokenKind::LinearSyntax_Bang, token_start);
        }
        Char(CODEPOINT_LINEARSYNTAX_OPENPAREN) => {
            // MUSTTAIL
            return Tokenizer_handleMBLinearSyntaxBlob(session, token_start, c, policy);
        }
        _ => ()
    }


    if c.isMBLinearSyntax() {
        //        MUSTTAIL
        return Tokenizer_handleNakedMBLinearSyntax(
            session,
            token_start,
            c,
            policy,
        );
    }

    if c.isMBUninterpretable() {
        return session.token(TokenKind::Error_UnhandledCharacter, token_start);
    }

    if c.isMBStrangeWhitespace() {
        //        MUSTTAIL
        return Tokenizer_handleMBStrangeWhitespace(
            session,
            token_start,
            c,
            policy,
        );
    }

    if c.isMBWhitespace() {
        return session.token(TokenKind::Whitespace, token_start);
    }

    if c.isMBStrangeNewline() {
        //        MUSTTAIL
        return Tokenizer_handleMBStrangeNewline(
            session,
            token_start,
            c,
            policy,
        );
    }

    if c.isMBNewline() {
        //
        // Return INTERNALNEWLINE or TOPLEVELNEWLINE, depending on policy
        //
        return session
            .token(TokenKind::newline_with_policy(policy), token_start);
    }

    if c.isMBPunctuation() {
        //        MUSTTAIL
        return Tokenizer_handleMBPunctuation(session, token_start, c, policy);
    }

    if c.isMBStringMeta() {
        return session.token(TokenKind::Error_UnhandledCharacter, token_start);
    }

    //
    // if nothing else, then it is letterlike
    //

    assert!(c.isMBLetterlike());

    //    MUSTTAIL
    return Tokenizer_handleSymbol(session, token_start, c, policy);
}

pub(crate) fn Tokenizer_nextToken_stringifyAsTag<'i>(
    session: &mut Tokenizer<'i>,
) -> TokenRef<'i> {
    let token_start = &TokenStart {
        buf: session.buffer(),
        loc: session.SrcLoc,
    };

    let policy = INSIDE_STRINGIFY_AS_TAG;

    let c = Tokenizer_nextWLCharacter(session, token_start, policy);

    match c.to_point() {
        EndOfFile => {
            //
            // EndOfFile is special, so invent source
            //

            return session
                .error_token_at(TokenKind::Error_ExpectedTag, token_start);
        },
        Char('\n' | '\r') | CRLF => {
            //
            // Newline is special, so invent source
            //

            return session
                .error_token_at(TokenKind::Error_ExpectedTag, token_start);
        },
        Char('"') => {
            return Tokenizer_handleString(session, token_start, c, policy);
        },
        //
        // Default
        //
        _ => {
            return Tokenizer_handleString_stringifyAsTag(
                session,
                token_start,
                c,
                policy,
            );
        },
    }
}

//
// Use SourceCharacters here, not WLCharacters
//
pub(crate) fn Tokenizer_nextToken_stringifyAsFile<'i>(
    session: &mut Tokenizer<'i>,
) -> TokenRef<'i> {
    let token_start = &TokenStart {
        buf: session.buffer(),
        loc: session.SrcLoc,
    };

    let policy = INSIDE_STRINGIFY_AS_FILE;

    let c = session.next_source_char(policy);

    match c {
        EndOfFile => {
            return session
                .error_token_at(TokenKind::Error_ExpectedFile, token_start);
        },
        Char('\n' | '\r') | CRLF => {
            //
            // Stringifying as a file can span lines
            //
            // Something like  a >>
            //                    b
            //
            // should work
            //
            // Do not use TokenKind::Error_EMPTYSTRING here
            //

            //
            // Return INTERNALNEWLINE or TOPLEVELNEWLINE, depending on policy
            //
            return session
                .token(TokenKind::newline_with_policy(policy), token_start);
        },
        Char(' ' | '\t') => {
            //
            // There could be space, something like  << abc
            //
            // or something like:
            // a >>
            //   b
            //
            return session.token(TokenKind::Whitespace, token_start);
        },
        Char('"') => {
            return Tokenizer_handleString(
                session,
                token_start,
                WLCharacter::new(c),
                policy,
            );
        },
        //
        // Default case
        //
        _ => {
            return Tokenizer_handleString_stringifyAsFile(
                session,
                token_start,
                c,
                policy,
            );
        },
    }
}

pub(crate) fn Tokenizer_currentToken_stringifyAsTag<'i>(
    session: &mut Tokenizer<'i>,
) -> TokenRef<'i> {
    let mark = session.mark();

    let Tok = Tokenizer_nextToken_stringifyAsTag(session);

    session.seek(mark);

    return Tok;
}

pub(crate) fn Tokenizer_currentToken_stringifyAsFile<'i>(
    session: &mut Tokenizer<'i>,
) -> TokenRef<'i> {
    let mark = session.mark();

    let Tok = Tokenizer_nextToken_stringifyAsFile(session);

    session.seek(mark);

    return Tok;
}

//
// Handling line continuations belongs in some layer strictly above CharacterDecoder and below Tokenizer.
//
// Some middle layer that deals with "parts" of a token.
//
fn Tokenizer_nextWLCharacter<'i>(
    session: &mut Tokenizer<'i>,
    // TODO(cleanup): Just take a Location, token_start.buf is never used
    token_start: &TokenStart<'i>,
    policy: NextPolicy,
) -> WLCharacter {
    incr_diagnostic!(Tokenizer_LineContinuationCount);

    let mut c = session.next_wolfram_char(policy);

    loop {
        if !c.point.is_line_continuation() {
            return c;
        }

        c = session.peek_wolfram_char(policy);

        //
        // Even though strings preserve the whitespace after a line continuation, and
        // e.g., integers do NOT preserve the whitespace after a line continuation,
        // we do not need to worry about that here.
        //
        // There are no choices to be made here.
        // All whitespace after a line continuation can be ignored for the purposes of tokenization
        //
        while c.isWhitespace() {
            if session.compute_oob {
                if c.point == '\t' {
                    if policy.contains(STRING_OR_COMMENT) {
                        //
                        // It is possible to have e.g.:
                        //
                        //"a\
                        //<tab>b"
                        //
                        // where the embedded tab gets consumed by the whitespace loop after the line continuation.
                        //
                        // Must still count the embedded tab

                        session.addEmbeddedTab(token_start.loc);
                    }
                }
            }

            session.next_wolfram_char(policy);

            c = session.peek_wolfram_char(policy);
        }

        if session.compute_oob {
            if policy.contains(TRACK_LC) {
                if policy.contains(STRING_OR_COMMENT) {
                    session.addComplexLineContinuation(token_start.loc);
                } else {
                    session.addSimpleLineContinuation(token_start.loc);
                }
            }
        }

        session.next_wolfram_char(policy);
    } // loop
}

fn Tokenizer_currentWLCharacter<'i>(
    session: &mut Tokenizer<'i>,
    token_start: &TokenStart<'i>,
    mut policy: NextPolicy,
) -> WLCharacter {
    let mark = session.mark();

    //
    //
    //
    policy.remove(TRACK_LC);

    let c = Tokenizer_nextWLCharacter(session, token_start, policy);

    session.seek(mark);

    return c;
}

fn Tokenizer_handleComma<'i>(
    session: &mut Tokenizer<'i>,
    token_start: &TokenStart<'i>,
    _firstChar: WLCharacter,
    _policy: NextPolicy,
) -> TokenRef<'i> {
    incr_diagnostic!(Tokenizer_CommaCount);

    return session.token(TokenKind::Comma, token_start);
}

fn Tokenizer_handleLineFeed<'i>(
    session: &mut Tokenizer<'i>,
    token_start: &TokenStart<'i>,
    _firstChar: WLCharacter,
    policy: NextPolicy,
) -> TokenRef<'i> {
    incr_diagnostic!(Tokenizer_NewlineCount);

    //
    // Return INTERNALNEWLINE or TOPLEVELNEWLINE, depending on policy
    //
    return session.token(TokenKind::newline_with_policy(policy), token_start);
}

fn Tokenizer_handleOpenSquare<'i>(
    session: &mut Tokenizer<'i>,
    token_start: &TokenStart<'i>,
    _firstChar: WLCharacter,
    _policy: NextPolicy,
) -> TokenRef<'i> {
    incr_diagnostic!(Tokenizer_OpenSquareCount);

    return session.token(TokenKind::OpenSquare, token_start);
}

fn Tokenizer_handleOpenCurly<'i>(
    session: &mut Tokenizer<'i>,
    token_start: &TokenStart<'i>,
    _firstChar: WLCharacter,
    _policy: NextPolicy,
) -> TokenRef<'i> {
    incr_diagnostic!(Tokenizer_OpenCurlyCount);

    return session.token(TokenKind::OpenCurly, token_start);
}

fn Tokenizer_handleSpace<'i>(
    session: &mut Tokenizer<'i>,
    token_start: &TokenStart<'i>,
    _firstChar: WLCharacter,
    _policy: NextPolicy,
) -> TokenRef<'i> {
    incr_diagnostic!(Tokenizer_WhitespaceCount);

    return session.token(TokenKind::Whitespace, token_start);
}

fn Tokenizer_handleCloseSquare<'i>(
    session: &mut Tokenizer<'i>,
    token_start: &TokenStart<'i>,
    _firstChar: WLCharacter,
    _policy: NextPolicy,
) -> TokenRef<'i> {
    incr_diagnostic!(Tokenizer_CloseSquareCount);

    return session.token(TokenKind::CloseSquare, token_start);
}

fn Tokenizer_handleCloseCurly<'i>(
    session: &mut Tokenizer<'i>,
    token_start: &TokenStart<'i>,
    _firstChar: WLCharacter,
    _policy: NextPolicy,
) -> TokenRef<'i> {
    incr_diagnostic!(Tokenizer_CloseCurlyCount);

    return session.token(TokenKind::CloseCurly, token_start);
}

fn Tokenizer_handleStrangeWhitespace<'i>(
    session: &mut Tokenizer<'i>,
    token_start: &TokenStart<'i>,
    c: WLCharacter,
    _policy: NextPolicy,
) -> TokenRef<'i> {
    assert!(c.isStrangeWhitespace());

    if session.check_issues {
        add_unexpected_char_issue(
            session,
            c,
            token_start.loc,
            IssueTag::UnexpectedSpaceCharacter,
            0.95,
        )
    }

    return session.token(TokenKind::Whitespace, token_start);
}

//
// Use SourceCharacters here, not WLCharacters
//
// Comments deal with (**) SourceCharacters
// Escaped characters do not work
//
// Important to process SourceCharacters here: (* \\.28\\.2a *)
//
fn Tokenizer_handleComment<'i>(
    session: &mut Tokenizer<'i>,
    token_start: &TokenStart<'i>,
    mut c: SourceCharacter,
    mut policy: NextPolicy,
) -> TokenRef<'i> {
    //
    // comment is already started
    //

    assert!(c == '*');

    session.next_source_char(policy);

    incr_diagnostic!(Tokenizer_CommentCount);

    policy |= STRING_OR_COMMENT;

    let mut depth = 1;

    c = session.next_source_char(policy);

    loop {
        //
        // No need to check for comment length
        //

        match c {
            Char('(') => {
                c = session.next_source_char(policy);

                if c == '*' {
                    depth = depth + 1;

                    c = session.next_source_char(policy);
                }
            },
            Char('*') => {
                c = session.next_source_char(policy);

                if c == ')' {
                    // This comment is closing

                    depth = depth - 1;

                    if depth == 0 {
                        return session.token(TokenKind::Comment, token_start);
                    }

                    c = session.next_source_char(policy);
                }
            },
            EndOfFile => {
                return session
                    .token(TokenKind::Error_UnterminatedComment, token_start);
            },
            Char('\n' | '\r') | CRLF => {
                if session.compute_oob {
                    session.addEmbeddedNewline(token_start.loc);
                }

                c = session.next_source_char(policy);
            },
            Char('\t') => {
                if session.compute_oob {
                    session.addEmbeddedTab(token_start.loc);
                }

                c = session.next_source_char(policy);
            },
            _ => {
                c = session.next_source_char(policy);
            },
        }
    } // loop
}

fn Tokenizer_handleMBLinearSyntaxBlob<'i>(
    session: &mut Tokenizer<'i>,
    token_start: &TokenStart<'i>,
    mut c: WLCharacter,
    policy: NextPolicy,
) -> TokenRef<'i> {
    assert!(c.to_point() == CODEPOINT_LINEARSYNTAX_OPENPAREN);

    let mut depth = 1;

    c = Tokenizer_nextWLCharacter(session, token_start, policy);

    loop {
        match c.to_point() {
            Char(CODEPOINT_LINEARSYNTAX_OPENPAREN) => {
                depth = depth + 1;

                c = Tokenizer_nextWLCharacter(session, token_start, policy);
            },
            Char(CODEPOINT_LINEARSYNTAX_CLOSEPAREN) => {
                depth = depth - 1;

                if depth == 0 {
                    return session
                        .token(TokenKind::LinearSyntaxBlob, token_start);
                }

                c = Tokenizer_nextWLCharacter(session, token_start, policy);
            },
            EndOfFile => {
                return session.token(
                    TokenKind::Error_UnterminatedLinearSyntaxBlob,
                    token_start,
                );
            },
            _ => {
                c = Tokenizer_nextWLCharacter(session, token_start, policy);
            },
        }
    } // loop
}

//
// a segment is: [a-z$]([a-z$0-9])*
// a symbol is: (segment)?(`segment)*
//
fn Tokenizer_handleSymbol<'i>(
    session: &mut Tokenizer<'i>,
    token_start: &TokenStart<'i>,
    mut c: WLCharacter,
    policy: NextPolicy,
) -> TokenRef<'i> {
    assert!(c.to_point() == '`' || c.isLetterlike() || c.isMBLetterlike());

    incr_diagnostic!(Tokenizer_SymbolCount);

    if c.isLetterlike() || c.isMBLetterlike() {
        c = Tokenizer_handleSymbolSegment(
            session,
            token_start,
            token_start.buf,
            token_start.loc,
            c,
            policy,
        );
    }

    //
    // if c == '`', then buffer is pointing past ` now
    //

    loop {
        if c.to_point() != '`' {
            break;
        }

        if session.check_issues && policy.contains(INSIDE_SLOT) {
            //
            // Something like  #`a
            //
            // It's hard to keep track of the ` characters, so just report the entire symbol. Oh well
            //

            let I = SyntaxIssue(
                IssueTag::UndocumentedSlotSyntax,
                "The name following ``#`` is not documented to allow the **`** character.".into(),
                Severity::Warning,
                session.get_token_span(token_start.loc),
                0.33,
                vec![],
                vec![],
            );

            session.addIssue(I);
        }

        c = Tokenizer_currentWLCharacter(session, token_start, policy);

        if c.isLetterlike() || c.isMBLetterlike() {
            let letterlikeBuf = session.buffer();
            let letterlikeLoc = session.SrcLoc;

            Tokenizer_nextWLCharacter(session, token_start, policy);

            c = Tokenizer_handleSymbolSegment(
                session,
                token_start,
                letterlikeBuf,
                letterlikeLoc,
                c,
                policy,
            );
        } else {
            //
            // Something like  a`1
            //

            return session
                .token(TokenKind::Error_ExpectedLetterlike, token_start);
        }
    } // while

    return session.token(
        if policy.contains(INSIDE_SLOT) {
            TokenKind::String
        } else {
            TokenKind::Symbol
        },
        token_start,
    );
}

//
// Precondition: currentWLCharacter is letterlike
// Postcondition: buffer is pointing to first NON-SYMBOLSEGMENT character after all symbol segment characters
//
// return: the first NON-SYMBOLSEGMENT character after all symbol segment characters
//
fn Tokenizer_handleSymbolSegment<'i>(
    session: &mut Tokenizer<'i>,
    token_start: &TokenStart<'i>,
    _charBuf: Buffer,
    mut charLoc: Location,
    mut c: WLCharacter,
    policy: NextPolicy,
) -> WLCharacter {
    assert!(c.isLetterlike() || c.isMBLetterlike());

    if session.check_issues {
        if c.to_point() == '$' {
            if policy.contains(INSIDE_SLOT) {
                //
                // Something like  #$a
                //

                let I = SyntaxIssue(
                    IssueTag::UndocumentedSlotSyntax,
                    "The name following ``#`` is not documented to allow the ``$`` character."
                        .to_owned(),
                    Severity::Warning,
                    session.get_token_span(charLoc),
                    0.33,
                    vec![],
                    vec![],
                );

                session.addIssue(I);
            }
        } else if c.isStrangeLetterlike() {
            add_unexpected_char_issue(
                session,
                c,
                charLoc,
                IssueTag::UnexpectedLetterlikeCharacter,
                0.85,
            )
        } else if c.isMBStrangeLetterlike() {
            add_unexpected_char_issue(
                session,
                c,
                charLoc,
                IssueTag::UnexpectedLetterlikeCharacter,
                0.80,
            )
        } else if !c.isAlpha() {
            if policy.contains(INSIDE_STRINGIFY_AS_TAG) {
                //
                // Something like  a::\[Beta]
                //

                let I = SyntaxIssue(
                    IssueTag::UnexpectedCharacter,
                    "The tag has non-alphanumeric source characters."
                        .to_owned(),
                    Severity::Warning,
                    Span::new(charLoc, session.SrcLoc),
                    0.85,
                    vec![],
                    vec![],
                );

                session.addIssue(I);
            }
        }
    }

    charLoc = session.SrcLoc;

    c = Tokenizer_currentWLCharacter(session, token_start, policy);

    loop {
        if c.isDigit() {
            Tokenizer_nextWLCharacter(session, token_start, policy);

            charLoc = session.SrcLoc;

            c = Tokenizer_currentWLCharacter(session, token_start, policy);
        } else if c.isLetterlike() || c.isMBLetterlike() {
            Tokenizer_nextWLCharacter(session, token_start, policy);

            if session.check_issues {
                if c.to_point() == '$' {
                    if policy.contains(INSIDE_SLOT) {
                        //
                        // Something like  #$a
                        //

                        let I = SyntaxIssue(
                            IssueTag::UndocumentedSlotSyntax,
                            format!("The name following ``#`` is not documented to allow the ``$`` character."),
                            Severity::Warning,
                            session.get_token_span( charLoc),
                            0.33,
                            vec![],
                            vec![],
                        );

                        session.addIssue(I);
                    }
                } else if c.isStrangeLetterlike() {
                    add_unexpected_char_issue(
                        session,
                        c,
                        charLoc,
                        IssueTag::UnexpectedLetterlikeCharacter,
                        0.85,
                    )
                } else if c.isMBStrangeLetterlike() {
                    add_unexpected_char_issue(
                        session,
                        c,
                        charLoc,
                        IssueTag::UnexpectedLetterlikeCharacter,
                        0.80,
                    )
                } else if !c.isAlphaOrDigit() {
                    if policy.contains(INSIDE_STRINGIFY_AS_TAG) {
                        //
                        // Something like  a::b\[Beta]
                        //

                        let I = SyntaxIssue(
                            IssueTag::UnexpectedCharacter,
                            "The tag has non-alphanumeric source characters."
                                .to_owned(),
                            Severity::Warning,
                            Span::new(charLoc, session.SrcLoc),
                            0.85,
                            vec![],
                            vec![],
                        );

                        session.addIssue(I);
                    }
                }
            }

            charLoc = session.SrcLoc;

            c = Tokenizer_currentWLCharacter(session, token_start, policy);
        } else if c.to_point() == '`' {
            //
            // Advance past trailing `
            //

            Tokenizer_nextWLCharacter(session, token_start, policy);

            break;
        } else {
            break;
        }
    } // while

    return c;
}

fn Tokenizer_handleString<'i>(
    session: &mut Tokenizer<'i>,
    token_start: &TokenStart<'i>,
    mut c: WLCharacter,
    mut policy: NextPolicy,
) -> TokenRef<'i> {
    assert!(c.to_point() == '"');

    if session.check_issues && policy.contains(INSIDE_SLOT) {
        //
        // Something like  #"a"
        //

        let I = SyntaxIssue(
            IssueTag::UndocumentedSlotSyntax,
            format!("The name following ``#`` is not documented to allow the ``\"`` character."),
            Severity::Warning,
            session.get_token_span(token_start.loc),
            0.33,
            vec![],
            vec![],
        );

        session.addIssue(I);
    }

    let mut quot_offset: Option<usize> = None;
    let mut fast = false;
    let mut terminated = false;

    if feature::FAST_STRING_SCAN
        && !session.compute_oob
        && !session.check_issues
        && !feature::COMPUTE_SOURCE
    {
        //
        // !CHECK_ISSUES (so do not need to warn about strange SourceCharacters)
        // !COMPUTE_OOB (so do not need to care about embedded newlines or tabs)
        // !COMPUTE_SOURCE (so do not need to keep track of line and column information)
        //
        // FAST_STRING_SCAN (as a final check that skipping bad SourceCharacters and WLCharacters is ok)
        //

        //
        // The idea is to use memchr to scan for the next '"' character byte and then just jump to it.
        //
        // This is faster than explicitly calling TheCharacterDecoder->nextWLCharacter0 over and over again.
        //
        // Diagnostics that count SourceCharacters and WLCharacters will not be accurate inside of fast strings.
        //
        // Bad SourceCharacters will not be detected. This means that incomplete sequences, stray surrogates, and BOM will not be reported.
        //
        // Bad WLCharacters will not be detected. This means that badly escaped characters will not be reported.
        //

        let quot_relative_offset = memchr::memchr(b'"', session.buffer());

        quot_offset = quot_relative_offset
            .map(|buffer_offset| session.offset + buffer_offset);

        if let Some(quot_offset) = quot_offset {
            let prev_char = match quot_offset.checked_sub(1) {
                Some(index) => Some(session.buffer_at(index)[0]),
                None => None,
            };

            if prev_char != Some(b'\\') {
                //
                // first double-quote character is NOT preceded by a backslash character
                //

                fast = true;
                terminated = true;
            } else {
                //
                // there is a backslash character, so fall-through to SLOW
                //

                fast = false;
                terminated = true;
            }
        } else {
            //
            // unterminated, so fall-through to SLOW
            //

            fast = false;
            terminated = false;
        }
    }

    if fast {
        incr_diagnostic!(Tokenizer_StringFastCount);

        //
        // just set buffer to quotPtr + 1
        //

        if terminated {
            session.offset = quot_offset.unwrap() + 1;

            return session.token(TokenKind::String, token_start);
        } else {
            session.offset = session.input.len();
            session.wasEOF = true;

            return session
                .token(TokenKind::Error_UnterminatedString, token_start);
        }
    }

    //
    // SLOW FALL-THROUGH
    //

    incr_diagnostic!(Tokenizer_StringSlowCount);

    policy |= STRING_OR_COMMENT;

    loop {
        c = Tokenizer_nextWLCharacter(session, token_start, policy);

        match c.to_point() {
            Char('"') => {
                return session.token(TokenKind::String, token_start);
            },
            EndOfFile => {
                return session
                    .token(TokenKind::Error_UnterminatedString, token_start);
            },
            Char('\n' | '\r') | CRLF if session.compute_oob => {
                session.addEmbeddedNewline(token_start.loc);
            },
            Char('\t') if session.compute_oob => {
                session.addEmbeddedTab(token_start.loc);
            },
            _ => (),
        }
    } // while
}

fn Tokenizer_handleString_stringifyAsTag<'i>(
    session: &mut Tokenizer<'i>,
    token_start: &TokenStart<'i>,
    c: WLCharacter,
    policy: NextPolicy,
) -> TokenRef<'i> {
    //
    // Nothing to assert
    //

    //
    // magically turn into a string
    //

    if c.isLetterlike() || c.isMBLetterlike() {
        let letterlikeBuf = session.buffer();
        let letterlikeLoc = session.SrcLoc;

        Tokenizer_handleSymbolSegment(
            session,
            token_start,
            letterlikeBuf,
            letterlikeLoc,
            c,
            policy,
        );

        return session.token(TokenKind::String, token_start);
    }

    //
    // Something like  a::5
    //

    return Token::new2(
        TokenKind::Error_ExpectedTag,
        BufferAndLength::from_buffer_with_len(token_start.buf, 0),
        Span::at(token_start.loc),
    );
}

//
// Use SourceCharacters here, not WLCharacters
//
fn Tokenizer_handleString_stringifyAsFile<'i>(
    session: &mut Tokenizer<'i>,
    token_start: &TokenStart<'i>,
    mut c: SourceCharacter,
    policy: NextPolicy,
) -> TokenRef<'i> {
    //
    // Nothing to assert
    //

    //
    // magically turn into a string
    //

    //
    // sync-up with current character
    //

    match c {
        #[rustfmt::skip]
        Char(
            'A'..='Z' | 'a'..='z' | '0'..='9'
            | '$' | '`' | '/' | '.' | '\\' | '!'
            | '-' | '_' | ':' | '*' | '~' | '?',
        ) => {
            c = session.peek_source_char(policy);
        },
        Char('[') => {
            // handle matched pairs of [] enclosing any characters other than spaces, tabs, and newlines

            match Tokenizer_handleFileOpsBrackets(
                session,
                token_start,
                c,
                policy,
            ) {
                HandledFileOpsBracket::Finished(char) => c = char,
                HandledFileOpsBracket::UnterminatedFileString => {
                    return session.token(
                        TokenKind::Error_UnterminatedFileString,
                        token_start,
                    );
                },
            }
        },
        _ => {
            //
            // Something like  <<EOF
            //
            // EndOfFile is special because there is no source
            //
            // So invent source
            //

            return session
                .error_token_at(TokenKind::Error_ExpectedFile, token_start);
        },
    }

    loop {
        //
        // tutorial/OperatorInputForms
        //
        // File Names
        //
        // Any file name can be given in quotes after <<, >>, and >>>.
        // File names can also be given without quotes if they contain only alphanumeric
        // characters and the characters `, /, ., \[Backslash], !, -, _, :, $, *, ~, and ?, together with
        // matched pairs of square brackets enclosing any characters other than spaces, tabs, and newlines.
        // Note that file names given without quotes can be followed only by spaces, tabs, or newlines, or
        // by the characters ), ], or }, as well as semicolons and commas.
        //

        match c {
            #[rustfmt::skip]
            Char(
                'A'..='Z' | 'a'..='z' | '0'..='9'
                | '`' | '/' | '.' | '\\' | '!'
                | '-' | '_' | ':' | '*' | '~' | '?',
            ) => {
                session.next_source_char(policy);

                c = session.peek_source_char(policy);
            },
            Char('[') => {
                // handle matched pairs of [] enclosing any characters other than spaces, tabs, and newlines

                session.next_source_char(policy);

                match Tokenizer_handleFileOpsBrackets(
                    session,
                    token_start,
                    c,
                    policy,
                ) {
                    HandledFileOpsBracket::Finished(char) => {
                        c = char;
                    },
                    HandledFileOpsBracket::UnterminatedFileString => {
                        return session.token(
                            TokenKind::Error_UnterminatedFileString,
                            token_start,
                        );
                    },
                }
            },
            _ => {
                return session.token(TokenKind::String, token_start);
            },
        }
    } // while
}

/// Outcome from [`Tokenizer_handleFileOpsBrackets()`].
enum HandledFileOpsBracket {
    Finished(SourceCharacter),
    UnterminatedFileString,
}


//
// Handle parsing the brackets in:
// a >> foo[[]]
//
// tutorial/OperatorInputForms
//
// File Names
//
// handle matched pairs of [] enclosing any characters other than spaces, tabs, and newlines
//
// Use SourceCharacters here, not WLCharacters
//
fn Tokenizer_handleFileOpsBrackets<'i>(
    session: &mut Tokenizer<'i>,
    _token_start: &TokenStart<'i>,
    mut c: SourceCharacter,
    policy: NextPolicy,
) -> HandledFileOpsBracket {
    assert!(c == '[');

    //
    // sync-up with current character
    //

    c = session.peek_source_char(policy);

    let mut depth = 1;

    loop {
        match c {
            //
            // Spaces and Newlines
            //
            Char(' ' | '\t' | ASCII_VTAB | ASCII_FORM_FEED | '\n' | '\r')
            | CRLF => {
                //
                // Cannot have spaces in the string here, so bail out
                //

                return HandledFileOpsBracket::UnterminatedFileString;
            },
            EndOfFile => {
                return HandledFileOpsBracket::UnterminatedFileString;
            },
            Char('[') => {
                depth = depth + 1;

                session.next_source_char(policy);

                c = session.peek_source_char(policy);
            },
            Char(']') => {
                depth = depth - 1;

                session.next_source_char(policy);

                c = session.peek_source_char(policy);

                if depth == 0 {
                    return HandledFileOpsBracket::Finished(c);
                }
            },
            _ => {
                if c.isMBWhitespace() || c.isMBNewline() {
                    return HandledFileOpsBracket::UnterminatedFileString;
                }

                session.next_source_char(policy);

                c = session.peek_source_char(policy);
            },
        }
    } // loop
}

//
//digits                  integer
//digits.digits           approximate number
//base^^digits            integer in specified base
//base^^digits.digits     approximate number in specified base
//mantissa*^n             scientific notation (mantissa*10^n)
//base^^mantissa*^n       scientific notation in specified base (mantissa*base^n)
//number`                 machine-precision approximate number
//number`s                arbitrary-precision number with precision s
//number``s               arbitrary-precision number with accuracy s
//
// base = (digits^^)?
// approximate = digits(.digits?)?|.digits
// precision = `(-?approximate)?
// accuracy = ``-?approximate
// mantissa = approximate+(precision|accuracy)?
// exponent = (*^-?digits)?
//
// numer = base+mantissa+exponent
//
fn Tokenizer_handleNumber<'i>(
    session: &mut Tokenizer<'i>,
    token_start: &TokenStart<'i>,
    mut c: WLCharacter,
    policy: NextPolicy,
) -> TokenRef<'i> {
    assert!(c.isDigit() || c.to_point() == '.');

    incr_diagnostic!(Tokenizer_NumberCount);

    let mut Ctxt = NumberTokenizationContext::new();

    let mut leadingDigitsCount: u32 = 0;

    //
    // leading_digits_end_mark will point to the first character after all leading digits and ^^
    //
    // 16^^0.F
    //      ^leading_digits_end_mark
    //
    // 16^^.F
    //     ^leading_digits_end_mark
    //
    // 0.123
    //  ^leading_digits_end_mark
    //
    // TODO(cleanup): Replace InputMark::new() with new InputMark::from_token_start()?
    let mut leading_digits_end_mark =
        InputMark::new(session.offset_of(token_start.buf), token_start.loc);

    let mut caret1Buf: Option<Buffer> = None;
    let mut caret_1_mark: Option<InputMark> = None;

    let mut starBuf: Option<Buffer> = None;
    let mut star_mark: Option<InputMark> = None;

    if c.isDigit() {
        //        leadingDigitsCount++;

        //
        // Count leading zeros
        //

        //
        // 002^^111
        //   ^nonZeroStartBuf
        //
        let mut nonZeroStartBuf = token_start.buf;

        if c.to_point() == '0' {
            let mut leadingZeroCount: u32 = 0;
            (leadingZeroCount, c) =
                Tokenizer_handleZeros(session, token_start, policy, c);

            leadingDigitsCount += leadingZeroCount;

            nonZeroStartBuf = session.buffer();
        }

        //
        // Count the rest of the leading digits
        //

        leading_digits_end_mark = session.mark();

        if c.isDigit() {
            let mut count: u32 = 0;
            (count, c) =
                Tokenizer_handleDigits(session, token_start, policy, c);

            leadingDigitsCount += count;

            leading_digits_end_mark = session.mark();
        }

        if policy.contains(INTEGER_SHORT_CIRCUIT) {
            if session.check_issues && c.to_point() == '.' {
                //
                // Something like  #2.a
                //

                let dotLoc = session.SrcLoc;

                let mut Actions: Vec<CodeAction> = Vec::new();

                Actions.push(CodeAction::insert_text(
                    "Insert space".to_owned(),
                    Span::at(dotLoc),
                    " ".to_owned(),
                ));

                let I = FormatIssue(
                    IssueTag::Ambiguous,
                    format!("Ambiguous syntax."),
                    Severity::Formatting,
                    session.get_token_span(dotLoc),
                    1.0,
                    Actions,
                    vec![],
                );

                session.addIssue(I);
            }

            //
            // Success!
            //

            return session.token(Ctxt.computeTok(), token_start);
        }

        match c.to_point() {
            //
            // These are the possible next characters for a number
            //
            Char('^' | '*' | '.' | '`') => {
                if c.to_point() == '^' {
                    caret1Buf = Some(session.buffer());
                    caret_1_mark = Some(session.mark());

                    assert!(utils::ifASCIIWLCharacter(
                        caret1Buf.unwrap()[0],
                        b'^'
                    ));
                } else if c.to_point() == '*' {
                    starBuf = Some(session.buffer());
                    star_mark = Some(session.mark());

                    assert!(utils::ifASCIIWLCharacter(
                        starBuf.unwrap()[0],
                        b'*'
                    ));
                }

                //
                // Preserve c, but advance buffer to next character
                //

                Tokenizer_nextWLCharacter(session, token_start, policy);
            },
            _ => {
                //
                // Something else
                //

                //
                // Success!
                //

                return session.token(Ctxt.computeTok(), token_start);
            },
        }

        if c.to_point() == '^' {
            //
            // Could be 16^^blah
            //

            c = Tokenizer_currentWLCharacter(session, token_start, policy);

            if c.to_point() != '^' {
                //
                // Something like  2^a
                //
                // Must now do surgery and back up
                //

                session.seek(caret_1_mark.unwrap());

                //
                // Success!
                //

                return session.token(Ctxt.computeTok(), token_start);
            }

            assert!(c.to_point() == '^');

            //
            // Something like  2^^
            //
            // Must be a number
            //

            Tokenizer_nextWLCharacter(session, token_start, policy);

            if nonZeroStartBuf == caret1Buf.unwrap() {
                //
                // Something like  0^^2
                //

                Ctxt.InvalidBase = true;
            } else {
                // PRE_COMMIT: Compute string length differently
                let baseStrLen = BufferAndLength::between(
                    nonZeroStartBuf,
                    caret1Buf.unwrap(),
                )
                .buf
                .len();

                //
                // bases can only be between 2 and 36, so we know they can only be 1 or 2 characters
                //
                if baseStrLen > 2 {
                    Ctxt.InvalidBase = true;
                } else if baseStrLen == 2 {
                    let d1 = i32::from(utils::toDigit(nonZeroStartBuf[0]));
                    let d0 = i32::from(utils::toDigit(nonZeroStartBuf[1]));
                    Ctxt.Base = d1 * 10 + d0;
                } else {
                    assert!(baseStrLen == 1);

                    let d0 = i32::from(utils::toDigit(nonZeroStartBuf[0]));
                    Ctxt.Base = d0;
                }

                if !(2 <= Ctxt.Base && Ctxt.Base <= 36) {
                    Ctxt.InvalidBase = true;
                }
            }

            c = Tokenizer_currentWLCharacter(session, token_start, policy);

            //
            // What can come after ^^ ?
            //

            leadingDigitsCount = 0;

            match c.to_point() {
                Char('A'..='Z' | 'a'..='z' | '0'..='9') => {
                    //
                    // Something like  16^^A
                    //

                    (leadingDigitsCount, c) = Tokenizer_handleAlphaOrDigits(
                        session,
                        token_start,
                        c,
                        Ctxt.Base,
                        policy,
                        &mut Ctxt,
                    );

                    leading_digits_end_mark = session.mark();

                    match c.to_point() {
                        //
                        // These are the possible next characters for a number
                        //
                        Char('*' | '.' | '`') => {
                            if c.to_point() == '*' {
                                starBuf = Some(session.buffer());
                                star_mark = Some(session.mark());

                                assert!(utils::ifASCIIWLCharacter(
                                    starBuf.unwrap()[0],
                                    b'*'
                                ));
                            }

                            //
                            // Preserve c, but advance buffer to next character
                            //

                            Tokenizer_nextWLCharacter(
                                session,
                                token_start,
                                policy,
                            );
                        },
                        _ => {
                            //
                            // Something else
                            //

                            //
                            // Success!
                            //

                            return session
                                .token(Ctxt.computeTok(), token_start);
                        },
                    }
                },
                Char('.') => {
                    //
                    // Something like  2^^.0
                    //

                    leading_digits_end_mark = session.mark();

                    //
                    // Preserve c, but advance buffer to next character
                    //

                    Tokenizer_nextWLCharacter(session, token_start, policy);
                },
                EndOfFile => {
                    //
                    // Something like  2^^<EOF>
                    //

                    //
                    // Make sure that bad character is read
                    //

                    Tokenizer_nextWLCharacter(session, token_start, policy);

                    c = Tokenizer_currentWLCharacter(
                        session,
                        token_start,
                        policy,
                    );

                    // nee TokenKind::Error_ExpectedDIGIT
                    return session.token(TokenKind::Error_Number, token_start);
                },
                _ => {
                    //
                    // Something like  2^^@
                    //

                    // nee TokenKind::Error_UNRECOGNIZEDDIGIT
                    return session.token(TokenKind::Error_Number, token_start);
                },
            }
        } // if (c.to_point() == '^')
    } // if (c.isDigit())

    if c.to_point() == '.' {
        // PRE_COMMIT: Rename this assert
        // assert!(utils::ifASCIIWLCharacter(*(session.buffer - 1), b'.'));

        let handled: HandledFractionalPart;
        (handled, c) = Tokenizer_handlePossibleFractionalPart(
            session,
            token_start,
            leading_digits_end_mark,
            c,
            Ctxt.Base,
            policy,
            &mut Ctxt,
        );

        match handled {
            HandledFractionalPart::Bailout => {
                if leadingDigitsCount == 0 {
                    //
                    // Something like  2^^..
                    //

                    // nee TokenKind::Error_UNHANDLEDDOT
                    return session.token(TokenKind::Error_Number, token_start);
                }

                //
                // Something like  0..
                //

                //
                // Success!
                //

                return session.token(Ctxt.computeTok(), token_start);
            },
            HandledFractionalPart::Count(0) => {
                if leadingDigitsCount == 0 {
                    //
                    // Something like  2^^.
                    //

                    // nee TokenKind::Error_UNHANDLEDDOT
                    return session.token(TokenKind::Error_Number, token_start);
                }

                //
                // Something like  0.
                //

                Ctxt.Real = true;

                match c.to_point() {
                    //
                    // These are the possible next characters for a number
                    //
                    Char('`' | '*') => {
                        if c.to_point() == '*' {
                            starBuf = Some(session.buffer());
                            star_mark = Some(session.mark());

                            assert!(utils::ifASCIIWLCharacter(
                                starBuf.unwrap()[0],
                                b'*'
                            ));
                        }

                        //
                        // Preserve c, but advance buffer to next character
                        //

                        Tokenizer_nextWLCharacter(session, token_start, policy);
                    },
                    _ => {
                        //
                        // Something like  123.
                        //

                        //
                        // Success!
                        //

                        return session.token(Ctxt.computeTok(), token_start);
                    },
                }
            },
            HandledFractionalPart::Count(_) => {
                //
                // Something like  123.456
                //

                Ctxt.Real = true;

                match c.to_point() {
                    //
                    // These are the possible next characters for a number
                    //
                    Char('`' | '*') => {
                        if c.to_point() == '*' {
                            starBuf = Some(session.buffer());
                            star_mark = Some(session.mark());

                            assert!(utils::ifASCIIWLCharacter(
                                starBuf.unwrap()[0],
                                b'*'
                            ));
                        }

                        //
                        // Preserve c, but advance buffer to next character
                        //

                        Tokenizer_nextWLCharacter(session, token_start, policy);
                    },
                    _ => {
                        //
                        // Something like  123.456
                        //

                        //
                        // Success!
                        //

                        return session.token(Ctxt.computeTok(), token_start);
                    },
                }
            },
        }
    }

    assert!(c.to_point() == '`' || c.to_point() == '*');

    //
    // Handle all ` logic here
    //
    // foo`
    // foo`bar
    // foo``bar
    //
    if c.to_point() == '`' {
        Ctxt.Real = true;

        c = Tokenizer_currentWLCharacter(session, token_start, policy);

        let mut accuracy = false;
        let mut sign = false;
        let mut precOrAccSupplied = false;

        let mut sign_mark: Option<InputMark> = None;

        if c.to_point() == '`' {
            Tokenizer_nextWLCharacter(session, token_start, policy);

            c = Tokenizer_currentWLCharacter(session, token_start, policy);

            accuracy = true;
        }

        match c.to_point() {
            Char('-' | '+') => {
                //
                // Something like  1.2`-
                //

                // Take one character so we can display this
                let signBuf =
                    BufferAndLength::from_buffer_with_len(session.buffer(), 1);

                sign_mark = Some(session.mark());

                assert!(
                    utils::ifASCIIWLCharacter(signBuf.buf[0], b'-')
                        || utils::ifASCIIWLCharacter(signBuf.buf[0], b'+')
                );

                //
                // Eat the sign
                //

                Tokenizer_nextWLCharacter(session, token_start, policy);

                c = Tokenizer_currentWLCharacter(session, token_start, policy);

                match c.to_point() {
                    //
                    // These are the possible next characters for a number
                    //
                    Char(
                        '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8'
                        | '9',
                    ) => {
                        //
                        // Something like  1.2`-3
                        //

                        sign = true;

                        if session.check_issues {
                            if accuracy {
                                //
                                // do not warn about 1.2``+3 for now
                                //
                            } else {
                                let I = SyntaxIssue(
                                    IssueTag::UnexpectedSign,
                                    format!(
                                        "The real number has a ``{}`` sign in its precision specification.",
                                        signBuf.as_str()
                                    ),
                                    Severity::Warning,
                                    Span::at(sign_mark.unwrap().src_loc),
                                    0.95,
                                    vec![],
                                    vec!["This is usually unintentional.".into()],
                                );

                                session.addIssue(I);
                            }
                        }
                    },
                    Char('.') => {
                        //
                        // Something like  1.2`-.3
                        //

                        sign = true;

                        if session.check_issues {
                            if accuracy {
                                //
                                // do not warn about 1.2``+.3 for now
                                //
                            } else {
                                let I = SyntaxIssue(
                                    IssueTag::UnexpectedSign,
                                    format!(
                                        "The real number has a ``{}`` sign in its precision specification.",
                                        signBuf.as_str()
                                    ),
                                    Severity::Warning,
                                    Span::at(sign_mark.unwrap().src_loc),
                                    0.95,
                                    vec![],
                                    vec!["This is usually unintentional.".into()],
                                );

                                session.addIssue(I);
                            }
                        }
                    },
                    _ => {
                        //
                        // Something like  1.2`->
                        //

                        if accuracy {
                            //
                            // Something like  1.2``->3
                            //

                            // nee TokenKind::Error_ExpectedACCURACY
                            return session
                                .token(TokenKind::Error_Number, token_start);
                        }

                        //
                        // Something like  1.2`->3  or  1`+#
                        //
                        // Must now do surgery and back up
                        //
                        Tokenizer_backupAndWarn(session, sign_mark.unwrap());

                        //
                        // Success!
                        //

                        return session.token(Ctxt.computeTok(), token_start);
                    },
                }
            }, // case '-': case '+'
            _ => (),
        }

        match c.to_point() {
            Char('0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9') => {
                let mut count: u32 = 0;

                (count, c) =
                    Tokenizer_handleDigits(session, token_start, policy, c);

                if count > 0 {
                    precOrAccSupplied = true;
                }

                match c.to_point() {
                    //
                    // These are the possible next characters for a number
                    //
                    Char('.') => {},
                    Char('*') => {},
                    _ => {
                        //
                        // Success!
                        //

                        return session.token(Ctxt.computeTok(), token_start);
                    },
                }
            },
            _ => (),
        }

        match c.to_point() {
            Char('.') => {
                let dotBuf = session.buffer();
                let dot_mark = session.mark();

                assert!(utils::ifASCIIWLCharacter(dotBuf[0], b'.'));

                let mut tentativeActualDecimalPoint = false;

                //
                // If there was already a sign, or if the leading digits have already been supplied,
                // then this is an actual decimal point
                //
                if sign || precOrAccSupplied {
                    tentativeActualDecimalPoint = true;
                }

                //
                // Need to decide if the  .  here is actual radix point, or something like
                // the . in  123`.xxx  (which is Dot)
                //

                if !tentativeActualDecimalPoint {
                    //
                    // Need to peek ahead
                    //
                    // Something like  123`.xxx
                    //

                    // look ahead

                    Tokenizer_nextWLCharacter(session, token_start, policy);

                    let NextChar = Tokenizer_currentWLCharacter(
                        session,
                        token_start,
                        policy,
                    );

                    if !NextChar.isDigit() {
                        if accuracy {
                            //
                            // Something like  123``.EOF
                            //

                            // TokenKind::Error_ExpectedDIGIT
                            return session
                                .token(TokenKind::Error_Number, token_start);
                        }

                        if NextChar.isSign() {
                            //
                            // Something like  123`.+4
                            //

                            Tokenizer_nextWLCharacter(
                                session,
                                token_start,
                                policy,
                            );

                            // nee TokenKind::Error_ExpectedDIGIT
                            return session
                                .token(TokenKind::Error_Number, token_start);
                        }

                        //
                        // Something like  123`.xxx  where the . could be a Dot operator
                        //
                        // Number stops at `
                        //
                        // NOT actual decimal point
                        //
                        // Must now do surgery and back up
                        //

                        Tokenizer_backupAndWarn(session, dot_mark);

                        //
                        // Success!
                        //

                        return session.token(Ctxt.computeTok(), token_start);
                    } else {
                        //
                        // digit
                        //

                        c = NextChar;
                    }
                } else {
                    //
                    // actual decimal point
                    //

                    Tokenizer_nextWLCharacter(session, token_start, policy);

                    c = Tokenizer_currentWLCharacter(
                        session,
                        token_start,
                        policy,
                    );
                }

                //
                // actual decimal point
                //

                let handled: HandledFractionalPart;
                //
                // The base to use inside of precision/accuracy processing is 0, i.e., implied 10
                //
                let baseToUse: i32 = 0;

                (handled, c) = Tokenizer_handlePossibleFractionalPartPastDot(
                    session,
                    token_start,
                    dot_mark,
                    c,
                    baseToUse,
                    policy,
                    &mut Ctxt,
                );

                match handled {
                    HandledFractionalPart::Bailout => {
                        if precOrAccSupplied {
                            //
                            // Something like  6`5..
                            //

                            //
                            // Success!
                            //

                            return session
                                .token(Ctxt.computeTok(), token_start);
                        }

                        if sign {
                            //
                            // Something like  1`+..
                            //

                            Tokenizer_backupAndWarn(
                                session,
                                sign_mark.unwrap(),
                            );

                            //
                            // Success!
                            //

                            return session
                                .token(Ctxt.computeTok(), token_start);
                        }

                        assert!(false);
                    },
                    HandledFractionalPart::Count(0) => {},
                    HandledFractionalPart::Count(_) => {
                        precOrAccSupplied = true;
                    },
                }

                if !precOrAccSupplied {
                    //
                    // Something like  1`+.a
                    //

                    // nee TokenKind::Error_ExpectedDIGIT
                    return session.token(TokenKind::Error_Number, token_start);
                }
            }, // case '.'
            _ => (),
        } // switch (c.to_point())

        match c.to_point() {
            //
            // These are the possible next characters for a number
            //
            Char('*') => {
                if accuracy {
                    if !precOrAccSupplied {
                        //
                        // Something like  123.45``*^6
                        //

                        // nee TokenKind::Error_ExpectedACCURACY
                        return session
                            .token(TokenKind::Error_Number, token_start);
                    }
                }

                starBuf = Some(session.buffer());
                star_mark = Some(session.mark());

                assert!(utils::ifASCIIWLCharacter(starBuf.unwrap()[0], b'*'));

                //
                // Preserve c, but advance buffer to next character
                //

                Tokenizer_nextWLCharacter(session, token_start, policy);
            },
            _ => {
                if accuracy {
                    if !precOrAccSupplied {
                        //
                        // Something like  123``EOF
                        //

                        // nee TokenKind::Error_ExpectedACCURACY
                        return session
                            .token(TokenKind::Error_Number, token_start);
                    }
                }

                //
                // Success!
                //

                return session.token(Ctxt.computeTok(), token_start);
            },
        }
    } // if (c.to_point() == '`')

    assert!(c.to_point() == '*');

    assert!(utils::ifASCIIWLCharacter(starBuf.unwrap()[0], b'*'));

    c = Tokenizer_currentWLCharacter(session, token_start, policy);

    if c.to_point() != '^' {
        //
        // Something like  1*a
        //
        // Must now do surgery and back up
        //

        session.seek(star_mark.unwrap());

        //
        // Success!
        //

        return session.token(Ctxt.computeTok(), token_start);
    }

    assert!(c.to_point() == '^');

    //
    // c is '^'
    //
    // So now examine *^ notation
    //

    Tokenizer_nextWLCharacter(session, token_start, policy);

    c = Tokenizer_currentWLCharacter(session, token_start, policy);

    if let Char(c2 @ ('+' | '-')) = c.to_point() {
        if c2 == '-' {
            Ctxt.NegativeExponent = true;
        }

        Tokenizer_nextWLCharacter(session, token_start, policy);
        c = Tokenizer_currentWLCharacter(session, token_start, policy);
    }

    if !c.isDigit() {
        //
        // Something like  123*^-<EOF>
        //

        // TokenKind::Error_ExpectedEXPONENT
        return session.token(TokenKind::Error_Number, token_start);
    }

    assert!(c.isDigit());

    //
    // Count leading zeros in exponent
    //
    if c.to_point() == '0' {
        let _exponentLeadingZeroCount: u32;

        (_exponentLeadingZeroCount, c) =
            Tokenizer_handleZeros(session, token_start, policy, c);
    }

    if c.isDigit() {
        (Ctxt.NonZeroExponentDigitCount, c) =
            Tokenizer_handleDigits(session, token_start, policy, c);
    }

    if c.to_point() != '.' {
        //
        // Success!
        //

        return session.token(Ctxt.computeTok(), token_start);
    }

    assert!(c.to_point() == '.');

    let dotBuf = session.buffer();
    let dot_mark = session.mark();

    assert!(utils::ifASCIIWLCharacter(dotBuf[0], b'.'));

    Tokenizer_nextWLCharacter(session, token_start, policy);

    c = Tokenizer_currentWLCharacter(session, token_start, policy);

    let handled: HandledFractionalPart;
    (handled, c) = Tokenizer_handlePossibleFractionalPartPastDot(
        session,
        token_start,
        dot_mark,
        c,
        Ctxt.Base,
        policy,
        &mut Ctxt,
    );

    match handled {
        HandledFractionalPart::Bailout => {
            //
            // Something like  123*^2..
            //
            // The first . is not actually a radix point
            //

            //
            // Success!
            //

            return session.token(Ctxt.computeTok(), token_start);
        },
        HandledFractionalPart::Count(_) => {
            //
            // Something like  123*^0.5
            //
            // Make this an error; do NOT make this Dot[123*^0, 5]
            //

            // nee TokenKind::Error_ExpectedEXPONENT
            return session.token(TokenKind::Error_Number, token_start);
        },
    }
}

impl NumberTokenizationContext {
    pub(crate) fn new() -> Self {
        NumberTokenizationContext {
            InvalidBase: false,
            UnrecognizedDigit: false,
            NegativeExponent: false,
            Real: false,
            NonZeroExponentDigitCount: 0,
            Base: 0,
        }
    }

    fn computeTok(&self) -> TokenKind {
        //
        // We wait until returning to handle these errors because we do not want invalid base or unrecognized digit to prevent further parsing
        //
        // e.g., we want  0^^1.2``3  to parse completely before returning the error
        //

        if self.InvalidBase {
            // nee TokenKind::Error_INVALIDBASE
            return TokenKind::Error_Number;
        }

        if self.UnrecognizedDigit {
            // nee TokenKind::Error_UNRECOGNIZEDDIGIT
            return TokenKind::Error_Number;
        }

        if self.Real {
            return TokenKind::Real;
        }

        if self.NegativeExponent && self.NonZeroExponentDigitCount != 0 {
            //
            // Something like  1*^-2..
            //

            return TokenKind::Rational;
        }

        return TokenKind::Integer;
    }
}

/// Outcome from attempting to handle the fractional part (i.e. the digits after
/// the decimal point) of a number.
enum HandledFractionalPart {
    Count(u32),
    /// Bailed out from handling the fractional part because the first dot
    /// was followed by a second, e.g. the input was `0..`.
    Bailout,
}

//
// Precondition: currentWLCharacter is NOT in String
//
// Return: number of digits handled after ., possibly 0, or -1 if error
//
fn Tokenizer_handlePossibleFractionalPart<'i>(
    session: &mut Tokenizer<'i>,
    token_start: &TokenStart<'i>,
    dot_mark: InputMark,
    mut c: WLCharacter,
    base: i32,
    policy: NextPolicy,
    Ctxt: &mut NumberTokenizationContext,
) -> (HandledFractionalPart, WLCharacter) {
    assert!(c.to_point() == '.');

    c = Tokenizer_currentWLCharacter(session, token_start, policy);

    // MUSTTAIL
    return Tokenizer_handlePossibleFractionalPartPastDot(
        session,
        token_start,
        dot_mark,
        c,
        base,
        policy,
        Ctxt,
    );
}

/// Precondition: currentWLCharacter is NOT in String
///
/// Returns:
///
/// * number of digits handled after ., possibly 0
/// * UNRECOGNIZED_DIGIT if base error
/// * [`HandledFractionalPart::Bailout`] if not a radix point (and also backup before dot)
///
fn Tokenizer_handlePossibleFractionalPartPastDot<'i>(
    session: &mut Tokenizer<'i>,
    token_start: &TokenStart<'i>,
    dot_mark: InputMark,
    mut c: WLCharacter,
    base: i32,
    policy: NextPolicy,
    Ctxt: &mut NumberTokenizationContext,
) -> (HandledFractionalPart, WLCharacter) {
    //
    // Nothing to assert
    //

    if c.to_point() == '.' {
        //
        // Something like  0..
        //
        // The first . is not actually a radix point
        //
        // Must now do surgery and back up
        //

        Tokenizer_backupAndWarn(session, dot_mark);

        c = Tokenizer_currentWLCharacter(session, token_start, policy);

        return (HandledFractionalPart::Bailout, c);
    }

    if c.isAlphaOrDigit() {
        let handled: u32;
        (handled, c) = Tokenizer_handleAlphaOrDigits(
            session,
            token_start,
            c,
            base,
            policy,
            Ctxt,
        );

        if handled > 0 {
            if session.check_issues && c.to_point() == '.' {
                //
                // Something like  1.2.3
                //

                let mut Actions = Vec::new();

                Actions.push(CodeAction::insert_text(
                    "Insert ``*``".into(),
                    Span::at(dot_mark.src_loc),
                    "*".into(),
                ));

                let I = SyntaxIssue(
                    IssueTag::UnexpectedImplicitTimes,
                    format!("Suspicious syntax."),
                    Severity::Error,
                    Span::at(dot_mark.src_loc),
                    0.99,
                    Actions,
                    vec![],
                );

                session.addIssue(I);
            }
        }

        return (HandledFractionalPart::Count(handled), c);
    }

    return (HandledFractionalPart::Count(0), c);
}

fn Tokenizer_backupAndWarn<'i>(session: &mut Tokenizer<'i>, reset: InputMark) {
    if session.check_issues {
        let mut Actions: Vec<CodeAction> = Vec::new();

        Actions.push(CodeAction::insert_text(
            "Insert space".into(),
            Span::at(reset.src_loc),
            " ".into(),
        ));

        let I = FormatIssue(
            IssueTag::Ambiguous,
            "Ambiguous syntax.".into(),
            Severity::Formatting,
            Span::at(reset.src_loc),
            1.0,
            Actions,
            vec![],
        );

        session.addIssue(I);
    }

    session.seek(reset);
}

//
// Precondition: currentWLCharacter is 0
// Postcondition: buffer is pointing to first NON-ZERO character after all zeros
//
// return: the first NON-ZERO character after all digits
//
fn Tokenizer_handleZeros<'i>(
    session: &mut Tokenizer<'i>,
    token_start: &TokenStart<'i>,
    policy: NextPolicy,
    mut c: WLCharacter,
) -> (u32, WLCharacter) {
    assert!(c.to_point() == '0');

    let mut count = 1;

    c = Tokenizer_currentWLCharacter(session, token_start, policy);

    loop {
        if c.to_point() != '0' {
            break;
        }

        Tokenizer_nextWLCharacter(session, token_start, policy);

        c = Tokenizer_currentWLCharacter(session, token_start, policy);

        count += 1;
    } // while

    return (count, c);
}

//
// Precondition: currentWLCharacter is a digit
// Postcondition: buffer is pointing to first NON-DIGIT character after all digits
//
// return: the first NON-DIGIT character after all digits
//
fn Tokenizer_handleDigits<'i>(
    session: &mut Tokenizer<'i>,
    token_start: &TokenStart<'i>,
    policy: NextPolicy,
    mut c: WLCharacter,
) -> (u32, WLCharacter) {
    assert!(c.isDigit());

    let mut count = 1;

    c = Tokenizer_currentWLCharacter(session, token_start, policy);

    loop {
        if !c.isDigit() {
            break;
        }

        Tokenizer_nextWLCharacter(session, token_start, policy);

        c = Tokenizer_currentWLCharacter(session, token_start, policy);

        count += 1;
    } // while

    return (count, c);
}

//
// Precondition: currentWLCharacter is NOT in String
// Postcondition: currentWLCharacter is the first WLCharacter AFTER all good digits or alphas
//
// Return: number of digits handled, possibly 0, or -1 if error
//
fn Tokenizer_handleAlphaOrDigits<'i>(
    session: &mut Tokenizer<'i>,
    token_start: &TokenStart<'i>,
    mut c: WLCharacter,
    base: i32,
    policy: NextPolicy,
    Ctxt: &mut NumberTokenizationContext,
) -> (u32, WLCharacter) {
    assert!(c.isAlphaOrDigit());

    let mut count = 0;

    loop {
        if !c.isAlphaOrDigit() {
            break;
        }

        if base == 0 {
            if !c.isDigit() {
                break;
            }
        } else {
            let cp: CodePoint = c.to_point();
            let cp_i32 = cp.as_i32();
            let cp_u8 = u8::try_from(cp_i32)
                .expect("unable to convert digit character to u8 value");
            let dig = i32::from(utils::toDigit(cp_u8));

            if base <= dig {
                Ctxt.UnrecognizedDigit = true;
            }
        }

        Tokenizer_nextWLCharacter(session, token_start, policy);

        c = Tokenizer_currentWLCharacter(session, token_start, policy);

        count += 1;
    } // while

    return (count, c);
}

fn Tokenizer_handleColon<'i>(
    session: &mut Tokenizer<'i>,
    token_start: &TokenStart<'i>,
    mut c: WLCharacter,
    policy: NextPolicy,
) -> TokenRef<'i> {
    assert!(c.to_point() == ':');

    c = Tokenizer_currentWLCharacter(session, token_start, policy);

    match c.to_point() {
        Char(':') => {
            Tokenizer_nextWLCharacter(session, token_start, policy);

            c = Tokenizer_currentWLCharacter(session, token_start, policy);

            if c.to_point() == '[' {
                //
                // ::[
                //

                Tokenizer_nextWLCharacter(session, token_start, policy);

                return session
                    .token(TokenKind::ColonColonOpenSquare, token_start);
            }

            //
            // ::
            //

            return session.token(TokenKind::ColonColon, token_start);
        },
        Char('=') => {
            //
            // :=
            //

            Tokenizer_nextWLCharacter(session, token_start, policy);

            return session.token(TokenKind::ColonEqual, token_start);
        },
        Char('>') => {
            //
            // :>
            //

            incr_diagnostic!(Tokenizer_ColonGreaterCount);

            Tokenizer_nextWLCharacter(session, token_start, policy);

            return session.token(TokenKind::ColonGreater, token_start);
        },
        _ => {
            //
            // :
            //

            return session.token(TokenKind::Colon, token_start);
        },
    }
}

fn Tokenizer_handleOpenParen<'i>(
    session: &mut Tokenizer<'i>,
    token_start: &TokenStart<'i>,
    c: WLCharacter,
    policy: NextPolicy,
) -> TokenRef<'i> {
    assert!(c.to_point() == '(');

    let secondChar = session.peek_source_char(policy);

    //
    // Comments must start literally with (*
    // Escaped characters do not work
    //
    if (c.to_point() == '(' && c.escape() == Escape::None) && secondChar == '*'
    {
        //
        // secondChar is a SourceCharacter, so cannot MUSTTAIL
        //
        //        MUSTTAIL
        return Tokenizer_handleComment(
            session,
            token_start,
            secondChar,
            policy,
        );
    }

    //
    // (
    //

    incr_diagnostic!(Tokenizer_OpenParenCount);

    return session.token(TokenKind::OpenParen, token_start);
}

fn Tokenizer_handleDot<'i>(
    session: &mut Tokenizer<'i>,
    token_start: &TokenStart<'i>,
    firstChar: WLCharacter,
    policy: NextPolicy,
) -> TokenRef<'i> {
    let mut c = firstChar;

    //
    // handleDot
    // Could be  .  or  ..  or ...  or  .0
    //

    assert!(c.to_point() == '.');

    c = Tokenizer_currentWLCharacter(session, token_start, policy);

    if c.isDigit() {
        //        MUSTTAIL
        return Tokenizer_handleNumber(session, token_start, firstChar, policy);
    }

    if c.to_point() == '.' {
        Tokenizer_nextWLCharacter(session, token_start, policy);

        c = Tokenizer_currentWLCharacter(session, token_start, policy);

        if c.to_point() == '.' {
            //
            // ...
            //

            Tokenizer_nextWLCharacter(session, token_start, policy);

            return session.token(TokenKind::DotDotDot, token_start);
        }

        //
        // ..
        //

        return session.token(TokenKind::DotDot, token_start);
    }

    //
    // .
    //

    return session.token(TokenKind::Dot, token_start);
}

fn Tokenizer_handleEqual<'i>(
    session: &mut Tokenizer<'i>,
    token_start: &TokenStart<'i>,
    mut c: WLCharacter,
    policy: NextPolicy,
) -> TokenRef<'i> {
    assert!(c.to_point() == '=');

    c = Tokenizer_currentWLCharacter(session, token_start, policy);

    match c.to_point() {
        Char('=') => {
            Tokenizer_nextWLCharacter(session, token_start, policy);

            c = Tokenizer_currentWLCharacter(session, token_start, policy);

            if c.to_point() == '=' {
                //
                // ===
                //

                Tokenizer_nextWLCharacter(session, token_start, policy);

                return session.token(TokenKind::EqualEqualEqual, token_start);
            }

            //
            // ==
            //

            return session.token(TokenKind::EqualEqual, token_start);
        },
        Char('!') => {
            let bang_mark = session.mark();

            Tokenizer_nextWLCharacter(session, token_start, policy);

            c = Tokenizer_currentWLCharacter(session, token_start, policy);

            if c.to_point() == '=' {
                //
                // =!=
                //

                Tokenizer_nextWLCharacter(session, token_start, policy);

                return session.token(TokenKind::EqualBangEqual, token_start);
            }

            //
            // Something like  x=!y
            //
            // Must now do surgery and back up
            //

            Tokenizer_backupAndWarn(session, bang_mark);

            return session.token(TokenKind::Equal, token_start);
        },
        _ => (),
    }

    //
    // =
    //

    return session.token(TokenKind::Equal, token_start);
}

fn Tokenizer_handleUnder<'i>(
    session: &mut Tokenizer<'i>,
    token_start: &TokenStart<'i>,
    mut c: WLCharacter,
    policy: NextPolicy,
) -> TokenRef<'i> {
    assert!(c.to_point() == '_');

    c = Tokenizer_currentWLCharacter(session, token_start, policy);

    match c.to_point() {
        Char('_') => {
            //
            // __
            //

            Tokenizer_nextWLCharacter(session, token_start, policy);

            c = Tokenizer_currentWLCharacter(session, token_start, policy);

            if c.to_point() == '_' {
                //
                // ___
                //

                Tokenizer_nextWLCharacter(session, token_start, policy);

                return session.token(TokenKind::UnderUnderUnder, token_start);
            }

            return session.token(TokenKind::UnderUnder, token_start);
        },
        Char('.') => {
            //
            // _.
            //

            Tokenizer_nextWLCharacter(session, token_start, policy);

            if session.check_issues {
                let afterLoc = session.SrcLoc;

                c = Tokenizer_currentWLCharacter(session, token_start, policy);

                if c.to_point() == '.' {
                    //
                    // Something like  a_..b  or  _...
                    //
                    // Prior to 12.2,  a_..b  was parsed as Times[(a_).., b]
                    //
                    // 12.2 and onward,  a_..b  is parsed as Dot[a_., b]
                    //
                    // Related bugs: 390755
                    //

                    let dotLoc = afterLoc;

                    let mut Actions: Vec<CodeAction> = Vec::new();

                    Actions.push(CodeAction::insert_text(
                        "Insert space".into(),
                        Span::at(dotLoc),
                        " ".into(),
                    ));

                    let I = SyntaxIssue(
                        IssueTag::UnexpectedDot,
                        "Suspicious syntax.".into(),
                        Severity::Error,
                        Span::at(dotLoc),
                        0.95,
                        Actions,
                        vec![],
                    );

                    session.addIssue(I);
                }
            }

            return session.token(TokenKind::UnderDot, token_start);
        },
        _ => (),
    }

    //
    // _
    //

    return session.token(TokenKind::Under, token_start);
}

fn Tokenizer_handleLess<'i>(
    session: &mut Tokenizer<'i>,
    token_start: &TokenStart<'i>,
    mut c: WLCharacter,
    policy: NextPolicy,
) -> TokenRef<'i> {
    assert!(c.to_point() == '<');

    c = Tokenizer_currentWLCharacter(session, token_start, policy);

    match c.to_point() {
        Char('|') => {
            //
            // <|
            //

            Tokenizer_nextWLCharacter(session, token_start, policy);

            return session.token(TokenKind::LessBar, token_start);
        },
        Char('<') => {
            //
            // <<
            //

            Tokenizer_nextWLCharacter(session, token_start, policy);

            return session.token(TokenKind::LessLess, token_start);
        },
        Char('>') => {
            //
            // <>
            //

            Tokenizer_nextWLCharacter(session, token_start, policy);

            return session.token(TokenKind::LessGreater, token_start);
        },
        Char('=') => {
            //
            // <=
            //

            Tokenizer_nextWLCharacter(session, token_start, policy);

            return session.token(TokenKind::LessEqual, token_start);
        },
        Char('-') => {
            let minus_mark = session.mark();

            Tokenizer_nextWLCharacter(session, token_start, policy);

            c = Tokenizer_currentWLCharacter(session, token_start, policy);

            if c.to_point() == '>' {
                //
                // <->
                //

                Tokenizer_nextWLCharacter(session, token_start, policy);

                return session.token(TokenKind::LessMinusGreater, token_start);
            }

            //
            // Something like  a<-4
            //
            // Must now do surgery and back up
            //

            Tokenizer_backupAndWarn(session, minus_mark);

            return session.token(TokenKind::Less, token_start);
        },
        _ => (),
    }

    //
    // <
    //

    return session.token(TokenKind::Less, token_start);
}

fn Tokenizer_handleGreater<'i>(
    session: &mut Tokenizer<'i>,
    token_start: &TokenStart<'i>,
    mut c: WLCharacter,
    policy: NextPolicy,
) -> TokenRef<'i> {
    assert!(c.to_point() == '>');

    c = Tokenizer_currentWLCharacter(session, token_start, policy);

    match c.to_point() {
        Char('>') => {
            //
            // >>
            //

            Tokenizer_nextWLCharacter(session, token_start, policy);

            c = Tokenizer_currentWLCharacter(session, token_start, policy);

            if c.to_point() == '>' {
                //
                // >>>
                //

                Tokenizer_nextWLCharacter(session, token_start, policy);

                return session
                    .token(TokenKind::GreaterGreaterGreater, token_start);
            }

            return session.token(TokenKind::GreaterGreater, token_start);
        },
        Char('=') => {
            //
            // >=
            //

            Tokenizer_nextWLCharacter(session, token_start, policy);

            return session.token(TokenKind::GreaterEqual, token_start);
        },
        _ => (),
    }

    //
    // >
    //

    return session.token(TokenKind::Greater, token_start);
}

fn Tokenizer_handleMinus<'i>(
    session: &mut Tokenizer<'i>,
    token_start: &TokenStart<'i>,
    mut c: WLCharacter,
    policy: NextPolicy,
) -> TokenRef<'i> {
    assert!(c.to_point() == '-');

    c = Tokenizer_currentWLCharacter(session, token_start, policy);

    //
    // Do not lex as a number here
    // Makes it easier to handle implicit times later
    //
    // Because if we lexed - as a number here, then it is
    // harder to know that b-1 is Plus[b, -1] instead of
    // b<invisiblespace>-1 which is Times[b, -1]
    //

    match c.to_point() {
        Char('>') => {
            //
            // ->
            //

            incr_diagnostic!(Tokenizer_MinusGreaterCount);

            Tokenizer_nextWLCharacter(session, token_start, policy);

            return session.token(TokenKind::MinusGreater, token_start);
        },
        Char('-') => {
            //
            // --
            //

            Tokenizer_nextWLCharacter(session, token_start, policy);

            if session.check_issues {
                let afterLoc = session.SrcLoc;

                c = Tokenizer_currentWLCharacter(session, token_start, policy);

                if c.to_point() == '>' {
                    //
                    // Something like  a-->0
                    //
                    // Was originally just a FormatIssue
                    //
                    // But a real-world example was demonstrated and this is now considered a real thing that could happen
                    //
                    // https://stash.wolfram.com/projects/WA/repos/alphasource/pull-requests/30963/overview
                    //

                    let greaterLoc = afterLoc;

                    let mut Actions: Vec<CodeAction> = Vec::new();

                    //
                    // HACK: little bit of a hack here
                    // would like to replace  -->  with  ->
                    // but the current token is only -- and I would prefer to not read past the > just for this action
                    //
                    // So actually just replace the -- with -
                    //
                    Actions.push(CodeAction::replace_text(
                        "Replace with ``->``".into(),
                        Span::new(token_start.loc, afterLoc),
                        "-".into(),
                    ));

                    Actions.push(CodeAction::insert_text(
                        "Insert space".into(),
                        Span::at(greaterLoc),
                        " ".into(),
                    ));

                    let I = SyntaxIssue(
                        IssueTag::Ambiguous,
                        "``-->`` is ambiguous syntax.".into(),
                        Severity::Error,
                        Span::new(token_start.loc, afterLoc),
                        0.95,
                        Actions,
                        vec![],
                    );

                    session.addIssue(I);
                } else if c.to_point() == '=' {
                    //
                    // Something like  a--=0
                    //

                    let equalLoc = afterLoc;

                    let mut Actions: Vec<CodeAction> = Vec::new();

                    Actions.push(CodeAction::insert_text(
                        "Insert space".into(),
                        Span::at(equalLoc),
                        " ".into(),
                    ));

                    let I = FormatIssue(
                        IssueTag::Ambiguous,
                        "Put a space between ``--`` and ``=`` to reduce ambiguity".into(),
                        Severity::Formatting,
                        Span::at(equalLoc),
                        1.0,
                        Actions,
                        vec![],
                    );

                    session.addIssue(I);
                }
            }

            return session.token(TokenKind::MinusMinus, token_start);
        },
        Char('=') => {
            //
            // -=
            //

            Tokenizer_nextWLCharacter(session, token_start, policy);

            return session.token(TokenKind::MinusEqual, token_start);
        },
        _ => (),
    }

    //
    // -
    //

    incr_diagnostic!(Tokenizer_MinusCount);

    return session.token(TokenKind::Minus, token_start);
}

fn Tokenizer_handleBar<'i>(
    session: &mut Tokenizer<'i>,
    token_start: &TokenStart<'i>,
    mut c: WLCharacter,
    policy: NextPolicy,
) -> TokenRef<'i> {
    assert!(c.to_point() == '|');

    c = Tokenizer_currentWLCharacter(session, token_start, policy);

    match c.to_point() {
        Char('>') => {
            //
            // |>
            //

            Tokenizer_nextWLCharacter(session, token_start, policy);

            if session.check_issues {
                let afterLoc = session.SrcLoc;

                c = Tokenizer_currentWLCharacter(session, token_start, policy);

                if c.to_point() == '=' {
                    //
                    // Something like  <||>=0
                    //

                    let equalLoc = afterLoc;

                    let mut Actions: Vec<CodeAction> = Vec::new();

                    Actions.push(CodeAction::insert_text(
                        "Insert space".into(),
                        Span::at(equalLoc),
                        " ".into(),
                    ));

                    let I = FormatIssue(
                        IssueTag::Ambiguous,
                        "Put a space between ``|>`` and ``=`` to reduce ambiguity".into(),
                        Severity::Formatting,
                        Span::at(equalLoc),
                        1.0,
                        Actions,
                        vec![],
                    );

                    session.addIssue(I);
                }
            }

            return session.token(TokenKind::BarGreater, token_start);
        },
        Char('|') => {
            //
            // ||
            //

            Tokenizer_nextWLCharacter(session, token_start, policy);

            return session.token(TokenKind::BarBar, token_start);
        },
        Char('-') => {
            let bar_mark = session.mark();

            Tokenizer_nextWLCharacter(session, token_start, policy);

            c = Tokenizer_currentWLCharacter(session, token_start, policy);

            if c.to_point() == '>' {
                //
                // |->
                //

                Tokenizer_nextWLCharacter(session, token_start, policy);

                return session.token(TokenKind::BarMinusGreater, token_start);
            }

            //
            // Something like  x|-y
            //
            // Must now do surgery and back up
            //

            Tokenizer_backupAndWarn(session, bar_mark);

            return session.token(TokenKind::Bar, token_start);
        },
        _ => (),
    }

    //
    // |
    //

    return session.token(TokenKind::Bar, token_start);
}

fn Tokenizer_handleSemi<'i>(
    session: &mut Tokenizer<'i>,
    token_start: &TokenStart<'i>,
    mut c: WLCharacter,
    policy: NextPolicy,
) -> TokenRef<'i> {
    assert!(c.to_point() == ';');

    c = Tokenizer_currentWLCharacter(session, token_start, policy);

    if c.to_point() == ';' {
        //
        // ;;
        //

        Tokenizer_nextWLCharacter(session, token_start, policy);

        return session.token(TokenKind::SemiSemi, token_start);
    }

    //
    // ;
    //

    return session.token(TokenKind::Semi, token_start);
}

fn Tokenizer_handleBang<'i>(
    session: &mut Tokenizer<'i>,
    token_start: &TokenStart<'i>,
    mut c: WLCharacter,
    policy: NextPolicy,
) -> TokenRef<'i> {
    assert!(c.to_point() == '!');

    c = Tokenizer_currentWLCharacter(session, token_start, policy);

    match c.to_point() {
        Char('=') => {
            //
            // !=
            //

            Tokenizer_nextWLCharacter(session, token_start, policy);

            return session.token(TokenKind::BangEqual, token_start);
        },
        Char('!') => {
            //
            // !!
            //

            Tokenizer_nextWLCharacter(session, token_start, policy);

            return session.token(TokenKind::BangBang, token_start);
        },
        _ => (),
    }

    //
    // !
    //

    return session.token(TokenKind::Bang, token_start);
}

fn Tokenizer_handleHash<'i>(
    session: &mut Tokenizer<'i>,
    token_start: &TokenStart<'i>,
    mut c: WLCharacter,
    policy: NextPolicy,
) -> TokenRef<'i> {
    assert!(c.to_point() == '#');

    c = Tokenizer_currentWLCharacter(session, token_start, policy);

    if c.to_point() == '#' {
        //
        // ##
        //

        Tokenizer_nextWLCharacter(session, token_start, policy);

        return session.token(TokenKind::HashHash, token_start);
    }

    //
    // #
    //

    incr_diagnostic!(Tokenizer_HashCount);

    return session.token(TokenKind::Hash, token_start);
}

fn Tokenizer_handlePercent<'i>(
    session: &mut Tokenizer<'i>,
    token_start: &TokenStart<'i>,
    mut c: WLCharacter,
    policy: NextPolicy,
) -> TokenRef<'i> {
    assert!(c.to_point() == '%');

    c = Tokenizer_currentWLCharacter(session, token_start, policy);

    if c.to_point() == '%' {
        //
        // %%
        //

        c = Tokenizer_currentWLCharacter(session, token_start, policy);

        loop {
            if c.to_point() != '%' {
                break;
            }

            Tokenizer_nextWLCharacter(session, token_start, policy);

            c = Tokenizer_currentWLCharacter(session, token_start, policy);
        } // while

        return session.token(TokenKind::PercentPercent, token_start);
    }

    //
    // %
    //

    return session.token(TokenKind::Percent, token_start);
}

fn Tokenizer_handleAmp<'i>(
    session: &mut Tokenizer<'i>,
    token_start: &TokenStart<'i>,
    mut c: WLCharacter,
    policy: NextPolicy,
) -> TokenRef<'i> {
    assert!(c.to_point() == '&');

    c = Tokenizer_currentWLCharacter(session, token_start, policy);

    if c.to_point() == '&' {
        //
        // &&
        //

        Tokenizer_nextWLCharacter(session, token_start, policy);

        return session.token(TokenKind::AmpAmp, token_start);
    }

    //
    // &
    //

    incr_diagnostic!(Tokenizer_AmpCount);

    return session.token(TokenKind::Amp, token_start);
}

fn Tokenizer_handleSlash<'i>(
    session: &mut Tokenizer<'i>,
    token_start: &TokenStart<'i>,
    mut c: WLCharacter,
    policy: NextPolicy,
) -> TokenRef<'i> {
    assert!(c.to_point() == '/');

    c = Tokenizer_currentWLCharacter(session, token_start, policy);

    match c.to_point() {
        Char('@') => {
            //
            // /@
            //

            Tokenizer_nextWLCharacter(session, token_start, policy);

            return session.token(TokenKind::SlashAt, token_start);
        },
        Char(';') => {
            //
            // /;
            //

            Tokenizer_nextWLCharacter(session, token_start, policy);

            return session.token(TokenKind::SlashSemi, token_start);
        },
        Char('.') => {
            let dot_mark = session.mark();

            Tokenizer_nextWLCharacter(session, token_start, policy);

            c = Tokenizer_currentWLCharacter(session, token_start, policy);

            if !c.isDigit() {
                //
                // /.
                //

                return session.token(TokenKind::SlashDot, token_start);
            }

            //
            // Something like  t/.3
            //
            // Must now do surgery and back up
            //

            Tokenizer_backupAndWarn(session, dot_mark);

            return session.token(TokenKind::Slash, token_start);
        },
        Char('/') => {
            //
            // //
            //

            Tokenizer_nextWLCharacter(session, token_start, policy);

            c = Tokenizer_currentWLCharacter(session, token_start, policy);

            match c.to_point() {
                Char('.') => {
                    //
                    // //.
                    //

                    Tokenizer_nextWLCharacter(session, token_start, policy);

                    return session
                        .token(TokenKind::SlashSlashDot, token_start);
                },
                Char('@') => {
                    //
                    // //@
                    //

                    Tokenizer_nextWLCharacter(session, token_start, policy);

                    return session.token(TokenKind::SlashSlashAt, token_start);
                },
                Char('=') => {
                    //
                    // //=
                    //

                    Tokenizer_nextWLCharacter(session, token_start, policy);

                    return session
                        .token(TokenKind::SlashSlashEqual, token_start);
                },
                _ => (),
            }

            //
            // //
            //

            return session.token(TokenKind::SlashSlash, token_start);
        },
        Char(':') => {
            //
            // /:
            //

            Tokenizer_nextWLCharacter(session, token_start, policy);

            return session.token(TokenKind::SlashColon, token_start);
        },
        Char('=') => {
            //
            // /=
            //

            Tokenizer_nextWLCharacter(session, token_start, policy);

            return session.token(TokenKind::SlashEqual, token_start);
        },
        Char('*') => {
            //
            // /*
            //

            Tokenizer_nextWLCharacter(session, token_start, policy);

            return session.token(TokenKind::SlashStar, token_start);
        },
        _ => (),
    }

    //
    // /
    //

    return session.token(TokenKind::Slash, token_start);
}

fn Tokenizer_handleAt<'i>(
    session: &mut Tokenizer<'i>,
    token_start: &TokenStart<'i>,
    mut c: WLCharacter,
    policy: NextPolicy,
) -> TokenRef<'i> {
    assert!(c.to_point() == '@');

    c = Tokenizer_currentWLCharacter(session, token_start, policy);

    match c.to_point() {
        Char('@') => {
            Tokenizer_nextWLCharacter(session, token_start, policy);

            c = Tokenizer_currentWLCharacter(session, token_start, policy);

            if c.to_point() == '@' {
                //
                // @@@
                //

                Tokenizer_nextWLCharacter(session, token_start, policy);

                return session.token(TokenKind::AtAtAt, token_start);
            }

            //
            // @@
            //

            return session.token(TokenKind::AtAt, token_start);
        },
        Char('*') => {
            //
            // @*
            //

            Tokenizer_nextWLCharacter(session, token_start, policy);

            return session.token(TokenKind::AtStar, token_start);
        },
        _ => (),
    }

    //
    // @
    //

    return session.token(TokenKind::At, token_start);
}

fn Tokenizer_handlePlus<'i>(
    session: &mut Tokenizer<'i>,
    token_start: &TokenStart<'i>,
    mut c: WLCharacter,
    policy: NextPolicy,
) -> TokenRef<'i> {
    assert!(c.to_point() == '+');

    c = Tokenizer_currentWLCharacter(session, token_start, policy);

    match c.to_point() {
        Char('+') => {
            //
            // ++
            //

            Tokenizer_nextWLCharacter(session, token_start, policy);

            if session.check_issues {
                c = Tokenizer_currentWLCharacter(session, token_start, policy);

                if c.to_point() == '=' {
                    //
                    // Something like  a++=0
                    //

                    let loc = session.SrcLoc;

                    let mut Actions: Vec<CodeAction> = Vec::new();

                    Actions.push(CodeAction::insert_text(
                        "Insert space".into(),
                        Span::at(loc),
                        " ".into(),
                    ));

                    let I = FormatIssue(
                        IssueTag::Ambiguous,
                        "Put a space between ``++`` and ``=`` to reduce ambiguity".into(),
                        Severity::Formatting,
                        Span::at(loc),
                        1.0,
                        Actions,
                        vec![],
                    );

                    session.addIssue(I);
                }
            }

            return session.token(TokenKind::PlusPlus, token_start);
        },
        Char('=') => {
            //
            // +=
            //

            Tokenizer_nextWLCharacter(session, token_start, policy);

            return session.token(TokenKind::PlusEqual, token_start);
        },
        _ => (),
    }

    //
    // +
    //

    incr_diagnostic!(Tokenizer_PlusCount);

    return session.token(TokenKind::Plus, token_start);
}

fn Tokenizer_handleTilde<'i>(
    session: &mut Tokenizer<'i>,
    token_start: &TokenStart<'i>,
    mut c: WLCharacter,
    policy: NextPolicy,
) -> TokenRef<'i> {
    assert!(c.to_point() == '~');

    c = Tokenizer_currentWLCharacter(session, token_start, policy);

    if c.to_point() == '~' {
        //
        // ~~
        //

        Tokenizer_nextWLCharacter(session, token_start, policy);

        return session.token(TokenKind::TildeTilde, token_start);
    }

    //
    // ~
    //

    return session.token(TokenKind::Tilde, token_start);
}

fn Tokenizer_handleQuestion<'i>(
    session: &mut Tokenizer<'i>,
    token_start: &TokenStart<'i>,
    mut c: WLCharacter,
    policy: NextPolicy,
) -> TokenRef<'i> {
    assert!(c.to_point() == '?');

    c = Tokenizer_currentWLCharacter(session, token_start, policy);

    if c.to_point() == '?' {
        //
        // ??
        //

        Tokenizer_nextWLCharacter(session, token_start, policy);

        return session.token(TokenKind::QuestionQuestion, token_start);
    }

    //
    // ?
    //

    return session.token(TokenKind::Question, token_start);
}

fn Tokenizer_handleStar<'i>(
    session: &mut Tokenizer<'i>,
    token_start: &TokenStart<'i>,
    mut c: WLCharacter,
    policy: NextPolicy,
) -> TokenRef<'i> {
    assert!(c.to_point() == '*');

    c = Tokenizer_currentWLCharacter(session, token_start, policy);

    match c.to_point() {
        Char('=') => {
            //
            // *=
            //

            Tokenizer_nextWLCharacter(session, token_start, policy);

            return session.token(TokenKind::StarEqual, token_start);
        },
        Char('*') => {
            //
            // **
            //

            Tokenizer_nextWLCharacter(session, token_start, policy);

            return session.token(TokenKind::StarStar, token_start);
        },
        Char(')') => {
            //
            // *)
            //

            Tokenizer_nextWLCharacter(session, token_start, policy);

            return session
                .token(TokenKind::Error_UnexpectedCommentCloser, token_start);
        },
        _ => (),
    }

    //
    // *
    //

    return session.token(TokenKind::Star, token_start);
}

fn Tokenizer_handleCaret<'i>(
    session: &mut Tokenizer<'i>,
    token_start: &TokenStart<'i>,
    mut c: WLCharacter,
    policy: NextPolicy,
) -> TokenRef<'i> {
    assert!(c.to_point() == '^');

    c = Tokenizer_currentWLCharacter(session, token_start, policy);

    match c.to_point() {
        Char(':') => {
            Tokenizer_nextWLCharacter(session, token_start, policy);

            c = Tokenizer_currentWLCharacter(session, token_start, policy);

            if c.to_point() == '=' {
                //
                // ^:=
                //

                Tokenizer_nextWLCharacter(session, token_start, policy);

                return session.token(TokenKind::CaretColonEqual, token_start);
            }

            //
            // Has to be ^:=
            //

            return session.token(TokenKind::Error_ExpectedEqual, token_start);
        },
        Char('=') => {
            //
            // ^=
            //

            Tokenizer_nextWLCharacter(session, token_start, policy);

            return session.token(TokenKind::CaretEqual, token_start);
        },
        _ => (),
    }

    //
    // ^
    //

    return session.token(TokenKind::Caret, token_start);
}

fn Tokenizer_handleUnhandledBackslash<'i>(
    session: &mut Tokenizer<'i>,
    token_start: &TokenStart<'i>,
    mut c: WLCharacter,
    policy: NextPolicy,
) -> TokenRef<'i> {
    //
    // Unhandled \
    //
    // Something like  \A  or  \{  or  \<EOF>
    //
    // If the bad character looks like a special input, then try to reconstruct the character up to the bad SourceCharacter
    // This duplicates some logic in CharacterDecoder
    //

    assert!(c.to_point() == '\\');

    c = Tokenizer_nextWLCharacter(session, token_start, policy);

    match c.to_point() {
        Char('[') => {
            //
            // Try to reconstruct \[XXX]
            //

            let mut reset_mark = session.mark();

            c = Tokenizer_nextWLCharacter(session, token_start, policy);

            let mut wellFormed = false;

            if c.isUpper() {
                reset_mark = session.mark();

                c = Tokenizer_nextWLCharacter(session, token_start, policy);

                loop {
                    if c.isAlphaOrDigit() {
                        reset_mark = session.mark();

                        c = Tokenizer_nextWLCharacter(
                            session,
                            token_start,
                            policy,
                        );

                        continue;
                    }

                    if c.to_point() == ']' {
                        wellFormed = true;
                    } else {
                        session.seek(reset_mark);
                    }

                    break;
                }
            }

            if wellFormed {
                return session
                    .token(TokenKind::Error_UnhandledCharacter, token_start);
            }

            return session
                .token(TokenKind::Error_UnhandledCharacter, token_start);
        },
        Char(':') => {
            //
            // Try to reconstruct \:XXXX
            //

            let mut reset_mark = session.mark();

            c = Tokenizer_nextWLCharacter(session, token_start, policy);

            for _ in 0..4 {
                if c.isHex() {
                    reset_mark = session.mark();

                    c = Tokenizer_nextWLCharacter(session, token_start, policy);

                    continue;
                }

                session.seek(reset_mark);

                break;
            }

            return session
                .token(TokenKind::Error_UnhandledCharacter, token_start);
        },
        Char('.') => {
            //
            // Try to reconstruct \.XX
            //

            let mut reset_mark = session.mark();

            c = Tokenizer_nextWLCharacter(session, token_start, policy);

            for _ in 0..2 {
                if c.isHex() {
                    reset_mark = session.mark();

                    c = Tokenizer_nextWLCharacter(session, token_start, policy);

                    continue;
                }

                session.seek(reset_mark);

                break;
            }

            return session
                .token(TokenKind::Error_UnhandledCharacter, token_start);
        },
        Char('0' | '1' | '2' | '3' | '4' | '5' | '6' | '7') => {
            //
            // Try to reconstruct \XXX
            //

            let mut reset_mark = session.mark();

            c = Tokenizer_nextWLCharacter(session, token_start, policy);

            for _ in 0..3 {
                if c.isOctal() {
                    reset_mark = session.mark();

                    c = Tokenizer_nextWLCharacter(session, token_start, policy);

                    continue;
                }

                session.seek(reset_mark);

                break;
            }

            return session
                .token(TokenKind::Error_UnhandledCharacter, token_start);
        },
        Char('|') => {
            //
            // Try to reconstruct \|XXXXXX
            //

            let mut reset_mark = session.mark();
            c = Tokenizer_nextWLCharacter(session, token_start, policy);

            for _ in 0..6 {
                if c.isHex() {
                    reset_mark = session.mark();

                    c = Tokenizer_nextWLCharacter(session, token_start, policy);

                    continue;
                }

                session.seek(reset_mark);

                break;
            }

            return session
                .token(TokenKind::Error_UnhandledCharacter, token_start);
        },
        EndOfFile => {
            return session
                .token(TokenKind::Error_UnhandledCharacter, token_start);
        },
        _ => (),
    } // switch

    //
    // Nothing special, just read next single character
    //

    return session.token(TokenKind::Error_UnhandledCharacter, token_start);
}

fn Tokenizer_handleMBStrangeNewline<'i>(
    session: &mut Tokenizer<'i>,
    token_start: &TokenStart<'i>,
    c: WLCharacter,
    policy: NextPolicy,
) -> TokenRef<'i> {
    assert!(c.isMBStrangeNewline());

    if session.check_issues {
        add_unexpected_char_issue(
            session,
            c,
            token_start.loc,
            IssueTag::UnexpectedNewlineCharacter,
            0.85,
        )
    }

    //
    // Return INTERNALNEWLINE or TOPLEVELNEWLINE, depending on policy
    //
    return session.token(TokenKind::newline_with_policy(policy), token_start);
}

fn Tokenizer_handleMBStrangeWhitespace<'i>(
    session: &mut Tokenizer<'i>,
    token_start: &TokenStart<'i>,
    c: WLCharacter,
    _policy: NextPolicy,
) -> TokenRef<'i> {
    assert!(c.isMBStrangeWhitespace());

    if session.check_issues {
        add_unexpected_char_issue(
            session,
            c,
            token_start.loc,
            IssueTag::UnexpectedSpaceCharacter,
            0.85,
        )
    }

    return session.token(TokenKind::Whitespace, token_start);
}

fn Tokenizer_handleMBPunctuation<'i>(
    session: &mut Tokenizer<'i>,
    token_start: &TokenStart<'i>,
    c: WLCharacter,
    _policy: NextPolicy,
) -> TokenRef<'i> {
    assert!(c.isMBPunctuation());

    let char = c
        .to_point()
        .as_char()
        .expect("expected MBPunctuation to be char");

    let Operator =
        crate::generated::long_names_registration::LongNameCodePointToOperator(
            char,
        );

    return session.token(Operator, token_start);
}

fn Tokenizer_handleNakedMBLinearSyntax<'i>(
    session: &mut Tokenizer<'i>,
    token_start: &TokenStart<'i>,
    c: WLCharacter,
    _policy: NextPolicy,
) -> TokenRef<'i> {
    assert!(c.isMBLinearSyntax());

    match c.to_point() {
        Char(CODEPOINT_LINEARSYNTAX_CLOSEPAREN) => {
            return session
                .token(TokenKind::LinearSyntax_CloseParen, token_start);
        },
        Char(CODEPOINT_LINEARSYNTAX_AT) => {
            return session.token(TokenKind::LinearSyntax_At, token_start);
        },
        Char(CODEPOINT_LINEARSYNTAX_PERCENT) => {
            return session.token(TokenKind::LinearSyntax_Percent, token_start);
        },
        Char(CODEPOINT_LINEARSYNTAX_CARET) => {
            return session.token(TokenKind::LinearSyntax_Caret, token_start);
        },
        Char(CODEPOINT_LINEARSYNTAX_AMP) => {
            return session.token(TokenKind::LinearSyntax_Amp, token_start);
        },
        Char(CODEPOINT_LINEARSYNTAX_STAR) => {
            return session.token(TokenKind::LinearSyntax_Star, token_start);
        },
        Char(CODEPOINT_LINEARSYNTAX_UNDER) => {
            return session.token(TokenKind::LinearSyntax_Under, token_start);
        },
        Char(CODEPOINT_LINEARSYNTAX_PLUS) => {
            return session.token(TokenKind::LinearSyntax_Plus, token_start);
        },
        Char(CODEPOINT_LINEARSYNTAX_SLASH) => {
            return session.token(TokenKind::LinearSyntax_Slash, token_start);
        },
        Char(CODEPOINT_LINEARSYNTAX_BACKTICK) => {
            return session
                .token(TokenKind::LinearSyntax_BackTick, token_start);
        },
        CodePoint::LinearSyntax_Space => {
            return session.token(TokenKind::LinearSyntax_Space, token_start);
        },
        _ => todo!(),
    }
}

//==========================================================
// Helper functions
//==========================================================

/// Add an [`Issue`][crate::issue::Issue] for an unexpected character.
fn add_unexpected_char_issue(
    session: &mut Tokenizer,
    c: WLCharacter,
    char_loc: Location,
    tag: IssueTag,
    confidence: f64,
) {
    let src = session.get_token_span(char_loc);

    let actions: Vec<CodeAction> =
        utils::certainCharacterReplacementActions(c, src);

    let formatted = match tag {
        IssueTag::UnexpectedNewlineCharacter => c.graphicalString(),
        _ => c.safeAndGraphicalString(),
    };

    let char_type = match tag {
        IssueTag::UnexpectedSpaceCharacter => "space",
        IssueTag::UnexpectedLetterlikeCharacter => "letterlike",
        IssueTag::UnexpectedNewlineCharacter => "newline",
        _ => panic!("unhandled unexpected char IssueTag: {tag:?}"),
    };

    let I = SyntaxIssue(
        tag,
        format!("Unexpected {char_type} character: ``{}``.", formatted),
        Severity::Warning,
        src,
        confidence,
        actions,
        vec![],
    );

    session.addIssue(I);
}
