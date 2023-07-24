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
        wl_character::{EscapeStyle, WLCharacter},
        InputMark, Reader,
    },
    source::{
        Buffer, BufferAndLength, Location, NextPolicy, SourceCharacter, Span, INSIDE_SLOT,
        INSIDE_STRINGIFY_AS_FILE, INSIDE_STRINGIFY_AS_TAG, TOPLEVEL,
    },
    tokenize::{token_enum::Closer, Token, TokenKind, TokenRef},
    utils, FirstLineBehavior,
};

use crate::source::NextPolicyBits::*;

#[derive(Debug)]
pub(crate) struct Tokenizer<'i> {
    pub(crate) reader: Reader<'i>,

    pub(crate) firstLineBehavior: FirstLineBehavior,

    pub GroupStack: Vec<Closer>,

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

impl UnsafeCharacterEncoding {
    pub fn as_str(&self) -> &'static str {
        match self {
            UnsafeCharacterEncoding::IncompleteUTF8Sequence => "IncompleteUTF8Sequence",
            UnsafeCharacterEncoding::StraySurrogate => "StraySurrogate",
            UnsafeCharacterEncoding::BOM => "BOM",
            // NOTE: When adding a case here, also update from_str().
        }
    }

    #[doc(hidden)]
    pub fn from_str(string: &str) -> Option<Self> {
        let value = match string {
            "IncompleteUTF8Sequence" => UnsafeCharacterEncoding::IncompleteUTF8Sequence,
            "StraySurrogate" => UnsafeCharacterEncoding::StraySurrogate,
            "BOM" => UnsafeCharacterEncoding::BOM,
            _ => return None,
        };

        Some(value)
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
    pub(crate) fn peek_token_with(&mut self, mut policy: NextPolicy) -> TokenRef<'i> {
        let insideGroup: bool = !self.GroupStack.is_empty();

        //
        // if insideGroup:
        //   returnInternalNewlineMask is 0b100
        // else:
        //   returnInternalNewlineMask is 0b000
        //
        let returnInternalNewlineMask = (insideGroup as u8) << 2;

        policy &= !returnInternalNewlineMask; // bitwise not

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

impl NumberTokenizationContext {
    pub fn new() -> Self {
        NumberTokenizationContext {
            InvalidBase: false,
            UnrecognizedDigit: false,
            NegativeExponent: false,
            Real: false,
            NonZeroExponentDigitCount: 0,
            Base: 0,
        }
    }
}

// TODO: Why does putting this type in the root module cause Rust Analyzer
//             to not work on this file?
// pub mod handler {
type HandlerFunction = for<'p, 'i> fn(
    session: &'p mut Tokenizer<'i>,
    startBuf: Buffer<'i>,
    startLoc: Location,
    c: WLCharacter,
    policy: NextPolicy,
) -> TokenRef<'i>;
// }

// use self::handler::HandlerFunction;

// #define U Tokenizer_nextToken_uncommon

macro_rules! U {
    () => {
        Tokenizer_nextToken_uncommon
    };
}

const TOKENIZER_HANDLER_TABLE: [HandlerFunction; 128] = [
    U!(),
    U!(),
    U!(),
    U!(),
    U!(),
    U!(),
    U!(),
    U!(),
    U!(),
    U!(),
    Tokenizer_handleLineFeed,
    U!(),
    U!(),
    U!(),
    U!(),
    U!(),
    U!(),
    U!(),
    U!(),
    U!(),
    U!(),
    U!(),
    U!(),
    U!(),
    U!(),
    U!(),
    U!(),
    U!(),
    U!(),
    U!(),
    U!(),
    U!(),
    Tokenizer_handleSpace,
    U!(),
    Tokenizer_handleString,
    U!(),
    Tokenizer_handleSymbol,
    U!(),
    U!(),
    U!(),
    U!(),
    U!(),
    U!(),
    U!(),
    Tokenizer_handleComma,
    Tokenizer_handleMinus,
    U!(),
    U!(),
    Tokenizer_handleNumber,
    Tokenizer_handleNumber,
    Tokenizer_handleNumber,
    Tokenizer_handleNumber,
    Tokenizer_handleNumber,
    Tokenizer_handleNumber,
    Tokenizer_handleNumber,
    Tokenizer_handleNumber,
    Tokenizer_handleNumber,
    Tokenizer_handleNumber,
    U!(),
    U!(),
    U!(),
    U!(),
    U!(),
    U!(),
    U!(),
    Tokenizer_handleSymbol,
    Tokenizer_handleSymbol,
    Tokenizer_handleSymbol,
    Tokenizer_handleSymbol,
    Tokenizer_handleSymbol,
    Tokenizer_handleSymbol,
    Tokenizer_handleSymbol,
    Tokenizer_handleSymbol,
    Tokenizer_handleSymbol,
    Tokenizer_handleSymbol,
    Tokenizer_handleSymbol,
    Tokenizer_handleSymbol,
    Tokenizer_handleSymbol,
    Tokenizer_handleSymbol,
    Tokenizer_handleSymbol,
    Tokenizer_handleSymbol,
    Tokenizer_handleSymbol,
    Tokenizer_handleSymbol,
    Tokenizer_handleSymbol,
    Tokenizer_handleSymbol,
    Tokenizer_handleSymbol,
    Tokenizer_handleSymbol,
    Tokenizer_handleSymbol,
    Tokenizer_handleSymbol,
    Tokenizer_handleSymbol,
    Tokenizer_handleSymbol,
    Tokenizer_handleOpenSquare,
    U!(),
    Tokenizer_handleCloseSquare,
    U!(),
    U!(),
    U!(),
    Tokenizer_handleSymbol,
    Tokenizer_handleSymbol,
    Tokenizer_handleSymbol,
    Tokenizer_handleSymbol,
    Tokenizer_handleSymbol,
    Tokenizer_handleSymbol,
    Tokenizer_handleSymbol,
    Tokenizer_handleSymbol,
    Tokenizer_handleSymbol,
    Tokenizer_handleSymbol,
    Tokenizer_handleSymbol,
    Tokenizer_handleSymbol,
    Tokenizer_handleSymbol,
    Tokenizer_handleSymbol,
    Tokenizer_handleSymbol,
    Tokenizer_handleSymbol,
    Tokenizer_handleSymbol,
    Tokenizer_handleSymbol,
    Tokenizer_handleSymbol,
    Tokenizer_handleSymbol,
    Tokenizer_handleSymbol,
    Tokenizer_handleSymbol,
    Tokenizer_handleSymbol,
    Tokenizer_handleSymbol,
    Tokenizer_handleSymbol,
    Tokenizer_handleSymbol,
    Tokenizer_handleOpenCurly,
    U!(),
    Tokenizer_handleCloseCurly,
    U!(),
    U!(),
];

pub(crate) const ASCII_VTAB: char = '\x0B';
pub(crate) const ASCII_FORM_FEED: char = '\x0C';

pub(crate) fn Token<'i, T: Into<TokenKind>>(
    tok: T,
    buf: BufferAndLength<'i>,
    src: Span,
) -> TokenRef<'i> {
    let tok = tok.into();
    Token::new(tok, buf, src)
}

fn Tokenizer_nextToken<'i>(session: &mut Tokenizer<'i>, policy: NextPolicy) -> TokenRef<'i> {
    let tokenStartBuf = session.buffer();
    let tokenStartLoc = session.SrcLoc;

    let c = Tokenizer_nextWLCharacter(session, tokenStartBuf, tokenStartLoc, policy);

    let point: CodePoint = c.to_point();
    let point = point.as_i32();

    if !(0x00 <= point && point <= 0x7f) {
        return Tokenizer_nextToken_uncommon(session, tokenStartBuf, tokenStartLoc, c, policy);
    }

    let func = TOKENIZER_HANDLER_TABLE[usize::try_from(point).unwrap()];
    return func(session, tokenStartBuf, tokenStartLoc, c, policy);
}

fn Tokenizer_nextToken_uncommon<'i>(
    session: &mut Tokenizer<'i>,
    tokenStartBuf: Buffer<'i>,
    tokenStartLoc: Location,
    c: WLCharacter,
    policy: NextPolicy,
) -> TokenRef<'i> {
    match c.to_point() {
        Char(_) => (),
        EndOfFile => {
            return Token(
                TokenKind::EndOfFile,
                Tokenizer_getTokenBufferAndLength(session, tokenStartBuf),
                Tokenizer_getTokenSource(session, tokenStartLoc),
            );
        },
        Unsafe1ByteUtf8Sequence | Unsafe2ByteUtf8Sequence | Unsafe3ByteUtf8Sequence => {
            //
            // This will be disposed before the user sees it
            //

            return Token(
                TokenKind::Error_UnsafeCharacterEncoding,
                Tokenizer_getTokenBufferAndLength(session, tokenStartBuf),
                Tokenizer_getTokenSource(session, tokenStartLoc),
            );
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

            return Tokenizer_handleSymbol(session, tokenStartBuf, tokenStartLoc, c, policy);
        }
        Char(CODEPOINT_BEL | CODEPOINT_DEL) => {
            return Token(TokenKind::Error_UnhandledCharacter, Tokenizer_getTokenBufferAndLength(session, tokenStartBuf), Tokenizer_getTokenSource(session, tokenStartLoc));
        }
        Char('\t') => {
            // MUSTTAIL
            return Token(TokenKind::Whitespace, Tokenizer_getTokenBufferAndLength(session, tokenStartBuf), Tokenizer_getTokenSource(session, tokenStartLoc));
        }
        Char(ASCII_VTAB | ASCII_FORM_FEED) => {

//            MUSTTAIL
            return Tokenizer_handleStrangeWhitespace(session, tokenStartBuf, tokenStartLoc, c, policy);
        }
        Char('\r') => {

            incr_diagnostic!(Tokenizer_NewlineCount);

            //
            // Return INTERNALNEWLINE or TOPLEVELNEWLINE, depending on policy
            //
            return Token(
                TokenKind::InternalNewline.with_policy(policy),
                Tokenizer_getTokenBufferAndLength(session, tokenStartBuf),
                Tokenizer_getTokenSource(session, tokenStartLoc)
            );
        }
        Char('(') => {

//            MUSTTAIL
            return Tokenizer_handleOpenParen(session, tokenStartBuf, tokenStartLoc, c, policy);
        }
        Char(')') => {

            incr_diagnostic!(Tokenizer_CloseParenCount);

            return Token(TokenKind::CloseParen, Tokenizer_getTokenBufferAndLength(session, tokenStartBuf), Tokenizer_getTokenSource(session, tokenStartLoc));
        }
        Char('+') => {

//            MUSTTAIL
            return Tokenizer_handlePlus(session, tokenStartBuf, tokenStartLoc, c, policy);
        }
        Char('^') => {

//            MUSTTAIL
            return Tokenizer_handleCaret(session, tokenStartBuf, tokenStartLoc, c, policy);
        }
        Char('=') => {

//            MUSTTAIL
            return Tokenizer_handleEqual(session, tokenStartBuf, tokenStartLoc, c, policy);
        }
        Char(';') => {

//            MUSTTAIL
            return Tokenizer_handleSemi(session, tokenStartBuf, tokenStartLoc, c, policy);
        }
        Char(':') => {

//            MUSTTAIL
            return Tokenizer_handleColon(session, tokenStartBuf, tokenStartLoc, c, policy);
        }
        Char('#') => {

//            MUSTTAIL
            return Tokenizer_handleHash(session, tokenStartBuf, tokenStartLoc, c, policy);
        }
        Char('&') => {

//            MUSTTAIL
            return Tokenizer_handleAmp(session, tokenStartBuf, tokenStartLoc, c, policy);
        }
        Char('!') => {

//            MUSTTAIL
            return Tokenizer_handleBang(session, tokenStartBuf, tokenStartLoc, c, policy);
        }
        Char('%') => {

//            MUSTTAIL
            return Tokenizer_handlePercent(session, tokenStartBuf, tokenStartLoc, c, policy);
        }
        Char('\'') => {

            return Token(TokenKind::SingleQuote, Tokenizer_getTokenBufferAndLength(session, tokenStartBuf), Tokenizer_getTokenSource(session, tokenStartLoc));
        }
        Char('*') => {

//            MUSTTAIL
            return Tokenizer_handleStar(session, tokenStartBuf, tokenStartLoc, c, policy);
        }
        Char('.') => {

//            MUSTTAIL
            return Tokenizer_handleDot(session, tokenStartBuf, tokenStartLoc, c, policy);
        }
        Char('/') => {

//            MUSTTAIL
            return Tokenizer_handleSlash(session, tokenStartBuf, tokenStartLoc, c, policy);
        }
        Char('<') => {

//            MUSTTAIL
            return Tokenizer_handleLess(session, tokenStartBuf, tokenStartLoc, c, policy);
        }
        Char('>') => {

//            MUSTTAIL
            return Tokenizer_handleGreater(session, tokenStartBuf, tokenStartLoc, c, policy);
        }
        Char('?') => {

//            MUSTTAIL
            return Tokenizer_handleQuestion(session, tokenStartBuf, tokenStartLoc, c, policy);
        }
        Char('@') => {

//            MUSTTAIL
            return Tokenizer_handleAt(session, tokenStartBuf, tokenStartLoc, c, policy);
        }
        Char('\\') => {

//            MUSTTAIL
            return Tokenizer_handleUnhandledBackslash(session, tokenStartBuf, tokenStartLoc, c, policy);
        }
        Char('_') => {

//            MUSTTAIL
            return Tokenizer_handleUnder(session, tokenStartBuf, tokenStartLoc, c, policy);
        }
        Char('|') => {

//            MUSTTAIL
            return Tokenizer_handleBar(session, tokenStartBuf, tokenStartLoc, c, policy);
        }
        Char('~') => {
            // MUSTTAIL
            return Tokenizer_handleTilde(session, tokenStartBuf, tokenStartLoc, c, policy);
        }
        Char(CODEPOINT_LINEARSYNTAX_BANG) => {
            return Token(TokenKind::LinearSyntax_Bang, Tokenizer_getTokenBufferAndLength(session, tokenStartBuf), Tokenizer_getTokenSource(session, tokenStartLoc));
        }
        Char(CODEPOINT_LINEARSYNTAX_OPENPAREN) => {
            // MUSTTAIL
            return Tokenizer_handleMBLinearSyntaxBlob(session, tokenStartBuf, tokenStartLoc, c, policy);
        }
        _ => ()
    }


    if c.isMBLinearSyntax() {
        //        MUSTTAIL
        return Tokenizer_handleNakedMBLinearSyntax(
            session,
            tokenStartBuf,
            tokenStartLoc,
            c,
            policy,
        );
    }

    if c.isMBUninterpretable() {
        return Token(
            TokenKind::Error_UnhandledCharacter,
            Tokenizer_getTokenBufferAndLength(session, tokenStartBuf),
            Tokenizer_getTokenSource(session, tokenStartLoc),
        );
    }

    if c.isMBStrangeWhitespace() {
        //        MUSTTAIL
        return Tokenizer_handleMBStrangeWhitespace(
            session,
            tokenStartBuf,
            tokenStartLoc,
            c,
            policy,
        );
    }

    if c.isMBWhitespace() {
        return Token(
            TokenKind::Whitespace,
            Tokenizer_getTokenBufferAndLength(session, tokenStartBuf),
            Tokenizer_getTokenSource(session, tokenStartLoc),
        );
    }

    if c.isMBStrangeNewline() {
        //        MUSTTAIL
        return Tokenizer_handleMBStrangeNewline(session, tokenStartBuf, tokenStartLoc, c, policy);
    }

    if c.isMBNewline() {
        //
        // Return INTERNALNEWLINE or TOPLEVELNEWLINE, depending on policy
        //
        return Token(
            TokenKind::InternalNewline.with_policy(policy),
            Tokenizer_getTokenBufferAndLength(session, tokenStartBuf),
            Tokenizer_getTokenSource(session, tokenStartLoc),
        );
    }

    if c.isMBPunctuation() {
        //        MUSTTAIL
        return Tokenizer_handleMBPunctuation(session, tokenStartBuf, tokenStartLoc, c, policy);
    }

    if c.isMBStringMeta() {
        return Token(
            TokenKind::Error_UnhandledCharacter,
            Tokenizer_getTokenBufferAndLength(session, tokenStartBuf),
            Tokenizer_getTokenSource(session, tokenStartLoc),
        );
    }

    //
    // if nothing else, then it is letterlike
    //

    assert!(c.isMBLetterlike());

    //    MUSTTAIL
    return Tokenizer_handleSymbol(session, tokenStartBuf, tokenStartLoc, c, policy);
}

pub(crate) fn Tokenizer_nextToken_stringifyAsTag<'i>(session: &mut Tokenizer<'i>) -> TokenRef<'i> {
    let tokenStartBuf = session.buffer();
    let tokenStartLoc = session.SrcLoc;

    let policy = INSIDE_STRINGIFY_AS_TAG;

    let c = Tokenizer_nextWLCharacter(session, tokenStartBuf, tokenStartLoc, policy);

    match c.to_point() {
        EndOfFile => {
            //
            // EndOfFile is special, so invent source
            //

            return Token(
                TokenKind::Error_ExpectedTag,
                // BufferAndLength::from_buffer(tokenStartBuf),
                Tokenizer_getTokenBufferAndLength(session, tokenStartBuf),
                Span::from_location(tokenStartLoc),
            );
        },
        Char('\n' | '\r') | CRLF => {
            //
            // Newline is special, so invent source
            //

            return Token(
                TokenKind::Error_ExpectedTag,
                // BufferAndLength::from_buffer(tokenStartBuf),
                Tokenizer_getTokenBufferAndLength(session, tokenStartBuf),
                Span::from_location(tokenStartLoc),
            );
        },
        Char('"') => {
            return Tokenizer_handleString(session, tokenStartBuf, tokenStartLoc, c, policy);
        },
        //
        // Default
        //
        _ => {
            return Tokenizer_handleString_stringifyAsTag(
                session,
                tokenStartBuf,
                tokenStartLoc,
                c,
                policy,
            );
        },
    }
}

//
// Use SourceCharacters here, not WLCharacters
//
pub(crate) fn Tokenizer_nextToken_stringifyAsFile<'i>(session: &mut Tokenizer<'i>) -> TokenRef<'i> {
    let tokenStartBuf = session.buffer();
    let tokenStartLoc = session.SrcLoc;

    let policy = INSIDE_STRINGIFY_AS_FILE;

    let c = session.next_source_char(policy);

    match c {
        EndOfFile => {
            return Token(
                TokenKind::Error_ExpectedFile,
                // BufferAndLength::from_buffer(tokenStartBuf),
                Tokenizer_getTokenBufferAndLength(session, tokenStartBuf),
                Span::from_location(tokenStartLoc),
            );
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
            return Token(
                TokenKind::InternalNewline.with_policy(policy),
                Tokenizer_getTokenBufferAndLength(session, tokenStartBuf),
                Tokenizer_getTokenSource(session, tokenStartLoc),
            );
        },
        Char(' ' | '\t') => {
            //
            // There could be space, something like  << abc
            //
            // or something like:
            // a >>
            //   b
            //
            return Token(
                TokenKind::Whitespace,
                Tokenizer_getTokenBufferAndLength(session, tokenStartBuf),
                Tokenizer_getTokenSource(session, tokenStartLoc),
            );
        },
        Char('"') => {
            return Tokenizer_handleString(
                session,
                tokenStartBuf,
                tokenStartLoc,
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
                tokenStartBuf,
                tokenStartLoc,
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
    _tokenStartBuf: Buffer,
    tokenStartLoc: Location,
    policy: NextPolicy,
) -> WLCharacter {
    incr_diagnostic!(Tokenizer_LineContinuationCount);

    let mut c = session.next_wolfram_char(policy);

    let mut point = c.to_point();

    loop {
        if !point.is_line_continuation() {
            return c;
        }

        c = session.peek_wolfram_char(policy);

        point = c.to_point();

        //
        // Even though strings preserve the whitespace after a line continuation, and
        // e.g., integers do NOT preserve the whitespace after a line continuation,
        // we do not need to worry about that here.
        //
        // There are no choices to be made here.
        // All whitespace after a line continuation can be ignored for the purposes of tokenization
        //
        while c.isWhitespace() {
            if feature::COMPUTE_OOB {
                if point == '\t' {
                    if (policy & STRING_OR_COMMENT) == STRING_OR_COMMENT {
                        //
                        // It is possible to have e.g.:
                        //
                        //"a\
                        //<tab>b"
                        //
                        // where the embedded tab gets consumed by the whitespace loop after the line continuation.
                        //
                        // Must still count the embedded tab

                        session.addEmbeddedTab(tokenStartLoc);
                    }
                }
            }

            session.next_wolfram_char(policy);

            c = session.peek_wolfram_char(policy);

            point = c.to_point();
        }

        if feature::COMPUTE_OOB {
            if (policy & TRACK_LC) == TRACK_LC {
                if (policy & STRING_OR_COMMENT) == STRING_OR_COMMENT {
                    session.addComplexLineContinuation(tokenStartLoc);
                } else {
                    session.addSimpleLineContinuation(tokenStartLoc);
                }
            }
        }

        session.next_wolfram_char(policy);
    } // loop
}

fn Tokenizer_currentWLCharacter<'i>(
    session: &mut Tokenizer<'i>,
    tokenStartBuf: Buffer<'i>,
    tokenStartLoc: Location,
    mut policy: NextPolicy,
) -> WLCharacter {
    let mark = session.mark();

    //
    //
    //
    policy &= !TRACK_LC; // bitwise not

    let c = Tokenizer_nextWLCharacter(session, tokenStartBuf, tokenStartLoc, policy);

    session.seek(mark);

    return c;
}

fn Tokenizer_handleComma<'i>(
    session: &mut Tokenizer<'i>,
    tokenStartBuf: Buffer<'i>,
    tokenStartLoc: Location,
    _firstChar: WLCharacter,
    _policy: NextPolicy,
) -> TokenRef<'i> {
    incr_diagnostic!(Tokenizer_CommaCount);

    return Token(
        TokenKind::Comma,
        Tokenizer_getTokenBufferAndLength(session, tokenStartBuf),
        Tokenizer_getTokenSource(session, tokenStartLoc),
    );
}

fn Tokenizer_handleLineFeed<'i>(
    session: &mut Tokenizer<'i>,
    tokenStartBuf: Buffer<'i>,
    tokenStartLoc: Location,
    _firstChar: WLCharacter,
    policy: NextPolicy,
) -> TokenRef<'i> {
    incr_diagnostic!(Tokenizer_NewlineCount);

    //
    // Return INTERNALNEWLINE or TOPLEVELNEWLINE, depending on policy
    //
    return Token(
        TokenKind::InternalNewline.with_policy(policy),
        Tokenizer_getTokenBufferAndLength(session, tokenStartBuf),
        Tokenizer_getTokenSource(session, tokenStartLoc),
    );
}

fn Tokenizer_handleOpenSquare<'i>(
    session: &mut Tokenizer<'i>,
    tokenStartBuf: Buffer<'i>,
    tokenStartLoc: Location,
    _firstChar: WLCharacter,
    _policy: NextPolicy,
) -> TokenRef<'i> {
    incr_diagnostic!(Tokenizer_OpenSquareCount);

    return Token(
        TokenKind::OpenSquare,
        Tokenizer_getTokenBufferAndLength(session, tokenStartBuf),
        Tokenizer_getTokenSource(session, tokenStartLoc),
    );
}

fn Tokenizer_handleOpenCurly<'i>(
    session: &mut Tokenizer<'i>,
    tokenStartBuf: Buffer<'i>,
    tokenStartLoc: Location,
    _firstChar: WLCharacter,
    _policy: NextPolicy,
) -> TokenRef<'i> {
    incr_diagnostic!(Tokenizer_OpenCurlyCount);

    return Token(
        TokenKind::OpenCurly,
        Tokenizer_getTokenBufferAndLength(session, tokenStartBuf),
        Tokenizer_getTokenSource(session, tokenStartLoc),
    );
}

fn Tokenizer_handleSpace<'i>(
    session: &mut Tokenizer<'i>,
    tokenStartBuf: Buffer<'i>,
    tokenStartLoc: Location,
    _firstChar: WLCharacter,
    _policy: NextPolicy,
) -> TokenRef<'i> {
    incr_diagnostic!(Tokenizer_WhitespaceCount);

    return Token(
        TokenKind::Whitespace,
        Tokenizer_getTokenBufferAndLength(session, tokenStartBuf),
        Tokenizer_getTokenSource(session, tokenStartLoc),
    );
}

fn Tokenizer_handleCloseSquare<'i>(
    session: &mut Tokenizer<'i>,
    tokenStartBuf: Buffer<'i>,
    tokenStartLoc: Location,
    _firstChar: WLCharacter,
    _policy: NextPolicy,
) -> TokenRef<'i> {
    incr_diagnostic!(Tokenizer_CloseSquareCount);

    return Token(
        TokenKind::CloseSquare,
        Tokenizer_getTokenBufferAndLength(session, tokenStartBuf),
        Tokenizer_getTokenSource(session, tokenStartLoc),
    );
}

fn Tokenizer_handleCloseCurly<'i>(
    session: &mut Tokenizer<'i>,
    tokenStartBuf: Buffer<'i>,
    tokenStartLoc: Location,
    _firstChar: WLCharacter,
    _policy: NextPolicy,
) -> TokenRef<'i> {
    incr_diagnostic!(Tokenizer_CloseCurlyCount);

    return Token(
        TokenKind::CloseCurly,
        Tokenizer_getTokenBufferAndLength(session, tokenStartBuf),
        Tokenizer_getTokenSource(session, tokenStartLoc),
    );
}

fn Tokenizer_handleStrangeWhitespace<'i>(
    session: &mut Tokenizer<'i>,
    tokenStartBuf: Buffer<'i>,
    tokenStartLoc: Location,
    c: WLCharacter,
    _policy: NextPolicy,
) -> TokenRef<'i> {
    assert!(c.isStrangeWhitespace());

    if feature::CHECK_ISSUES {
        let Src = Tokenizer_getTokenSource(session, tokenStartLoc);

        let mut Actions: Vec<CodeAction> = Vec::new();

        for A in utils::certainCharacterReplacementActions(c, Src) {
            Actions.push(A);
        }

        let I = SyntaxIssue(
            IssueTag::UnexpectedSpaceCharacter,
            format!(
                "Unexpected space character: ``{}``.",
                c.safeAndGraphicalString()
            ),
            Severity::Warning,
            Src,
            0.95,
            Actions,
            vec![],
        );

        session.addIssue(I);
    }

    return Token(
        TokenKind::Whitespace,
        Tokenizer_getTokenBufferAndLength(session, tokenStartBuf),
        Tokenizer_getTokenSource(session, tokenStartLoc),
    );
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
    tokenStartBuf: Buffer<'i>,
    tokenStartLoc: Location,
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
                        return Token(
                            TokenKind::Comment,
                            Tokenizer_getTokenBufferAndLength(session, tokenStartBuf),
                            Tokenizer_getTokenSource(session, tokenStartLoc),
                        );
                    }

                    c = session.next_source_char(policy);
                }
            },
            EndOfFile => {
                return Token(
                    TokenKind::Error_UnterminatedComment,
                    Tokenizer_getTokenBufferAndLength(session, tokenStartBuf),
                    Tokenizer_getTokenSource(session, tokenStartLoc),
                );
            },
            Char('\n' | '\r') | CRLF => {
                if feature::COMPUTE_OOB {
                    session.addEmbeddedNewline(tokenStartLoc);
                }

                c = session.next_source_char(policy);
            },
            Char('\t') => {
                if feature::COMPUTE_OOB {
                    session.addEmbeddedTab(tokenStartLoc);
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
    tokenStartBuf: Buffer<'i>,
    tokenStartLoc: Location,
    mut c: WLCharacter,
    policy: NextPolicy,
) -> TokenRef<'i> {
    assert!(c.to_point() == CODEPOINT_LINEARSYNTAX_OPENPAREN);

    let mut depth = 1;

    c = Tokenizer_nextWLCharacter(session, tokenStartBuf, tokenStartLoc, policy);

    loop {
        match c.to_point() {
            Char(CODEPOINT_LINEARSYNTAX_OPENPAREN) => {
                depth = depth + 1;

                c = Tokenizer_nextWLCharacter(session, tokenStartBuf, tokenStartLoc, policy);
            },
            Char(CODEPOINT_LINEARSYNTAX_CLOSEPAREN) => {
                depth = depth - 1;

                if depth == 0 {
                    return Token(
                        TokenKind::LinearSyntaxBlob,
                        Tokenizer_getTokenBufferAndLength(session, tokenStartBuf),
                        Tokenizer_getTokenSource(session, tokenStartLoc),
                    );
                }

                c = Tokenizer_nextWLCharacter(session, tokenStartBuf, tokenStartLoc, policy);
            },
            EndOfFile => {
                return Token(
                    TokenKind::Error_UnterminatedLinearSyntaxBlob,
                    Tokenizer_getTokenBufferAndLength(session, tokenStartBuf),
                    Tokenizer_getTokenSource(session, tokenStartLoc),
                );
            },
            _ => {
                c = Tokenizer_nextWLCharacter(session, tokenStartBuf, tokenStartLoc, policy);
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
    tokenStartBuf: Buffer<'i>,
    tokenStartLoc: Location,
    mut c: WLCharacter,
    policy: NextPolicy,
) -> TokenRef<'i> {
    assert!(c.to_point() == '`' || c.isLetterlike() || c.isMBLetterlike());

    incr_diagnostic!(Tokenizer_SymbolCount);

    if c.isLetterlike() || c.isMBLetterlike() {
        c = Tokenizer_handleSymbolSegment(
            session,
            tokenStartBuf,
            tokenStartLoc,
            tokenStartBuf,
            tokenStartLoc,
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

        if feature::CHECK_ISSUES && (policy & INSIDE_SLOT) == INSIDE_SLOT {
            //
            // Something like  #`a
            //
            // It's hard to keep track of the ` characters, so just report the entire symbol. Oh well
            //

            let I = SyntaxIssue(
                IssueTag::UndocumentedSlotSyntax,
                "The name following ``#`` is not documented to allow the **`** character.".into(),
                Severity::Warning,
                Tokenizer_getTokenSource(session, tokenStartLoc),
                0.33,
                vec![],
                vec![],
            );

            session.addIssue(I);
        }

        c = Tokenizer_currentWLCharacter(session, tokenStartBuf, tokenStartLoc, policy);

        if c.isLetterlike() || c.isMBLetterlike() {
            let letterlikeBuf = session.buffer();
            let letterlikeLoc = session.SrcLoc;

            Tokenizer_nextWLCharacter(session, tokenStartBuf, tokenStartLoc, policy);

            c = Tokenizer_handleSymbolSegment(
                session,
                tokenStartBuf,
                tokenStartLoc,
                letterlikeBuf,
                letterlikeLoc,
                c,
                policy,
            );
        } else {
            //
            // Something like  a`1
            //

            return Token(
                TokenKind::Error_ExpectedLetterlike,
                Tokenizer_getTokenBufferAndLength(session, tokenStartBuf),
                Tokenizer_getTokenSource(session, tokenStartLoc),
            );
        }
    } // while

    return Token(
        if (policy & INSIDE_SLOT) == INSIDE_SLOT {
            TokenKind::String
        } else {
            TokenKind::Symbol
        },
        Tokenizer_getTokenBufferAndLength(session, tokenStartBuf),
        Tokenizer_getTokenSource(session, tokenStartLoc),
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
    tokenStartBuf: Buffer<'i>,
    tokenStartLoc: Location,
    _charBuf: Buffer,
    mut charLoc: Location,
    mut c: WLCharacter,
    policy: NextPolicy,
) -> WLCharacter {
    assert!(c.isLetterlike() || c.isMBLetterlike());

    #[cfg(feature = "CHECK_ISSUES")]
    if c.to_point() == '$' {
        if (policy & INSIDE_SLOT) == INSIDE_SLOT {
            //
            // Something like  #$a
            //

            let I = SyntaxIssue(
                IssueTag::UndocumentedSlotSyntax,
                "The name following ``#`` is not documented to allow the ``$`` character."
                    .to_owned(),
                Severity::Warning,
                Tokenizer_getTokenSource(session, charLoc),
                0.33,
                vec![],
                vec![],
            );

            session.addIssue(I);
        }
    } else if c.isStrangeLetterlike() {
        let Src = Tokenizer_getTokenSource(session, charLoc);

        let mut Actions: Vec<CodeAction> = Vec::new();

        for A in utils::certainCharacterReplacementActions(c, Src) {
            Actions.push(A);
        }

        let I = SyntaxIssue(
            IssueTag::UnexpectedLetterlikeCharacter,
            format!(
                "Unexpected letterlike character: ``{}``.",
                c.safeAndGraphicalString()
            ),
            Severity::Warning,
            Src,
            0.85,
            Actions,
            vec![],
        );

        session.addIssue(I);
    } else if c.isMBStrangeLetterlike() {
        let Src = Tokenizer_getTokenSource(session, charLoc);

        let mut Actions: Vec<CodeAction> = Vec::new();

        for A in utils::certainCharacterReplacementActions(c, Src) {
            Actions.push(A);
        }

        let I = SyntaxIssue(
            IssueTag::UnexpectedLetterlikeCharacter,
            format!(
                "Unexpected letterlike character: ``{}``.",
                c.safeAndGraphicalString()
            ),
            Severity::Warning,
            Src,
            0.80,
            Actions,
            vec![],
        );

        session.addIssue(I);
    } else if !c.isAlpha() {
        if (policy & INSIDE_STRINGIFY_AS_TAG) == INSIDE_STRINGIFY_AS_TAG {
            //
            // Something like  a::\[Beta]
            //

            let I = SyntaxIssue(
                IssueTag::UnexpectedCharacter,
                "The tag has non-alphanumeric source characters.".to_owned(),
                Severity::Warning,
                Span::new(charLoc, session.SrcLoc),
                0.85,
                vec![],
                vec![],
            );

            session.addIssue(I);
        }
    }

    charLoc = session.SrcLoc;

    c = Tokenizer_currentWLCharacter(session, tokenStartBuf, tokenStartLoc, policy);

    loop {
        if c.isDigit() {
            Tokenizer_nextWLCharacter(session, tokenStartBuf, tokenStartLoc, policy);

            charLoc = session.SrcLoc;

            c = Tokenizer_currentWLCharacter(session, tokenStartBuf, tokenStartLoc, policy);
        } else if c.isLetterlike() || c.isMBLetterlike() {
            Tokenizer_nextWLCharacter(session, tokenStartBuf, tokenStartLoc, policy);

            #[cfg(feature = "CHECK_ISSUES")]
            if c.to_point() == '$' {
                if (policy & INSIDE_SLOT) == INSIDE_SLOT {
                    //
                    // Something like  #$a
                    //

                    let I = SyntaxIssue(
                        IssueTag::UndocumentedSlotSyntax,
                        format!("The name following ``#`` is not documented to allow the ``$`` character."),
                        Severity::Warning,
                        Tokenizer_getTokenSource(session, charLoc),
                        0.33,
                        vec![],
                        vec![],
                    );

                    session.addIssue(I);
                }
            } else if c.isStrangeLetterlike() {
                let Src = Tokenizer_getTokenSource(session, charLoc);

                let mut Actions: Vec<CodeAction> = Vec::new();

                for A in utils::certainCharacterReplacementActions(c, Src) {
                    Actions.push(A);
                }

                let I = SyntaxIssue(
                    IssueTag::UnexpectedLetterlikeCharacter,
                    format!(
                        "Unexpected letterlike character: ``{}``.",
                        c.safeAndGraphicalString()
                    ),
                    Severity::Warning,
                    Src,
                    0.85,
                    Actions,
                    vec![],
                );

                session.addIssue(I);
            } else if c.isMBStrangeLetterlike() {
                let Src = Tokenizer_getTokenSource(session, charLoc);

                let mut Actions: Vec<CodeAction> = Vec::new();

                for A in utils::certainCharacterReplacementActions(c, Src) {
                    Actions.push(A);
                }

                let I = SyntaxIssue(
                    IssueTag::UnexpectedLetterlikeCharacter,
                    format!(
                        "Unexpected letterlike character: ``{}``.",
                        c.safeAndGraphicalString()
                    ),
                    Severity::Warning,
                    Src,
                    0.80,
                    Actions,
                    vec![],
                );

                session.addIssue(I);
            } else if !c.isAlphaOrDigit() {
                if (policy & INSIDE_STRINGIFY_AS_TAG) == INSIDE_STRINGIFY_AS_TAG {
                    //
                    // Something like  a::b\[Beta]
                    //

                    let I = SyntaxIssue(
                        IssueTag::UnexpectedCharacter,
                        "The tag has non-alphanumeric source characters.".to_owned(),
                        Severity::Warning,
                        Span::new(charLoc, session.SrcLoc),
                        0.85,
                        vec![],
                        vec![],
                    );

                    session.addIssue(I);
                }
            }

            charLoc = session.SrcLoc;

            c = Tokenizer_currentWLCharacter(session, tokenStartBuf, tokenStartLoc, policy);
        } else if c.to_point() == '`' {
            //
            // Advance past trailing `
            //

            Tokenizer_nextWLCharacter(session, tokenStartBuf, tokenStartLoc, policy);

            break;
        } else {
            break;
        }
    } // while

    return c;
}

fn Tokenizer_handleString<'i>(
    session: &mut Tokenizer<'i>,
    tokenStartBuf: Buffer<'i>,
    tokenStartLoc: Location,
    mut c: WLCharacter,
    mut policy: NextPolicy,
) -> TokenRef<'i> {
    assert!(c.to_point() == '"');

    if feature::CHECK_ISSUES && (policy & INSIDE_SLOT) == INSIDE_SLOT {
        //
        // Something like  #"a"
        //

        let I = SyntaxIssue(
            IssueTag::UndocumentedSlotSyntax,
            format!("The name following ``#`` is not documented to allow the ``\"`` character."),
            Severity::Warning,
            Tokenizer_getTokenSource(session, tokenStartLoc),
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
        && !feature::COMPUTE_OOB
        && !feature::CHECK_ISSUES
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

        let quot_relative_offset = memchr::memchr(b'"', session.buffer().slice);

        quot_offset = quot_relative_offset.map(|buffer_offset| session.offset + buffer_offset);

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

            return Token(
                TokenKind::String,
                Tokenizer_getTokenBufferAndLength(session, tokenStartBuf),
                Tokenizer_getTokenSource(session, tokenStartLoc),
            );
        } else {
            session.offset = session.input.len();
            session.wasEOF = true;

            return Token(
                TokenKind::Error_UnterminatedString,
                Tokenizer_getTokenBufferAndLength(session, tokenStartBuf),
                Tokenizer_getTokenSource(session, tokenStartLoc),
            );
        }
    }

    //
    // SLOW FALL-THROUGH
    //

    incr_diagnostic!(Tokenizer_StringSlowCount);

    policy |= STRING_OR_COMMENT;

    loop {
        c = Tokenizer_nextWLCharacter(session, tokenStartBuf, tokenStartLoc, policy);

        match c.to_point() {
            Char('"') => {
                return Token(
                    TokenKind::String,
                    Tokenizer_getTokenBufferAndLength(session, tokenStartBuf),
                    Tokenizer_getTokenSource(session, tokenStartLoc),
                );
            },
            EndOfFile => {
                return Token(
                    TokenKind::Error_UnterminatedString,
                    Tokenizer_getTokenBufferAndLength(session, tokenStartBuf),
                    Tokenizer_getTokenSource(session, tokenStartLoc),
                );
            },
            Char('\n' | '\r') | CRLF if feature::COMPUTE_OOB => {
                session.addEmbeddedNewline(tokenStartLoc);
            },
            Char('\t') if feature::COMPUTE_OOB => {
                session.addEmbeddedTab(tokenStartLoc);
            },
            _ => (),
        }
    } // while
}

fn Tokenizer_handleString_stringifyAsTag<'i>(
    session: &mut Tokenizer<'i>,
    tokenStartBuf: Buffer<'i>,
    tokenStartLoc: Location,
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
            tokenStartBuf,
            tokenStartLoc,
            letterlikeBuf,
            letterlikeLoc,
            c,
            policy,
        );

        return Token(
            TokenKind::String,
            Tokenizer_getTokenBufferAndLength(session, tokenStartBuf),
            Tokenizer_getTokenSource(session, tokenStartLoc),
        );
    }

    //
    // Something like  a::5
    //

    return Token(
        TokenKind::Error_ExpectedTag,
        BufferAndLength::from_buffer_with_len(tokenStartBuf, 0),
        Span::from_location(tokenStartLoc),
    );
}

const UNTERMINATED_FILESTRING: c_int = -1;

//
// Use SourceCharacters here, not WLCharacters
//
pub(crate) fn Tokenizer_handleString_stringifyAsFile<'i>(
    session: &mut Tokenizer<'i>,
    tokenStartBuf: Buffer<'i>,
    tokenStartLoc: Location,
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

            // TODO: Make this a return value of the function below
            let mut handled: c_int = 0;

            c = Tokenizer_handleFileOpsBrackets(
                session,
                tokenStartBuf,
                tokenStartLoc,
                c,
                policy,
                &mut handled,
            );

            match handled {
                UNTERMINATED_FILESTRING => {
                    return Token(
                        TokenKind::Error_UnterminatedFileString,
                        Tokenizer_getTokenBufferAndLength(session, tokenStartBuf),
                        Tokenizer_getTokenSource(session, tokenStartLoc),
                    );
                },
                _ => (),
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

            return Token(
                TokenKind::Error_ExpectedFile,
                // BufferAndLength::from_buffer(tokenStartBuf),
                Tokenizer_getTokenBufferAndLength(session, tokenStartBuf),
                Span::from_location(tokenStartLoc),
            );
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

                // TODO: Make this a return value of the func below
                let mut handled: c_int = 0;

                c = Tokenizer_handleFileOpsBrackets(
                    session,
                    tokenStartBuf,
                    tokenStartLoc,
                    c,
                    policy,
                    &mut handled,
                );

                match handled {
                    UNTERMINATED_FILESTRING => {
                        return Token(
                            TokenKind::Error_UnterminatedFileString,
                            Tokenizer_getTokenBufferAndLength(session, tokenStartBuf),
                            Tokenizer_getTokenSource(session, tokenStartLoc),
                        );
                    },
                    _ => (),
                }
            },
            _ => {
                return Token(
                    TokenKind::String,
                    Tokenizer_getTokenBufferAndLength(session, tokenStartBuf),
                    Tokenizer_getTokenSource(session, tokenStartLoc),
                );
            },
        }
    } // while
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
    _tokenStartBuf: Buffer,
    _tokenStartLoc: Location,
    mut c: SourceCharacter,
    policy: NextPolicy,
    handled: &mut c_int,
) -> SourceCharacter {
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
            Char(' ' | '\t' | ASCII_VTAB | ASCII_FORM_FEED | '\n' | '\r') | CRLF => {
                //
                // Cannot have spaces in the string here, so bail out
                //

                *handled = UNTERMINATED_FILESTRING;

                return c;
            },
            EndOfFile => {
                *handled = UNTERMINATED_FILESTRING;

                return c;
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
                    *handled = 0;

                    return c;
                }
            },
            _ => {
                if c.isMBWhitespace() || c.isMBNewline() {
                    *handled = UNTERMINATED_FILESTRING;

                    return c;
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
    tokenStartBuf: Buffer<'i>,
    tokenStartLoc: Location,
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
    let mut leading_digits_end_mark = InputMark::new(tokenStartBuf.offset, tokenStartLoc);

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
        let mut nonZeroStartBuf = tokenStartBuf;

        if c.to_point() == '0' {
            let mut leadingZeroCount: u32 = 0;
            (leadingZeroCount, c) =
                Tokenizer_handleZeros(session, tokenStartBuf, tokenStartLoc, policy, c);

            leadingDigitsCount += leadingZeroCount;

            nonZeroStartBuf = session.buffer();
        }

        //
        // Count the rest of the leading digits
        //

        leading_digits_end_mark = session.mark();

        if c.isDigit() {
            let mut count: u32 = 0;
            (count, c) = Tokenizer_handleDigits(session, tokenStartBuf, tokenStartLoc, policy, c);

            leadingDigitsCount += count;

            leading_digits_end_mark = session.mark();
        }

        if (policy & INTEGER_SHORT_CIRCUIT) == INTEGER_SHORT_CIRCUIT {
            #[cfg(feature = "CHECK_ISSUES")]
            if c.to_point() == '.' {
                //
                // Something like  #2.a
                //

                let dotLoc = session.SrcLoc;

                let mut Actions: Vec<CodeAction> = Vec::new();

                Actions.push(CodeAction::insert_text(
                    "Insert space".to_owned(),
                    Span::from_location(dotLoc),
                    " ".to_owned(),
                ));

                let I = FormatIssue(
                    IssueTag::Ambiguous,
                    format!("Ambiguous syntax."),
                    Severity::Formatting,
                    Tokenizer_getTokenSource(session, dotLoc),
                    1.0,
                    Actions,
                    vec![],
                );

                session.addIssue(I);
            }

            //
            // Success!
            //

            return Token(
                Ctxt.computeTok(),
                Tokenizer_getTokenBufferAndLength(session, tokenStartBuf),
                Tokenizer_getTokenSource(session, tokenStartLoc),
            );
        }

        match c.to_point() {
            //
            // These are the possible next characters for a number
            //
            Char('^' | '*' | '.' | '`') => {
                if c.to_point() == '^' {
                    caret1Buf = Some(session.buffer());
                    caret_1_mark = Some(session.mark());

                    assert!(utils::ifASCIIWLCharacter(caret1Buf.unwrap()[0], b'^'));
                } else if c.to_point() == '*' {
                    starBuf = Some(session.buffer());
                    star_mark = Some(session.mark());

                    assert!(utils::ifASCIIWLCharacter(starBuf.unwrap()[0], b'*'));
                }

                //
                // Preserve c, but advance buffer to next character
                //

                Tokenizer_nextWLCharacter(session, tokenStartBuf, tokenStartLoc, policy);
            },
            _ => {
                //
                // Something else
                //

                //
                // Success!
                //

                return Token(
                    Ctxt.computeTok(),
                    Tokenizer_getTokenBufferAndLength(session, tokenStartBuf),
                    Tokenizer_getTokenSource(session, tokenStartLoc),
                );
            },
        }

        if c.to_point() == '^' {
            //
            // Could be 16^^blah
            //

            c = Tokenizer_currentWLCharacter(session, tokenStartBuf, tokenStartLoc, policy);

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

                return Token(
                    Ctxt.computeTok(),
                    Tokenizer_getTokenBufferAndLength(session, tokenStartBuf),
                    Tokenizer_getTokenSource(session, tokenStartLoc),
                );
            }

            assert!(c.to_point() == '^');

            //
            // Something like  2^^
            //
            // Must be a number
            //

            Tokenizer_nextWLCharacter(session, tokenStartBuf, tokenStartLoc, policy);

            if nonZeroStartBuf == caret1Buf.unwrap() {
                //
                // Something like  0^^2
                //

                Ctxt.InvalidBase = true;
            } else {
                // PRE_COMMIT: Compute string length differently
                let baseStrLen = caret1Buf.unwrap().offset - nonZeroStartBuf.offset;

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

            c = Tokenizer_currentWLCharacter(session, tokenStartBuf, tokenStartLoc, policy);

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
                        tokenStartBuf,
                        tokenStartLoc,
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

                                assert!(utils::ifASCIIWLCharacter(starBuf.unwrap()[0], b'*'));
                            }

                            //
                            // Preserve c, but advance buffer to next character
                            //

                            Tokenizer_nextWLCharacter(
                                session,
                                tokenStartBuf,
                                tokenStartLoc,
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

                            return Token(
                                Ctxt.computeTok(),
                                Tokenizer_getTokenBufferAndLength(session, tokenStartBuf),
                                Tokenizer_getTokenSource(session, tokenStartLoc),
                            );
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

                    Tokenizer_nextWLCharacter(session, tokenStartBuf, tokenStartLoc, policy);
                },
                EndOfFile => {
                    //
                    // Something like  2^^<EOF>
                    //

                    //
                    // Make sure that bad character is read
                    //

                    Tokenizer_nextWLCharacter(session, tokenStartBuf, tokenStartLoc, policy);

                    c = Tokenizer_currentWLCharacter(session, tokenStartBuf, tokenStartLoc, policy);

                    // nee TokenKind::Error_ExpectedDIGIT
                    return Token(
                        TokenKind::Error_Number,
                        Tokenizer_getTokenBufferAndLength(session, tokenStartBuf),
                        Tokenizer_getTokenSource(session, tokenStartLoc),
                    );
                },
                _ => {
                    //
                    // Something like  2^^@
                    //

                    // nee TokenKind::Error_UNRECOGNIZEDDIGIT
                    return Token(
                        TokenKind::Error_Number,
                        Tokenizer_getTokenBufferAndLength(session, tokenStartBuf),
                        Tokenizer_getTokenSource(session, tokenStartLoc),
                    );
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
            tokenStartBuf,
            tokenStartLoc,
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
                    return Token(
                        TokenKind::Error_Number,
                        Tokenizer_getTokenBufferAndLength(session, tokenStartBuf),
                        Tokenizer_getTokenSource(session, tokenStartLoc),
                    );
                }

                //
                // Something like  0..
                //

                //
                // Success!
                //

                return Token(
                    Ctxt.computeTok(),
                    Tokenizer_getTokenBufferAndLength(session, tokenStartBuf),
                    Tokenizer_getTokenSource(session, tokenStartLoc),
                );
            },
            HandledFractionalPart::Count(0) => {
                if leadingDigitsCount == 0 {
                    //
                    // Something like  2^^.
                    //

                    // nee TokenKind::Error_UNHANDLEDDOT
                    return Token(
                        TokenKind::Error_Number,
                        Tokenizer_getTokenBufferAndLength(session, tokenStartBuf),
                        Tokenizer_getTokenSource(session, tokenStartLoc),
                    );
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

                            assert!(utils::ifASCIIWLCharacter(starBuf.unwrap()[0], b'*'));
                        }

                        //
                        // Preserve c, but advance buffer to next character
                        //

                        Tokenizer_nextWLCharacter(session, tokenStartBuf, tokenStartLoc, policy);
                    },
                    _ => {
                        //
                        // Something like  123.
                        //

                        //
                        // Success!
                        //

                        return Token(
                            Ctxt.computeTok(),
                            Tokenizer_getTokenBufferAndLength(session, tokenStartBuf),
                            Tokenizer_getTokenSource(session, tokenStartLoc),
                        );
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

                            assert!(utils::ifASCIIWLCharacter(starBuf.unwrap()[0], b'*'));
                        }

                        //
                        // Preserve c, but advance buffer to next character
                        //

                        Tokenizer_nextWLCharacter(session, tokenStartBuf, tokenStartLoc, policy);
                    },
                    _ => {
                        //
                        // Something like  123.456
                        //

                        //
                        // Success!
                        //

                        return Token(
                            Ctxt.computeTok(),
                            Tokenizer_getTokenBufferAndLength(session, tokenStartBuf),
                            Tokenizer_getTokenSource(session, tokenStartLoc),
                        );
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

        c = Tokenizer_currentWLCharacter(session, tokenStartBuf, tokenStartLoc, policy);

        let mut accuracy = false;
        let mut sign = false;
        let mut precOrAccSupplied = false;

        let signBuf: BufferAndLength;
        let mut sign_mark: Option<InputMark> = None;

        if c.to_point() == '`' {
            Tokenizer_nextWLCharacter(session, tokenStartBuf, tokenStartLoc, policy);

            c = Tokenizer_currentWLCharacter(session, tokenStartBuf, tokenStartLoc, policy);

            accuracy = true;
        }

        match c.to_point() {
            Char('-' | '+') => {
                //
                // Something like  1.2`-
                //

                // Take one character so we can display this
                signBuf = BufferAndLength::from_buffer_with_len(session.buffer(), 1);

                sign_mark = Some(session.mark());

                assert!(
                    utils::ifASCIIWLCharacter(signBuf.buf[0], b'-')
                        || utils::ifASCIIWLCharacter(signBuf.buf[0], b'+')
                );

                //
                // Eat the sign
                //

                Tokenizer_nextWLCharacter(session, tokenStartBuf, tokenStartLoc, policy);

                c = Tokenizer_currentWLCharacter(session, tokenStartBuf, tokenStartLoc, policy);

                match c.to_point() {
                    //
                    // These are the possible next characters for a number
                    //
                    Char('0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9') => {
                        //
                        // Something like  1.2`-3
                        //

                        sign = true;

                        if feature::CHECK_ISSUES {
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
                                    Span::from_location(sign_mark.unwrap().src_loc),
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

                        if feature::CHECK_ISSUES {
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
                                    Span::from_location(sign_mark.unwrap().src_loc),
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
                            return Token(
                                TokenKind::Error_Number,
                                Tokenizer_getTokenBufferAndLength(session, tokenStartBuf),
                                Tokenizer_getTokenSource(session, tokenStartLoc),
                            );
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

                        return Token(
                            Ctxt.computeTok(),
                            Tokenizer_getTokenBufferAndLength(session, tokenStartBuf),
                            Tokenizer_getTokenSource(session, tokenStartLoc),
                        );
                    },
                }
            }, // case '-': case '+'
            _ => (),
        }

        match c.to_point() {
            Char('0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9') => {
                let mut count: u32 = 0;

                (count, c) =
                    Tokenizer_handleDigits(session, tokenStartBuf, tokenStartLoc, policy, c);

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

                        return Token(
                            Ctxt.computeTok(),
                            Tokenizer_getTokenBufferAndLength(session, tokenStartBuf),
                            Tokenizer_getTokenSource(session, tokenStartLoc),
                        );
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

                    Tokenizer_nextWLCharacter(session, tokenStartBuf, tokenStartLoc, policy);

                    let NextChar =
                        Tokenizer_currentWLCharacter(session, tokenStartBuf, tokenStartLoc, policy);

                    if !NextChar.isDigit() {
                        if accuracy {
                            //
                            // Something like  123``.EOF
                            //

                            // TokenKind::Error_ExpectedDIGIT
                            return Token(
                                TokenKind::Error_Number,
                                Tokenizer_getTokenBufferAndLength(session, tokenStartBuf),
                                Tokenizer_getTokenSource(session, tokenStartLoc),
                            );
                        }

                        if NextChar.isSign() {
                            //
                            // Something like  123`.+4
                            //

                            Tokenizer_nextWLCharacter(
                                session,
                                tokenStartBuf,
                                tokenStartLoc,
                                policy,
                            );

                            // nee TokenKind::Error_ExpectedDIGIT
                            return Token(
                                TokenKind::Error_Number,
                                Tokenizer_getTokenBufferAndLength(session, tokenStartBuf),
                                Tokenizer_getTokenSource(session, tokenStartLoc),
                            );
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

                        return Token(
                            Ctxt.computeTok(),
                            Tokenizer_getTokenBufferAndLength(session, tokenStartBuf),
                            Tokenizer_getTokenSource(session, tokenStartLoc),
                        );
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

                    Tokenizer_nextWLCharacter(session, tokenStartBuf, tokenStartLoc, policy);

                    c = Tokenizer_currentWLCharacter(session, tokenStartBuf, tokenStartLoc, policy);
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
                    tokenStartBuf,
                    tokenStartLoc,
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

                            return Token(
                                Ctxt.computeTok(),
                                Tokenizer_getTokenBufferAndLength(session, tokenStartBuf),
                                Tokenizer_getTokenSource(session, tokenStartLoc),
                            );
                        }

                        if sign {
                            //
                            // Something like  1`+..
                            //

                            Tokenizer_backupAndWarn(session, sign_mark.unwrap());

                            //
                            // Success!
                            //

                            return Token(
                                Ctxt.computeTok(),
                                Tokenizer_getTokenBufferAndLength(session, tokenStartBuf),
                                Tokenizer_getTokenSource(session, tokenStartLoc),
                            );
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
                    return Token(
                        TokenKind::Error_Number,
                        Tokenizer_getTokenBufferAndLength(session, tokenStartBuf),
                        Tokenizer_getTokenSource(session, tokenStartLoc),
                    );
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
                        return Token(
                            TokenKind::Error_Number,
                            Tokenizer_getTokenBufferAndLength(session, tokenStartBuf),
                            Tokenizer_getTokenSource(session, tokenStartLoc),
                        );
                    }
                }

                starBuf = Some(session.buffer());
                star_mark = Some(session.mark());

                assert!(utils::ifASCIIWLCharacter(starBuf.unwrap()[0], b'*'));

                //
                // Preserve c, but advance buffer to next character
                //

                Tokenizer_nextWLCharacter(session, tokenStartBuf, tokenStartLoc, policy);
            },
            _ => {
                if accuracy {
                    if !precOrAccSupplied {
                        //
                        // Something like  123``EOF
                        //

                        // nee TokenKind::Error_ExpectedACCURACY
                        return Token(
                            TokenKind::Error_Number,
                            Tokenizer_getTokenBufferAndLength(session, tokenStartBuf),
                            Tokenizer_getTokenSource(session, tokenStartLoc),
                        );
                    }
                }

                //
                // Success!
                //

                return Token(
                    Ctxt.computeTok(),
                    Tokenizer_getTokenBufferAndLength(session, tokenStartBuf),
                    Tokenizer_getTokenSource(session, tokenStartLoc),
                );
            },
        }
    } // if (c.to_point() == '`')

    assert!(c.to_point() == '*');

    assert!(utils::ifASCIIWLCharacter(starBuf.unwrap()[0], b'*'));

    c = Tokenizer_currentWLCharacter(session, tokenStartBuf, tokenStartLoc, policy);

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

        return Token(
            Ctxt.computeTok(),
            Tokenizer_getTokenBufferAndLength(session, tokenStartBuf),
            Tokenizer_getTokenSource(session, tokenStartLoc),
        );
    }

    assert!(c.to_point() == '^');

    //
    // c is '^'
    //
    // So now examine *^ notation
    //

    Tokenizer_nextWLCharacter(session, tokenStartBuf, tokenStartLoc, policy);

    c = Tokenizer_currentWLCharacter(session, tokenStartBuf, tokenStartLoc, policy);

    if let Char(c2 @ ('+' | '-')) = c.to_point() {
        if c2 == '-' {
            Ctxt.NegativeExponent = true;
        }

        Tokenizer_nextWLCharacter(session, tokenStartBuf, tokenStartLoc, policy);
        c = Tokenizer_currentWLCharacter(session, tokenStartBuf, tokenStartLoc, policy);
    }

    if !c.isDigit() {
        //
        // Something like  123*^-<EOF>
        //

        // TokenKind::Error_ExpectedEXPONENT
        return Token(
            TokenKind::Error_Number,
            Tokenizer_getTokenBufferAndLength(session, tokenStartBuf),
            Tokenizer_getTokenSource(session, tokenStartLoc),
        );
    }

    assert!(c.isDigit());

    //
    // Count leading zeros in exponent
    //
    if c.to_point() == '0' {
        let _exponentLeadingZeroCount: u32;

        (_exponentLeadingZeroCount, c) =
            Tokenizer_handleZeros(session, tokenStartBuf, tokenStartLoc, policy, c);
    }

    if c.isDigit() {
        (Ctxt.NonZeroExponentDigitCount, c) =
            Tokenizer_handleDigits(session, tokenStartBuf, tokenStartLoc, policy, c);
    }

    if c.to_point() != '.' {
        //
        // Success!
        //

        return Token(
            Ctxt.computeTok(),
            Tokenizer_getTokenBufferAndLength(session, tokenStartBuf),
            Tokenizer_getTokenSource(session, tokenStartLoc),
        );
    }

    assert!(c.to_point() == '.');

    let dotBuf = session.buffer();
    let dot_mark = session.mark();

    assert!(utils::ifASCIIWLCharacter(dotBuf[0], b'.'));

    Tokenizer_nextWLCharacter(session, tokenStartBuf, tokenStartLoc, policy);

    c = Tokenizer_currentWLCharacter(session, tokenStartBuf, tokenStartLoc, policy);

    let handled: HandledFractionalPart;
    (handled, c) = Tokenizer_handlePossibleFractionalPartPastDot(
        session,
        tokenStartBuf,
        tokenStartLoc,
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

            return Token(
                Ctxt.computeTok(),
                Tokenizer_getTokenBufferAndLength(session, tokenStartBuf),
                Tokenizer_getTokenSource(session, tokenStartLoc),
            );
        },
        HandledFractionalPart::Count(_) => {
            //
            // Something like  123*^0.5
            //
            // Make this an error; do NOT make this Dot[123*^0, 5]
            //

            // nee TokenKind::Error_ExpectedEXPONENT
            return Token(
                TokenKind::Error_Number,
                Tokenizer_getTokenBufferAndLength(session, tokenStartBuf),
                Tokenizer_getTokenSource(session, tokenStartLoc),
            );
        },
    }
}

impl NumberTokenizationContext {
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
    tokenStartBuf: Buffer<'i>,
    tokenStartLoc: Location,
    dot_mark: InputMark,
    mut c: WLCharacter,
    base: i32,
    policy: NextPolicy,
    Ctxt: &mut NumberTokenizationContext,
) -> (HandledFractionalPart, WLCharacter) {
    assert!(c.to_point() == '.');

    c = Tokenizer_currentWLCharacter(session, tokenStartBuf, tokenStartLoc, policy);

    // MUSTTAIL
    return Tokenizer_handlePossibleFractionalPartPastDot(
        session,
        tokenStartBuf,
        tokenStartLoc,
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
    tokenStartBuf: Buffer<'i>,
    tokenStartLoc: Location,
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

        c = Tokenizer_currentWLCharacter(session, tokenStartBuf, tokenStartLoc, policy);

        return (HandledFractionalPart::Bailout, c);
    }

    if c.isAlphaOrDigit() {
        let handled: u32;
        (handled, c) = Tokenizer_handleAlphaOrDigits(
            session,
            tokenStartBuf,
            tokenStartLoc,
            c,
            base,
            policy,
            Ctxt,
        );

        if handled > 0 {
            #[cfg(feature = "CHECK_ISSUES")]
            if c.to_point() == '.' {
                //
                // Something like  1.2.3
                //

                let mut Actions = Vec::new();

                Actions.push(CodeAction::insert_text(
                    "Insert ``*``".into(),
                    Span::from_location(dot_mark.src_loc),
                    "*".into(),
                ));

                let I = SyntaxIssue(
                    IssueTag::UnexpectedImplicitTimes,
                    format!("Suspicious syntax."),
                    Severity::Error,
                    Span::from_location(dot_mark.src_loc),
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
    if feature::CHECK_ISSUES {
        let mut Actions: Vec<CodeAction> = Vec::new();

        Actions.push(CodeAction::insert_text(
            "Insert space".into(),
            Span::from_location(reset.src_loc),
            " ".into(),
        ));

        let I = FormatIssue(
            IssueTag::Ambiguous,
            "Ambiguous syntax.".into(),
            Severity::Formatting,
            Span::from_location(reset.src_loc),
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
    tokenStartBuf: Buffer<'i>,
    tokenStartLoc: Location,
    policy: NextPolicy,
    mut c: WLCharacter,
) -> (u32, WLCharacter) {
    assert!(c.to_point() == '0');

    let mut count = 1;

    c = Tokenizer_currentWLCharacter(session, tokenStartBuf, tokenStartLoc, policy);

    loop {
        if c.to_point() != '0' {
            break;
        }

        Tokenizer_nextWLCharacter(session, tokenStartBuf, tokenStartLoc, policy);

        c = Tokenizer_currentWLCharacter(session, tokenStartBuf, tokenStartLoc, policy);

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
    tokenStartBuf: Buffer<'i>,
    tokenStartLoc: Location,
    policy: NextPolicy,
    mut c: WLCharacter,
) -> (u32, WLCharacter) {
    assert!(c.isDigit());

    let mut count = 1;

    c = Tokenizer_currentWLCharacter(session, tokenStartBuf, tokenStartLoc, policy);

    loop {
        if !c.isDigit() {
            break;
        }

        Tokenizer_nextWLCharacter(session, tokenStartBuf, tokenStartLoc, policy);

        c = Tokenizer_currentWLCharacter(session, tokenStartBuf, tokenStartLoc, policy);

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
    tokenStartBuf: Buffer<'i>,
    tokenStartLoc: Location,
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
            let cp_u8 =
                u8::try_from(cp_i32).expect("unable to convert digit character to u8 value");
            let dig = i32::from(utils::toDigit(cp_u8));

            if base <= dig {
                Ctxt.UnrecognizedDigit = true;
            }
        }

        Tokenizer_nextWLCharacter(session, tokenStartBuf, tokenStartLoc, policy);

        c = Tokenizer_currentWLCharacter(session, tokenStartBuf, tokenStartLoc, policy);

        count += 1;
    } // while

    return (count, c);
}

fn Tokenizer_handleColon<'i>(
    session: &mut Tokenizer<'i>,
    tokenStartBuf: Buffer<'i>,
    tokenStartLoc: Location,
    mut c: WLCharacter,
    policy: NextPolicy,
) -> TokenRef<'i> {
    assert!(c.to_point() == ':');

    c = Tokenizer_currentWLCharacter(session, tokenStartBuf, tokenStartLoc, policy);

    match c.to_point() {
        Char(':') => {
            Tokenizer_nextWLCharacter(session, tokenStartBuf, tokenStartLoc, policy);

            c = Tokenizer_currentWLCharacter(session, tokenStartBuf, tokenStartLoc, policy);

            if c.to_point() == '[' {
                //
                // ::[
                //

                Tokenizer_nextWLCharacter(session, tokenStartBuf, tokenStartLoc, policy);

                return Token(
                    TokenKind::ColonColonOpenSquare,
                    Tokenizer_getTokenBufferAndLength(session, tokenStartBuf),
                    Tokenizer_getTokenSource(session, tokenStartLoc),
                );
            }

            //
            // ::
            //

            return Token(
                TokenKind::ColonColon,
                Tokenizer_getTokenBufferAndLength(session, tokenStartBuf),
                Tokenizer_getTokenSource(session, tokenStartLoc),
            );
        },
        Char('=') => {
            //
            // :=
            //

            Tokenizer_nextWLCharacter(session, tokenStartBuf, tokenStartLoc, policy);

            return Token(
                TokenKind::ColonEqual,
                Tokenizer_getTokenBufferAndLength(session, tokenStartBuf),
                Tokenizer_getTokenSource(session, tokenStartLoc),
            );
        },
        Char('>') => {
            //
            // :>
            //

            incr_diagnostic!(Tokenizer_ColonGreaterCount);

            Tokenizer_nextWLCharacter(session, tokenStartBuf, tokenStartLoc, policy);

            return Token(
                TokenKind::ColonGreater,
                Tokenizer_getTokenBufferAndLength(session, tokenStartBuf),
                Tokenizer_getTokenSource(session, tokenStartLoc),
            );
        },
        _ => {
            //
            // :
            //

            return Token(
                TokenKind::Colon,
                Tokenizer_getTokenBufferAndLength(session, tokenStartBuf),
                Tokenizer_getTokenSource(session, tokenStartLoc),
            );
        },
    }
}

fn Tokenizer_handleOpenParen<'i>(
    session: &mut Tokenizer<'i>,
    tokenStartBuf: Buffer<'i>,
    tokenStartLoc: Location,
    c: WLCharacter,
    policy: NextPolicy,
) -> TokenRef<'i> {
    assert!(c.to_point() == '(');

    let secondChar = session.peek_source_char(policy);

    //
    // Comments must start literally with (*
    // Escaped characters do not work
    //
    if (c.to_point() == '(' && c.escape() == EscapeStyle::None) && secondChar == '*' {
        //
        // secondChar is a SourceCharacter, so cannot MUSTTAIL
        //
        //        MUSTTAIL
        return Tokenizer_handleComment(session, tokenStartBuf, tokenStartLoc, secondChar, policy);
    }

    //
    // (
    //

    incr_diagnostic!(Tokenizer_OpenParenCount);

    return Token(
        TokenKind::OpenParen,
        Tokenizer_getTokenBufferAndLength(session, tokenStartBuf),
        Tokenizer_getTokenSource(session, tokenStartLoc),
    );
}

fn Tokenizer_handleDot<'i>(
    session: &mut Tokenizer<'i>,
    tokenStartBuf: Buffer<'i>,
    tokenStartLoc: Location,
    firstChar: WLCharacter,
    policy: NextPolicy,
) -> TokenRef<'i> {
    let mut c = firstChar;

    //
    // handleDot
    // Could be  .  or  ..  or ...  or  .0
    //

    assert!(c.to_point() == '.');

    c = Tokenizer_currentWLCharacter(session, tokenStartBuf, tokenStartLoc, policy);

    if c.isDigit() {
        //        MUSTTAIL
        return Tokenizer_handleNumber(session, tokenStartBuf, tokenStartLoc, firstChar, policy);
    }

    if c.to_point() == '.' {
        Tokenizer_nextWLCharacter(session, tokenStartBuf, tokenStartLoc, policy);

        c = Tokenizer_currentWLCharacter(session, tokenStartBuf, tokenStartLoc, policy);

        if c.to_point() == '.' {
            //
            // ...
            //

            Tokenizer_nextWLCharacter(session, tokenStartBuf, tokenStartLoc, policy);

            return Token(
                TokenKind::DotDotDot,
                Tokenizer_getTokenBufferAndLength(session, tokenStartBuf),
                Tokenizer_getTokenSource(session, tokenStartLoc),
            );
        }

        //
        // ..
        //

        return Token(
            TokenKind::DotDot,
            Tokenizer_getTokenBufferAndLength(session, tokenStartBuf),
            Tokenizer_getTokenSource(session, tokenStartLoc),
        );
    }

    //
    // .
    //

    return Token(
        TokenKind::Dot,
        Tokenizer_getTokenBufferAndLength(session, tokenStartBuf),
        Tokenizer_getTokenSource(session, tokenStartLoc),
    );
}

fn Tokenizer_handleEqual<'i>(
    session: &mut Tokenizer<'i>,
    tokenStartBuf: Buffer<'i>,
    tokenStartLoc: Location,
    mut c: WLCharacter,
    policy: NextPolicy,
) -> TokenRef<'i> {
    assert!(c.to_point() == '=');

    c = Tokenizer_currentWLCharacter(session, tokenStartBuf, tokenStartLoc, policy);

    match c.to_point() {
        Char('=') => {
            Tokenizer_nextWLCharacter(session, tokenStartBuf, tokenStartLoc, policy);

            c = Tokenizer_currentWLCharacter(session, tokenStartBuf, tokenStartLoc, policy);

            if c.to_point() == '=' {
                //
                // ===
                //

                Tokenizer_nextWLCharacter(session, tokenStartBuf, tokenStartLoc, policy);

                return Token(
                    TokenKind::EqualEqualEqual,
                    Tokenizer_getTokenBufferAndLength(session, tokenStartBuf),
                    Tokenizer_getTokenSource(session, tokenStartLoc),
                );
            }

            //
            // ==
            //

            return Token(
                TokenKind::EqualEqual,
                Tokenizer_getTokenBufferAndLength(session, tokenStartBuf),
                Tokenizer_getTokenSource(session, tokenStartLoc),
            );
        },
        Char('!') => {
            let bang_mark = session.mark();

            Tokenizer_nextWLCharacter(session, tokenStartBuf, tokenStartLoc, policy);

            c = Tokenizer_currentWLCharacter(session, tokenStartBuf, tokenStartLoc, policy);

            if c.to_point() == '=' {
                //
                // =!=
                //

                Tokenizer_nextWLCharacter(session, tokenStartBuf, tokenStartLoc, policy);

                return Token(
                    TokenKind::EqualBangEqual,
                    Tokenizer_getTokenBufferAndLength(session, tokenStartBuf),
                    Tokenizer_getTokenSource(session, tokenStartLoc),
                );
            }

            //
            // Something like  x=!y
            //
            // Must now do surgery and back up
            //

            Tokenizer_backupAndWarn(session, bang_mark);

            return Token(
                TokenKind::Equal,
                Tokenizer_getTokenBufferAndLength(session, tokenStartBuf),
                Tokenizer_getTokenSource(session, tokenStartLoc),
            );
        },
        _ => (),
    }

    //
    // =
    //

    return Token(
        TokenKind::Equal,
        Tokenizer_getTokenBufferAndLength(session, tokenStartBuf),
        Tokenizer_getTokenSource(session, tokenStartLoc),
    );
}

fn Tokenizer_handleUnder<'i>(
    session: &mut Tokenizer<'i>,
    tokenStartBuf: Buffer<'i>,
    tokenStartLoc: Location,
    mut c: WLCharacter,
    policy: NextPolicy,
) -> TokenRef<'i> {
    assert!(c.to_point() == '_');

    c = Tokenizer_currentWLCharacter(session, tokenStartBuf, tokenStartLoc, policy);

    match c.to_point() {
        Char('_') => {
            //
            // __
            //

            Tokenizer_nextWLCharacter(session, tokenStartBuf, tokenStartLoc, policy);

            c = Tokenizer_currentWLCharacter(session, tokenStartBuf, tokenStartLoc, policy);

            if c.to_point() == '_' {
                //
                // ___
                //

                Tokenizer_nextWLCharacter(session, tokenStartBuf, tokenStartLoc, policy);

                return Token(
                    TokenKind::UnderUnderUnder,
                    Tokenizer_getTokenBufferAndLength(session, tokenStartBuf),
                    Tokenizer_getTokenSource(session, tokenStartLoc),
                );
            }

            return Token(
                TokenKind::UnderUnder,
                Tokenizer_getTokenBufferAndLength(session, tokenStartBuf),
                Tokenizer_getTokenSource(session, tokenStartLoc),
            );
        },
        Char('.') => {
            //
            // _.
            //

            Tokenizer_nextWLCharacter(session, tokenStartBuf, tokenStartLoc, policy);

            if feature::CHECK_ISSUES {
                let afterLoc = session.SrcLoc;

                c = Tokenizer_currentWLCharacter(session, tokenStartBuf, tokenStartLoc, policy);

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
                        Span::from_location(dotLoc),
                        " ".into(),
                    ));

                    let I = SyntaxIssue(
                        IssueTag::UnexpectedDot,
                        "Suspicious syntax.".into(),
                        Severity::Error,
                        Span::from_location(dotLoc),
                        0.95,
                        Actions,
                        vec![],
                    );

                    session.addIssue(I);
                }
            }

            return Token(
                TokenKind::UnderDot,
                Tokenizer_getTokenBufferAndLength(session, tokenStartBuf),
                Tokenizer_getTokenSource(session, tokenStartLoc),
            );
        },
        _ => (),
    }

    //
    // _
    //

    return Token(
        TokenKind::Under,
        Tokenizer_getTokenBufferAndLength(session, tokenStartBuf),
        Tokenizer_getTokenSource(session, tokenStartLoc),
    );
}

fn Tokenizer_handleLess<'i>(
    session: &mut Tokenizer<'i>,
    tokenStartBuf: Buffer<'i>,
    tokenStartLoc: Location,
    mut c: WLCharacter,
    policy: NextPolicy,
) -> TokenRef<'i> {
    assert!(c.to_point() == '<');

    c = Tokenizer_currentWLCharacter(session, tokenStartBuf, tokenStartLoc, policy);

    match c.to_point() {
        Char('|') => {
            //
            // <|
            //

            Tokenizer_nextWLCharacter(session, tokenStartBuf, tokenStartLoc, policy);

            return Token(
                TokenKind::LessBar,
                Tokenizer_getTokenBufferAndLength(session, tokenStartBuf),
                Tokenizer_getTokenSource(session, tokenStartLoc),
            );
        },
        Char('<') => {
            //
            // <<
            //

            Tokenizer_nextWLCharacter(session, tokenStartBuf, tokenStartLoc, policy);

            return Token(
                TokenKind::LessLess,
                Tokenizer_getTokenBufferAndLength(session, tokenStartBuf),
                Tokenizer_getTokenSource(session, tokenStartLoc),
            );
        },
        Char('>') => {
            //
            // <>
            //

            Tokenizer_nextWLCharacter(session, tokenStartBuf, tokenStartLoc, policy);

            return Token(
                TokenKind::LessGreater,
                Tokenizer_getTokenBufferAndLength(session, tokenStartBuf),
                Tokenizer_getTokenSource(session, tokenStartLoc),
            );
        },
        Char('=') => {
            //
            // <=
            //

            Tokenizer_nextWLCharacter(session, tokenStartBuf, tokenStartLoc, policy);

            return Token(
                TokenKind::LessEqual,
                Tokenizer_getTokenBufferAndLength(session, tokenStartBuf),
                Tokenizer_getTokenSource(session, tokenStartLoc),
            );
        },
        Char('-') => {
            let minus_mark = session.mark();

            Tokenizer_nextWLCharacter(session, tokenStartBuf, tokenStartLoc, policy);

            c = Tokenizer_currentWLCharacter(session, tokenStartBuf, tokenStartLoc, policy);

            if c.to_point() == '>' {
                //
                // <->
                //

                Tokenizer_nextWLCharacter(session, tokenStartBuf, tokenStartLoc, policy);

                return Token(
                    TokenKind::LessMinusGreater,
                    Tokenizer_getTokenBufferAndLength(session, tokenStartBuf),
                    Tokenizer_getTokenSource(session, tokenStartLoc),
                );
            }

            //
            // Something like  a<-4
            //
            // Must now do surgery and back up
            //

            Tokenizer_backupAndWarn(session, minus_mark);

            return Token(
                TokenKind::Less,
                Tokenizer_getTokenBufferAndLength(session, tokenStartBuf),
                Tokenizer_getTokenSource(session, tokenStartLoc),
            );
        },
        _ => (),
    }

    //
    // <
    //

    return Token(
        TokenKind::Less,
        Tokenizer_getTokenBufferAndLength(session, tokenStartBuf),
        Tokenizer_getTokenSource(session, tokenStartLoc),
    );
}

fn Tokenizer_handleGreater<'i>(
    session: &mut Tokenizer<'i>,
    tokenStartBuf: Buffer<'i>,
    tokenStartLoc: Location,
    mut c: WLCharacter,
    policy: NextPolicy,
) -> TokenRef<'i> {
    assert!(c.to_point() == '>');

    c = Tokenizer_currentWLCharacter(session, tokenStartBuf, tokenStartLoc, policy);

    match c.to_point() {
        Char('>') => {
            //
            // >>
            //

            Tokenizer_nextWLCharacter(session, tokenStartBuf, tokenStartLoc, policy);

            c = Tokenizer_currentWLCharacter(session, tokenStartBuf, tokenStartLoc, policy);

            if c.to_point() == '>' {
                //
                // >>>
                //

                Tokenizer_nextWLCharacter(session, tokenStartBuf, tokenStartLoc, policy);

                return Token(
                    TokenKind::GreaterGreaterGreater,
                    Tokenizer_getTokenBufferAndLength(session, tokenStartBuf),
                    Tokenizer_getTokenSource(session, tokenStartLoc),
                );
            }

            return Token(
                TokenKind::GreaterGreater,
                Tokenizer_getTokenBufferAndLength(session, tokenStartBuf),
                Tokenizer_getTokenSource(session, tokenStartLoc),
            );
        },
        Char('=') => {
            //
            // >=
            //

            Tokenizer_nextWLCharacter(session, tokenStartBuf, tokenStartLoc, policy);

            return Token(
                TokenKind::GreaterEqual,
                Tokenizer_getTokenBufferAndLength(session, tokenStartBuf),
                Tokenizer_getTokenSource(session, tokenStartLoc),
            );
        },
        _ => (),
    }

    //
    // >
    //

    return Token(
        TokenKind::Greater,
        Tokenizer_getTokenBufferAndLength(session, tokenStartBuf),
        Tokenizer_getTokenSource(session, tokenStartLoc),
    );
}

fn Tokenizer_handleMinus<'i>(
    session: &mut Tokenizer<'i>,
    tokenStartBuf: Buffer<'i>,
    tokenStartLoc: Location,
    mut c: WLCharacter,
    policy: NextPolicy,
) -> TokenRef<'i> {
    assert!(c.to_point() == '-');

    c = Tokenizer_currentWLCharacter(session, tokenStartBuf, tokenStartLoc, policy);

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

            Tokenizer_nextWLCharacter(session, tokenStartBuf, tokenStartLoc, policy);

            return Token(
                TokenKind::MinusGreater,
                Tokenizer_getTokenBufferAndLength(session, tokenStartBuf),
                Tokenizer_getTokenSource(session, tokenStartLoc),
            );
        },
        Char('-') => {
            //
            // --
            //

            Tokenizer_nextWLCharacter(session, tokenStartBuf, tokenStartLoc, policy);

            if feature::CHECK_ISSUES {
                let afterLoc = session.SrcLoc;

                c = Tokenizer_currentWLCharacter(session, tokenStartBuf, tokenStartLoc, policy);

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
                        Span::new(tokenStartLoc, afterLoc),
                        "-".into(),
                    ));

                    Actions.push(CodeAction::insert_text(
                        "Insert space".into(),
                        Span::from_location(greaterLoc),
                        " ".into(),
                    ));

                    let I = SyntaxIssue(
                        IssueTag::Ambiguous,
                        "``-->`` is ambiguous syntax.".into(),
                        Severity::Error,
                        Span::new(tokenStartLoc, afterLoc),
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
                        Span::from_location(equalLoc),
                        " ".into(),
                    ));

                    let I = FormatIssue(
                        IssueTag::Ambiguous,
                        "Put a space between ``--`` and ``=`` to reduce ambiguity".into(),
                        Severity::Formatting,
                        Span::from_location(equalLoc),
                        1.0,
                        Actions,
                        vec![],
                    );

                    session.addIssue(I);
                }
            }

            return Token(
                TokenKind::MinusMinus,
                Tokenizer_getTokenBufferAndLength(session, tokenStartBuf),
                Tokenizer_getTokenSource(session, tokenStartLoc),
            );
        },
        Char('=') => {
            //
            // -=
            //

            Tokenizer_nextWLCharacter(session, tokenStartBuf, tokenStartLoc, policy);

            return Token(
                TokenKind::MinusEqual,
                Tokenizer_getTokenBufferAndLength(session, tokenStartBuf),
                Tokenizer_getTokenSource(session, tokenStartLoc),
            );
        },
        _ => (),
    }

    //
    // -
    //

    incr_diagnostic!(Tokenizer_MinusCount);

    return Token(
        TokenKind::Minus,
        Tokenizer_getTokenBufferAndLength(session, tokenStartBuf),
        Tokenizer_getTokenSource(session, tokenStartLoc),
    );
}

fn Tokenizer_handleBar<'i>(
    session: &mut Tokenizer<'i>,
    tokenStartBuf: Buffer<'i>,
    tokenStartLoc: Location,
    mut c: WLCharacter,
    policy: NextPolicy,
) -> TokenRef<'i> {
    assert!(c.to_point() == '|');

    c = Tokenizer_currentWLCharacter(session, tokenStartBuf, tokenStartLoc, policy);

    match c.to_point() {
        Char('>') => {
            //
            // |>
            //

            Tokenizer_nextWLCharacter(session, tokenStartBuf, tokenStartLoc, policy);

            if feature::CHECK_ISSUES {
                let afterLoc = session.SrcLoc;

                c = Tokenizer_currentWLCharacter(session, tokenStartBuf, tokenStartLoc, policy);

                if c.to_point() == '=' {
                    //
                    // Something like  <||>=0
                    //

                    let equalLoc = afterLoc;

                    let mut Actions: Vec<CodeAction> = Vec::new();

                    Actions.push(CodeAction::insert_text(
                        "Insert space".into(),
                        Span::from_location(equalLoc),
                        " ".into(),
                    ));

                    let I = FormatIssue(
                        IssueTag::Ambiguous,
                        "Put a space between ``|>`` and ``=`` to reduce ambiguity".into(),
                        Severity::Formatting,
                        Span::from_location(equalLoc),
                        1.0,
                        Actions,
                        vec![],
                    );

                    session.addIssue(I);
                }
            }

            return Token(
                TokenKind::BarGreater,
                Tokenizer_getTokenBufferAndLength(session, tokenStartBuf),
                Tokenizer_getTokenSource(session, tokenStartLoc),
            );
        },
        Char('|') => {
            //
            // ||
            //

            Tokenizer_nextWLCharacter(session, tokenStartBuf, tokenStartLoc, policy);

            return Token(
                TokenKind::BarBar,
                Tokenizer_getTokenBufferAndLength(session, tokenStartBuf),
                Tokenizer_getTokenSource(session, tokenStartLoc),
            );
        },
        Char('-') => {
            let bar_mark = session.mark();

            Tokenizer_nextWLCharacter(session, tokenStartBuf, tokenStartLoc, policy);

            c = Tokenizer_currentWLCharacter(session, tokenStartBuf, tokenStartLoc, policy);

            if c.to_point() == '>' {
                //
                // |->
                //

                Tokenizer_nextWLCharacter(session, tokenStartBuf, tokenStartLoc, policy);

                return Token(
                    TokenKind::BarMinusGreater,
                    Tokenizer_getTokenBufferAndLength(session, tokenStartBuf),
                    Tokenizer_getTokenSource(session, tokenStartLoc),
                );
            }

            //
            // Something like  x|-y
            //
            // Must now do surgery and back up
            //

            Tokenizer_backupAndWarn(session, bar_mark);

            return Token(
                TokenKind::Bar,
                Tokenizer_getTokenBufferAndLength(session, tokenStartBuf),
                Tokenizer_getTokenSource(session, tokenStartLoc),
            );
        },
        _ => (),
    }

    //
    // |
    //

    return Token(
        TokenKind::Bar,
        Tokenizer_getTokenBufferAndLength(session, tokenStartBuf),
        Tokenizer_getTokenSource(session, tokenStartLoc),
    );
}

fn Tokenizer_handleSemi<'i>(
    session: &mut Tokenizer<'i>,
    tokenStartBuf: Buffer<'i>,
    tokenStartLoc: Location,
    mut c: WLCharacter,
    policy: NextPolicy,
) -> TokenRef<'i> {
    assert!(c.to_point() == ';');

    c = Tokenizer_currentWLCharacter(session, tokenStartBuf, tokenStartLoc, policy);

    if c.to_point() == ';' {
        //
        // ;;
        //

        Tokenizer_nextWLCharacter(session, tokenStartBuf, tokenStartLoc, policy);

        return Token(
            TokenKind::SemiSemi,
            Tokenizer_getTokenBufferAndLength(session, tokenStartBuf),
            Tokenizer_getTokenSource(session, tokenStartLoc),
        );
    }

    //
    // ;
    //

    return Token(
        TokenKind::Semi,
        Tokenizer_getTokenBufferAndLength(session, tokenStartBuf),
        Tokenizer_getTokenSource(session, tokenStartLoc),
    );
}

fn Tokenizer_handleBang<'i>(
    session: &mut Tokenizer<'i>,
    tokenStartBuf: Buffer<'i>,
    tokenStartLoc: Location,
    mut c: WLCharacter,
    policy: NextPolicy,
) -> TokenRef<'i> {
    assert!(c.to_point() == '!');

    c = Tokenizer_currentWLCharacter(session, tokenStartBuf, tokenStartLoc, policy);

    match c.to_point() {
        Char('=') => {
            //
            // !=
            //

            Tokenizer_nextWLCharacter(session, tokenStartBuf, tokenStartLoc, policy);

            return Token(
                TokenKind::BangEqual,
                Tokenizer_getTokenBufferAndLength(session, tokenStartBuf),
                Tokenizer_getTokenSource(session, tokenStartLoc),
            );
        },
        Char('!') => {
            //
            // !!
            //

            Tokenizer_nextWLCharacter(session, tokenStartBuf, tokenStartLoc, policy);

            return Token(
                TokenKind::BangBang,
                Tokenizer_getTokenBufferAndLength(session, tokenStartBuf),
                Tokenizer_getTokenSource(session, tokenStartLoc),
            );
        },
        _ => (),
    }

    //
    // !
    //

    return Token(
        TokenKind::Bang,
        Tokenizer_getTokenBufferAndLength(session, tokenStartBuf),
        Tokenizer_getTokenSource(session, tokenStartLoc),
    );
}

fn Tokenizer_handleHash<'i>(
    session: &mut Tokenizer<'i>,
    tokenStartBuf: Buffer<'i>,
    tokenStartLoc: Location,
    mut c: WLCharacter,
    policy: NextPolicy,
) -> TokenRef<'i> {
    assert!(c.to_point() == '#');

    c = Tokenizer_currentWLCharacter(session, tokenStartBuf, tokenStartLoc, policy);

    if c.to_point() == '#' {
        //
        // ##
        //

        Tokenizer_nextWLCharacter(session, tokenStartBuf, tokenStartLoc, policy);

        return Token(
            TokenKind::HashHash,
            Tokenizer_getTokenBufferAndLength(session, tokenStartBuf),
            Tokenizer_getTokenSource(session, tokenStartLoc),
        );
    }

    //
    // #
    //

    incr_diagnostic!(Tokenizer_HashCount);

    return Token(
        TokenKind::Hash,
        Tokenizer_getTokenBufferAndLength(session, tokenStartBuf),
        Tokenizer_getTokenSource(session, tokenStartLoc),
    );
}

fn Tokenizer_handlePercent<'i>(
    session: &mut Tokenizer<'i>,
    tokenStartBuf: Buffer<'i>,
    tokenStartLoc: Location,
    mut c: WLCharacter,
    policy: NextPolicy,
) -> TokenRef<'i> {
    assert!(c.to_point() == '%');

    c = Tokenizer_currentWLCharacter(session, tokenStartBuf, tokenStartLoc, policy);

    if c.to_point() == '%' {
        //
        // %%
        //

        c = Tokenizer_currentWLCharacter(session, tokenStartBuf, tokenStartLoc, policy);

        loop {
            if c.to_point() != '%' {
                break;
            }

            Tokenizer_nextWLCharacter(session, tokenStartBuf, tokenStartLoc, policy);

            c = Tokenizer_currentWLCharacter(session, tokenStartBuf, tokenStartLoc, policy);
        } // while

        return Token(
            TokenKind::PercentPercent,
            Tokenizer_getTokenBufferAndLength(session, tokenStartBuf),
            Tokenizer_getTokenSource(session, tokenStartLoc),
        );
    }

    //
    // %
    //

    return Token(
        TokenKind::Percent,
        Tokenizer_getTokenBufferAndLength(session, tokenStartBuf),
        Tokenizer_getTokenSource(session, tokenStartLoc),
    );
}

fn Tokenizer_handleAmp<'i>(
    session: &mut Tokenizer<'i>,
    tokenStartBuf: Buffer<'i>,
    tokenStartLoc: Location,
    mut c: WLCharacter,
    policy: NextPolicy,
) -> TokenRef<'i> {
    assert!(c.to_point() == '&');

    c = Tokenizer_currentWLCharacter(session, tokenStartBuf, tokenStartLoc, policy);

    if c.to_point() == '&' {
        //
        // &&
        //

        Tokenizer_nextWLCharacter(session, tokenStartBuf, tokenStartLoc, policy);

        return Token(
            TokenKind::AmpAmp,
            Tokenizer_getTokenBufferAndLength(session, tokenStartBuf),
            Tokenizer_getTokenSource(session, tokenStartLoc),
        );
    }

    //
    // &
    //

    incr_diagnostic!(Tokenizer_AmpCount);

    return Token(
        TokenKind::Amp,
        Tokenizer_getTokenBufferAndLength(session, tokenStartBuf),
        Tokenizer_getTokenSource(session, tokenStartLoc),
    );
}

fn Tokenizer_handleSlash<'i>(
    session: &mut Tokenizer<'i>,
    tokenStartBuf: Buffer<'i>,
    tokenStartLoc: Location,
    mut c: WLCharacter,
    policy: NextPolicy,
) -> TokenRef<'i> {
    assert!(c.to_point() == '/');

    c = Tokenizer_currentWLCharacter(session, tokenStartBuf, tokenStartLoc, policy);

    match c.to_point() {
        Char('@') => {
            //
            // /@
            //

            Tokenizer_nextWLCharacter(session, tokenStartBuf, tokenStartLoc, policy);

            return Token(
                TokenKind::SlashAt,
                Tokenizer_getTokenBufferAndLength(session, tokenStartBuf),
                Tokenizer_getTokenSource(session, tokenStartLoc),
            );
        },
        Char(';') => {
            //
            // /;
            //

            Tokenizer_nextWLCharacter(session, tokenStartBuf, tokenStartLoc, policy);

            return Token(
                TokenKind::SlashSemi,
                Tokenizer_getTokenBufferAndLength(session, tokenStartBuf),
                Tokenizer_getTokenSource(session, tokenStartLoc),
            );
        },
        Char('.') => {
            let dot_mark = session.mark();

            Tokenizer_nextWLCharacter(session, tokenStartBuf, tokenStartLoc, policy);

            c = Tokenizer_currentWLCharacter(session, tokenStartBuf, tokenStartLoc, policy);

            if !c.isDigit() {
                //
                // /.
                //

                return Token(
                    TokenKind::SlashDot,
                    Tokenizer_getTokenBufferAndLength(session, tokenStartBuf),
                    Tokenizer_getTokenSource(session, tokenStartLoc),
                );
            }

            //
            // Something like  t/.3
            //
            // Must now do surgery and back up
            //

            Tokenizer_backupAndWarn(session, dot_mark);

            return Token(
                TokenKind::Slash,
                Tokenizer_getTokenBufferAndLength(session, tokenStartBuf),
                Tokenizer_getTokenSource(session, tokenStartLoc),
            );
        },
        Char('/') => {
            //
            // //
            //

            Tokenizer_nextWLCharacter(session, tokenStartBuf, tokenStartLoc, policy);

            c = Tokenizer_currentWLCharacter(session, tokenStartBuf, tokenStartLoc, policy);

            match c.to_point() {
                Char('.') => {
                    //
                    // //.
                    //

                    Tokenizer_nextWLCharacter(session, tokenStartBuf, tokenStartLoc, policy);

                    return Token(
                        TokenKind::SlashSlashDot,
                        Tokenizer_getTokenBufferAndLength(session, tokenStartBuf),
                        Tokenizer_getTokenSource(session, tokenStartLoc),
                    );
                },
                Char('@') => {
                    //
                    // //@
                    //

                    Tokenizer_nextWLCharacter(session, tokenStartBuf, tokenStartLoc, policy);

                    return Token(
                        TokenKind::SlashSlashAt,
                        Tokenizer_getTokenBufferAndLength(session, tokenStartBuf),
                        Tokenizer_getTokenSource(session, tokenStartLoc),
                    );
                },
                Char('=') => {
                    //
                    // //=
                    //

                    Tokenizer_nextWLCharacter(session, tokenStartBuf, tokenStartLoc, policy);

                    return Token(
                        TokenKind::SlashSlashEqual,
                        Tokenizer_getTokenBufferAndLength(session, tokenStartBuf),
                        Tokenizer_getTokenSource(session, tokenStartLoc),
                    );
                },
                _ => (),
            }

            //
            // //
            //

            return Token(
                TokenKind::SlashSlash,
                Tokenizer_getTokenBufferAndLength(session, tokenStartBuf),
                Tokenizer_getTokenSource(session, tokenStartLoc),
            );
        },
        Char(':') => {
            //
            // /:
            //

            Tokenizer_nextWLCharacter(session, tokenStartBuf, tokenStartLoc, policy);

            return Token(
                TokenKind::SlashColon,
                Tokenizer_getTokenBufferAndLength(session, tokenStartBuf),
                Tokenizer_getTokenSource(session, tokenStartLoc),
            );
        },
        Char('=') => {
            //
            // /=
            //

            Tokenizer_nextWLCharacter(session, tokenStartBuf, tokenStartLoc, policy);

            return Token(
                TokenKind::SlashEqual,
                Tokenizer_getTokenBufferAndLength(session, tokenStartBuf),
                Tokenizer_getTokenSource(session, tokenStartLoc),
            );
        },
        Char('*') => {
            //
            // /*
            //

            Tokenizer_nextWLCharacter(session, tokenStartBuf, tokenStartLoc, policy);

            return Token(
                TokenKind::SlashStar,
                Tokenizer_getTokenBufferAndLength(session, tokenStartBuf),
                Tokenizer_getTokenSource(session, tokenStartLoc),
            );
        },
        _ => (),
    }

    //
    // /
    //

    return Token(
        TokenKind::Slash,
        Tokenizer_getTokenBufferAndLength(session, tokenStartBuf),
        Tokenizer_getTokenSource(session, tokenStartLoc),
    );
}

fn Tokenizer_handleAt<'i>(
    session: &mut Tokenizer<'i>,
    tokenStartBuf: Buffer<'i>,
    tokenStartLoc: Location,
    mut c: WLCharacter,
    policy: NextPolicy,
) -> TokenRef<'i> {
    assert!(c.to_point() == '@');

    c = Tokenizer_currentWLCharacter(session, tokenStartBuf, tokenStartLoc, policy);

    match c.to_point() {
        Char('@') => {
            Tokenizer_nextWLCharacter(session, tokenStartBuf, tokenStartLoc, policy);

            c = Tokenizer_currentWLCharacter(session, tokenStartBuf, tokenStartLoc, policy);

            if c.to_point() == '@' {
                //
                // @@@
                //

                Tokenizer_nextWLCharacter(session, tokenStartBuf, tokenStartLoc, policy);

                return Token(
                    TokenKind::AtAtAt,
                    Tokenizer_getTokenBufferAndLength(session, tokenStartBuf),
                    Tokenizer_getTokenSource(session, tokenStartLoc),
                );
            }

            //
            // @@
            //

            return Token(
                TokenKind::AtAt,
                Tokenizer_getTokenBufferAndLength(session, tokenStartBuf),
                Tokenizer_getTokenSource(session, tokenStartLoc),
            );
        },
        Char('*') => {
            //
            // @*
            //

            Tokenizer_nextWLCharacter(session, tokenStartBuf, tokenStartLoc, policy);

            return Token(
                TokenKind::AtStar,
                Tokenizer_getTokenBufferAndLength(session, tokenStartBuf),
                Tokenizer_getTokenSource(session, tokenStartLoc),
            );
        },
        _ => (),
    }

    //
    // @
    //

    return Token(
        TokenKind::At,
        Tokenizer_getTokenBufferAndLength(session, tokenStartBuf),
        Tokenizer_getTokenSource(session, tokenStartLoc),
    );
}

fn Tokenizer_handlePlus<'i>(
    session: &mut Tokenizer<'i>,
    tokenStartBuf: Buffer<'i>,
    tokenStartLoc: Location,
    mut c: WLCharacter,
    policy: NextPolicy,
) -> TokenRef<'i> {
    assert!(c.to_point() == '+');

    c = Tokenizer_currentWLCharacter(session, tokenStartBuf, tokenStartLoc, policy);

    match c.to_point() {
        Char('+') => {
            //
            // ++
            //

            Tokenizer_nextWLCharacter(session, tokenStartBuf, tokenStartLoc, policy);

            if feature::CHECK_ISSUES {
                c = Tokenizer_currentWLCharacter(session, tokenStartBuf, tokenStartLoc, policy);

                if c.to_point() == '=' {
                    //
                    // Something like  a++=0
                    //

                    let loc = session.SrcLoc;

                    let mut Actions: Vec<CodeAction> = Vec::new();

                    Actions.push(CodeAction::insert_text(
                        "Insert space".into(),
                        Span::from_location(loc),
                        " ".into(),
                    ));

                    let I = FormatIssue(
                        IssueTag::Ambiguous,
                        "Put a space between ``++`` and ``=`` to reduce ambiguity".into(),
                        Severity::Formatting,
                        Span::from_location(loc),
                        1.0,
                        Actions,
                        vec![],
                    );

                    session.addIssue(I);
                }
            }

            return Token(
                TokenKind::PlusPlus,
                Tokenizer_getTokenBufferAndLength(session, tokenStartBuf),
                Tokenizer_getTokenSource(session, tokenStartLoc),
            );
        },
        Char('=') => {
            //
            // +=
            //

            Tokenizer_nextWLCharacter(session, tokenStartBuf, tokenStartLoc, policy);

            return Token(
                TokenKind::PlusEqual,
                Tokenizer_getTokenBufferAndLength(session, tokenStartBuf),
                Tokenizer_getTokenSource(session, tokenStartLoc),
            );
        },
        _ => (),
    }

    //
    // +
    //

    incr_diagnostic!(Tokenizer_PlusCount);

    return Token(
        TokenKind::Plus,
        Tokenizer_getTokenBufferAndLength(session, tokenStartBuf),
        Tokenizer_getTokenSource(session, tokenStartLoc),
    );
}

fn Tokenizer_handleTilde<'i>(
    session: &mut Tokenizer<'i>,
    tokenStartBuf: Buffer<'i>,
    tokenStartLoc: Location,
    mut c: WLCharacter,
    policy: NextPolicy,
) -> TokenRef<'i> {
    assert!(c.to_point() == '~');

    c = Tokenizer_currentWLCharacter(session, tokenStartBuf, tokenStartLoc, policy);

    if c.to_point() == '~' {
        //
        // ~~
        //

        Tokenizer_nextWLCharacter(session, tokenStartBuf, tokenStartLoc, policy);

        return Token(
            TokenKind::TildeTilde,
            Tokenizer_getTokenBufferAndLength(session, tokenStartBuf),
            Tokenizer_getTokenSource(session, tokenStartLoc),
        );
    }

    //
    // ~
    //

    return Token(
        TokenKind::Tilde,
        Tokenizer_getTokenBufferAndLength(session, tokenStartBuf),
        Tokenizer_getTokenSource(session, tokenStartLoc),
    );
}

fn Tokenizer_handleQuestion<'i>(
    session: &mut Tokenizer<'i>,
    tokenStartBuf: Buffer<'i>,
    tokenStartLoc: Location,
    mut c: WLCharacter,
    policy: NextPolicy,
) -> TokenRef<'i> {
    assert!(c.to_point() == '?');

    c = Tokenizer_currentWLCharacter(session, tokenStartBuf, tokenStartLoc, policy);

    if c.to_point() == '?' {
        //
        // ??
        //

        Tokenizer_nextWLCharacter(session, tokenStartBuf, tokenStartLoc, policy);

        return Token(
            TokenKind::QuestionQuestion,
            Tokenizer_getTokenBufferAndLength(session, tokenStartBuf),
            Tokenizer_getTokenSource(session, tokenStartLoc),
        );
    }

    //
    // ?
    //

    return Token(
        TokenKind::Question,
        Tokenizer_getTokenBufferAndLength(session, tokenStartBuf),
        Tokenizer_getTokenSource(session, tokenStartLoc),
    );
}

fn Tokenizer_handleStar<'i>(
    session: &mut Tokenizer<'i>,
    tokenStartBuf: Buffer<'i>,
    tokenStartLoc: Location,
    mut c: WLCharacter,
    policy: NextPolicy,
) -> TokenRef<'i> {
    assert!(c.to_point() == '*');

    c = Tokenizer_currentWLCharacter(session, tokenStartBuf, tokenStartLoc, policy);

    match c.to_point() {
        Char('=') => {
            //
            // *=
            //

            Tokenizer_nextWLCharacter(session, tokenStartBuf, tokenStartLoc, policy);

            return Token(
                TokenKind::StarEqual,
                Tokenizer_getTokenBufferAndLength(session, tokenStartBuf),
                Tokenizer_getTokenSource(session, tokenStartLoc),
            );
        },
        Char('*') => {
            //
            // **
            //

            Tokenizer_nextWLCharacter(session, tokenStartBuf, tokenStartLoc, policy);

            return Token(
                TokenKind::StarStar,
                Tokenizer_getTokenBufferAndLength(session, tokenStartBuf),
                Tokenizer_getTokenSource(session, tokenStartLoc),
            );
        },
        Char(')') => {
            //
            // *)
            //

            Tokenizer_nextWLCharacter(session, tokenStartBuf, tokenStartLoc, policy);

            return Token(
                TokenKind::Error_UnexpectedCommentCloser,
                Tokenizer_getTokenBufferAndLength(session, tokenStartBuf),
                Tokenizer_getTokenSource(session, tokenStartLoc),
            );
        },
        _ => (),
    }

    //
    // *
    //

    return Token(
        TokenKind::Star,
        Tokenizer_getTokenBufferAndLength(session, tokenStartBuf),
        Tokenizer_getTokenSource(session, tokenStartLoc),
    );
}

fn Tokenizer_handleCaret<'i>(
    session: &mut Tokenizer<'i>,
    tokenStartBuf: Buffer<'i>,
    tokenStartLoc: Location,
    mut c: WLCharacter,
    policy: NextPolicy,
) -> TokenRef<'i> {
    assert!(c.to_point() == '^');

    c = Tokenizer_currentWLCharacter(session, tokenStartBuf, tokenStartLoc, policy);

    match c.to_point() {
        Char(':') => {
            Tokenizer_nextWLCharacter(session, tokenStartBuf, tokenStartLoc, policy);

            c = Tokenizer_currentWLCharacter(session, tokenStartBuf, tokenStartLoc, policy);

            if c.to_point() == '=' {
                //
                // ^:=
                //

                Tokenizer_nextWLCharacter(session, tokenStartBuf, tokenStartLoc, policy);

                return Token(
                    TokenKind::CaretColonEqual,
                    Tokenizer_getTokenBufferAndLength(session, tokenStartBuf),
                    Tokenizer_getTokenSource(session, tokenStartLoc),
                );
            }

            //
            // Has to be ^:=
            //

            return Token(
                TokenKind::Error_ExpectedEqual,
                Tokenizer_getTokenBufferAndLength(session, tokenStartBuf),
                Tokenizer_getTokenSource(session, tokenStartLoc),
            );
        },
        Char('=') => {
            //
            // ^=
            //

            Tokenizer_nextWLCharacter(session, tokenStartBuf, tokenStartLoc, policy);

            return Token(
                TokenKind::CaretEqual,
                Tokenizer_getTokenBufferAndLength(session, tokenStartBuf),
                Tokenizer_getTokenSource(session, tokenStartLoc),
            );
        },
        _ => (),
    }

    //
    // ^
    //

    return Token(
        TokenKind::Caret,
        Tokenizer_getTokenBufferAndLength(session, tokenStartBuf),
        Tokenizer_getTokenSource(session, tokenStartLoc),
    );
}

fn Tokenizer_handleUnhandledBackslash<'i>(
    session: &mut Tokenizer<'i>,
    tokenStartBuf: Buffer<'i>,
    tokenStartLoc: Location,
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

    c = Tokenizer_nextWLCharacter(session, tokenStartBuf, tokenStartLoc, policy);

    match c.to_point() {
        Char('[') => {
            //
            // Try to reconstruct \[XXX]
            //

            let mut reset_mark = session.mark();

            c = Tokenizer_nextWLCharacter(session, tokenStartBuf, tokenStartLoc, policy);

            let mut wellFormed = false;

            if c.isUpper() {
                reset_mark = session.mark();

                c = Tokenizer_nextWLCharacter(session, tokenStartBuf, tokenStartLoc, policy);

                loop {
                    if c.isAlphaOrDigit() {
                        reset_mark = session.mark();

                        c = Tokenizer_nextWLCharacter(
                            session,
                            tokenStartBuf,
                            tokenStartLoc,
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
                return Token(
                    TokenKind::Error_UnhandledCharacter,
                    Tokenizer_getTokenBufferAndLength(session, tokenStartBuf),
                    Tokenizer_getTokenSource(session, tokenStartLoc),
                );
            }

            return Token(
                TokenKind::Error_UnhandledCharacter,
                Tokenizer_getTokenBufferAndLength(session, tokenStartBuf),
                Tokenizer_getTokenSource(session, tokenStartLoc),
            );
        },
        Char(':') => {
            //
            // Try to reconstruct \:XXXX
            //

            let mut reset_mark = session.mark();

            c = Tokenizer_nextWLCharacter(session, tokenStartBuf, tokenStartLoc, policy);

            for _ in 0..4 {
                if c.isHex() {
                    reset_mark = session.mark();

                    c = Tokenizer_nextWLCharacter(session, tokenStartBuf, tokenStartLoc, policy);

                    continue;
                }

                session.seek(reset_mark);

                break;
            }

            return Token(
                TokenKind::Error_UnhandledCharacter,
                Tokenizer_getTokenBufferAndLength(session, tokenStartBuf),
                Tokenizer_getTokenSource(session, tokenStartLoc),
            );
        },
        Char('.') => {
            //
            // Try to reconstruct \.XX
            //

            let mut reset_mark = session.mark();

            c = Tokenizer_nextWLCharacter(session, tokenStartBuf, tokenStartLoc, policy);

            for _ in 0..2 {
                if c.isHex() {
                    reset_mark = session.mark();

                    c = Tokenizer_nextWLCharacter(session, tokenStartBuf, tokenStartLoc, policy);

                    continue;
                }

                session.seek(reset_mark);

                break;
            }

            return Token(
                TokenKind::Error_UnhandledCharacter,
                Tokenizer_getTokenBufferAndLength(session, tokenStartBuf),
                Tokenizer_getTokenSource(session, tokenStartLoc),
            );
        },
        Char('0' | '1' | '2' | '3' | '4' | '5' | '6' | '7') => {
            //
            // Try to reconstruct \XXX
            //

            let mut reset_mark = session.mark();

            c = Tokenizer_nextWLCharacter(session, tokenStartBuf, tokenStartLoc, policy);

            for _ in 0..3 {
                if c.isOctal() {
                    reset_mark = session.mark();

                    c = Tokenizer_nextWLCharacter(session, tokenStartBuf, tokenStartLoc, policy);

                    continue;
                }

                session.seek(reset_mark);

                break;
            }

            return Token(
                TokenKind::Error_UnhandledCharacter,
                Tokenizer_getTokenBufferAndLength(session, tokenStartBuf),
                Tokenizer_getTokenSource(session, tokenStartLoc),
            );
        },
        Char('|') => {
            //
            // Try to reconstruct \|XXXXXX
            //

            let mut reset_mark = session.mark();
            c = Tokenizer_nextWLCharacter(session, tokenStartBuf, tokenStartLoc, policy);

            for _ in 0..6 {
                if c.isHex() {
                    reset_mark = session.mark();

                    c = Tokenizer_nextWLCharacter(session, tokenStartBuf, tokenStartLoc, policy);

                    continue;
                }

                session.seek(reset_mark);

                break;
            }

            return Token(
                TokenKind::Error_UnhandledCharacter,
                Tokenizer_getTokenBufferAndLength(session, tokenStartBuf),
                Tokenizer_getTokenSource(session, tokenStartLoc),
            );
        },
        EndOfFile => {
            return Token(
                TokenKind::Error_UnhandledCharacter,
                Tokenizer_getTokenBufferAndLength(session, tokenStartBuf),
                Tokenizer_getTokenSource(session, tokenStartLoc),
            );
        },
        _ => (),
    } // switch

    //
    // Nothing special, just read next single character
    //

    return Token(
        TokenKind::Error_UnhandledCharacter,
        Tokenizer_getTokenBufferAndLength(session, tokenStartBuf),
        Tokenizer_getTokenSource(session, tokenStartLoc),
    );
}

fn Tokenizer_handleMBStrangeNewline<'i>(
    session: &mut Tokenizer<'i>,
    tokenStartBuf: Buffer<'i>,
    tokenStartLoc: Location,
    c: WLCharacter,
    policy: NextPolicy,
) -> TokenRef<'i> {
    assert!(c.isMBStrangeNewline());

    if feature::CHECK_ISSUES {
        let Src = Tokenizer_getTokenSource(session, tokenStartLoc);

        let mut Actions: Vec<CodeAction> = Vec::new();

        for A in utils::certainCharacterReplacementActions(c, Src) {
            Actions.push(A);
        }

        let I = SyntaxIssue(
            IssueTag::UnexpectedNewlineCharacter,
            format!("Unexpected newline character: ``{}``.", c.graphicalString()),
            Severity::Warning,
            Src,
            0.85,
            Actions,
            vec![],
        );

        session.addIssue(I);
    }

    //
    // Return INTERNALNEWLINE or TOPLEVELNEWLINE, depending on policy
    //
    return Token(
        TokenKind::InternalNewline.with_policy(policy),
        Tokenizer_getTokenBufferAndLength(session, tokenStartBuf),
        Tokenizer_getTokenSource(session, tokenStartLoc),
    );
}

fn Tokenizer_handleMBStrangeWhitespace<'i>(
    session: &mut Tokenizer<'i>,
    tokenStartBuf: Buffer<'i>,
    tokenStartLoc: Location,
    c: WLCharacter,
    _policy: NextPolicy,
) -> TokenRef<'i> {
    assert!(c.isMBStrangeWhitespace());

    if feature::CHECK_ISSUES {
        let Src = Tokenizer_getTokenSource(session, tokenStartLoc);

        let mut Actions: Vec<CodeAction> = Vec::new();

        for A in utils::certainCharacterReplacementActions(c, Src) {
            Actions.push(A);
        }

        let I = SyntaxIssue(
            IssueTag::UnexpectedSpaceCharacter,
            format!(
                "Unexpected space character: ``{}``.",
                c.safeAndGraphicalString()
            ),
            Severity::Warning,
            Src,
            0.85,
            Actions,
            vec![],
        );

        session.addIssue(I);
    }

    return Token(
        TokenKind::Whitespace,
        Tokenizer_getTokenBufferAndLength(session, tokenStartBuf),
        Tokenizer_getTokenSource(session, tokenStartLoc),
    );
}

fn Tokenizer_handleMBPunctuation<'i>(
    session: &mut Tokenizer<'i>,
    tokenStartBuf: Buffer<'i>,
    tokenStartLoc: Location,
    c: WLCharacter,
    _policy: NextPolicy,
) -> TokenRef<'i> {
    assert!(c.isMBPunctuation());

    let Operator = crate::long_names_registration::LongNameCodePointToOperator(c.to_point());

    return Token(
        Operator,
        Tokenizer_getTokenBufferAndLength(session, tokenStartBuf),
        Tokenizer_getTokenSource(session, tokenStartLoc),
    );
}

fn Tokenizer_handleNakedMBLinearSyntax<'i>(
    session: &mut Tokenizer<'i>,
    tokenStartBuf: Buffer<'i>,
    tokenStartLoc: Location,
    c: WLCharacter,
    _policy: NextPolicy,
) -> TokenRef<'i> {
    assert!(c.isMBLinearSyntax());

    match c.to_point() {
        Char(CODEPOINT_LINEARSYNTAX_CLOSEPAREN) => {
            return Token(
                TokenKind::LinearSyntax_CloseParen,
                Tokenizer_getTokenBufferAndLength(session, tokenStartBuf),
                Tokenizer_getTokenSource(session, tokenStartLoc),
            );
        },
        Char(CODEPOINT_LINEARSYNTAX_AT) => {
            return Token(
                TokenKind::LinearSyntax_At,
                Tokenizer_getTokenBufferAndLength(session, tokenStartBuf),
                Tokenizer_getTokenSource(session, tokenStartLoc),
            );
        },
        Char(CODEPOINT_LINEARSYNTAX_PERCENT) => {
            return Token(
                TokenKind::LinearSyntax_Percent,
                Tokenizer_getTokenBufferAndLength(session, tokenStartBuf),
                Tokenizer_getTokenSource(session, tokenStartLoc),
            );
        },
        Char(CODEPOINT_LINEARSYNTAX_CARET) => {
            return Token(
                TokenKind::LinearSyntax_Caret,
                Tokenizer_getTokenBufferAndLength(session, tokenStartBuf),
                Tokenizer_getTokenSource(session, tokenStartLoc),
            );
        },
        Char(CODEPOINT_LINEARSYNTAX_AMP) => {
            return Token(
                TokenKind::LinearSyntax_Amp,
                Tokenizer_getTokenBufferAndLength(session, tokenStartBuf),
                Tokenizer_getTokenSource(session, tokenStartLoc),
            );
        },
        Char(CODEPOINT_LINEARSYNTAX_STAR) => {
            return Token(
                TokenKind::LinearSyntax_Star,
                Tokenizer_getTokenBufferAndLength(session, tokenStartBuf),
                Tokenizer_getTokenSource(session, tokenStartLoc),
            );
        },
        Char(CODEPOINT_LINEARSYNTAX_UNDER) => {
            return Token(
                TokenKind::LinearSyntax_Under,
                Tokenizer_getTokenBufferAndLength(session, tokenStartBuf),
                Tokenizer_getTokenSource(session, tokenStartLoc),
            );
        },
        Char(CODEPOINT_LINEARSYNTAX_PLUS) => {
            return Token(
                TokenKind::LinearSyntax_Plus,
                Tokenizer_getTokenBufferAndLength(session, tokenStartBuf),
                Tokenizer_getTokenSource(session, tokenStartLoc),
            );
        },
        Char(CODEPOINT_LINEARSYNTAX_SLASH) => {
            return Token(
                TokenKind::LinearSyntax_Slash,
                Tokenizer_getTokenBufferAndLength(session, tokenStartBuf),
                Tokenizer_getTokenSource(session, tokenStartLoc),
            );
        },
        Char(CODEPOINT_LINEARSYNTAX_BACKTICK) => {
            return Token(
                TokenKind::LinearSyntax_BackTick,
                Tokenizer_getTokenBufferAndLength(session, tokenStartBuf),
                Tokenizer_getTokenSource(session, tokenStartLoc),
            );
        },
        CodePoint::LinearSyntax_Space => {
            return Token(
                TokenKind::LinearSyntax_Space,
                Tokenizer_getTokenBufferAndLength(session, tokenStartBuf),
                Tokenizer_getTokenSource(session, tokenStartLoc),
            );
        },
        _ => todo!(),
    }
}

// TODO(cleanup): Rename to Tokenizer::get_token_span()
fn Tokenizer_getTokenSource<'i>(session: &Tokenizer<'i>, tokStartLoc: Location) -> Span {
    let loc = session.SrcLoc;

    return Span::new(tokStartLoc, loc);
}

fn Tokenizer_getTokenBufferAndLength<'i>(
    session: &Tokenizer<'i>,
    tokStartBuf: Buffer<'i>,
) -> BufferAndLength<'i> {
    // return BufferAndLength::new(tokStartBuf, session.buffer - tokStartBuf);

    BufferAndLength::between(tokStartBuf, session.buffer())
}
