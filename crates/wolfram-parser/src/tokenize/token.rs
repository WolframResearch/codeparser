use std::fmt::{self, Debug};

use crate::{
    source::{Buffer, BufferAndLength, Source, Span},
    tokenize::{TokenKind, Tokenizer},
};

pub(crate) type TokenRef<'i> = Token<BorrowedTokenInput<'i>>;

#[derive(Debug, Copy, Clone, PartialEq)]
pub struct Token<I = OwnedTokenInput, S = Span> {
    pub tok: TokenKind,

    pub input: I,

    pub src: S,
}

pub trait TokenInput: Clone {
    fn as_bytes(&self) -> &[u8];

    fn as_str(&self) -> &str {
        std::str::from_utf8(self.as_bytes())
            .expect("TokenInput::as_str(): as_bytes() for this token did not return valid UTF-8")
    }

    fn into_owned(self) -> OwnedTokenInput;

    #[doc(hidden)]
    fn fake(input: &'static str) -> Self;
}

pub trait TokenSource: Clone {
    /// Canonicalize source region representation into the more general
    /// [`Source`] type.
    ///
    /// Typically this means converting a [`Span`] into a [`Source`].
    fn into_general(self) -> Source;

    #[doc(hidden)]
    fn unknown() -> Self;

    fn from_span(source: Span) -> Self;
}

impl TokenSource for Source {
    fn into_general(self) -> Source {
        self
    }

    fn unknown() -> Self {
        Source::unknown()
    }

    fn from_span(source: Span) -> Self {
        Source::Span(source)
    }
}

impl TokenSource for Span {
    fn into_general(self) -> Source {
        Source::Span(self)
    }

    fn unknown() -> Self {
        Span::unknown()
    }

    fn from_span(source: Span) -> Self {
        source
    }
}

impl<'i> TokenInput for BorrowedTokenInput<'i> {
    fn as_bytes(&self) -> &[u8] {
        let BorrowedTokenInput { buf } = self;

        buf.as_bytes()
    }

    fn into_owned(self) -> OwnedTokenInput {
        let BorrowedTokenInput { buf } = self;

        OwnedTokenInput {
            buf: buf.as_bytes().to_vec(),
        }
    }

    fn fake(input: &'static str) -> Self {
        BorrowedTokenInput {
            buf: BufferAndLength {
                buf: Buffer {
                    slice: input.as_bytes(),
                },
            },
        }
    }
}

impl TokenInput for OwnedTokenInput {
    fn as_bytes(&self) -> &[u8] {
        let OwnedTokenInput { buf } = self;

        buf.as_slice()
    }

    fn into_owned(self) -> OwnedTokenInput {
        self
    }

    fn fake(input: &'static str) -> Self {
        OwnedTokenInput {
            buf: input.as_bytes().to_vec(),
        }
    }
}

impl OwnedTokenInput {
    pub fn to_str(&self) -> &str {
        std::str::from_utf8(self.as_bytes()).expect(
            "OwnedTokenInput::to_str(): token source is not valid UTF-8",
        )
    }
}

/// Borrowed subslice of the input that is associated with a particular
/// [`Token`] instance.
///
/// This type is used for efficient zero-copy parsing of input during the
/// tokenization and parsing steps.
#[derive(Debug, Copy, Clone, PartialEq)]
pub struct BorrowedTokenInput<'i> {
    pub(crate) buf: BufferAndLength<'i>,
}

/// Owned subslice of the input that is associated with a particular
/// [`Token`] instance.
#[derive(Clone, PartialEq)]
pub struct OwnedTokenInput {
    pub buf: Vec<u8>,
}

impl<'i> BorrowedTokenInput<'i> {
    #[doc(hidden)]
    pub fn new(slice: &'i [u8]) -> Self {
        BorrowedTokenInput {
            buf: BufferAndLength {
                buf: Buffer { slice },
            },
        }
    }

    pub(crate) fn from_buf(buf: BufferAndLength<'i>) -> Self {
        BorrowedTokenInput { buf: buf }
    }

    fn into_empty(self) -> Self {
        let BorrowedTokenInput { mut buf } = self;

        buf.buf.slice = &buf.buf.slice[..0];

        BorrowedTokenInput { buf }
    }
}


//
// Sizes of structs with bit-fields are implementation-dependent
//
// TODO(optimize): In the C++ version (which used bitfields to pack the `len` to
//                 48 bits), this was 32 bytes.
const _: () = assert!(std::mem::size_of::<TokenRef>() == 40);
const _: () = assert!(std::mem::size_of::<BorrowedTokenInput>() == 16);

#[cfg(target_pointer_width = "64")]
#[test]
fn test_token_size() {
    assert_eq!(std::mem::size_of::<TokenRef>(), 40);
    assert_eq!(std::mem::size_of::<BorrowedTokenInput>(), 16);
}

impl<'i> TokenRef<'i> {
    pub(crate) fn new(
        tok: TokenKind,
        buf: BufferAndLength<'i>,
        src: Span,
    ) -> Self {
        let token = Token {
            src,
            input: BorrowedTokenInput::from_buf(buf),
            tok,
        };

        //
        // verify BufLen and Src are equivalent
        //

        #[cfg(debug_assertions)]
        match tok {
            TokenKind::Unknown => {
                panic!("illegal TokenKind::Unknown")
            },
            //
            // Both \n and \r\n newlines have a size of 1
            // And other newlines like \[IndentingNewLine] have size > 1
            //
            TokenKind::ToplevelNewline | TokenKind::InternalNewline => {},
            _ if crate::feature::COMPUTE_SOURCE => {
                use crate::source::{
                    LineColumn, LineColumnSpan, SourceCharacter, SpanKind,
                };

                if tok.isEmpty() {
                    assert!(
                        (buf.length() == 0) ||
                        //
                        // There could be a line continuation in front.
                        // Token is still empty.
                        //
                        (buf.as_bytes()[0] == b'\\' && SourceCharacter::from(buf.as_bytes()[1]).isNewline()),
                    );
                } else {
                    assert!(buf.length() > 0);

                    //
                    // This is all just to do an assert.
                    // But it's a good assert because it catches problems.
                    //
                    // Only bother checking if the token is all on one line
                    // Spanning multiple lines is too complicated to care about
                    //
                    match src.kind() {
                        SpanKind::CharacterSpan(_) => {
                            //
                            // SourceConvention of "SourceCharacterIndex"
                            // so nothing to do
                            //
                        },
                        SpanKind::LineColumnSpan(LineColumnSpan {
                            start: LineColumn(start_line, _),
                            end: LineColumn(end_line, _),
                        }) => {
                            if start_line == end_line {
                                if src.column_width() != buf.length() {
                                    //
                                    // If the sizes do not match, then check if there are multi-byte characters
                                    // If there are multi-bytes characters, then it is too complicated to compare sizes
                                    //
                                    // Note that this also catches changes in character representation, e.g.,
                                    // If a character was in source with \XXX octal notation but was stringified with \:XXXX hex notation
                                    //
                                    assert!(
                                        !buf.containsOnlyASCII()
                                            || buf.containsTab()
                                    );
                                }
                            }
                        },
                        SpanKind::Unknown => (),
                    }
                }
            },
            _ => (),
        }

        token
    }

    pub(crate) fn at_start(
        error_tok: TokenKind,
        mut token: TokenRef<'i>,
    ) -> TokenRef<'i> {
        // The error is at the start of this token.
        token.src = Span::from_location(token.src.start);

        Token::at(error_tok, token)
    }

    /// Construct a new token positioned at `token` but with the [`TokenKind`]
    /// specified by `kind`.
    ///
    /// Typically used to construct new error or "fake" tokens.
    pub(crate) fn at(kind: TokenKind, token: TokenRef<'i>) -> TokenRef<'i> {
        // Note: Same as BufferAndLength(Buffer Buf), which inits the Len to 0

        let Token {
            tok: _,
            mut input,
            src,
        } = token;

        fn is_len_zero(tok: TokenKind) -> bool {
            match tok {
                TokenKind::Fake_ImplicitOne
                | TokenKind::Fake_ImplicitAll
                | TokenKind::Fake_ImplicitTimes
                | TokenKind::Fake_ImplicitNull => true,
                TokenKind::Error_InfixImplicitNull => true,
                TokenKind::Error_ExpectedOperand => true,
                TokenKind::Error_PrefixImplicitNull => true,
                _ => false,
            }
        }

        if is_len_zero(kind) {
            input = input.into_empty();
        }

        Token {
            tok: kind,
            src,
            input,
        }
    }
}

impl<I: TokenInput, S> Token<I, S> {
    pub(crate) fn into_owned_input(self) -> Token<OwnedTokenInput, S> {
        let Token { tok, src, input } = self;

        Token {
            tok,
            src,
            input: input.into_owned(),
        }
    }
}

impl<'i> TokenRef<'i> {
    pub(crate) fn reset(&self, session: &mut Tokenizer) {
        //
        //
        // Just need to reset the global buffer to the buffer of the token
        //

        session.offset = session.offset_of(self.input.buf.buf);
        session.SrcLoc = self.src.start;
    }

    pub(crate) fn skip(&self, session: &mut Tokenizer) {
        let end = session.offset_of(self.input.buf.buf)
            + self.input.buf.buf.slice.len();

        session.offset = end;
        session.wasEOF = self.tok == TokenKind::EndOfFile;
        session.SrcLoc = self.src.end;
    }
}

impl<I, S> Token<I, S> {
    pub(crate) fn check(&self) -> bool {
        return !self.tok.isError();
    }

    // TODO: impl Display
    // fn print(std::ostream& s) const {

    //     let Sym = TokenToSymbol(Tok);

    //     //
    //     // printing the token  123  as LeafNode[Integer, "123", <||>] seems the wrong way around, but it is convenient
    //     //

    //     if Tok.isError() {

    //         if Tok.isUnterminated() {

    //             SYMBOL_CODEPARSER_UNTERMINATEDTOKENERRORNEEDSREPARSENODE.print(s);

    //         } else {

    //             SYMBOL_CODEPARSER_ERRORNODE.print(s);
    //         }

    //     } else {

    //         SYMBOL_CODEPARSER_LEAFNODE.print(s);
    //     }

    //     s << "[";

    //     s << Sym.name;
    //     s << ", ";

    //     bufLen().print(s);
    //     s << ", ";

    //     Src.print(s);
    //     s << "]";
    // }
}

impl PartialEq<Token> for Token<BorrowedTokenInput<'_>> {
    fn eq(&self, other: &Token) -> bool {
        let Token { tok, src, input } = *self;

        if !(tok == other.tok && src == other.src) {
            return false;
        }

        let BorrowedTokenInput { buf } = input;

        buf.as_bytes() == other.input.buf
    }
}

//======================================
// Format Impls
//======================================

impl Debug for OwnedTokenInput {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let OwnedTokenInput { buf } = self;

        match std::str::from_utf8(buf) {
            Ok(str) => f
                .debug_struct("OwnedTokenInput")
                .field("buf", &str)
                .finish(),
            Err(_) => {
                f.debug_struct("OwnedTokenInput").field("buf", buf).finish()
            },
        }
    }
}