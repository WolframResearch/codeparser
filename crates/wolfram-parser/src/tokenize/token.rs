use std::fmt::{self, Debug, Display};

use crate::{
    source::{BoxPosition, BufferAndLength, Source, Span},
    tokenize::{TokenKind, Tokenizer},
};

pub(crate) type TokenRef<'i> = Token<TokenStr<'i>>;

/// Minimal syntactically-meaningful piece of Wolfram Language input.
///
/// Examples of common and not-so-common portions of input that constitute a
/// single token include:
///
/// Input        | [`TokenKind`] Variant                       | Notes
/// -------------|---------------------------------------------|--------
/// `abc`        | [`Symbol`][TokenKind::Symbol]               | Wolfram Language symbol
/// `123`        | [`Integer`][TokenKind::Integer]             |
/// `1.2`        | [`Integer`][TokenKind::Real]                |
/// `[`          | [`OpenSquare`][TokenKind::OpenSquare]       | Function call opener
/// `_`          | [`Under`][TokenKind::Under]                 |
/// `/@`         | [`SlashAt`][TokenKind::SlashAt]             |
/// `\[Alpha]bc` | [`Symbol`][TokenKind::Symbol]               | Letterlike named character
/// `\[Rule]`    | [`LongName_Rule`][TokenKind::LongName_Rule] | Operator named character
#[derive(Copy, Clone, PartialEq)]
pub struct Token<I = TokenString, S = Span> {
    pub tok: TokenKind,

    pub input: I,

    pub src: S,
}

/// Trait implemented for types that can store the piece of input associated
/// with a [`Token`].
///
/// This trait is implemented for two types:
///
/// * [`TokenStr`] — the [`Token`] input is borrowed from a buffer
/// * [`TokenString`] — the [`Token`] input is its own owned allocation
pub trait TokenInput: Clone {
    fn as_bytes(&self) -> &[u8];

    fn as_str(&self) -> &str {
        std::str::from_utf8(self.as_bytes())
            .expect("TokenInput::as_str(): as_bytes() for this token did not return valid UTF-8")
    }

    fn into_owned(self) -> TokenString;
}

pub trait TokenSource: Clone {
    /// Canonicalize source region representation into the more general
    /// [`Source`] type.
    ///
    /// Typically this means converting a [`Span`] into a [`Source`].
    fn into_general(self) -> Source;

    fn between(start: Self, end: Self) -> Self;
}

impl TokenSource for Source {
    fn into_general(self) -> Source {
        self
    }

    fn between(start: Source, end: Source) -> Self {
        match (&start, &end) {
            (a, b) if a.is_unknown() || b.is_unknown() => Source::unknown(),
            (Source::Span(start), Source::Span(end)) => {
                Source::Span(Span::between(*start, *end))
            },
            (Source::Box(start), Source::Box(end)) => {
                match BoxPosition::between(start, end) {
                    Some(box_source) => Source::Box(box_source),
                    None => Source::Unknown,
                }
            },
            (start, end) => {
                panic!("Unexpected combination of Source variants: start = {start:?}, end = {end:?}")
            },
        }
    }
}

impl TokenSource for Span {
    fn into_general(self) -> Source {
        Source::Span(self)
    }

    /// Construct a new [`Span`] that encloses everything between `start` and
    /// `end`.
    fn between(start: Span, end: Span) -> Self {
        assert!(start <= end);

        Span::new(start.start(), end.end())
    }
}

impl<'i> TokenInput for TokenStr<'i> {
    fn as_bytes(&self) -> &[u8] {
        let TokenStr { buf } = self;

        buf.as_bytes()
    }

    fn into_owned(self) -> TokenString {
        let TokenStr { buf } = self;

        TokenString {
            buf: buf.as_bytes().to_vec(),
        }
    }
}

impl TokenInput for TokenString {
    fn as_bytes(&self) -> &[u8] {
        let TokenString { buf } = self;

        buf.as_slice()
    }

    fn into_owned(self) -> TokenString {
        self
    }
}

impl TokenString {
    pub fn new(s: &'static str) -> Self {
        TokenString {
            buf: s.as_bytes().to_vec(),
        }
    }

    pub(crate) fn from_string(string: String) -> Self {
        TokenString {
            buf: string.into_bytes(),
        }
    }

    pub fn to_str(&self) -> &str {
        std::str::from_utf8(self.as_bytes())
            .expect("TokenString::to_str(): token source is not valid UTF-8")
    }
}

/// Borrowed subslice of the input that is associated with a particular
/// [`Token`] instance.
///
/// This type is used for efficient zero-copy parsing of input during the
/// tokenization and parsing steps.
///
/// **Naming:** The data contained in a [`TokenStr`] is in almost all cases
/// valid UTF-8. However, if the input contains a
/// [`TokenKind::Error_UnsafeCharacterEncoding`] token, then this may be invalid.
#[derive(Copy, Clone, PartialEq)]
pub struct TokenStr<'i> {
    pub(crate) buf: BufferAndLength<'i>,
}

/// Owned subslice of the input that is associated with a particular
/// [`Token`] instance.
#[derive(Clone, PartialEq)]
pub struct TokenString {
    pub buf: Vec<u8>,
}

impl<'i> TokenStr<'i> {
    #[doc(hidden)]
    pub fn new(slice: &'i [u8]) -> Self {
        TokenStr {
            buf: BufferAndLength { buf: slice },
        }
    }

    pub(crate) fn from_buf(buf: BufferAndLength<'i>) -> Self {
        TokenStr { buf }
    }

    fn into_empty(self) -> Self {
        let TokenStr { mut buf } = self;

        buf.buf = &buf.buf[..0];

        TokenStr { buf }
    }
}


//
// Sizes of structs with bit-fields are implementation-dependent
//
// TODO(optimize): In the C++ version (which used bitfields to pack the `len` to
//                 48 bits), this was 32 bytes.
const _: () = assert!(std::mem::size_of::<TokenRef>() == 40);
const _: () = assert!(std::mem::size_of::<TokenStr>() == 16);

#[cfg(target_pointer_width = "64")]
#[test]
fn test_token_size() {
    assert_eq!(std::mem::size_of::<TokenRef>(), 40);
    assert_eq!(std::mem::size_of::<TokenStr>(), 16);
}

impl<'i> TokenRef<'i> {
    /// Construct a new [`Token`] with borrowed input string.
    ///
    /// # Example
    ///
    /// Construct the token representing the input `123`:
    ///
    /// ```
    /// use wolfram_parser::{tokenize::{Token, TokenKind}, macros::src};
    ///
    /// let token = Token::new(TokenKind::Integer, "123", src!(1:1-4));
    /// ```
    ///
    /// Alternatively, the [`token!()`][crate::macros::token] macro can
    /// be used as a convenient way to construct tokens:
    ///
    /// ```
    /// use wolfram_parser::macros::token;
    ///
    /// let token = token!(Integer, "123", 1:1-4);
    /// ```
    pub fn new<S>(kind: TokenKind, input: &'i str, src: S) -> Self
    where
        S: Into<Span>,
    {
        Token {
            tok: kind,
            input: TokenStr {
                buf: BufferAndLength {
                    buf: input.as_bytes(),
                },
            },
            src: src.into(),
        }
    }

    pub(crate) fn new2(
        tok: TokenKind,
        buf: BufferAndLength<'i>,
        src: Span,
    ) -> Self {
        let token = Token {
            src,
            input: TokenStr::from_buf(buf),
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
                        SpanKind::LineColumnSpan(
                            src @ LineColumnSpan {
                                start: LineColumn(start_line, _),
                                end: LineColumn(end_line, _),
                            },
                        ) => {
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
        token.src = Span::at(token.src.start());

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
    pub(crate) fn into_owned_input(self) -> Token<TokenString, S> {
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
        session.SrcLoc = self.src.start();
    }

    pub(crate) fn skip(&self, session: &mut Tokenizer) {
        let end =
            session.offset_of(self.input.buf.buf) + self.input.buf.buf.len();

        session.offset = end;
        session.wasEOF = self.tok == TokenKind::EndOfFile;
        session.SrcLoc = self.src.end();
    }
}

impl<I, S> Token<I, S> {
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

impl PartialEq<Token> for Token<TokenStr<'_>> {
    fn eq(&self, other: &Token) -> bool {
        let Token { tok, src, input } = *self;

        if !(tok == other.tok && src == other.src) {
            return false;
        }

        let TokenStr { buf } = input;

        buf.as_bytes() == other.input.buf
    }
}

//======================================
// Format Impls
//======================================

impl<I: Debug, S: Debug> Debug for Token<I, S> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let Token { tok, input, src } = self;

        // If this is a test run, format this Token as the token! macro
        // invocation that would construct an equivalent Token instance. This
        // makes it very easy to copy and paste printed tokens from failed tests
        // back into the testing code itself, speeding up the writing of tests
        // by reducing the labor involved in manually writing Token { ... }
        //  struct instances.
        if cfg!(test) {
            write!(f, "token!({:?}, {:#?}, {:?})", tok, input, src)
        } else {
            f.debug_struct("Token")
                .field("tok", tok)
                .field("input", input)
                .field("src", src)
                .finish()
        }
    }
}

impl<'i> Debug for TokenStr<'i> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let TokenStr { buf } = self;

        match std::str::from_utf8(&buf.buf) {
            Ok(str) => {
                if f.alternate() {
                    write!(f, "{:?}", str)
                } else {
                    write!(f, "TokenStr({})", str)
                }
            },
            Err(_) => write!(f, "TokenStr({:?})", buf.buf),
        }
    }
}

impl Debug for TokenString {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let TokenString { buf } = self;

        match std::str::from_utf8(buf) {
            Ok(str) => {
                if cfg!(test) {
                    write!(f, "{self}")
                } else {
                    f.debug_struct("TokenString").field("buf", &str).finish()
                }
            },
            Err(_) => f.debug_struct("TokenString").field("buf", buf).finish(),
        }
    }
}

impl Display for TokenString {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let TokenString { buf } = self;

        match std::str::from_utf8(buf) {
            Ok(str) => {
                write!(f, "{str:?}")
            },
            Err(_) => f.debug_struct("TokenString").field("buf", buf).finish(),
        }
    }
}
