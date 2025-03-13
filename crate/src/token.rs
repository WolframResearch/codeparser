use crate::{
    feature,
    source::{Buffer, BufferAndLength, ByteSpan, Source},
    tokenizer::Tokenizer,
};

pub use crate::token_enum_registration::TokenKind;

pub(crate) type TokenRef<'i> = Token<BorrowedTokenInput<'i>>;

#[derive(Debug, Copy, Clone, PartialEq)]
pub struct Token<I = OwnedTokenInput> {
    pub tok: TokenKind,

    pub src: Source,

    pub input: I,
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
#[derive(Debug, Clone, PartialEq)]
pub struct OwnedTokenInput {
    pub buf: Vec<u8>,
}

impl<'i> BorrowedTokenInput<'i> {
    #[doc(hidden)]
    pub fn new(slice: &'i [u8], offset: usize) -> Self {
        BorrowedTokenInput {
            buf: BufferAndLength {
                buf: Buffer { slice, offset },
            },
        }
    }

    pub(crate) fn from_buf(buf: BufferAndLength<'i>) -> Self {
        BorrowedTokenInput { buf: buf }
    }

    pub(crate) fn byte_span(&self) -> ByteSpan {
        let BorrowedTokenInput { buf } = self;

        buf.byte_span()
    }

    fn into_empty(self) -> Self {
        let BorrowedTokenInput { mut buf } = self;

        buf.buf.slice = &buf.buf.slice[..0];

        BorrowedTokenInput { buf }
    }

    fn into_owned_input(self) -> OwnedTokenInput {
        let BorrowedTokenInput { buf } = self;

        OwnedTokenInput {
            buf: buf.as_bytes().to_owned(),
        }
    }
}


//
// Sizes of structs with bit-fields are implementation-dependent
//
// TODO(optimize): In the C++ version (which used bitfields to pack the `len` to
//                 48 bits), this was 32 bytes.
const _: () = assert!(std::mem::size_of::<TokenRef>() == 48);
const _: () = assert!(std::mem::size_of::<BorrowedTokenInput>() == 24);

#[cfg(target_pointer_width = "64")]
#[test]
fn test_token_size() {
    assert_eq!(std::mem::size_of::<TokenRef>(), 48);
    assert_eq!(std::mem::size_of::<BorrowedTokenInput>(), 24);
}

//
// For googletest
//
#[cfg(feature = "BUILD_TESTS")]
fn PrintTo(token: &Token, stream: &mut std::ostream);

impl<'i> TokenRef<'i> {
    // pub(crate) fn new(tok: TokenKind, buf: BufferAndLength, src: Source) -> Self {
    pub(crate) fn new(tok: TokenKind, buf: BufferAndLength<'i>, src: Source) -> Self {
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
            _ if feature::COMPUTE_SOURCE => {
                use crate::source::SourceCharacter;

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
                    if src.start.first == 0 && src.end.first == 0 {
                        //
                        // SourceConvention of "SourceCharacterIndex"
                        // so nothing to do
                        //
                    } else if src.start.first == src.end.first {
                        if src.len() != buf.length() {
                            //
                            // If the sizes do not match, then check if there are multi-byte characters
                            // If there are multi-bytes characters, then it is too complicated to compare sizes
                            //
                            // Note that this also catches changes in character representation, e.g.,
                            // If a character was in source with \XXX octal notation but was stringified with \:XXXX hex notation
                            //
                            assert!(!buf.containsOnlyASCII() || buf.containsTab());
                        }
                    }
                }
            },
            _ => (),
        }

        token
    }

    pub(crate) fn error_at_start(error_tok: TokenKind, mut token: TokenRef<'i>) -> TokenRef<'i> {
        // The error is at the start of this token.
        token.src = Source::from_location(token.src.start);

        Token::error_at(error_tok, token)
    }

    pub(crate) fn error_at(error_tok: TokenKind, token: TokenRef<'i>) -> TokenRef<'i> {
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

        if is_len_zero(error_tok) {
            input = input.into_empty();
        }

        Token {
            tok: error_tok,
            src,
            input,
        }
    }

    pub(crate) fn into_owned_input(self) -> Token {
        let Token { tok, src, input } = self;

        Token {
            tok,
            src,
            input: input.into_owned_input(),
        }
    }
}

impl<'i> TokenRef<'i> {
    fn end(&self) -> usize {
        return self.input.byte_span().end();
    }

    pub(crate) fn reset(&self, session: &mut Tokenizer) {
        //
        //
        // Just need to reset the global buffer to the buffer of the token
        //

        session.offset = self.input.byte_span().offset;
        session.SrcLoc = self.src.start;
    }

    pub(crate) fn skip(&self, session: &mut Tokenizer) {
        session.offset = self.end();
        session.wasEOF = self.tok == TokenKind::EndOfFile;
        session.SrcLoc = self.src.end;
    }
}

impl<T> Token<T> {
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

    //
    // For googletest
    //
    #[cfg(feature = "BUILD_TESTS")]
    fn PrintTo(T: &Token, s: &mut std::ostream) {
        T.print(*s);
    }
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
