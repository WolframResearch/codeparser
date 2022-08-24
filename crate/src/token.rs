use crate::{
    source::{BufferAndLength, ByteSpan, Source},
    token_enum::TokenEnum,
    tokenizer::Tokenizer,
};

#[derive(Debug, Copy, Clone, PartialEq)]
pub struct Token {
    pub tok: TokenEnum,

    pub src: Source,

    pub span: ByteSpan,
}

//
// Sizes of structs with bit-fields are implementation-dependent
//
// TODO(optimize): In the C++ version (which used bitfields to pack the `len` to
//                 48 bits), this was 32 bytes.
#[cfg(target_pointer_width = "64")]
const _: () = assert!(std::mem::size_of::<Token>() == 40, "Check your assumptions");

//
// For googletest
//
#[cfg(feature = "BUILD_TESTS")]
fn PrintTo(token: &Token, stream: &mut std::ostream);

impl Token {
    // pub(crate) fn new(tok: TokenEnum, buf: BufferAndLength, src: Source) -> Self {
    pub(crate) fn new(tok: TokenEnum, buf: BufferAndLength, src: Source) -> Self {
        let token = Token {
            src,
            span: buf.byte_span(),
            tok,
        };

        //
        // verify BufLen and Src are equivalent
        //

        #[cfg(debug_assertions)]
        match tok {
            TokenEnum::TOKEN_UNKNOWN => {
                panic!("illegal TOKEN_UNKNOWN")
            },
            //
            // Both \n and \r\n newlines have a size of 1
            // And other newlines like \[IndentingNewLine] have size > 1
            //
            TokenEnum::TOKEN_TOPLEVELNEWLINE | TokenEnum::TOKEN_INTERNALNEWLINE => {},
            _ => {
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
        }

        token
    }

    // TODO: Rename. THis is only used for cosntructing error tokens, so
    // the span length should be zero, to mark the first byte where the error
    // occurrs.
    // Names: error_at? note that the structure of arguments passed to this function
    //        is very similar, perhaps it could take a (error: TokenEnum, prev: Token)
    //        and do the ByteSpan/Source massaging automatically.
    pub(crate) fn new2(tok: TokenEnum, mut span: ByteSpan, src: Source) -> Self {
        // Note: Same as BufferAndLength(Buffer Buf), which inits the Len to 0

        fn is_len_zero(tok: TokenEnum) -> bool {
            use crate::token_enum::TokenEnum::*;

            match tok {
                TOKEN_FAKE_IMPLICITONE
                | TOKEN_FAKE_IMPLICITALL
                | TOKEN_FAKE_IMPLICITTIMES
                | TOKEN_FAKE_IMPLICITNULL => true,
                TOKEN_ERROR_INFIXIMPLICITNULL => true,
                TOKEN_ERROR_EXPECTEDOPERAND => true,
                TOKEN_ERROR_PREFIXIMPLICITNULL => true,
                _ => false,
            }
        }

        if is_len_zero(tok) {
            span.len = 0;
        }

        Token { src, span, tok }
    }

    /// Used in testing code.
    #[cfg(test)]
    pub(crate) fn new3(tok: TokenEnum, span: ByteSpan, src: Source) -> Self {
        Token { tok, span, src }
    }

    fn end(&self) -> usize {
        return self.span.offset + self.span.len;
    }

    pub(crate) fn reset(&self, session: &mut Tokenizer) {
        //
        //
        // Just need to reset the global buffer to the buffer of the token
        //

        session.offset = self.span.offset;
        session.SrcLoc = self.src.start;
    }

    pub(crate) fn skip(&self, session: &mut Tokenizer) {
        session.offset = self.end();
        session.wasEOF = self.tok == TokenEnum::TOKEN_ENDOFFILE;
        session.SrcLoc = self.src.end;
    }

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