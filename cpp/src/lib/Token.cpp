
#include "Token.h"

bool containsOnlyASCII(std::string s);

Token::Token(TokenEnum Tok, std::string Str, Source Src) : Tok(Tok), Str(Str), Src(Src) {
    
    switch (Tok) {
        case TOKEN_UNKNOWN:
            break;
        //
        // These are the tokens that do not quite have correct spans.
        // start and end are set to the same character, so size is 1
        // But they take up 0 characters
        //
        case TOKEN_ENDOFFILE:
        case TOKEN_FAKE_IMPLICITTIMES:
        case TOKEN_ERROR_EMPTYSTRING:
        case TOKEN_ERROR_ABORTED:
        case TOKEN_FAKE_IMPLICITNULL:
        case TOKEN_FAKE_IMPLICITONE:
        case TOKEN_FAKE_IMPLICITALL:
        case TOKEN_ERROR_EXPECTEDOPERAND:
            switch (Src.style) {
                case SOURCESTYLE_UNKNOWN:
                    break;
                case SOURCESTYLE_LINECOL:
                    assert(Src.size() == 1);
                    break;
                case SOURCESTYLE_OFFSETLEN:
                    assert(Src.size() == 1);
                    break;
            }
            break;
        default:
            switch (Src.style) {
                case SOURCESTYLE_UNKNOWN:
                    break;
                case SOURCESTYLE_LINECOL:
                    //
                    // This is all just to do an assert.
                    // But it's a good assert because it catches problems.
                    //
                    // Only bother checking if the token is all on one line
                    // Spanning multiple lines is too complicated to care about
                    //
                    if (Src.lineCol.start.Line == Src.lineCol.end.Line) {
                        if (Src.size() != Str.size()) {
                            //
                            // If the sizes do not match, then check if there are multi-byte characters
                            // If there are multi-bytes characters, then it is too complicated to compare sizes
                            //
                            // Note that this also catches changes in character representation, e.g.,
                            // If a character was in source with \XXX notation but was stringified with \:XXXX notation
                            //
                            assert(!containsOnlyASCII(Str));
                        }
                    }
                    break;
                case SOURCESTYLE_OFFSETLEN:
                    if (Src.size() != Str.size()) {
                        assert(!containsOnlyASCII(Str));
                    }
                    break;
            }
            break;
    }
    
}

bool containsOnlyASCII(std::string s) {
    for (auto c : s) {
        //
        // Take care to cast to int before comparing
        //
        if ((static_cast<int>(c) & 0xff) >= 0x80) {
            return false;
        }
    }
    return true;
}

bool operator==(Token a, Token b) {
    return a.Src.lineCol == b.Src.lineCol;
}
