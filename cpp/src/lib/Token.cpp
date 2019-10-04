
#include "Token.h"

#include <cassert>

bool containsOnlyASCII(std::string s);

Token::Token(TokenEnum Tok, std::string&& StrIn, Source&& SrcIn) : Tok(Tok), Str(std::move(StrIn)), Src(std::move(SrcIn)) {
    
    switch (Tok) {
        case TOKEN_UNKNOWN:
            break;
        //
        // These are the tokens that do not quite have correct spans.
        // start and end are set to the same character, so size is 1
        // But they actually take up 0 characters
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
                            // If a character was in source with \XXX octal notation but was stringified with \:XXXX hex notation
                            //
                            assert(!containsOnlyASCII(Str));
                        }
                    }
                    break;
                case SOURCESTYLE_OFFSETLEN:
                    break;
            }
            break;
    }
    
}

Token::Token(const Token& o) : Tok(o.Tok), Str(o.Str), Src(o.Src) {}

Token::Token(Token&& o) : Tok(o.Tok), Str(std::move(o.Str)), Src(std::move(o.Src)) {}

Token& Token::operator=(const Token& o) {
    Tok = o.Tok;
    Str = o.Str;
    Src = o.Src;
    return *this;
}

Token& Token::operator=(Token&& o) {
    Tok = o.Tok;
    Str = std::move(o.Str);
    Src = std::move(o.Src);
    return *this;
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

bool Token::isTrivia() const {
    switch (Tok) {
        case TOKEN_WHITESPACE:
        case TOKEN_NEWLINE:
        case TOKEN_COMMENT:
        case TOKEN_LINECONTINUATION:
            return true;
        default:
            return false;
            
    }
}

bool operator==(Token a, Token b) {
    assert(a.Src.style == SOURCESTYLE_LINECOL);
    return a.Src.lineCol == b.Src.lineCol;
}
