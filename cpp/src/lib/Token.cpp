
#include "Token.h"

#include <cassert>

bool containsOnlyASCII(std::string s);

Token::Token(TokenEnum Tok, std::string&& StrIn, Source&& SrcIn) : T(Tok), Str(std::move(StrIn)), Src(std::move(SrcIn)) {
    
    switch (Tok.value()) {
        case TOKEN_UNKNOWN.value():
            break;
        //
        // These are the tokens that do not quite have correct spans.
        // start and end are set to the same character, so size is 1
        // But they actually take up 0 characters
        //
        case TOKEN_ENDOFFILE.value():
        case TOKEN_FAKE_IMPLICITTIMES.value():
        case TOKEN_ERROR_EMPTYSTRING.value():
        case TOKEN_ERROR_ABORTED.value():
        case TOKEN_FAKE_IMPLICITNULL.value():
        case TOKEN_FAKE_IMPLICITONE.value():
        case TOKEN_FAKE_IMPLICITALL.value():
        case TOKEN_ERROR_EXPECTEDOPERAND.value():
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
        //
        // Both \n and \r\n newlines have a size of 1
        // And other newlines like \[IndentingNewLine] have size > 1
        //
        case TOKEN_NEWLINE.value():
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
//                            assert(!containsOnlyASCII(Str));
                        }
                    }
                    break;
                case SOURCESTYLE_OFFSETLEN:
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
    assert(a.Src.style == SOURCESTYLE_LINECOL);
    return a.Src.lineCol == b.Src.lineCol;
}

#if USE_MATHLINK
void Token::put(MLINK mlp) const {
    
    MLPutFunction(mlp, SYMBOL_AST_LIBRARY_MAKELEAFNODE->name(), static_cast<int>(2 + Src.count()));
    
    MLPutSymbol(mlp, TokenToSymbol(static_cast<TokenEnum>(T))->name());
    
    MLPutUTF8String(mlp, reinterpret_cast<unsigned const char *>(Str.c_str()), static_cast<int>(Str.size()));
    
    Src.put(mlp);
}
#endif

void Token::print(std::ostream& s) const {
    
    s << SYMBOL_AST_LIBRARY_MAKELEAFNODE->name() << "[";
    
    s << TokenToSymbol(static_cast<TokenEnum>(T))->name();
    s << ", ";
    
    s << Str;
    s << ", ";
    
    Src.print(s);
    
    s << "]";
}
