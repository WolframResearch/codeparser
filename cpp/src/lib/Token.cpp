
#include "Token.h"

#include "API.h" // for TheParserSession

#include <cassert>

#ifndef NDEBUG
bool containsOnlyASCII(BufferAndLength BufLen);
bool containsTab(BufferAndLength BufLen);
#endif // NDEBUG

Token::Token() : BufLen(), Src(), Tok() {}

Token::Token(TokenEnum Tok, BufferAndLength BufLen, Source Src) : BufLen(BufLen), Src(Src), Tok(Tok) {

#ifndef NDEBUG
    
    //
    // verify BufLen and Src are equivalent
    //
    
    switch (Tok.value()) {
        case TOKEN_UNKNOWN.value():
            break;
            //
            // Both \n and \r\n newlines have a size of 1
            // And other newlines like \[IndentingNewLine] have size > 1
            //
        case TOKEN_TOPLEVELNEWLINE.value():
        case TOKEN_INTERNALNEWLINE.value():
            break;
        default:
            
            if (Tok.isEmpty()) {
                assert((BufLen.length() == 0) ||
                       //
                       // There could be a line continuation in front.
                       // Token is still empty.
                       //
                       (BufLen.buffer[0] == '\\' && SourceCharacter(BufLen.buffer[1]).isNewline()));
            } else {
                assert(BufLen.length() > 0);
                //
                // This is all just to do an assert.
                // But it's a good assert because it catches problems.
                //
                // Only bother checking if the token is all on one line
                // Spanning multiple lines is too complicated to care about
                //
                if (Src.Start.first == 0 && Src.End.first == 0) {
                
                    //
                    // SourceConvention of "SourceCharacterIndex"
                    // so nothing to do
                    //
                    ;
                    
                } else {
                    if (Src.Start.first == Src.End.first) {
                        if (Src.size() != BufLen.length()) {
                            //
                            // If the sizes do not match, then check if there are multi-byte characters
                            // If there are multi-bytes characters, then it is too complicated to compare sizes
                            //
                            // Note that this also catches changes in character representation, e.g.,
                            // If a character was in source with \XXX octal notation but was stringified with \:XXXX hex notation
                            //
                            assert(!containsOnlyASCII(BufLen) || containsTab(BufLen));
                        }
                    }
                }
            }
            break;
    }
#endif // NDEBUG
    
}

#ifndef NDEBUG
bool containsOnlyASCII(BufferAndLength BufLen) {
    for (auto p = BufLen.buffer; p < BufLen.end; p++) {
        auto c = *p;
        //
        // Take care to cast to int before comparing
        //
        if ((static_cast<int>(c) & 0xff) >= 0x80) {
            return false;
        }
    }
    return true;
}

bool containsTab(BufferAndLength BufLen) {
    for (auto p = BufLen.buffer; p < BufLen.end; p++) {
        auto c = *p;
        //
        // Take care to cast to int before comparing
        //
        if ((static_cast<int>(c) & 0xff) == 0x09) {
            return true;
        }
    }
    return false;
}
#endif // NDEBUG

bool operator==(Token a, Token b) {
    return a.Tok == b.Tok && a.BufLen == b.BufLen && a.Src == b.Src;
}

bool operator!=(Token a, Token b) {
    return a.Tok != b.Tok || a.BufLen != b.BufLen || a.Src != b.Src;
}

void Token::print(std::ostream& s) const {
    
    auto& Sym = TokenToSymbol(Tok);
    
    s << SYMBOL_CODEPARSER_LIBRARY_MAKELEAFNODE->name() << "[";
    
    s << Sym->name();
    s << ", ";
    
    if (!Tok.isEmpty()) {
        
        BufLen.print(s);
    }
    
    s << ", ";
        
    Src.print(s);
    
    s << "]";
}

//
// For googletest
//
void PrintTo(const Token& T, std::ostream *stream) {
    T.print(*stream);
}

