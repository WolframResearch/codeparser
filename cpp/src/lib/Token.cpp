
#include "Token.h"

#include "Symbol.h"
#include "ByteBuffer.h"
#include "ByteDecoder.h"

#if USE_EXPR_LIB
#include "ExprLibrary.h"
#endif // USE_EXPR_LIB

#include <cassert>

bool containsOnlyASCII(BufferAndLength BufLen);
bool containsTab(BufferAndLength BufLen);

Token::Token() : BufLen(), Src(), Tok() {}

Token::Token(TokenEnum Tok, BufferAndLength BufLen, Source Src) : BufLen(BufLen), Src(Src), Tok(Tok) {

#ifndef NDEBUG
    
    //
    // verify BufLen and Src are equivalent
    //
    
    switch (Tok.value()) {
        case TOKEN_UNKNOWN.value(): {
            break;
        }
            //
            // Both \n and \r\n newlines have a size of 1
            // And other newlines like \[IndentingNewLine] have size > 1
            //
        case TOKEN_TOPLEVELNEWLINE.value():
        case TOKEN_INTERNALNEWLINE.value(): {
            break;
        }
        default: {
            
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
    }
#endif // NDEBUG
    
}

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

bool operator==(Token a, Token b) {
    return a.Tok == b.Tok && a.BufLen == b.BufLen && a.Src == b.Src;
}

bool operator!=(Token a, Token b) {
    return a.Tok != b.Tok || a.BufLen != b.BufLen || a.Src != b.Src;
}

void Token::reset() {
    
    //
    //
    // Just need to reset the global buffer to the buffer of the token
    //
    
    TheByteBuffer->buffer = BufLen.buffer;
    
    TheByteDecoder->SrcLoc = Src.Start;
}

bool Token::check() const {
    return !Tok.isError();
}

void Token::print(std::ostream& s) const {
    
    auto Sym = TokenToSymbol(Tok);
    
    //
    // printing the token  123  as LeafNode[Integer, "123", <||>] seems the wrong way around, but it is convenient
    //
    
    if (Tok.isError()) {
        
        if (Tok.isUnterminated()) {
            
            SYMBOL_CODEPARSER_UNTERMINATEDTOKENERRORNEEDSREPARSENODE.print(s);
            
        } else {
            
            SYMBOL_CODEPARSER_ERRORNODE.print(s);
        }
        
    } else {
        
        SYMBOL_CODEPARSER_LEAFNODE.print(s);
    }
    
    s << "[";
    
    s << Sym.name();
    s << ", ";
    
    BufLen.print(s);
    s << ", ";
    
    Src.print(s);
    s << "]";
}

//
// For googletest
//
void PrintTo(const Token& T, std::ostream *s) {
    T.print(*s);
}

#if USE_MATHLINK
void Token::put(MLINK mlp) const {
    
    if (Tok.isError()) {
        
        if (Tok.isUnterminated()) {
            
            if (!MLPutFunction(mlp, SYMBOL_CODEPARSER_UNTERMINATEDTOKENERRORNEEDSREPARSENODE.name(), 3)) {
                assert(false);
            }
            
        } else {
            
            if (!MLPutFunction(mlp, SYMBOL_CODEPARSER_ERRORNODE.name(), 3)) {
                assert(false);
            }
        }
        
    } else {
        
        //
        // These are Symbols, Strings, Integers, Reals, Rationals.
        //
        
        if (!MLPutFunction(mlp, SYMBOL_CODEPARSER_LEAFNODE.name(), 3)) {
            assert(false);
        }
    }
    
    auto Sym = TokenToSymbol(Tok);

    Sym.put(mlp);

    BufLen.put(mlp);
    
    if (!MLPutFunction(mlp, SYMBOL_ASSOCIATION.name(), 1)) {
        assert(false);
    }
    
    Src.put(mlp);
}
#endif // USE_MATHLINK

#if USE_EXPR_LIB
expr Token::toExpr() const {
    
    expr head;
    
    if (Tok.isError()) {
        
        if (Tok.isUnterminated()) {
            
            head = SYMBOL_CODEPARSER_UNTERMINATEDTOKENERRORNEEDSREPARSENODE.toExpr();
            
        } else {
            
            head = SYMBOL_CODEPARSER_ERRORNODE.toExpr();
        }
        
    } else {
        
        //
        // These are Symbols, Strings, Integers, Reals, Rationals.
        //
        
        head = SYMBOL_CODEPARSER_LEAFNODE.toExpr();
    }
    
    auto e = Expr_BuildExprA(head, 3);
    
    auto Sym = TokenToSymbol(Tok);
    
    auto SymExpr = Sym.toExpr();
    Expr_InsertA(e, 0 + 1, SymExpr);
    
    auto TokBufLenExpr = BufLen.toExpr();
    Expr_InsertA(e, 1 + 1, TokBufLenExpr);
    
    {
        auto head = SYMBOL_ASSOCIATION.toExpr();
        
        auto DataExpr = Expr_BuildExprA(head, 1);
        
        auto SrcExpr = Src.toExpr();
        Expr_InsertA(DataExpr, 0 + 1, SrcExpr);
        
        Expr_InsertA(e, 2 + 1, DataExpr);
    }
    
    return e;
}
#endif // USE_EXPR_LIB
