
#include "Token.h"

#include "SymbolRegistration.h"
#include "ByteBuffer.h"
#include "ByteDecoder.h"
#include "ParserSession.h"
#include "TokenEnumRegistration.h"

#if USE_MATHLINK
#include "mathlink.h"
#undef P
#endif // USE_MATHLINK

#if USE_EXPR_LIB
#include "ExprLibrary.h"
#endif // USE_EXPR_LIB

#include <cassert>


Token::Token() : Src(), Buf(), Len(), Tok() {}

Token::Token(TokenEnum Tok, BufferAndLength BufLen, Source Src) : Src(Src), Buf(BufLen.Buf), Len(BufLen.length()), Tok(Tok) {

#ifndef NDEBUG
    
    //
    // verify BufLen and Src are equivalent
    //
    
    switch (Tok.value()) {
        case TOKEN_UNKNOWN.value(): {
            
            assert(false);
            
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
                       (BufLen.Buf[0] == '\\' && SourceCharacter(BufLen.Buf[1]).isNewline()));
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
                    
                } else if (Src.Start.first == Src.End.first) {
                    
                    if (Src.size() != BufLen.length()) {
                        //
                        // If the sizes do not match, then check if there are multi-byte characters
                        // If there are multi-bytes characters, then it is too complicated to compare sizes
                        //
                        // Note that this also catches changes in character representation, e.g.,
                        // If a character was in source with \XXX octal notation but was stringified with \:XXXX hex notation
                        //
                        assert(!BufLen.containsOnlyASCII() || BufLen.containsTab());
                    }
                }
            }
            
            break;
        }
    }
#endif // NDEBUG
}

BufferAndLength Token::bufLen() const {
    return BufferAndLength(Buf, Len);
}

Buffer Token::end() const {
    return Buf + Len;
}

bool operator==(Token a, Token b) {
    return a.Tok == b.Tok && a.Buf == b.Buf && a.Len == b.Len && a.Src == b.Src;
}

void Token::reset(ParserSessionPtr session) {
    
    //
    //
    // Just need to reset the global buffer to the buffer of the token
    //
    
    session->buffer = Buf;
    session->SrcLoc = Src.Start;
}

void Token::skip(ParserSessionPtr session) {
    
    session->buffer = end();
    session->SrcLoc = Src.End;
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
    
    s << Sym.Name;
    s << ", ";
    
    bufLen().print(s);
    s << ", ";
    
    Src.print(s);
    s << "]";
}

#if BUILD_TESTS
//
// For googletest
//
void PrintTo(const Token& T, std::ostream *s) {
    T.print(*s);
}
#endif // BUILD_TESTS


#if USE_MATHLINK
void Token::put(ParserSessionPtr session, MLINK callLink) const {
    
    if (Tok.isError()) {
        
        if (Tok.isUnterminated()) {
            
            if (!MLPutFunction(callLink, SYMBOL_CODEPARSER_UNTERMINATEDTOKENERRORNEEDSREPARSENODE.Name, 3)) {
                assert(false);
            }
            
        } else {
            
            if (!MLPutFunction(callLink, SYMBOL_CODEPARSER_ERRORNODE.Name, 3)) {
                assert(false);
            }
        }
        
    } else {
        
        //
        // These are Symbols, Strings, Integers, Reals, Rationals.
        //
        
        if (!MLPutFunction(callLink, SYMBOL_CODEPARSER_LEAFNODE.Name, 3)) {
            assert(false);
        }
    }
    
    auto Sym = TokenToSymbol(Tok);

    Sym.put(session, callLink);

    bufLen().put(session, callLink);
    
    if (!MLPutFunction(callLink, SYMBOL_ASSOCIATION.Name, 1)) {
        assert(false);
    }
    
    Src.put(session, callLink);
}
#endif // USE_MATHLINK

#if USE_EXPR_LIB
expr Token::toExpr(ParserSessionPtr session) const {
    
    expr head;
    
    if (Tok.isError()) {
        
        if (Tok.isUnterminated()) {
            
            head = SYMBOL_CODEPARSER_UNTERMINATEDTOKENERRORNEEDSREPARSENODE.toExpr(session);
            
        } else {
            
            head = SYMBOL_CODEPARSER_ERRORNODE.toExpr(session);
        }
        
    } else {
        
        //
        // These are Symbols, Strings, Integers, Reals, Rationals.
        //
        
        head = SYMBOL_CODEPARSER_LEAFNODE.toExpr(session);
    }
    
    auto e = Expr_BuildExprA(head, 3);
    
    auto Sym = TokenToSymbol(Tok);
    
    auto SymExpr = Sym.toExpr(session);
    Expr_InsertA(e, 0 + 1, SymExpr);
    
    auto TokBufLenExpr = bufLen().toExpr(session);
    Expr_InsertA(e, 1 + 1, TokBufLenExpr);
    
    {
        auto head = SYMBOL_ASSOCIATION.toExpr(session);
        
        auto DataExpr = Expr_BuildExprA(head, 1);
        
        auto SrcExpr = Src.toExpr(session);
        Expr_InsertA(DataExpr, 0 + 1, SrcExpr);
        
        Expr_InsertA(e, 2 + 1, DataExpr);
    }
    
    return e;
}
#endif // USE_EXPR_LIB
