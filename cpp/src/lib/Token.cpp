
#include "Token.h"

#include "ByteDecoder.h"

#include <cassert>


Token::Token(TokenEnum tok, BufferAndLength bufAndLen) : tok(tok), bufferAndLength(bufAndLen) {}

bool operator==(Token a, Token b) {
    return a.tok == b.tok && a.bufferAndLength == b.bufferAndLength;
}

Source Token::getSource() const {
    
    if (bufferAndLength.length == 0) {
        auto End = TheByteDecoder->convertBufferToEnd(bufferAndLength.buffer);
        return Source(End);
    }
    
    auto Start = TheByteDecoder->convertBufferToStart(bufferAndLength.buffer);
    auto End = TheByteDecoder->convertBufferToEnd(bufferAndLength.buffer + bufferAndLength.length);
    return Source(Start, End);
}

#if USE_MATHLINK
void Token::put(MLINK mlp) const {
    
    if ((TheParserSession->policy & INCLUDE_SOURCE) == INCLUDE_SOURCE) {
        
        if (!MLPutFunction(mlp, SYMBOL_AST_LIBRARY_MAKELEAFNODE->name(), static_cast<int>(2 + 4))) {
            assert(false);
        }
        
        auto& Sym = TokenToSymbol(tok);
        
        if (!MLPutSymbol(mlp, Sym->name())) {
            assert(false);
        }
        
        bufferAndLength.putUTF8String(mlp);
        
        getSource().put(mlp);
        
        return;
    }
    
    if (!MLPutFunction(mlp, SYMBOL_AST_LIBRARY_MAKELEAFNODE->name(), static_cast<int>(2))) {
        assert(false);
    }
    
    auto& Sym = TokenToSymbol(tok);
    
    if (!MLPutSymbol(mlp, Sym->name())) {
        assert(false);
    }
    
    bufferAndLength.putUTF8String(mlp);
}
#endif // USE_MATHLINK

void Token::print(std::ostream& s) const {
    
    if ((TheParserSession->policy & INCLUDE_SOURCE) == INCLUDE_SOURCE) {
        
        auto& Sym = TokenToSymbol(tok);
        
        s << SYMBOL_AST_LIBRARY_MAKELEAFNODE->name() << "[";
        
        s << Sym->name();
        s << ", ";
        
        if (!tok.isEmpty()) {
            
            bufferAndLength.printUTF8String(s);
        }
        
        s << ", ";
        
        getSource().print(s);
        
        s << "]";
        
        return;
    }
    
    auto& Sym = TokenToSymbol(tok);
    
    s << SYMBOL_AST_LIBRARY_MAKELEAFNODE->name() << "[";
    
    s << Sym->name();
    s << ", ";
    
    if (!tok.isEmpty()) {
        
        bufferAndLength.printUTF8String(s);
    }
    
    s << "]";
}

//
// For googletest
//
void PrintTo(const Token& T, std::ostream* stream) {
    T.print(*stream);
}

