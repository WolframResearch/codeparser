
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
    
    if (!MLPutFunction(mlp, SYMBOL_AST_LIBRARY_MAKELEAFNODE->name(), static_cast<int>(2 + 4))) {
        assert(false);
    }
    
    auto& Sym = TokenToSymbol(tok);
    
    if (!MLPutSymbol(mlp, Sym->name())) {
        assert(false);
    }
    
    bufferAndLength.put(mlp);
    
    getSource().put(mlp);
}
#endif // USE_MATHLINK

void Token::print(std::ostream& s) const {
    
    auto& Sym = TokenToSymbol(tok);
    
    s << SYMBOL_AST_LIBRARY_MAKELEAFNODE->name() << "[";
    
    s << Sym->name();
    s << ", ";
    
    if (!tok.isEmpty()) {
        
        bufferAndLength.write(s);
    }
    
    s << ", ";
    
    getSource().print(s);
    
    s << "]";
}

//
// For googletest
//
void PrintTo(const Token& T, std::ostream* stream) {
    T.print(*stream);
}

