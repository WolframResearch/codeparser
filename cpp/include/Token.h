
#pragma once

#include "ByteDecoder.h"
#include "API.h"
#include "Source.h"
#include "TokenEnum.h"

#include <string>
#include <ostream>

struct Token {
    
    TokenEnum tok;
    BufferAndLength bufferAndLength;
    
    Token(TokenEnum, BufferAndLength);
    
    TokenEnum getTokenEnum() const {
        return tok;
    }
    
    Source getSource() const;
    
#if USE_MATHLINK
    void put(MLINK ) const;
#endif // USE_MATHLINK
    
    void print(std::ostream&) const;
};

bool operator==(Token a, Token b);

//
// For googletest
//
void PrintTo(const Token&, std::ostream*);
