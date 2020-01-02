
#pragma once

#include "Source.h" // for BufferAndLength, Source
#include "TokenEnum.h" // for TokenEnum

//#include <string>
#include <ostream>

struct Token {
    
    TokenEnum Tok;
    BufferAndLength BufLen;
    Source Src;
    
    Token(TokenEnum Tok, BufferAndLength BufLen, Source Src);
    
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
