
#pragma once

#include "Source.h" // for BufferAndLength, Source
#include "TokenEnum.h" // for TokenEnum

#include <ostream>

struct Token {
    
    BufferAndLength BufLen;
    Source Src;
    TokenEnum Tok;
    
    Token(TokenEnum Tok, BufferAndLength BufLen, Source Src);
    
#if USE_MATHLINK
    void put(MLINK ) const;
#endif // USE_MATHLINK
    
    void print(std::ostream&) const;
};

static_assert(sizeof(Token) == 48, "Check your assumptions");

bool operator==(Token a, Token b);

//
// For googletest
//
void PrintTo(const Token&, std::ostream*);
