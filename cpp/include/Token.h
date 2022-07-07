
#pragma once

#include "Source.h" // for BufferAndLength, Source
#include "TokenEnum.h" // for TokenEnum

#include <ostream>

#if USE_EXPR_LIB
using expr = void *;
#endif // USE_EXPR_LIB


//
//
//
struct Token {
    
    Source Src;
    Buffer Buf;
    uint64_t Len : 48;
    TokenEnum Tok;
    
    Token();
    Token(TokenEnum Tok, BufferAndLength BufLen, Source Src);
    
    BufferAndLength bufLen() const;
    
    Buffer end() const;
    
    void reset(ParserSessionPtr session);
    
    void print(std::ostream& s) const;
    
    bool check() const;
    
#if USE_MATHLINK
    void put(ParserSessionPtr session) const;
#endif // USE_MATHLINK
    
#if USE_EXPR_LIB
    expr toExpr(ParserSessionPtr session) const;
#endif // USE_EXPR_LIB
};

static_assert((SIZEOF_VOID_P == 8 && sizeof(Token) == 32) || (SIZEOF_VOID_P == 4), "Check your assumptions");

bool operator==(Token a, Token b);
bool operator!=(Token a, Token b);

//
// For googletest
//
void PrintTo(const Token& T, std::ostream *stream);
