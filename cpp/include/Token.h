
#pragma once

#include "Source.h" // for BufferAndLength, Source
#include "TokenEnum.h" // for TokenEnum

#include <ostream>

//
//
//
struct Token {
    
    BufferAndLength BufLen;
    Source Src;
    TokenEnum Tok;
    
    Token();
    Token(TokenEnum Tok, BufferAndLength BufLen, Source Src);
    
    void print(std::ostream&) const;
};

static_assert((SIZEOF_VOID_P == 8 && sizeof(Token) == 48) || (SIZEOF_VOID_P == 4), "Check your assumptions");

bool operator==(Token a, Token b);
bool operator!=(Token a, Token b);

//
// For googletest
//
void PrintTo(const Token&, std::ostream*);
