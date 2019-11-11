
#pragma once

#include "Source.h"
#include "TokenEnum.h"

#include <string>
#include <ostream>

//
// Version 1 of Token encoding
//
// 16 bits:
//
// fedcba9876543210
//        ^~~~~~~~~
//        Enum bits (9 bits)
// ^~~~~~~
// Currently unused (7 bits)
//
struct Token {
    
    uint16_t T : 9;
    
    std::string Str;
    Source Src;
    
    Token(TokenEnum, std::string&&, Source&& );
    
    bool isTrivia() const;
    
    TokenEnum Tok() const {
        return static_cast<TokenEnum>(T);
    }
    
    void put(MLINK ) const;
    
    void print(std::ostream&) const;
};

bool operator==(Token a, Token b);
