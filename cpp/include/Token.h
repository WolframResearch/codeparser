
#pragma once

#include "Source.h"
#include "TokenEnum.h"

#include <string>

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
    
    Token(TokenEnum, std::string&& , Source&& );
    
    Token(const Token& o);
    
    Token(Token&& o);
    
    Token& operator=(const Token& o);
    
    Token& operator=(Token&& o);
    
    bool isTrivia() const;
    
    TokenEnum Tok() const {
        return static_cast<TokenEnum>(T);
    }
};

bool operator==(Token a, Token b);
