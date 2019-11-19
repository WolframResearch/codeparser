
#pragma once

#include "Source.h"
#include "TokenEnum.h"

#include <string>
#include <ostream>

//
// Version 1 of Token encoding
//
// There are currently ~422 tokens, so 9 bits are required to enumerate them
//
// 16 bits:
//
// fedcba9876543210
//        ^~~~~~~~~
//        Enum bits (9 bits)
//       ^
//       Possible bit
//      ^
//      Closer bit
//     ^
//     Error bit
//    ^
//    Trivia bit
//   ^
//   Infix bit
//  ^
//  Inequality bit
// ^
// VectorInequality bit
//
struct Token {
    
    TokenEnum T;
    std::string Str;
    Source Src;
    
    Token(TokenEnum, std::string&&, Source&& );
    
    TokenEnum getTokenEnum() const {
        return T;
    }
    
#if USE_MATHLINK
    void put(MLINK ) const;
#endif
    
    void print(std::ostream&) const;
};

bool operator==(Token a, Token b);
