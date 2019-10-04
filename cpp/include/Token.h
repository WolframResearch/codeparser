
#pragma once

#include "Source.h"
#include "TokenEnum.h"

#include <string>

struct Token {
    
    TokenEnum Tok;
    std::string Str;
    Source Src;
    
    Token(TokenEnum, std::string&& , Source&& );
    
    Token(const Token& o);
    
    Token(Token&& o);
    
    Token& operator=(const Token& o);
    
    Token& operator=(Token&& o);
    
    bool isTrivia() const;
};

bool operator==(Token a, Token b);
