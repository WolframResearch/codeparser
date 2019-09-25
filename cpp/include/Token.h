
#pragma once

#include "Source.h"
#include "TokenEnum.h"

#include <string>

struct Token {
    
    TokenEnum Tok;
    std::string Str;
    Source Src;
    
    Token(TokenEnum, std::string, Source);
};

bool operator==(Token a, Token b);
