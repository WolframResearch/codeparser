
#pragma once

#include "Token.h"

#if USE_MATHLINK
#include "mathlink.h"
#undef P
#endif // USE_MATHLINK

#include <ostream>
#include <cstddef> // for size_t

class ParserSession;

using ParserSessionPtr = ParserSession *;

#if USE_EXPR_LIB
using expr = void *;
#endif // USE_EXPR_LIB


//
// A kernel symbol
//
struct Symbol {

    const char *Name;
    size_t Len;
    const int Id;

    constexpr Symbol(const char *Name, size_t Len, int Id) : Name(Name), Len(Len), Id(Id) {}
    
    Token token() const;
    
    void print(std::ostream& s) const;

#if USE_MATHLINK
    void put(ParserSessionPtr session, MLINK callLink) const;
#endif // USE_MATHLINK

#if USE_EXPR_LIB
    expr toExpr(ParserSessionPtr session) const;
#endif // USE_EXPR_LIB
};

bool operator==(Symbol a, Symbol b);

bool operator!=(Symbol a, Symbol b);
