
#pragma once

#include <ostream>

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
    const int Id;

    constexpr Symbol(const char *Name, int Id) : Name(Name), Id(Id) {}

    void print(std::ostream& s) const;

#if USE_MATHLINK
    void put(ParserSessionPtr session) const;
#endif // USE_MATHLINK

#if USE_EXPR_LIB
    expr toExpr(ParserSessionPtr session) const;
#endif // USE_EXPR_LIB
};

bool operator==(Symbol a, Symbol b);

bool operator!=(Symbol a, Symbol b);
