
#pragma once

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
//
//
struct MyString {

    const char *Val;
    const size_t Len;
    const int Id;

    constexpr MyString(const char *Val, size_t Len, int Id) : Val(Val), Len(Len), Id(Id) {}

    void print(ParserSessionPtr session, std::ostream& s) const;

#if USE_MATHLINK
    void put(ParserSessionPtr session, MLINK callLink) const;
#endif // USE_MATHLINK

#if USE_EXPR_LIB
    expr toExpr(ParserSessionPtr session) const;
#endif // USE_EXPR_LIB
};

bool operator==(MyString a, MyString b);

bool operator!=(MyString a, MyString b);

bool operator<(MyString a, MyString b);
