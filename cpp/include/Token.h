
#pragma once

#include "Source.h" // for Source
#include "TokenEnum.h" // for TokenEnum

#if USE_MATHLINK
#include "mathlink.h"
#undef P
#endif // USE_MATHLINK

#include <ostream>
#include <variant>

#if USE_EXPR_LIB
using expr = void *;
#endif // USE_EXPR_LIB

class Node;
using NodePtr = const Node *;
using NodeVariant = std::variant<NodePtr, struct Token>;

//
//
//
struct Token {
    
    Source Src;
    Buffer Buf;
    uint64_t Len : 48;
    TokenEnum Tok;
    
    Token();
    //
    // Used during abstracting work
    //
    // Usually there is no Source
    //
    explicit Token(TokenEnum Tok, Buffer Buf, size_t Len);
    //
    // Used when parsing
    //
    explicit Token(TokenEnum Tok, Buffer Buf, size_t Len, Source Src);
    
    Buffer end() const;
    
    void reset(ParserSessionPtr session);
    void skip(ParserSessionPtr session);
    
    void print(std::ostream& s) const;
    
    bool check() const;
    
#if USE_MATHLINK
    void put(ParserSessionPtr session, MLINK callLink) const;
#endif // USE_MATHLINK
    
#if USE_EXPR_LIB
    expr toExpr(ParserSessionPtr session) const;
#endif // USE_EXPR_LIB
};

//
// Sizes of structs with bit-fields are implementation-dependent
//
#ifdef __clang__
static_assert((SIZEOF_VOID_P == 8 && sizeof(Token) == 32) || (SIZEOF_VOID_P == 4), "Check your assumptions");
#endif // #ifdef __clang__

bool operator==(Token a, Token b);

#if BUILD_TESTS
//
// For googletest
//
void PrintTo(const Token& T, std::ostream *stream);
#endif // BUILD_TESTS
