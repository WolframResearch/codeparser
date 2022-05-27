
#pragma once

#include "WolframLibrary.h" // for mint
#undef True
#undef False

#include <cstdint> // for int64_t

using expr = void *;
using Buffer = const unsigned char *;


EXTERN_C expr Expr_FromInteger64(int64_t val);

EXTERN_C expr Expr_FromReal64(double val);

EXTERN_C expr Expr_UTF8BytesToStringExpr(Buffer buf, mint size);

EXTERN_C expr Expr_MEncodedStringToSymbolExpr(const char *str);

//
// The suffix A means automatically handle releasing reference to head
//
EXTERN_C expr Expr_BuildExprA(expr head, mint argCount);

//
// The suffix A means automatically handle releasing reference to arg
//
// index is base 1
//
EXTERN_C void Expr_InsertA(expr e, mint index, expr arg);

EXTERN_C void Expr_Release(expr e);

EXTERN_C void Expr_StringExprToUTF8Bytes(expr e, Buffer *buffer, mint *len);

EXTERN_C expr Expr_LongNameSuggestion(expr input);
