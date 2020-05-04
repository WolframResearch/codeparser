
#pragma once

#if USE_MATHLINK
#include "mathlink.h"
#undef P
#endif // USE_MATHLINK

#include "WolframLibrary.h"
#undef True
#undef False

using expr = void *;


EXTERN_C mint Expr_Length(expr);

EXTERN_C int64_t Expr_ToInteger64(expr);

EXTERN_C expr Expr_FromInteger64(int64_t);

EXTERN_C expr Expr_FromReal64(double);

EXTERN_C expr Expr_MEncodedStringToSymbolExpr(const unsigned char *);

EXTERN_C expr Expr_BuildExpr(expr, mint);

EXTERN_C void Expr_Insert(expr, mint, expr);

EXTERN_C mint Expr_Pointer(expr);

EXTERN_C expr Expr_FromPointer(void *);

EXTERN_C void Expr_Release(expr);

EXTERN_C expr Expr_UTF8BytesToStringExpr(const unsigned char*, mint);

//
// Will create either a machine integer or a big integer
//
EXTERN_C expr Expr_CStringToIntegerExpr(const char*, mint, mbool);



#if USE_MATHLINK

EXTERN_C DLLEXPORT int ExprTest_LibraryLink(WolframLibraryData libData, mint Argc, MArgument *Args, MArgument Res);

EXTERN_C DLLEXPORT int Get_LibraryLink(WolframLibraryData libData, mint Argc, MArgument *Args, MArgument Res);

#endif // USE_MATHLINK
