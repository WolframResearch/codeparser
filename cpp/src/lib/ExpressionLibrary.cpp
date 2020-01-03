
#include "ExpressionLibrary.h"

#if USE_MATHLINK

DLLEXPORT int ExprTest_LibraryLink(WolframLibraryData libData, mint Argc, MArgument *Args, MArgument Res) {

  auto head = Expr_LookupSymbol("foo");
  // auto head = Expr_FromInteger(444);

  auto arg1 = Expr_FromInteger(37);
  auto arg2 = Expr_FromInteger(45);
  auto ef = Expr_BuildExpression(head, 2);
  Expr_Insert(ef, 1, arg1);
  Expr_Insert(ef, 2, arg2);

  auto I0 = (mint)ef;
  MArgument_setInteger(Res, I0);
  
  return LIBRARY_NO_ERROR;
}

#endif // USE_MATHLINK
