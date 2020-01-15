
#include "ExpressionLibrary.h"

#if USE_MATHLINK

DLLEXPORT int ExprTest_LibraryLink(WolframLibraryData libData, mint Argc, MArgument *Args, MArgument Res) {

  auto head = Expr_LookupSymbol("foo");

  auto arg1 = Expr_FromInteger(37);
  auto arg2 = Expr_FromInteger(45);
  auto ef = Expr_BuildExpression(head, 2);
  Expr_Insert(ef, 1, arg1);
  Expr_Insert(ef, 2, arg2);


  auto head2 = Expr_LookupSymbol("bar");

  auto arg11 = Expr_FromInteger(56);
  auto arg21 = Expr_FromInteger(67);
  auto ef1 = Expr_BuildExpression(head2, 2);
  Expr_Insert(ef1, 1, arg11);
  Expr_Insert(ef1, 2, arg21);


  auto head3 = Expr_LookupSymbol("baz");
  auto ef2 = Expr_BuildExpression(head3, 2);
  Expr_Insert(ef2, 1, ef);
  Expr_Release(ef);

  Expr_Insert(ef2, 2, ef1);
  Expr_Release(ef1);

  // auto I0 = (mint)ef;
  
  auto p = Expr_Pointer(ef2);
  MArgument_setInteger(Res, p);
  
  return LIBRARY_NO_ERROR;
}

#endif // USE_MATHLINK
