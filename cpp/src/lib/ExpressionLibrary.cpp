
#include "ExpressionLibrary.h"

#if USE_MATHLINK

DLLEXPORT int ExprTest_LibraryLink(WolframLibraryData libData, mint Argc, MArgument *Args, MArgument Res) {

  auto head = Expr_LookupSymbol("foo");

  auto arg1 = Expr_FromInteger(37);
  auto arg2 = Expr_FromInteger(45);
  auto foo = Expr_BuildExpression(head, 2);
  Expr_Insert(foo, 1, arg1);
  Expr_Insert(foo, 2, arg2);


  auto head2 = Expr_LookupSymbol("bar");

  auto arg11 = Expr_FromInteger(56);
  auto arg21 = Expr_FromInteger(67);
  auto bar = Expr_BuildExpression(head2, 2);
  Expr_Insert(bar, 1, arg11);
  Expr_Insert(bar, 2, arg21);


  unsigned char arr1[] = {'H', 'e', 'l', 'l', 'o', '!'};
  auto str1 = Expr_FromUTF8String(arr1, 6);

  // UTF-8 for \[Alpha]
  unsigned char arr2[] = {0xce, 0xb1};
  auto str2 = Expr_FromUTF8String(arr2, 2);


  auto head3 = Expr_LookupSymbol("baz");
  auto baz = Expr_BuildExpression(head3, 4);
  Expr_Insert(baz, 1, foo);
  Expr_Release(foo);

  Expr_Insert(baz, 2, bar);
  Expr_Release(bar);

  Expr_Insert(baz, 3, str1);
  Expr_Release(str1);

  Expr_Insert(baz, 4, str2);
  Expr_Release(str2);
  
  // auto I0 = (mint)ef;
  
  auto p = Expr_Pointer(baz);
  MArgument_setInteger(Res, p);
  
  return LIBRARY_NO_ERROR;
}

#endif // USE_MATHLINK
