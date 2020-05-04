
#include "ExprLibrary.h"

#include <map>

std::map<expr, int> metadata;

#if USE_MATHLINK

DLLEXPORT int ExprTest_LibraryLink(WolframLibraryData libData, mint Argc, MArgument *Args, MArgument Res) {

  const unsigned char arr4[] = {'f', 'o', 'o', 0x00};
  auto foo = Expr_MEncodedStringToSymbolExpr(arr4);
  auto e1 = Expr_BuildExpr(foo, 7);
  metadata[e1] = 137;

  const unsigned char arr5[] = {'b', 'a', 'r', 0x00};
  auto bar = Expr_MEncodedStringToSymbolExpr(arr5);
  auto e2 = Expr_BuildExpr(bar, 2);
  metadata[e2] = 142;
  auto mint1 = Expr_FromInteger64(37);
  metadata[mint1] = 153;
  Expr_Insert(e2, 0 + 1, mint1);
  Expr_Release(mint1);
  auto mint2 = Expr_FromInteger64(45);
  metadata[mint2] = 161;
  Expr_Insert(e2, 1 + 1, mint2);
  Expr_Release(mint2);
  Expr_Insert(e1, 0 + 1, e2);
  Expr_Release(e2);

  const unsigned char arr6[] = {'b', 'a', 'z', 0x00};
  auto baz = Expr_MEncodedStringToSymbolExpr(arr6);
  metadata[baz] = 179;
  Expr_Insert(e1, 1 + 1, baz);
  Expr_Release(baz);

  const unsigned char arr1[] = {'H', 'e', 'l', 'l', 'o', '!'};
  auto str1 = Expr_UTF8BytesToStringExpr(arr1, 6);
  metadata[str1] = 188;
  Expr_Insert(e1, 2 + 1, str1);
  Expr_Release(str1);

  // UTF-8 for \[Alpha]
  const unsigned char arr2[] = {0xce, 0xb1};
  auto str2 = Expr_UTF8BytesToStringExpr(arr2, 2);
  metadata[str2] = 197;
  Expr_Insert(e1, 3 + 1, str2);
  Expr_Release(str2);

  auto real1 = Expr_FromReal64(1.23);
  metadata[real1] = 203;
  Expr_Insert(e1, 4 + 1, real1);
  Expr_Release(real1);

  const char *cstr1 = "1234567890123456789012345678901234567890";
  auto bigInt1 = Expr_CStringToIntegerExpr(cstr1, 40, 0);
  metadata[bigInt1] = 215;
  Expr_Insert(e1, 5 + 1, bigInt1);
  Expr_Release(bigInt1);
  
  // UTF-8 for \[Beta]
  const unsigned char arr3[] = {0xce, 0xb2, 0x00};
  auto beta = Expr_MEncodedStringToSymbolExpr(arr3);
  metadata[beta] = 224;
  Expr_Insert(e1, 6 + 1, beta);
  Expr_Release(beta);

  auto p = Expr_Pointer(e1);
  MArgument_setInteger(Res, p);
  
  return LIBRARY_NO_ERROR;
}


DLLEXPORT int Get_LibraryLink(WolframLibraryData libData, mint Argc, MArgument *Args, MArgument Res) {

  auto arg1 = MArgument_getInteger(Args[0]);

  auto e1 = Expr_FromPointer(reinterpret_cast<void *>(arg1));

  auto search = metadata.find(e1);
  int res;
  if (search != metadata.end()) {
      res = search->second;
  } else {
      res = 0;
  }

  MArgument_setInteger(Res, res);

  return LIBRARY_NO_ERROR;
}

#endif // USE_MATHLINK
