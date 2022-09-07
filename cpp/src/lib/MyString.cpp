
#include "MyString.h"

#include "ParserSession.h"

#if USE_MATHLINK
#include "mathlink.h"
#undef P
#endif // USE_MATHLINK

#if USE_EXPR_LIB
#include "ExprLibrary.h"
#endif // USE_EXPR_LIB

#include <cassert>
#include <cstring> // for strcmp

using Buffer = const unsigned char *;


bool operator==(MyString a, MyString b) {
    return a.Id == b.Id;
}

bool operator!=(MyString a, MyString b) {
    return a.Id != b.Id;
}

bool operator<(MyString a, MyString b) {
    return strcmp(a.Val, b.Val);
}

void MyString::print(std::ostream& s) const {
    s << Val;
}

#if USE_MATHLINK
void MyString::put(ParserSessionPtr session, MLINK callLink) const {
    if (!MLPutUTF8String(callLink, reinterpret_cast<Buffer>(Val), static_cast<int>(Len))) {
        assert(false);
    }
}
#endif // USE_MATHLINK

#if USE_EXPR_LIB
expr MyString::toExpr(ParserSessionPtr session) const {
    return Expr_UTF8BytesToStringExpr(reinterpret_cast<Buffer>(Val), Len);
}
#endif // USE_EXPR_LIB
