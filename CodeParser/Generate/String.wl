(* ::Package::"Tags"-><|"SuspiciousSessionSymbol" -> <|Enabled -> False|>|>:: *)

If[!MemberQ[$Path, #], PrependTo[$Path, #]]&[DirectoryName[$InputFileName, 3]]

BeginPackage["CodeParser`Generate`String`"]

Begin["`Private`"]

(*
Do not allow PacletManager to participate in finding `Generate` files

PacletManager will find e.g. CodeParser/Kernel/TokenEnum.wl when asked to find CodeParser`Generate`TokenEnum`

related issues: PACMAN-54
*)
Block[{Internal`PacletFindFile = Null&},
Needs["CodeParser`Generate`Common`"];
Needs["CodeTools`Generate`GenerateSources`"];
]


strings = Union[Join[
    {"AdditionalDescriptions"},
    {"InsertionText", "ReplacementText"},
    {"UnsafeCharacterEncoding_IncompleteUTF8Sequence",
      "UnsafeCharacterEncoding_StraySurrogate",
      "UnsafeCharacterEncoding_BOM",
      "UnsafeCharacterEncoding_Unknown"},
    (*
    SyntaxIssue Tags
    *)
    {"UnhandledCharacter", "UnsupportedCharacter",
      "UndocumentedCharacter", "UnexpectedEscapeSequence",
      "UnexpectedCharacter", "UnexpectedNewlineCharacter",
      "UnexpectedSpaceCharacter", "UnexpectedLetterlikeCharacter",
      "UndocumentedSlotSyntax", "UnexpectedImplicitTimes",
      "UnexpectedDot", "Comma", "UnexpectedSign", "Ambiguous"},
    (*
    FormatIssue Tags
    *)
    {"Ambiguous"},
    (*
    EncodingIssue Tags
    *)
    {"IncompleteUTF8Sequence", "StraySurrogate", "BOM",
      "UnexpectedCarriageReturn", "UnexpectedCharacter",
      "NonASCIICharacter"},
    (*
    Severities
    *)
    {"Remark", "Warning", "Error", "Fatal", "Formatting"}
  ]]



generate[] := (

Print["Generating String..."];

stringCPPHeader = {
"
//
// AUTO GENERATED FILE
// DO NOT MODIFY
//

#pragma once

#if USE_MATHLINK
#include \"mathlink.h\"
#undef P
#endif // USE_MATHLINK

#include <memory>
#include <ostream>
#include <cstddef> // for size_t

#if USE_EXPR_LIB
using expr = void *;
#endif // USE_EXPR_LIB

using Buffer = const unsigned char *;


//
//
//
class MyString {
public:

  const char *Val;
  const size_t Len;

  constexpr MyString(const char *Val, size_t Len) : Val(Val), Len(Len) {}

  void print(std::ostream& s) const;

#if USE_MATHLINK
  void put(MLINK mlp) const;
#endif // USE_MATHLINK

#if USE_EXPR_LIB
  expr toExpr() const;
#endif // USE_EXPR_LIB
};

bool operator<(MyString a, MyString b);

using MyStringPtr = std::unique_ptr<MyString>;

//
// All strings that are used by CodeParser
//"} ~Join~
(Row[{"extern", " ", "MyStringPtr", " ", toGlobal["String`"<>#], ";"}]& /@ strings) ~Join~
{""};

Print["exporting MyString.h"];
res = Export[FileNameJoin[{generatedCPPIncludeDir, "MyString.h"}], Column[stringCPPHeader], "String"];

Print[res];

If[FailureQ[res],
  Quit[1]
];

stringCPPSource = {
"
//
// AUTO GENERATED FILE
// DO NOT MODIFY
//

#include \"MyString.h\"

#if USE_EXPR_LIB
#include \"ExprLibrary.h\"
#endif // USE_EXPR_LIB

#include <cassert>

bool operator<(MyString a, MyString b) {
  return a.Val < b.Val;
}

void MyString::print(std::ostream& s) const {
    s << Val;
}

#if USE_MATHLINK
void MyString::put(MLINK mlp) const {
    if (!MLPutUTF8String(mlp, reinterpret_cast<Buffer>(Val), static_cast<int>(Len))) {
        assert(false);
    }
}
#endif // USE_MATHLINK

#if USE_EXPR_LIB
expr MyString::toExpr() const {
    return Expr_UTF8BytesToStringExpr(reinterpret_cast<Buffer>(Val), Len);
}
#endif // USE_EXPR_LIB
"} ~Join~

(Row[{"MyStringPtr", " ", toGlobal["String`"<>#], " = MyStringPtr(new MyString(\"", #, "\", ", StringLength[#], "));"}]& /@ strings) ~Join~

{""};

Print["exporting MyString.cpp"];
res = Export[FileNameJoin[{generatedCPPSrcDir, "MyString.cpp"}], Column[stringCPPSource], "String"];

Print[res];

If[FailureQ[res],
  Quit[1]
];

Print["Done String"]
)

If[!StringQ[script],
  Quit[1]
]
If[AbsoluteFileName[script] === AbsoluteFileName[$InputFileName],
generate[]
]

End[]

EndPackage[]
