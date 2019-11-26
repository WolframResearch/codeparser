BeginPackage["AST`Generate`CodePoint`"]

Begin["`Private`"]

Needs["AST`Generate`"]

Print["Generating CodePoint..."]


(*
\r\n is technically multi-byte...
*)
mbNewlines = toGlobal /@ ( ( ("CodePoint`LongName`"<>#)& /@ importedNewlineLongNames) ~Join~
  { CodePoint`CRLF } )


punctuationSource = 
  {"std::unordered_set<int> punctuationCodePoints {"} ~Join~
    (Row[{toGlobal["CodePoint`LongName`"<>#], ","}]& /@ importedPunctuationLongNames) ~Join~
    {"};", "",
    "bool Utils::isMBPunctuation(int32_t point) { return punctuationCodePoints.find(point) != punctuationCodePoints.end(); }", ""}

spaceSource = 
  {"std::unordered_set<int> spaceCodePoints {"} ~Join~
    (Row[{toGlobal["CodePoint`LongName`"<>#], ","}]& /@ importedSpaceLongNames) ~Join~
    {"};", "",
    "bool Utils::isMBSpace(int32_t point) { return spaceCodePoints.find(point) != spaceCodePoints.end(); }", ""}

newlineSource = 
  {"std::unordered_set<int> newlineCodePoints {"} ~Join~
    (Row[{#, ","}]& /@ mbNewlines) ~Join~
    {"};", "",
    "bool Utils::isMBNewline(int32_t point) { return newlineCodePoints.find(point) != newlineCodePoints.end();}", ""}

uninterpretableSource = 
  {"std::unordered_set<int> uninterpretableCodePoints {"} ~Join~
    (Row[{toGlobal["CodePoint`LongName`"<>#], ","}]& /@ importedUninterpretableLongNames) ~Join~
    {"};", "",
    "bool Utils::isMBUninterpretable(int32_t point) { return uninterpretableCodePoints.find(point) != uninterpretableCodePoints.end(); }", ""}

LongNameCodePointToOperatorSource = 
  {"TokenEnum LongNameCodePointToOperator(int c) {
switch (c) {"} ~Join~
    (Row[{"case", " ", toGlobal["CodePoint`LongName`"<>#], ":", " ", "return", " ", toGlobal["Token`LongName`"<>#], ";"}]& /@ importedPunctuationLongNames) ~Join~
    {"default:
assert(false && \"Need to add operator\");
return TOKEN_UNKNOWN;
}
}"}

LongNameOperatorToCodePointSource = 
  {"
int LongNameOperatorToCodePoint(TokenEnum t) {
switch (t.value()) {"} ~Join~
    (Row[{"case", " ", toGlobal["Token`LongName`"<>#], ".value():", " ", "return", " ", toGlobal["CodePoint`LongName`"<>#], ";"}]& /@ importedPunctuationLongNames) ~Join~
{"default:
assert(false && \"Need to add operator\");
return CODEPOINT_UNKNOWN;
}
}
"}

codePointCPPSource = Join[{
"
//
// AUTO GENERATED FILE
// DO NOT MODIFY
//


#include \"Utils.h\"
#include \"CodePoint.h\"
#include \"LongNameDefines.h\"

#include <unordered_set>
#include <cassert>
"}, punctuationSource, spaceSource, newlineSource, uninterpretableSource,
    LongNameCodePointToOperatorSource, 
    LongNameOperatorToCodePointSource]

Print["exporting CodePoint.cpp"]
res = Export[FileNameJoin[{generatedCPPSrcDir, "CodePoint.cpp"}], Column[codePointCPPSource], "String"]

If[FailureQ[res],
  Print[res];
  Quit[1]
]

Print["Done CodePoint"]

End[]

EndPackage[]
