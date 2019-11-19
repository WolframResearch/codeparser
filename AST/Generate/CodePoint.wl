BeginPackage["AST`Generate`CodePoint`"]

Begin["`Private`"]

Needs["AST`Generate`"]

Print["Generating CodePoint..."]

punctuationSource = 
  {"std::unordered_set<int> punctuationCodePoints {"} ~Join~
    (Row[{toGlobal["CodePoint`LongName`"<>#], ","}]& /@ importedPunctuationLongNames) ~Join~
    {"};", "",
    "bool WLCharacter::isPunctuationCharacter() const { return punctuationCodePoints.find(to_point()) != punctuationCodePoints.end(); }", ""}

spaceSource = 
  {"std::unordered_set<int> spaceCodePoints {"} ~Join~
    (Row[{toGlobal["CodePoint`LongName`"<>#], ","}]& /@ importedSpaceLongNames) ~Join~
    {"};", "",
    "bool WLCharacter::isSpaceCharacter() const { return spaceCodePoints.find(to_point()) != spaceCodePoints.end(); }", ""}

newlineSource = 
  {"std::unordered_set<int> newlineCodePoints {"} ~Join~
    (Row[{toGlobal["CodePoint`LongName`"<>#], ","}]& /@ importedNewlineLongNames) ~Join~
    {"};", "",
    "bool WLCharacter::isNewlineCharacter() const { return newlineCodePoints.find(to_point()) != newlineCodePoints.end();}", ""}

uninterpretableSource = 
  {"std::unordered_set<int> uninterpretableCodePoints {"} ~Join~
    (Row[{toGlobal["CodePoint`LongName`"<>#], ","}]& /@ importedUninterpretableLongNames) ~Join~
    {"};", "",
    "bool WLCharacter::isUninterpretableCharacter() const { return uninterpretableCodePoints.find(to_point()) != uninterpretableCodePoints.end(); }", ""}

LongNameCodePointToOperatorSource = 
  {"TokenEnum LongNameCodePointToOperator(int c) {
switch (c) {"} ~Join~
    (Row[{"case", " ", toGlobal["CodePoint`LongName`"<>#], ":", " ", "return", " ", toGlobal["Token`LongName`"<>#], ";"}]& /@ importedPunctuationLongNames) ~Join~
    {"default:
assert(false && \"Need to add operator\");
return TOKEN_ERROR_UNKNOWN;
}
}"}

LongNameOperatorToCodePointSource = 
  {"
int LongNameOperatorToCodePoint(TokenEnum t) {
switch (t.value()) {"} ~Join~
    (Row[{"case", " ", toGlobal["Token`LongName`"<>#], ".value():", " ", "return", " ", toGlobal["CodePoint`LongName`"<>#], ";"}]& /@ importedPunctuationLongNames) ~Join~
{"default:
assert(false && \"Need to add operator\");
return CODEPOINT_ERROR_INTERNAL;
}
}
"}

codePointCPPSource = Join[{
"
//
// AUTO GENERATED FILE
// DO NOT MODIFY
//

#include \"CodePoint.h\"

#include \"LongNameDefines.h\"
#include \"CharacterDecoder.h\"

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
