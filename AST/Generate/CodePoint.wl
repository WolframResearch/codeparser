BeginPackage["AST`Generate`CodePoint`"]

Begin["`Private`"]

Needs["AST`Generate`GenerateSources`"]

Print[OutputForm["Generating CodePoint..."]]


(*
\r\n is technically multi-byte...
*)
mbNewlines = toGlobal /@ ( ( ("CodePoint`LongName`"<>#)& /@ importedNewlineLongNames) ~Join~
  { CodePoint`CRLF } )


punctuationSource = 
  {"std::unordered_set<int32_t> punctuationCodePoints {"} ~Join~
    (Row[{toGlobal["CodePoint`LongName`"<>#], ","}]& /@ importedPunctuationLongNames) ~Join~
    {"};", "",
    "bool Utils::isMBPunctuation(int32_t point) { return punctuationCodePoints.find(point) != punctuationCodePoints.end(); }", ""}

whitespaceSource = 
  {"std::unordered_set<int32_t> whitespaceCodePoints {"} ~Join~
    (Row[{toGlobal["CodePoint`LongName`"<>#], ","}]& /@ importedWhitespaceLongNames) ~Join~
    {"};", "",
    "bool Utils::isMBWhitespace(int32_t point) { return whitespaceCodePoints.find(point) != whitespaceCodePoints.end(); }", ""}

newlineSource = 
  {"std::unordered_set<int32_t> newlineCodePoints {"} ~Join~
    (Row[{#, ","}]& /@ mbNewlines) ~Join~
    {"};", "",
    "bool Utils::isMBNewline(int32_t point) { return newlineCodePoints.find(point) != newlineCodePoints.end();}", ""}

uninterpretableSource = 
  {"std::unordered_set<int32_t> uninterpretableCodePoints {"} ~Join~
    (Row[{toGlobal["CodePoint`LongName`"<>#], ","}]& /@ importedUninterpretableLongNames) ~Join~
    {"};", "",
    "bool Utils::isMBUninterpretable(int32_t point) { return uninterpretableCodePoints.find(point) != uninterpretableCodePoints.end(); }", ""}

unsupportedSource = 
  {"std::unordered_set<std::string> unsupportedLongNames {"} ~Join~
    (Row[{"\""<>#<>"\"", ","}]& /@ importedUnsupportedLongNames) ~Join~
    {"};", "",
    "bool Utils::isUnsupportedLongName(std::string name) { return unsupportedLongNames.find(name) != unsupportedLongNames.end(); }", ""}

LongNameCodePointToOperatorSource = 
  {"TokenEnum LongNameCodePointToOperator(int32_t c) {
switch (c) {"} ~Join~
    (Row[{"case", " ", toGlobal["CodePoint`LongName`"<>#], ":", " ", "return", " ", toGlobal["Token`LongName`"<>#], ";"}]& /@ importedPunctuationLongNames) ~Join~
    {"default:
assert(false && \"Need to add operator\");
return TOKEN_UNKNOWN;
}
}"}

LongNameOperatorToCodePointSource = 
  {"
int32_t LongNameOperatorToCodePoint(TokenEnum t) {
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


#include \"Utils.h\" // for isMBPunctuation, etc.
#include \"CodePoint.h\"
#include \"LongNameDefines.h\"

#include <unordered_set> // for unordered_set
#include <cassert>
"}, punctuationSource, whitespaceSource, newlineSource, uninterpretableSource, unsupportedSource,
    LongNameCodePointToOperatorSource, 
    LongNameOperatorToCodePointSource]

Print[OutputForm["exporting CodePoint.cpp"]]
res = Export[FileNameJoin[{generatedCPPSrcDir, "CodePoint.cpp"}], Column[codePointCPPSource], "String"]

If[FailureQ[res],
  Print[res];
  Quit[1]
]

Print[OutputForm["Done CodePoint"]]

End[]

EndPackage[]
