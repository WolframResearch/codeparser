BeginPackage["AST`Generate`LongNames`"]

Begin["`Private`"]

Needs["AST`Generate`GenerateSources`"]



escapeString[s_] :=
  ToString[s, InputForm, CharacterEncoding -> "ASCII"]




Print[OutputForm["Generating LongNames..."]]

Check[
longNameDefines = ("constexpr codepoint " <> toGlobal["CodePoint`LongName`" <> #] <> "(" <> longNameToHexDigits[#] <> ");")& /@ Keys[importedLongNames]
,
Print[OutputForm["Message while generating LongNames"]];
Quit[1]
]







(*
\r\n is technically multi-byte...
*)
mbNewlines = toGlobal /@ ( ( ("CodePoint`LongName`"<>#)& /@ importedNewlineLongNames) ~Join~
  { CodePoint`CRLF } )


punctuationSource = 
  {"std::unordered_set<codepoint> punctuationCodePoints {"} ~Join~
    (Row[{toGlobal["CodePoint`LongName`"<>#], ","}]& /@ importedPunctuationLongNames) ~Join~
    {"};", "",
    "bool LongNames::isMBPunctuation(codepoint point) { return punctuationCodePoints.find(point) != punctuationCodePoints.end(); }", ""}

whitespaceSource = 
  {"std::unordered_set<codepoint> whitespaceCodePoints {"} ~Join~
    (Row[{toGlobal["CodePoint`LongName`"<>#], ","}]& /@ importedWhitespaceLongNames) ~Join~
    {"};", "",
    "bool LongNames::isMBWhitespace(codepoint point) { return whitespaceCodePoints.find(point) != whitespaceCodePoints.end(); }", ""}

newlineSource = 
  {"std::unordered_set<codepoint> newlineCodePoints {"} ~Join~
    (Row[{#, ","}]& /@ mbNewlines) ~Join~
    {"};", "",
    "bool LongNames::isMBNewline(codepoint point) { return newlineCodePoints.find(point) != newlineCodePoints.end();}", ""}

uninterpretableSource = 
  {"std::unordered_set<codepoint> uninterpretableCodePoints {"} ~Join~
    (Row[{toGlobal["CodePoint`LongName`"<>#], ","}]& /@ importedUninterpretableLongNames) ~Join~
    {"};", "",
    "bool LongNames::isMBUninterpretable(codepoint point) { return uninterpretableCodePoints.find(point) != uninterpretableCodePoints.end(); }", ""}

unsupportedSource = 
  {"std::unordered_set<std::string> unsupportedLongNames {"} ~Join~
    (Row[{"\""<>#<>"\"", ","}]& /@ importedUnsupportedLongNames) ~Join~
    {"};", "",
    "bool LongNames::isUnsupportedLongName(std::string name) { return unsupportedLongNames.find(name) != unsupportedLongNames.end(); }", ""}

LongNameCodePointToOperatorSource = 
  {"TokenEnum LongNameCodePointToOperator(codepoint c) {
switch (c) {"} ~Join~
    (Row[{"case", " ", toGlobal["CodePoint`LongName`"<>#], ":", " ", "return", " ", toGlobal["Token`LongName`"<>#], ";"}]& /@ importedPunctuationLongNames) ~Join~
    {"default:
assert(false && \"Need to add operator\");
return TOKEN_UNKNOWN;
}
}"}

LongNameOperatorToCodePointSource = 
  {"
codepoint LongNameOperatorToCodePoint(TokenEnum t) {
switch (t.value()) {"} ~Join~
    (Row[{"case", " ", toGlobal["Token`LongName`"<>#], ".value():", " ", "return", " ", toGlobal["CodePoint`LongName`"<>#], ";"}]& /@ importedPunctuationLongNames) ~Join~
{"default:
assert(false && \"Need to add operator\");
return CODEPOINT_UNKNOWN;
}
}
"}










longNamesCPPHeader = {
"
//
// AUTO GENERATED FILE
// DO NOT MODIFY
//

#pragma once

#include \"CodePoint.h\" // for codepoint

#include <map>
#include <string>

extern std::map<std::string, codepoint> LongNameToCodePointMap;
extern std::map<codepoint, std::string> CodePointToLongNameMap;

class LongNames {
public:
    
    static bool isMBPunctuation(codepoint point);

    static bool isMBWhitespace(codepoint point);

    static bool isMBNewline(codepoint point);

    static bool isMBUninterpretable(codepoint point);

    static bool isUnsupportedLongName(std::string s);

    //
    // Is this \\[Raw] something?
    //
    static bool isRaw(std::string LongNameStr);
};
"} ~Join~
longNameDefines ~Join~
{""}

Print[OutputForm["exporting LongNames.h"]]
res = Export[FileNameJoin[{generatedCPPIncludeDir, "LongNames.h"}], Column[longNamesCPPHeader], "String"]

If[FailureQ[res],
  Print[res];
  Quit[1]
]






longNameToCodePointMap = {
"std::map<std::string, codepoint> LongNameToCodePointMap {"} ~Join~
  (Row[{"{", escapeString[#], ",", " ", toGlobal["CodePoint`LongName`"<>#], "}", ","}]& /@ Keys[importedLongNames]) ~Join~
  {"};", ""}

codePointToLongNameMap = {
"std::map<codepoint, std::string> CodePointToLongNameMap {"} ~Join~
  (Row[{"{", toGlobal["CodePoint`LongName`"<>#], ",", " ", escapeString[#], "}", ","}] & /@ Keys[importedLongNames]) ~Join~
  {"};", ""}

rawSet = {
"std::unordered_set<std::string> RawSet {"} ~Join~
(Row[{"{", "\""<>#<>"\"", "}", ","}]& /@ importedRawLongNames) ~Join~
{"};",
"
bool LongNames::isRaw(std::string LongNameStr) {
  return RawSet.find(LongNameStr) != RawSet.end();
}
"}


longNamesCPPSource = {
"
//
// AUTO GENERATED FILE
// DO NOT MODIFY
//

#include \"LongNames.h\"

#include <unordered_set> // for unordered_set
#include <cassert>
"} ~Join~
longNameToCodePointMap ~Join~
codePointToLongNameMap ~Join~
rawSet ~Join~
punctuationSource ~Join~
whitespaceSource ~Join~
newlineSource ~Join~
uninterpretableSource ~Join~
unsupportedSource ~Join~
LongNameCodePointToOperatorSource ~Join~
LongNameOperatorToCodePointSource

Print[OutputForm["exporting LongNames.cpp"]]
res = Export[FileNameJoin[{generatedCPPSrcDir, "LongNames.cpp"}], Column[longNamesCPPSource], "String"]

If[FailureQ[res],
  Print[res];
  Quit[1]
]









Check[
longNames = ("\"" <> # <> "\", ")& /@ Keys[importedLongNames]
,
Print[OutputForm["Message while generating LongNames"]];
Quit[1]
]

longNamesWL = {
"
(*
AUTO GENERATED FILE
DO NOT MODIFY
*)

{"} ~Join~ longNames ~Join~ {
"Nothing
}
"	
}

Print[OutputForm["exporting LongNames.wl"]]
res = Export[FileNameJoin[{generatedWLDir, "LongNames.wl"}], Column[longNamesWL], "String"]

If[FailureQ[res],
  Print[res];
  Quit[1]
]




Print[OutputForm["Done LongNames"]]

End[]

EndPackage[]
